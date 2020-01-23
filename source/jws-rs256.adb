--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with GNAT.SHA256;

with JWS.Integers;

package body JWS.RS256 is

   type Private_Key is record
      Modulus          : JWS.Integers.Value;  --  n
      Public_Exponent  : JWS.Integers.Value;  --  e
      Private_Exponent : JWS.Integers.Value;  --  d
      Prime_1          : JWS.Integers.Value;  --  p
      Prime_2          : JWS.Integers.Value;  --  q
      Exponent_1       : JWS.Integers.Value;  --  d mod (p-1)
      Exponent_2       : JWS.Integers.Value;  --  d mod (q-1)
      Coefficient      : JWS.Integers.Value;  --  (inverse of q) mod p
   end record;

   type Public_Key is record
      Modulus          : JWS.Integers.Value;  --  n
      Public_Exponent  : JWS.Integers.Value;  --  e
   end record;

   function Do_Sign
     (Data : Ada.Streams.Stream_Element_Array;
      Key  : Private_Key) return Ada.Streams.Stream_Element_Array;

   function Do_Validate
     (Data : Ada.Streams.Stream_Element_Array;
      Key  : Public_Key;
      Sign : Ada.Streams.Stream_Element_Array) return Boolean;


   package Read_DER is
      procedure Read_Private_Key
        (Input : Ada.Streams.Stream_Element_Array;
         Value : out Private_Key);

      procedure Read_Public_Key
        (Input : Ada.Streams.Stream_Element_Array;
         Value : out Public_Key);
   end Read_DER;

   function Signature
     (Data   : League.Stream_Element_Vectors.Stream_Element_Vector;
      Secret : Ada.Streams.Stream_Element_Array)
        return League.Stream_Element_Vectors.Stream_Element_Vector;

   function Validate
     (Data   : League.Stream_Element_Vectors.Stream_Element_Vector;
      Secret : Ada.Streams.Stream_Element_Array;
      Value  : League.Stream_Element_Vectors.Stream_Element_Vector)
      return Boolean;

   package body Read_DER is separate;

   procedure Encode
     (Data  : Ada.Streams.Stream_Element_Array;
      Value : out Ada.Streams.Stream_Element_Array);
   --  Calculate EMSA-PKCS1-V1_5-ENCODE from RFC 8017

   -------------
   -- Do_Sign --
   -------------

   function Do_Sign
     (Data : Ada.Streams.Stream_Element_Array;
      Key  : Private_Key) return Ada.Streams.Stream_Element_Array
   is
      k  : constant Ada.Streams.Stream_Element_Count :=
        JWS.Integers.Length (Key.Modulus);
      EM : Ada.Streams.Stream_Element_Array (1 .. k);
      M  : JWS.Integers.Value;
      S  : JWS.Integers.Value;
   begin
      Encode (Data, EM);
      M := JWS.Integers.BER_Value (EM);
      --  s = m^d mod n.
      S := JWS.Integers.RSASP1 (M, Key.Private_Exponent, Key.Modulus);

      return JWS.Integers.To_BER (S);
   end Do_Sign;

   -----------------
   -- Do_Validate --
   -----------------

   function Do_Validate
     (Data : Ada.Streams.Stream_Element_Array;
      Key  : Public_Key;
      Sign : Ada.Streams.Stream_Element_Array) return Boolean
   is
      use type Ada.Streams.Stream_Element_Array;
      use type Ada.Streams.Stream_Element_Offset;

      k  : constant Ada.Streams.Stream_Element_Count :=
        JWS.Integers.Length (Key.Modulus);
      EM : Ada.Streams.Stream_Element_Array (1 .. k);
      S  : JWS.Integers.Value;
      M  : JWS.Integers.Value;
   begin
      Encode (Data, EM);
      S := JWS.Integers.BER_Value (Sign);
      M := JWS.Integers.RSASP1 (S, Key.Public_Exponent, Key.Modulus);

      declare
         ME : constant Ada.Streams.Stream_Element_Array :=
           JWS.Integers.To_BER (M);
         Last : constant Ada.Streams.Stream_Element_Count :=
           EM'Last - ME'Length;
      begin
         return EM (Last + 1 .. EM'Last) = ME
           and EM (1 .. Last) = (1 .. Last => 0);
      end;
   end Do_Validate;

   ------------
   -- Encode --
   ------------

   procedure Encode
     (Data  : Ada.Streams.Stream_Element_Array;
      Value : out Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Array;
      use type Ada.Streams.Stream_Element_Offset;

      H : constant GNAT.SHA256.Binary_Message_Digest :=
        GNAT.SHA256.Digest (Data);
      T : constant Ada.Streams.Stream_Element_Array :=
        (16#30#, 16#31#, 16#30#, 16#0d#, 16#06#, 16#09#, 16#60#, 16#86#,
         16#48#, 16#01#, 16#65#, 16#03#, 16#04#, 16#02#, 16#01#, 16#05#,
         16#00#, 16#04#, 16#20#) & H;
   begin
      Value := (00, 01)
        & (1 .. Value'Length - T'Length - 3 => 16#FF#)
        & 00 & T;
      null;
   end Encode;

   ---------------
   -- Signature --
   ---------------

   function Signature
     (Data   : League.Stream_Element_Vectors.Stream_Element_Vector;
      Secret : Ada.Streams.Stream_Element_Array)
      return League.Stream_Element_Vectors.Stream_Element_Vector
   is
      Key : Private_Key;
   begin
      Read_DER.Read_Private_Key (Secret, Key);

      return League.Stream_Element_Vectors.To_Stream_Element_Vector
        (Do_Sign (Data.To_Stream_Element_Array, Key));
   end Signature;

   --------------
   -- Validate --
   --------------

   function Validate
     (Data   : League.Stream_Element_Vectors.Stream_Element_Vector;
      Secret : Ada.Streams.Stream_Element_Array;
      Value  : League.Stream_Element_Vectors.Stream_Element_Vector)
      return Boolean
   is
      Key : Public_Key;
   begin
      Read_DER.Read_Public_Key (Secret, Key);

      return Do_Validate
        (Data.To_Stream_Element_Array,
         Key,
         Value.To_Stream_Element_Array);
   end Validate;

begin
   RS256_Signature_Link := Signature'Access;
   RS256_Validation_Link := Validate'Access;
end JWS.RS256;
