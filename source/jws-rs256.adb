--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with GNAT.SHA256;

package body JWS.RS256 is

   procedure Read_DER
     (Input : Ada.Streams.Stream_Element_Array;
      Value : out Private_Key);

   function Signature
     (Data   : League.Stream_Element_Vectors.Stream_Element_Vector;
      Secret : Ada.Streams.Stream_Element_Array)
        return League.Stream_Element_Vectors.Stream_Element_Vector;

   procedure Read_DER
     (Input : Ada.Streams.Stream_Element_Array;
      Value : out Private_Key) is separate;

   function Sign
     (Data : Ada.Streams.Stream_Element_Array;
      Key  : Private_Key) return Ada.Streams.Stream_Element_Array
   is
      use type Ada.Streams.Stream_Element_Array;
      use type Ada.Streams.Stream_Element_Count;

      procedure Encode
        (Data  : Ada.Streams.Stream_Element_Array;
         Value : out Ada.Streams.Stream_Element_Array);

      ------------
      -- Encode --
      ------------

      procedure Encode
        (Data  : Ada.Streams.Stream_Element_Array;
         Value : out Ada.Streams.Stream_Element_Array)
      is
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
   end Sign;

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
      Read_DER (Secret, Key);

      return League.Stream_Element_Vectors.To_Stream_Element_Vector
        (Sign (Data.To_Stream_Element_Array, Key));
   end Signature;

begin
   RS256_Soft_Link := Signature'Access;
end JWS.RS256;
