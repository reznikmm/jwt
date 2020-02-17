--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

separate (JWS.RS256)
package body Read_DER is

   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Count;

   procedure Algorithm
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count);

   procedure Expect
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count;
      Item  : Ada.Streams.Stream_Element);

   procedure Get_Length
     (Input  : Ada.Streams.Stream_Element_Array;
      J      : in out Ada.Streams.Stream_Element_Count;
      Length : out Ada.Streams.Stream_Element_Count);

   function Get_Integer
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count)
        return JWS.Integers.Number;

   procedure PrivateKeyAlgorithmIdentifier
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count);

   function RSAPrivateKey
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count) return Private_Key;

   function PrivateKey
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count) return Private_Key;

   function OneAsymmetricKey
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count) return Private_Key;

   function Public_Key_String
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count) return Public_Key;

   function RSAPublicKey
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count) return Public_Key;

   ---------------
   -- Algorithm --
   ---------------

   procedure Algorithm
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count)
   is
      RSA_Id : constant Ada.Streams.Stream_Element_Array :=
        (16#06#, 16#09#, 16#2A#, 16#86#, 16#48#, 16#86#,
         16#F7#, 16#0D#, 16#01#, 16#01#, 16#01#);

      Null_Params : constant Ada.Streams.Stream_Element_Array :=
        (05, 00);  --  NULL
   begin
      for X of RSA_Id loop
         Expect (Input, J, X);
      end loop;

      for X of Null_Params loop
         Expect (Input, J, X);
      end loop;
   end Algorithm;

   ------------
   -- Expect --
   ------------

   procedure Expect
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count;
      Item  : Ada.Streams.Stream_Element) is
   begin
      if Input (J) = Item then
         J := J + 1;
      else
         raise Constraint_Error;
      end if;
   end Expect;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count)
        return JWS.Integers.Number
   is
      Length : Ada.Streams.Stream_Element_Count;
      Last   : Ada.Streams.Stream_Element_Count;
      Count  : Positive;
   begin
      Expect (Input, J, 2);  --  Integer
      Get_Length (Input, J, Length);
      Last := J + Length - 1;
      Count := JWS.Integers.BER_Length (Input (J .. Last));

      return Result : JWS.Integers.Number (1 .. Count) do
         JWS.Integers.BER_Decode (Input (J .. Last), Result);
         J := Last + 1;
      end return;
   end Get_Integer;

   ----------------
   -- Get_Length --
   ----------------

   procedure Get_Length
     (Input  : Ada.Streams.Stream_Element_Array;
      J      : in out Ada.Streams.Stream_Element_Count;
      Length : out Ada.Streams.Stream_Element_Count)
   is
   begin
      Length := Ada.Streams.Stream_Element_Count (Input (J));

      if Length >= 128 then
         declare
            P : Ada.Streams.Stream_Element_Array renames
              Input (J + 1 .. J + Length - 128);
         begin
            Length := 0;
            J := P'Last + 1;

            for B of P loop
               Length := Length * 256 + Ada.Streams.Stream_Element_Count (B);
            end loop;
         end;
      else
         J := J + 1;
      end if;
   end Get_Length;

   ----------------------
   -- OneAsymmetricKey --
   ----------------------

   function OneAsymmetricKey
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count) return Private_Key
   is
      Length  : Ada.Streams.Stream_Element_Count;
      Last    : Ada.Streams.Stream_Element_Count;
   begin
      Expect (Input, J, 16#30#);  --  SEQUENCE
      Get_Length (Input, J, Length);
      Last := J + Length;

      declare
         Version : constant JWS.Integers.Number := Get_Integer (Input, J);
      begin
         pragma Assert (Version in (1 => 0));
         PrivateKeyAlgorithmIdentifier (Input, J);
      end;

      return R : constant Private_Key := PrivateKey (Input, J) do
         pragma Assert (J = Last);
      end return;
   end OneAsymmetricKey;

   ----------------
   -- PrivateKey --
   ----------------

   function PrivateKey
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count) return Private_Key
   is
      Length : Ada.Streams.Stream_Element_Count;
      Last   : Ada.Streams.Stream_Element_Count;
   begin
      Expect (Input, J, 16#04#);  --  OCTET STRING
      Get_Length (Input, J, Length);
      Last := J + Length;

      return R : constant Private_Key := RSAPrivateKey (Input, J) do
         pragma Assert (J = Last);
      end return;
   end PrivateKey;


   -----------------------------------
   -- PrivateKeyAlgorithmIdentifier --
   -----------------------------------

   procedure PrivateKeyAlgorithmIdentifier
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count)
   is
      Length : Ada.Streams.Stream_Element_Count;
      Last   : Ada.Streams.Stream_Element_Count;
   begin
      Expect (Input, J, 16#30#);  --  SEQUENCE
      Get_Length (Input, J, Length);
      Last := J + Length;
      Algorithm (Input, J);
      pragma Assert (J = Last);
   end PrivateKeyAlgorithmIdentifier;

   -----------------------
   -- Public_Key_String --
   -----------------------

   function Public_Key_String
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count)
        return Public_Key
   is
      Length  : Ada.Streams.Stream_Element_Count;
      Last    : Ada.Streams.Stream_Element_Count;
   begin
      Expect (Input, J, 16#03#);  --  BIT STRING
      Get_Length (Input, J, Length);
      Last := J + Length;

      Expect (Input, J, 16#00#);  --  EOC

      return R : constant Public_Key := RSAPublicKey (Input, J) do
         pragma Assert (J = Last);
      end return;
   end Public_Key_String;

   ----------------------
   -- Read_Private_Key --
   ----------------------

   function Read_Private_Key
     (Input : Ada.Streams.Stream_Element_Array)
       return Private_Key
   is
      J : Ada.Streams.Stream_Element_Count := Input'First;
   begin
      return OneAsymmetricKey (Input, J);
   end Read_Private_Key;

   ---------------------
   -- Read_Public_Key --
   ---------------------

   function Read_Public_Key
     (Input : Ada.Streams.Stream_Element_Array)
      return Public_Key
   is
      J : Ada.Streams.Stream_Element_Count := Input'First;

      Length  : Ada.Streams.Stream_Element_Count;
      Last    : Ada.Streams.Stream_Element_Count;
   begin
      Expect (Input, J, 16#30#);  --  SEQUENCE
      Get_Length (Input, J, Length);
      Last := J + Length;

      PrivateKeyAlgorithmIdentifier (Input, J);

      return R : constant Public_Key := Public_Key_String (Input, J) do
         pragma Assert (J = Last);
      end return;
   end Read_Public_Key;

   -------------------
   -- RSAPrivateKey --
   -------------------

   function RSAPrivateKey
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count) return Private_Key
   is
      Length  : Ada.Streams.Stream_Element_Count;
      Last    : Ada.Streams.Stream_Element_Count;
   begin
      Expect (Input, J, 16#30#);  --  SEQUENCE
      Get_Length (Input, J, Length);
      Last := J + Length;

      declare
         Version : constant JWS.Integers.Number := Get_Integer (Input, J);
      begin
         pragma Assert (Version in (1 => 0));
      end;

      declare
         Modulus          : constant JWS.Integers.Number :=
           Get_Integer (Input, J);
         Public_Exponent  : constant JWS.Integers.Number :=
           Get_Integer (Input, J);
         Private_Exponent : constant JWS.Integers.Number :=
           Get_Integer (Input, J);
         Prime_1          : constant JWS.Integers.Number :=
           Get_Integer (Input, J);
         Prime_2          : constant JWS.Integers.Number :=
           Get_Integer (Input, J);
         Exponent_1       : constant JWS.Integers.Number :=
           Get_Integer (Input, J);
         Exponent_2       : constant JWS.Integers.Number :=
           Get_Integer (Input, J);
         Coefficient      : constant JWS.Integers.Number :=
           Get_Integer (Input, J);
      begin
         pragma Assert (J = Last);

         return
           (N                => Modulus'Last,
            E                => Public_Exponent'Last,
            D                => Private_Exponent'Last,
            P                => Prime_1'Last,
            Q                => Prime_2'Last,
            E1               => Exponent_1'Last,
            E2               => Exponent_2'Last,
            C                => Coefficient'Last,
            Modulus          => Modulus,
            Public_Exponent  => Public_Exponent,
            Private_Exponent => Private_Exponent,
            Prime_1          => Prime_1,
            Prime_2          => Prime_2,
            Exponent_1       => Exponent_1,
            Exponent_2       => Exponent_2,
            Coefficient      => Coefficient);
      end;
   end RSAPrivateKey;

   ------------------
   -- RSAPublicKey --
   ------------------

   function RSAPublicKey
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count) return Public_Key
   is
      Length  : Ada.Streams.Stream_Element_Count;
      Last    : Ada.Streams.Stream_Element_Count;
   begin
      Expect (Input, J, 16#30#);  --  SEQUENCE
      Get_Length (Input, J, Length);
      Last := J + Length;

      declare
         Modulus  : constant JWS.Integers.Number := Get_Integer (Input, J);
         Exponent : constant JWS.Integers.Number := Get_Integer (Input, J);
      begin
         pragma Assert (J = Last);

         return (N               => Modulus'Last,
                 E               => Exponent'Last,
                 Modulus         => Modulus,
                 Public_Exponent => Exponent);
      end;
   end RSAPublicKey;

end Read_DER;
