separate (JWS.RS256)
package body Read_DER is

   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Count;
   use type JWS.Integers.Value;

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

   procedure Get_Integer
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count;
      Value : out JWS.Integers.Value);

   procedure PrivateKeyAlgorithmIdentifier
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count);

   procedure RSAPrivateKey
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count;
      Value : out Private_Key);

   procedure PrivateKey
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count;
      Value : out Private_Key);

   procedure OneAsymmetricKey
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count;
      Value : out Private_Key);

   procedure Public_Key_String
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count;
      Value : out Public_Key);

   procedure RSAPublicKey
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count;
      Value : out Public_Key);

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

   procedure Get_Integer
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count;
      Value : out JWS.Integers.Value)
   is
      Length : Ada.Streams.Stream_Element_Count;
      Last   : Ada.Streams.Stream_Element_Count;
   begin
      Expect (Input, J, 2);  --  Integer
      Get_Length (Input, J, Length);
      Last := J + Length - 1;
      Value := JWS.Integers.BER_Value (Input (J .. Last));
      J := Last + 1;
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

   procedure OneAsymmetricKey
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count;
      Value : out Private_Key)
   is
      Length  : Ada.Streams.Stream_Element_Count;
      Last    : Ada.Streams.Stream_Element_Count;
      Version : JWS.Integers.Value;
   begin
      Expect (Input, J, 16#30#);  --  SEQUENCE
      Get_Length (Input, J, Length);
      Last := J + Length;
      Get_Integer (Input, J, Version);
      pragma Assert (Version = JWS.Integers.Zero);
      PrivateKeyAlgorithmIdentifier (Input, J);
      PrivateKey (Input, J, Value);
      pragma Assert (J = Last);
   end OneAsymmetricKey;

   ----------------
   -- PrivateKey --
   ----------------

   procedure PrivateKey
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count;
      Value : out Private_Key)
   is
      Length : Ada.Streams.Stream_Element_Count;
      Last   : Ada.Streams.Stream_Element_Count;
   begin
      Expect (Input, J, 16#04#);  --  OCTET STRING
      Get_Length (Input, J, Length);
      Last := J + Length;
      RSAPrivateKey (Input, J, Value);
      pragma Assert (J = Last);
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

   procedure Public_Key_String
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count;
      Value : out Public_Key)
   is
      Length  : Ada.Streams.Stream_Element_Count;
      Last    : Ada.Streams.Stream_Element_Count;
   begin
      Expect (Input, J, 16#03#);  --  BIT STRING
      Get_Length (Input, J, Length);
      Last := J + Length;

      Expect (Input, J, 16#00#);  --  EOC
      RSAPublicKey (Input, J, Value);
      pragma Assert (J = Last);
   end Public_Key_String;

   ----------------------
   -- Read_Private_Key --
   ----------------------

   procedure Read_Private_Key
     (Input : Ada.Streams.Stream_Element_Array;
      Value : out Private_Key)
   is
      J : Ada.Streams.Stream_Element_Count := Input'First;
   begin
      OneAsymmetricKey (Input, J, Value);
   end Read_Private_Key;

   ---------------------
   -- Read_Public_Key --
   ---------------------

   procedure Read_Public_Key
     (Input : Ada.Streams.Stream_Element_Array;
      Value : out Public_Key)
   is
      J : Ada.Streams.Stream_Element_Count := Input'First;

      Length  : Ada.Streams.Stream_Element_Count;
      Last    : Ada.Streams.Stream_Element_Count;
   begin
      Expect (Input, J, 16#30#);  --  SEQUENCE
      Get_Length (Input, J, Length);
      Last := J + Length;

      PrivateKeyAlgorithmIdentifier (Input, J);
      Public_Key_String (Input, J, Value);
      pragma Assert (J = Last);
   end Read_Public_Key;

   -------------------
   -- RSAPrivateKey --
   -------------------

   procedure RSAPrivateKey
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count;
      Value : out Private_Key)
   is
      Length  : Ada.Streams.Stream_Element_Count;
      Last    : Ada.Streams.Stream_Element_Count;
      Version : JWS.Integers.Value;
   begin
      Expect (Input, J, 16#30#);  --  SEQUENCE
      Get_Length (Input, J, Length);
      Last := J + Length;
      Get_Integer (Input, J, Version);
      pragma Assert (Version = JWS.Integers.Zero);
      Get_Integer (Input, J, Value.Modulus);
      Get_Integer (Input, J, Value.Public_Exponent);
      Get_Integer (Input, J, Value.Private_Exponent);
      Get_Integer (Input, J, Value.Prime_1);
      Get_Integer (Input, J, Value.Prime_2);
      Get_Integer (Input, J, Value.Exponent_1);
      Get_Integer (Input, J, Value.Exponent_2);
      Get_Integer (Input, J, Value.Coefficient);
      pragma Assert (J = Last);
   end RSAPrivateKey;

   ------------------
   -- RSAPublicKey --
   ------------------

   procedure RSAPublicKey
     (Input : Ada.Streams.Stream_Element_Array;
      J     : in out Ada.Streams.Stream_Element_Count;
      Value : out Public_Key)
   is
      Length  : Ada.Streams.Stream_Element_Count;
      Last    : Ada.Streams.Stream_Element_Count;
   begin
      Expect (Input, J, 16#30#);  --  SEQUENCE
      Get_Length (Input, J, Length);
      Last := J + Length;
      Get_Integer (Input, J, Value.Modulus);
      Get_Integer (Input, J, Value.Public_Exponent);
      pragma Assert (J = Last);
   end RSAPublicKey;

end Read_DER;
