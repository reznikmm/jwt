separate (JWS.RS256)
procedure Read_DER
  (Input : Ada.Streams.Stream_Element_Array;
   Value : out Private_Key)
is
   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Count;
   use type JWS.Integers.Value;

   J : Ada.Streams.Stream_Element_Count := Input'First;

   procedure OneAsymmetricKey;
   procedure PrivateKeyAlgorithmIdentifier;
   procedure PrivateKey;
   procedure RSAPrivateKey;
   procedure Algorithm;
   procedure Expect (Item : Ada.Streams.Stream_Element);
   procedure Get_Length (Length : out Ada.Streams.Stream_Element_Count);
   procedure Get_Integer (Value : out JWS.Integers.Value);

   ---------------
   -- Algorithm --
   ---------------

   procedure Algorithm is
      RSA_Id : constant Ada.Streams.Stream_Element_Array :=
        (16#06#, 16#09#, 16#2A#, 16#86#, 16#48#, 16#86#,
         16#F7#, 16#0D#, 16#01#, 16#01#, 16#01#);

      Null_Params : constant Ada.Streams.Stream_Element_Array :=
        (05, 00);  --  NULL
   begin
      for X of RSA_Id loop
         Expect (X);
      end loop;

      for X of Null_Params loop
         Expect (X);
      end loop;
   end Algorithm;

   ------------
   -- Expect --
   ------------

   procedure Expect (Item : Ada.Streams.Stream_Element) is
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

   procedure Get_Integer (Value : out JWS.Integers.Value) is
      Length : Ada.Streams.Stream_Element_Count;
      Last   : Ada.Streams.Stream_Element_Count;
   begin
      Expect (2);  --  Integer
      Get_Length (Length);
      Last := J + Length - 1;
      Value := JWS.Integers.BER_Value (Input (J .. Last));
      J := Last + 1;
   end Get_Integer;

   ----------------
   -- Get_Length --
   ----------------

   procedure Get_Length (Length : out Ada.Streams.Stream_Element_Count) is
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

   procedure OneAsymmetricKey is
      Length  : Ada.Streams.Stream_Element_Count;
      Last    : Ada.Streams.Stream_Element_Count;
      Version : JWS.Integers.Value;
   begin
      Expect (16#30#);  --  SEQUENCE
      Get_Length (Length);
      Last := J + Length;
      Get_Integer (Version);
      pragma Assert (Version = JWS.Integers.Zero);
      PrivateKeyAlgorithmIdentifier;
      PrivateKey;
      pragma Assert (J = Last);
   end OneAsymmetricKey;

   procedure PrivateKey is
      Length : Ada.Streams.Stream_Element_Count;
      Last   : Ada.Streams.Stream_Element_Count;
   begin
      Expect (16#04#);  --  OCTET STRING
      Get_Length (Length);
      Last := J + Length;
      RSAPrivateKey;
      pragma Assert (J = Last);
   end PrivateKey;

   procedure PrivateKeyAlgorithmIdentifier is
      Length : Ada.Streams.Stream_Element_Count;
      Last   : Ada.Streams.Stream_Element_Count;
   begin
      Expect (16#30#);  --  SEQUENCE
      Get_Length (Length);
      Last := J + Length;
      Algorithm;
      pragma Assert (J = Last);
   end PrivateKeyAlgorithmIdentifier;

   procedure RSAPrivateKey is
      Length  : Ada.Streams.Stream_Element_Count;
      Last    : Ada.Streams.Stream_Element_Count;
      Version : JWS.Integers.Value;
   begin
      Expect (16#30#);  --  SEQUENCE
      Get_Length (Length);
      Last := J + Length;
      Get_Integer (Version);
      pragma Assert (Version = JWS.Integers.Zero);
      Get_Integer (Value.Modulus);
      Get_Integer (Value.Public_Exponent);
      Get_Integer (Value.Private_Exponent);
      Get_Integer (Value.Prime_1);
      Get_Integer (Value.Prime_2);
      Get_Integer (Value.Exponent_1);
      Get_Integer (Value.Exponent_2);
      Get_Integer (Value.Coefficient);
      pragma Assert (J = Last);
   end RSAPrivateKey;

begin
   OneAsymmetricKey;
end Read_DER;
