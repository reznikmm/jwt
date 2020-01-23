--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with GNAT.SHA256;

with League.Base_Codecs;
with League.JSON.Arrays;
with League.JSON.Documents;
with League.JSON.Values;
with League.Text_Codecs;

package body JWS is

   function "+"
     (Item : Wide_Wide_String) return League.Strings.Universal_String
      renames League.Strings.To_Universal_String;

   procedure From_Base_64_URL
     (Data    : League.Strings.Universal_String;
      Value   : in out League.Stream_Element_Vectors.Stream_Element_Vector;
      Success : out Boolean);

   function To_Base_64_URL
     (Data : League.Stream_Element_Vectors.Stream_Element_Vector)
       return League.Strings.Universal_String;

   ---------------
   -- Algorithm --
   ---------------

   function Algorithm
     (Self : JOSE_Header'Class) return League.Strings.Universal_String
   is
   begin
      return Self.Value (+"alg").To_String;
   end Algorithm;

   ---------------------------
   -- Compact_Serialization --
   ---------------------------

   function Compact_Serialization
     (Self : JSON_Web_Signature'Class) return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;

      ASCII : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec (+"USASCII");
      Encoded_Payload : constant League.Strings.Universal_String :=
        To_Base_64_URL (Self.Payload);
      Raw_Header : constant League.Stream_Element_Vectors.Stream_Element_Vector
        := Self.Header.To_JSON_Document.To_JSON;
      Encoded_Header : constant League.Strings.Universal_String :=
        To_Base_64_URL (Raw_Header);
      Text : constant League.Strings.Universal_String :=
        Encoded_Header & "." & Encoded_Payload;
      Signature : constant League.Stream_Element_Vectors.Stream_Element_Vector
        := Self.Header.Compute_Signature
          (ASCII.Encode (Text), Self.Secret.To_Stream_Element_Array);
      Encoded_Signature : constant League.Strings.Universal_String :=
        To_Base_64_URL (Signature);
   begin
      return Text & "." & Encoded_Signature;
   end Compact_Serialization;

   -----------------------
   -- Compute_Signature --
   -----------------------

   function Compute_Signature
     (Self   : JOSE_Header'Class;
      Data   : League.Stream_Element_Vectors.Stream_Element_Vector;
      Secret : Ada.Streams.Stream_Element_Array)
      return League.Stream_Element_Vectors.Stream_Element_Vector
   is
      use type League.Strings.Universal_String;

      function SHA256
        (Text : Ada.Streams.Stream_Element_Array;
         Key  : String) return GNAT.SHA256.Binary_Message_Digest;

      ------------
      -- SHA256 --
      ------------

      function SHA256
        (Text : Ada.Streams.Stream_Element_Array;
         Key  : String) return GNAT.SHA256.Binary_Message_Digest
      is
         Context : GNAT.SHA256.Context :=
           GNAT.SHA256.HMAC_Initial_Context (Key);
      begin
         GNAT.SHA256.Update (Context, Text);
         return GNAT.SHA256.Digest (Context);
      end SHA256;


      Alg : constant League.Strings.Universal_String := Self.Algorithm;
   begin
      if Alg = +"HS256" then
         declare
            SK  : String (1 .. Secret'Length)
              with Import, Address => Secret'Address;

            Result : constant GNAT.SHA256.Binary_Message_Digest :=
              SHA256 (Data.To_Stream_Element_Array, SK);
         begin
            return League.Stream_Element_Vectors.To_Stream_Element_Vector
              (Result);
         end;

      elsif Alg = +"RS256" then

         return RS256_Signature_Link (Data, Secret);

      elsif Alg = +"none" then
         return League.Stream_Element_Vectors.Empty_Stream_Element_Vector;
      end if;

      raise Constraint_Error with "Unknown algorithm " & Alg.To_UTF_8_String;
   end Compute_Signature;

   ------------
   -- Create --
   ------------

   procedure Create
     (Self    : out JSON_Web_Signature'Class;
      Header  : JOSE_Header;
      Payload : Ada.Streams.Stream_Element_Array;
      Secret  : Ada.Streams.Stream_Element_Array) is
   begin
      Self.Header := Header;
      Self.Payload.Clear;
      Self.Payload.Append (Payload);
      Self.Secret.Clear;
      Self.Secret.Append (Secret);

      if not Self.Header.Contains (+"alg") then
         Self.Header.Insert
           (+"alg", League.JSON.Values.To_JSON_Value (+"none"));
      end if;
   end Create;

   --------------
   -- Critical --
   --------------

   function Critical (Self : JOSE_Header'Class)
      return League.String_Vectors.Universal_String_Vector
   is
      Result : League.String_Vectors.Universal_String_Vector;
      Crit   : constant League.JSON.Values.JSON_Value := Self.Value (+"crit");
      Vector : constant League.JSON.Arrays.JSON_Array := Crit.To_Array;
      Item   : League.JSON.Values.JSON_Value;
   begin
      for J in 1 .. Vector.Length loop
         Item := Vector.Element (J);

         if Item.Is_String then
            Result.Append (Item.To_String);
         end if;
      end loop;

      return Result;
   end Critical;

   ----------------------
   -- From_Base_64_URL --
   ----------------------

   procedure From_Base_64_URL
     (Data    : League.Strings.Universal_String;
      Value   : in out League.Stream_Element_Vectors.Stream_Element_Vector;
      Success : out Boolean)
   is
      Text : League.Strings.Universal_String := Data;
   begin
      case Text.Length mod 4 is
         when 2 =>
            Text.Append ("==");
         when 3 =>
            Text.Append ("=");
         when 0 =>
            null;
         when others =>
            Success := False;
            return;
      end case;

      League.Base_Codecs.From_Base_64_URL
        (Data    => Text,
         Value   => Value,
         Success => Success);
   end From_Base_64_URL;

   ------------
   -- Header --
   ------------

   not overriding function Header
     (Self : JSON_Web_Signature) return JOSE_Header is
   begin
      return Self.Header;
   end Header;

   -------------
   -- Payload --
   -------------

   function Payload
     (Self : JSON_Web_Signature'Class)
        return Ada.Streams.Stream_Element_Array is
   begin
      return Self.Payload.To_Stream_Element_Array;
   end Payload;

   --------------------
   -- Payload_Object --
   --------------------

   function Payload_Object
     (Self : JSON_Web_Signature'Class)
      return League.JSON.Objects.JSON_Object
   is
      Document : constant League.JSON.Documents.JSON_Document :=
        League.JSON.Documents.From_JSON (Self.Payload);
   begin
      return Document.To_JSON_Object;
   end Payload_Object;

   --------------------
   -- Payload_Vector --
   --------------------

   function Payload_Vector
     (Self : JSON_Web_Signature'Class)
      return League.Stream_Element_Vectors.Stream_Element_Vector is
   begin
      return Self.Payload;
   end Payload_Vector;

   -------------------
   -- Set_Algorithm --
   -------------------

   procedure Set_Algorithm
     (Self : in out JOSE_Header'Class; Value : League.Strings.Universal_String)
   is
   begin
      Self.Insert (+"alg", League.JSON.Values.To_JSON_Value (Value));
   end Set_Algorithm;

   ------------------
   -- Set_Critical --
   ------------------

   procedure Set_Critical
     (Self  : in out JOSE_Header'Class;
      Value : League.String_Vectors.Universal_String_Vector)
   is
      Vector : League.JSON.Arrays.JSON_Array;
   begin
      for J in 1 .. Value.Length loop
         Vector.Append (League.JSON.Values.To_JSON_Value (Value.Element (J)));
      end loop;

      Self.Insert (+"crit", Vector.To_JSON_Value);
   end Set_Critical;

   --------------------
   -- To_Base_64_URL --
   --------------------

   function To_Base_64_URL
     (Data : League.Stream_Element_Vectors.Stream_Element_Vector)
        return League.Strings.Universal_String
   is
      Result : constant League.Strings.Universal_String :=
        League.Base_Codecs.To_Base_64_URL (Data);
   begin
      if Result.Ends_With ("==") then
         return Result.Head_To (Result.Length - 2);
      elsif Result.Ends_With ("=") then
         return Result.Head_To (Result.Length - 1);
      else
         return Result;
      end if;
   end To_Base_64_URL;

   ------------------------------------
   -- Validate_Compact_Serialization --
   ------------------------------------

   procedure Validate_Compact_Serialization
     (Self   : out JSON_Web_Signature'Class;
      Value  : League.Strings.Universal_String;
      Secret : Ada.Streams.Stream_Element_Array;
      Valid  : out Boolean)
   is
      use type League.Strings.Universal_String;

      function Alg return Wide_Wide_String;

      function Alg return Wide_Wide_String is
      begin
         return Self.Header.Value (+"alg").To_String.To_Wide_Wide_String;
      end Alg;

      List : constant League.String_Vectors.Universal_String_Vector :=
        Value.Split ('.');

      Ok : Boolean;
      Document : League.JSON.Documents.JSON_Document;
      ASCII : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec (+"USASCII");
      Encoded_Payload : League.Strings.Universal_String;
      Raw_Header : League.Stream_Element_Vectors.Stream_Element_Vector;
      Encoded_Header : League.Strings.Universal_String;
      Text : League.Strings.Universal_String;
      Signature : League.Stream_Element_Vectors.Stream_Element_Vector;
      Encoded_Signature : League.Strings.Universal_String;

   begin
      Valid := False;
      --  1. Parse the JWS representation to extract the serialized values...
      if List.Length /= 3 then
         return;
      end if;

      Encoded_Header := List.Element (1);
      Encoded_Payload := List.Element (2);
      Encoded_Signature := List.Element (3);
      --  2. decode the encoded representation of the JWS Protected Header
      From_Base_64_URL
        (Data    => Encoded_Header,
         Value   => Raw_Header,
         Success => Ok);

      if not Ok then
         return;
      end if;

      --  3.  Verify that the resulting octet sequence is ... JSON object
      Document := League.JSON.Documents.From_JSON (Raw_Header);

      if not Document.Is_Object or Document.Is_Empty then
         return;
      end if;

      Self.Header := (Document.To_JSON_Object with null record);
      --  5. Verify that the implementation understands and can process...
      if not Self.Header.Value (+"alg").Is_String then
         return;
      elsif Alg not in "none" | "HS256"
        and (Alg /= "RS256" or RS256_Validation_Link = null)
      then
         return;
      elsif Self.Header.Critical.Length > 0 then
         --  Any critical extensions are not supported here
         return;
      end if;

      --  6.  decode the encoded representation of the JWS Payload
      From_Base_64_URL
        (Data    => Encoded_Payload,
         Value   => Self.Payload,
         Success => Ok);

      if not Ok then
         return;
      end if;

      --   7. decode the encoded representation of the JWS Signature
      From_Base_64_URL
        (Data    => Encoded_Signature,
         Value   => Signature,
         Success => Ok);

      if not Ok then
         return;
      end if;

      --  8. Validate the JWS Signature against the JWS Signing Input
      Text := Encoded_Header & "." & Encoded_Payload;

      if not Self.Header.Validate_Signature
        (ASCII.Encode (Text), Secret, Signature)
      then
         return;
      end if;

      Valid := True;
   end Validate_Compact_Serialization;

   ------------------------
   -- Validate_Signature --
   ------------------------

   function Validate_Signature
     (Self   : JOSE_Header'Class;
      Data   : League.Stream_Element_Vectors.Stream_Element_Vector;
      Secret : Ada.Streams.Stream_Element_Array;
      Value  : League.Stream_Element_Vectors.Stream_Element_Vector)
      return Boolean
   is
      use type League.Strings.Universal_String;

      Alg : constant League.Strings.Universal_String := Self.Algorithm;
   begin
      if Alg = +"HS256" then
         declare
            use type League.Stream_Element_Vectors.Stream_Element_Vector;
         begin
            return Self.Compute_Signature (Data, Secret) = Value;
         end;

      elsif Alg = +"RS256" then

         return RS256_Validation_Link (Data, Secret, Value);

      elsif Alg = +"none" then
         return Value.Is_Empty;
      end if;

      raise Constraint_Error with "Unknown algorithm " & Alg.To_UTF_8_String;
   end Validate_Signature;

end JWS;
