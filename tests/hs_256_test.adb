--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Streams;

with League.Strings;
with League.Base_Codecs;

with JWS;

procedure HS_256_Test is
   function "+"
     (Item : Wide_Wide_String) return League.Strings.Universal_String
      renames League.Strings.To_Universal_String;

--  This data is from rfc7515 example.

   Header_Encoded : constant Wide_Wide_String :=
     "eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9";

   Payload_Encoded : constant Wide_Wide_String :=
     "eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFt" &
     "cGxlLmNvbS9pc19yb290Ijp0cnVlfQ";

   Signature_Encoded : constant Wide_Wide_String :=
     "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk";

   Secret_Encoded : constant Wide_Wide_String :=
     "AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ+EstJQLr/T+" &
     "1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9CAow==";

   Token : constant Wide_Wide_String := Header_Encoded &
     "." & Payload_Encoded &
     "." & Signature_Encoded;

   Secret : constant Ada.Streams.Stream_Element_Array :=
     League.Base_Codecs.From_Base_64 (+Secret_Encoded).To_Stream_Element_Array;

   Backward_Token : constant Wide_Wide_String :=
     "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" & "." &
     Payload_Encoded & "." &
     "SfgggA-oZk7ztlq1i8Uz5VhmPmustakoDa9wAf8uHyQ";

   Signature : JWS.JSON_Web_Signature;
   Ok        : Boolean;
   Backward  : JWS.JSON_Web_Signature;
   Result    : League.Strings.Universal_String;

begin

   Signature.Validate_Compact_Serialization
     (Value  => +Token,
      Secret => Secret,
      Valid  => Ok);

   pragma Assert (Ok, "Validation failed");

   Backward.Create
     (Header  => Signature.Header,
      Payload => Signature.Payload,
      Secret  => Secret);

   --  Backward token differs from the original, because Header has more
   --  compact representation then original.

   Result := Backward.Compact_Serialization;

   pragma Assert
     (Result.To_Wide_Wide_String = Backward_Token,
      "Invalid Compact_Serialization");

end HS_256_Test;
