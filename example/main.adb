--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with JWS;
with League.Base_Codecs;
with League.Stream_Element_Vectors;
with League.Strings;

procedure Main is
   T : constant Wide_Wide_String :=
     "eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9" &
     "." &
     "eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFt" &
     "cGxlLmNvbS9pc19yb290Ijp0cnVlfQ" &
     "." &
     "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk";
   K : constant Wide_Wide_String :=
     "AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ+EstJQLr/T+" &
     "1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9CAow==";
   X : JWS.JSON_Web_Signature;
   V : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String (T);
   Valid : Boolean;
   Key : constant League.Stream_Element_Vectors.Stream_Element_Vector :=
     League.Base_Codecs.From_Base_64
       (League.Strings.To_Universal_String (K));
begin
   X.Validate_Compact_Serialization
     (Value  => V,
      Secret => Key,
      Valid  => Valid);
end Main;
