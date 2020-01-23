--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Streams;

with League.Strings;
with League.Base_Codecs;

with JWS;
with JWS.RS256; pragma Unreferenced (JWS.RS256);

procedure RS_256_Test is
   function "+"
     (Item : Wide_Wide_String) return League.Strings.Universal_String
      renames League.Strings.To_Universal_String;

--  This data is from rfc7515 example.

   Header_Encoded : constant Wide_Wide_String :=
     "eyJhbGciOiJSUzI1NiJ9";

   Payload_Encoded : constant Wide_Wide_String :=
     "eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFt" &
     "cGxlLmNvbS9pc19yb290Ijp0cnVlfQ";

   Signature_Encoded : constant Wide_Wide_String :=
     "cC4hiUPoj9Eetdgtv3hF80EGrhuB__dzERat0XF9g2VtQgr9PJbu3XOiZj5RZmh7" &
     "AAuHIm4Bh-0Qc_lF5YKt_O8W2Fp5jujGbds9uJdbF9CUAr7t1dnZcAcQjbKBYNX4" &
     "BAynRFdiuB--f_nZLgrnbyTyWzO75vRK5h6xBArLIARNPvkSjtQBMHlb1L07Qe7K" &
     "0GarZRmB_eSN9383LcOLn6_dO--xi12jzDwusC-eOkHWEsqtFZESc6BfI7noOPqv" &
     "hJ1phCnvWh6IeYI2w9QOYEUipUTI8np6LbgGY9Fs98rqVt5AXLIhWkWywlVmtVrB" &
     "p0igcN_IoypGlUPQGe77Rw";

   Public_Key_Encoded : constant Wide_Wide_String :=
     "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAofgWCuLjybRlzo0tZWJj" &
     "NiuSfb4p4fAkd/wWJcyQoTbji9k0l8W26mPddxHmfHQp+Vaw+4qPCJrcS2mJPMEz" &
     "P1Pt0Bm4d4QlL+yRT+SFd2lZS+pCgNMsD1W/YpRPEwOWvG6b32690r2jZ47soMZo" &
     "9wGzjb/7OMg0LOL+bSf63kpaSHSXndS5z5rexMdbBYUsLA9e+KXBdQOS+UTo7WTB" &
     "EMa2R2CapHg665xsmtdVMTBQY4uDZlxvb3qCo5ZwKh9kG4LT6/I5IhlJH7aGhyxX" &
     "FvUK+DWNmoudF8NAco9/h9iaGNj8q2ethFkMLs91kzk2PAcDTW9gb54h4FRWyuXp" &
     "oQIDAQAB";

   Token : constant Wide_Wide_String := Header_Encoded &
     "." & Payload_Encoded &
     "." & Signature_Encoded;

   Public_Key : constant Ada.Streams.Stream_Element_Array :=
     League.Base_Codecs.From_Base_64
       (+Public_Key_Encoded).To_Stream_Element_Array;

   pragma Style_Checks (Off);
   Private_Key_Encoded : constant Wide_Wide_String :=
     "MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQCh+BYK4uPJtGXOjS1lYmM2K5J9" &
     "vinh8CR3/BYlzJChNuOL2TSXxbbqY913EeZ8dCn5VrD7io8ImtxLaYk8wTM/U+3QGbh3hCUv7JFP" &
     "5IV3aVlL6kKA0ywPVb9ilE8TA5a8bpvfbr3SvaNnjuygxmj3AbONv/s4yDQs4v5tJ/reSlpIdJed" &
     "1LnPmt7Ex1sFhSwsD174pcF1A5L5ROjtZMEQxrZHYJqkeDrrnGya11UxMFBji4NmXG9veoKjlnAq" &
     "H2QbgtPr8jkiGUkftoaHLFcW9Qr4NY2ai50Xw0Byj3+H2JoY2PyrZ62EWQwuz3WTOTY8BwNNb2Bv" &
     "niHgVFbK5emhAgMBAAECggEAEq5xpGnNCivDflJsRQBXHx1hdR1k6Ulwe2JZD50LpXyWPEAeP88v" &
     "LNO97IjlA7/GQ5sLKMgvfTeXZx9SE+7YwVol2NXOoAJe46sui395IW/GO+pWJ1O0BkTGoVEn2bKV" &
     "RUCgu+GjBVaYLU6f3l9kJfFNS3E0QbVdxzubSu3Mkqzjkn439X0M/V51gfpRLI9JYanrC4D4qAdG" &
     "copV/0ZHHzQlBjudU2QvXt4ehNYTCBr6XCLQUShb1juUO1ZdiYoFaFQT5Tw8bGUl/x/jTj3ccPDV" &
     "ZFD9pIuhLhBOneufuBiB4cS98l2SR/RQyGWSeWjnczT0QU91p1DhOVRuOopznQKBgQDgHMQQ60im" &
     "ZV1URk0KpLttoLhyt3SmqdEf/kgHePDdtzEafpAu+cS191R2JiuoF2yzWXnwFDcqGigp5BNr0AHQ" &
     "fD8/Lk8l1Mk09jxxCfui5nYooNyac8YFjm3vItzVCVDnEd3BbVWG8qf6deqElMGAg+C2V0L4oNXn" &
     "P7LZcPAZRwKBgQC5A8R+CZW2MvRTLLHzwZkUXV866cff7tx6kqa0finGG0KgeqlaZPxgCZnFp7Ae" &
     "AcCMtiynVlJ7vMVgePsLq6XtON4tB5kP9jAPrq3rbAOal78eUH5OcED6eNuCV8ixEu1eWcPNCS/l" &
     "1OW1EnXUEoEHKl54Xrrz2uNwkgMTP1FZ1wKBgAcCn1dwJKufzBWQxWQp1vsM5fggqPN1qGb5y0MA" &
     "k3g7/Ls5bkUp5u9SN0Ai3Ya6hNnvWJMb7sXQX6U/zyO2M/hTip7tUeh7CXgwo59dkpN75gJLVds2" &
     "9+DAncu3KXU4f2Fa+7bLNrur53k8KwPOq2bbuTG69QtV7Jr5MR0AHWKNAoGAh/96+mK1R/7glhsu" &
     "m81dZxjTnYynPbZpHziZjeeHcXYsXaaMwkOlODsWa7I9xXDoRwbKgB719rrmI2oKr6N3Do9U0aja" &
     "HF+NKJnwgjMd2w9cjz3/+kyNlxAr2v4IKhGNpmM5iIgOS1VZnOZ68m6/pbLBSp3nssTdlqvd0tIi" &
     "THUCgYEAIYd7DHOhrWvxkwPQsRM2tOgrjbcrfvtQJipd+DlcxyVuuM9sQLdgjVk2oy26F0EmpScG" &
     "Lq2MowX7fhd/QJQ3ydy5cY7YIBi87w93IKLEdfnbJtoOPLUW0ITrJReOgo1cq9SbsxYawBgfp/gh" &
     "6A5603k2+ZQwVK0JKSHuLFkuQ3U=";
   pragma Style_Checks (On);

   Private_Key : constant Ada.Streams.Stream_Element_Array :=
     League.Base_Codecs.From_Base_64
       (+Private_Key_Encoded).To_Stream_Element_Array;

   Signature : JWS.JSON_Web_Signature;
   Ok        : Boolean;
   Backward  : JWS.JSON_Web_Signature;
   Result    : League.Strings.Universal_String;

begin

   Signature.Validate_Compact_Serialization
     (Value  => +Token,
      Secret => Public_Key,
      Valid  => Ok);

   pragma Assert (Ok, "Validation failed");

   Backward.Create
     (Header  => Signature.Header,
      Payload => Signature.Payload,
      Secret  => Private_Key);

   Result := Backward.Compact_Serialization;

   pragma Assert
     (Result.To_Wide_Wide_String = Token,
      "Invalid Compact_Serialization");

end RS_256_Test;
