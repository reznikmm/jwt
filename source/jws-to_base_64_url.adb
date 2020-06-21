--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Base_Codecs;

function JWS.To_Base_64_URL
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
end JWS.To_Base_64_URL;
