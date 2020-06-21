--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package JWS.RS256 is

--  With this package to enable RS256 support

   function Public_JWK
     (Raw_Key : Ada.Streams.Stream_Element_Array)
       return League.JSON.Objects.JSON_Object;

end JWS.RS256;
