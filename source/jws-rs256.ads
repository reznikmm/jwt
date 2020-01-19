--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with JWS.Integers;

package JWS.RS256 is
   pragma Elaborate_Body;

   type Private_Key is record
      Modulus          : JWS.Integers.Value;  --  n
      Public_Exponent  : JWS.Integers.Value;  --  e
      Private_Exponent : JWS.Integers.Value;  --  d
      Prime_1          : JWS.Integers.Value;  --  p
      Prime_2          : JWS.Integers.Value;  --  q
      Exponent_1       : JWS.Integers.Value;  --  d mod (p-1)
      Exponent_2       : JWS.Integers.Value;  --  d mod (q-1)
      Coefficient      : JWS.Integers.Value;  --  (inverse of q) mod p
   end record;

   function Sign
     (Data : Ada.Streams.Stream_Element_Array;
      Key  : Private_Key) return Ada.Streams.Stream_Element_Array;

end JWS.RS256;
