--  Copyright (c) 2006-2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers;
with Ada.Streams;

with League.Stream_Element_Vectors;

package JWS.Integers is
--   pragma Preelaborate;

   type Value is private;

   function Literal (Text : String) return Value;

   function "and" (Left, Right : Value) return Value;
   function "or" (Left, Right : Value) return Value;
   function "xor" (Left, Right : Value) return Value;

   function "=" (Left, Right : Value) return Boolean;
   function ">" (Left, Right : Value) return Boolean;
   function "<" (Left, Right : Value) return Boolean;
   function ">=" (Left, Right : Value) return Boolean;
   function "<=" (Left, Right : Value) return Boolean;

   function "-" (Left : Value) return Value;
   function "+" (Left, Right : Value) return Value;
   function "-" (Left, Right : Value) return Value;
   function "*" (Left, Right : Value) return Value;
   function "/" (Left, Right : Value) return Value;
   function "mod" (Left, Right : Value) return Value;
   function "rem" (Left, Right : Value) return Value;
   function "**" (Left, Right : Value) return Value;
   function "abs" (Left : Value) return Value;
   function "not" (Left : Value) return Value;

   function Image (Left : Value) return String;
   function Hash (Left : Value) return Ada.Containers.Hash_Type;
   function Length (Left : Value) return Ada.Streams.Stream_Element_Count;
   function BER_Value (Data : Ada.Streams.Stream_Element_Array) return Value;
   function To_BER (Data : Value) return Ada.Streams.Stream_Element_Array;
   function RSASP1 (M, D, N : Value) return Value;
   --  m^d mod n

   function Zero return Value;
   function One  return Value;
   function Two  return Value;
   function Ten  return Value;

private

   type Value is new League.Stream_Element_Vectors.Stream_Element_Vector
     with null record;

end JWS.Integers;
