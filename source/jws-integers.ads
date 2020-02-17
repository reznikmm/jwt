--  Copyright (c) 2006-2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package JWS.Integers is

   Digit_Size : constant := 32;

   type Digit is mod 2 ** Digit_Size;

   type Number is array (Positive range <>) of Digit
     with Dynamic_Predicate => Number'First = 1;
   --  Size is number of Digits in the Number

   function BER_Length
     (Raw   : Ada.Streams.Stream_Element_Array) return Positive;
   --  Return length of Number for given BER encoding

   procedure BER_Decode
     (Raw   : Ada.Streams.Stream_Element_Array;
      Value : out Number);
   --  Decode BER integer into Value

   procedure BER_Encode
     (Value : Number;
      Raw   : out Ada.Streams.Stream_Element_Array);
   --  Encode number into BER integer

   procedure Add
     (A, B   : Number;
      Result : out Number)
     with Pre => A'Length = B'Length and Result'Length = A'Length;
   --  Result is A + B

   procedure Add
     (A, B   : Number;
      Result : out Number;
      C      : Digit)
     with Pre => A'Length = B'Length and Result'Length = A'Length + 1;
   --  Result is A + B * C
   --  Result'Address can be A'Address or B'Address

   procedure Subtract
     (A, B   : Number;
      Result : out Number;
      C      : Digit := 1;
      Ok     : out Boolean)
     with Pre => A'Length = B'Length + 1 and Result'Length = A'Length;
   --  Result is A - B * C, Ok is A >= B * C

   procedure Multiply
     (A, B   : Number;
      Result : out Number)
     with Pre => Result'Length = A'Length + B'Length;
   --  Result is A * B

   procedure Fast_Devide
     (A      : Number;
      B      : Digit;
      Result : out Number;
      Rest   : out Digit)
     with Pre => Result'Length = A'Length and B /= 0;
   --  Result is A /B, while Rest is A mod B

   procedure Remainder
     (A, B   : Number;
      Result : out Number)
     with Pre => B /= (B'Range => 0) and Result'Length = B'Length;
   --  Result is A mod B

   procedure Power
     (Value    : Number;
      Exponent : Number;
      Module   : Number;
      Result   : out Number)
     with Pre => Result'Length = Module'Length;
   --  Result is (Value ** Exponent) mod Module

end JWS.Integers;
