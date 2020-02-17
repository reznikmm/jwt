--  Copyright (c) 2006-2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body JWS.Integers is

   type Double is mod 2 ** (2 * Digit_Size);
   subtype Double_Digit is Double range 0 .. 2 ** Digit_Size - 1;

   procedure Devide
     (Left   : in out Number;
      Right  : in     Number;
      Result :    out Digit)
     with Pre => Left'Length = Right'Length + 1
       and Right'Length >= 2
       and Left (Right'Range) < Right;

   ---------
   -- Add --
   ---------

   procedure Add
     (A, B   : Number;
      Result : out Number)
   is
      Temp  : Double;
      Carry : Double_Digit := 0;
   begin
      for J in reverse 1 .. A'Length loop
         Temp := Double (A (J)) + Double (B (J)) + Carry;
         Result (J) := Digit'Mod (Temp);
         Carry := Temp / Digit'Modulus;
      end loop;
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (A, B : Number; Result : out Number; C : Digit) is
      Mult  : constant Double_Digit := Double_Digit (C);
      Temp  : Double;
      Carry : Double_Digit := 0;
   begin
      for J in reverse 1 .. A'Length loop
         Temp := Double (A (J)) + Double (B (J)) * Mult + Carry;
         Result (J + 1) := Digit'Mod (Temp);
         Carry := Temp / Digit'Modulus;
      end loop;

      Result (1) := Digit (Carry);
   end Add;

   ----------------
   -- BER_Decode --
   ----------------

   procedure BER_Decode
     (Raw   : Ada.Streams.Stream_Element_Array;
      Value : out Number)
   is
      use type Ada.Streams.Stream_Element_Offset;

      function N (X : Ada.Streams.Stream_Element_Offset) return Digit;

      -------
      -- N --
      -------

      function N (X : Ada.Streams.Stream_Element_Offset) return Digit is
      begin
         if X in Raw'Range then
            return Digit (Raw (X));
         else
            return 0;
         end if;
      end N;

      X : Ada.Streams.Stream_Element_Offset := Raw'Last;
   begin
      for J in reverse Value'Range loop
         Value (J) :=
           256 * (256 * (256 * N (X - 3) + N (X - 2)) + N (X - 1)) + N (X);

         X := X - 4;
      end loop;
   end BER_Decode;

   ----------------
   -- BER_Encode --
   ----------------

   procedure BER_Encode
     (Value : Number;
      Raw   : out Ada.Streams.Stream_Element_Array)
   is
      X : Ada.Streams.Stream_Element_Offset := Raw'Last;
      V : Digit;
   begin
      for J in reverse Value'Range loop
         V := Value (J);

         for K in 1 .. 4 loop
            Raw (X) := Ada.Streams.Stream_Element'Mod (V);
            V := V / 256;
            X := Ada.Streams.Stream_Element_Offset'Pred (X);
         end loop;
      end loop;
   end BER_Encode;

   ----------------
   -- BER_Length --
   ----------------

   function BER_Length
     (Raw   : Ada.Streams.Stream_Element_Array) return Positive
   is
      use type Ada.Streams.Stream_Element;

      Result : constant Natural := Raw'Length / 4;
   begin
      if Result = 0 then
         return 1;
      elsif Raw'Length = (Result - 1) * 4 + 1 and then Raw (Raw'First) = 0 then
         --  Last extra byte contains zero
         return Result - 1;
      else
         return Result;
      end if;
   end BER_Length;

   ------------
   -- Devide --
   ------------

   procedure Devide
     (Left   : in out Number;
      Right  : in     Number;
      Result :    out Digit)
   is
      function Get_Digit (Left, Right : Number) return Digit
        with Pre => Left'Length = Right'Length + 1
          and Right'Length >= 2
          and Left (Right'Range) < Right;

      function Get_Digit (Left, Right : Number) return Digit is
         QHAT   : Double_Digit;
         RHAT   : Double_Digit;
         Temp   : Double;
      begin
         if Left (1) = Right (1) then
            Temp := Double_Digit (Left (2)) + Double_Digit (Right (1));
            if Temp >= Digit'Modulus then
               return Digit'Last;
            end if;

            RHAT := Temp;
            QHAT := Digit'Modulus - 1;
         elsif  Left (1) < Right (1) then
            declare
               L12 : constant Double := Digit'Modulus * Double_Digit (Left (1))
                 + Double_Digit (Left (2));
            begin
               QHAT := L12 / Double_Digit (Right (1));  --  < Digit'Modulus
               RHAT := L12 mod Double_Digit (Right (1));
            end;
         else
            raise Program_Error;
         end if;

         while QHAT * Double_Digit (Right (2)) >
           RHAT * Digit'Modulus + Double_Digit (Left (3))
         loop
            QHAT := QHAT - 1;
            Temp := RHAT + Double_Digit (Right (1));
            exit when Temp >= Digit'Modulus;
            RHAT := Temp;
         end loop;

         return Digit (QHAT);
      end Get_Digit;

      Ok : Boolean;
   begin
      Result := Get_Digit (Left, Right);

      Subtract
        (A      => Left,
         B      => Right,
         Result => Left,
         C      => Result,
         Ok     => Ok);

      if not Ok then
         Result := Result - 1;

         Add
           (A      => Left,
            B      => 0 & Right,
            Result => Left);
      end if;
   end Devide;

   -----------------
   -- Fast_Devide --
   -----------------

   procedure Fast_Devide
     (A      : Number;
      B      : Digit;
      Result : out Number;
      Rest   : out Digit)
   is
      Div    : constant Double_Digit := Double_Digit (B);
      Carry  : Double_Digit := 0;
      Temp   : Double;
   begin
      for J in A'Range loop
         Temp := Carry * Digit'Modulus + Double_Digit (A (J));
         Result (J) := Digit (Temp / Div);
         Carry := Temp mod Div;
      end loop;

      Rest := Digit (Carry);
   end Fast_Devide;

   --------------
   -- Multiply --
   --------------

   procedure Multiply (A, B : Number; Result : out Number) is
      Mult : constant Number (Result'Range) := B & (A'Range => 0);
      Temp : Digit;
   begin
      Result (B'Range) := (B'Range => 0);

      for J in 1 .. A'Length loop
         Temp := A (A'Length - J + 1);
         Add
           (Result (1 .. B'Length + J - 1),
            Mult   (1 .. B'Length + J - 1),
            Result (1 .. B'Length + J),
            Temp);
      end loop;
   end Multiply;

   --------------------------
   -- Normalize_For_Devide --
   --------------------------

   procedure Normalize_For_Devide
     (A      : Number;
      B      : in out Number;
      A_Copy : in out Number;
      Mult   :    out Digit)
     with Pre => A_Copy'Length >= A'Length + 1;

   procedure Normalize_For_Devide
     (A      : Number;
      B      : in out Number;
      A_Copy : in out Number;
      Mult   :    out Digit)
   is
   begin
      Mult := Digit (Digit'Modulus / (Double_Digit (B (1)) + 1));

      if Mult = 1 then
         A_Copy := (1 .. A_Copy'Length - A'Length => 0) & A;
      else
         declare
            Zero : constant Number (1 .. A_Copy'Length - 1) := (others => 0);
         begin
            Add
              (A      => Zero,
               B      => (1 .. Zero'Length - A'Length => 0) & A,
               Result => A_Copy,
               C      => Mult);
         end;

         declare
            Temp : Number (1 .. B'Length + 1);
         begin
            Add
              (A      => (B'Range => 0),
               B      => B,
               Result => Temp,
               C      => Mult);

            pragma Assert (Temp (1) = 0);

            B (1 .. B'Last) := Temp (2 .. Temp'Last);
         end;
      end if;
   end Normalize_For_Devide;

   procedure Power
     (Value    : Number;
      Exponent : Number;
      Module   : Number;
      Result   : out Number)
   is
      Mult : Number (Module'Range);
      Mask : Digit := 1;
      J    : Natural := Exponent'Last;
   begin
      if Value'Length < Module'Length
        or else (Value'Length = Module'Length and then Value <= Module)
      then
         Mult := (1 .. Module'Length - Value'Length => 0) & Value;
      else
         Remainder (Value, Module, Mult);
      end if;

      Result := (1 .. Result'Last - 1 => 0) & 1;
      while J > 0 loop
         if (Exponent (J) and Mask) /= 0 then
            declare
               Temp : Number (1 .. Result'Length + Mult'Length);
            begin
               Multiply (Result, Mult, Temp);
               Remainder (Temp, Module, Result);
            end;
         end if;

         declare
            Temp : Number (1 .. 2 * Mult'Length);
         begin
            Multiply (Mult, Mult, Temp);
            Remainder (Temp, Module, Mult);
         end;

         if Mask = 2 ** (Digit_Size - 1) then
            J := J - 1;
            Mask := 1;
         else
            Mask := Mask * 2;
         end if;
      end loop;
   end Power;

   ---------------
   -- Remainder --
   ---------------

   procedure Remainder
     (A, B   : Number;
      Result : out Number)
   is
   begin
      if A (B'Last + 1 .. A'Last) = (B'Last + 1 .. A'Last => 0)
        and then A (B'Range) < B
      then
         Result := A (B'Range);
      elsif B'Length = 1 then
         declare
            Ignore : Number (A'Range);
         begin
            Fast_Devide (A, B (1), Ignore, Result (1));
         end;
      else
         declare
            Length : constant Positive :=
              Positive'Max (B'Length + 2, A'Length + 1);
            A_Copy : Number (1 .. Length);
            B_Copy : Number := B;
            Temp   : Number (1 .. 1 + B'Length);
            Mult   : Digit;
            Ignore : Digit;
         begin
            Normalize_For_Devide (A, B_Copy, A_Copy, Mult);
            Temp (1 .. 1 + B'Length) := A_Copy (1 .. 1 + B'Length);

            for Index in 1 .. A_Copy'Length - B'Length loop
               Temp (Temp'Last) := A_Copy (Index + B'Length);

               Devide
                 (Left   => Temp,
                  Right  => B_Copy,
                  Result => Ignore);

               pragma Assert (Temp (1) = 0);
               Temp (1 .. B'Length) := Temp (2 .. Temp'Last);
            end loop;

            if Mult = 1 then
               Result := Temp (1 .. Result'Length);
            else
               Fast_Devide
                 (A      => Temp (1 .. Result'Length),
                  B      => Mult,
                  Result => Result,
                  Rest   => Ignore);
            end if;
         end;
      end if;
   end Remainder;

   --------------
   -- Subtract --
   --------------

   procedure Subtract
     (A, B   : Number;
      Result : out Number;
      C      : Digit := 1;
      Ok     : out Boolean)
   is
      Mult  : constant Double_Digit := Double_Digit (C);
      Temp  : Double;
      Carry : Digit := 0;
   begin
      for J in reverse 1 .. B'Length loop
         Temp := Double (A (J + 1)) - Double (B (J)) * Mult
           - Double_Digit (Carry);
         Result (J + 1) := Digit'Mod (Temp);
         Carry := -Digit (Temp / Digit'Modulus);
      end loop;

      Ok := A (1) >= Carry;
      Result (1) := A (1) - Carry;
   end Subtract;

end JWS.Integers;
