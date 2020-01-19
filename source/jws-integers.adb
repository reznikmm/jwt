--  Copyright (c) 2006-2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------


with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;

with League.Stream_Element_Vectors.Hash;

package body JWS.Integers is

   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Offset;

   type Small is mod 2**16;
   type Digit is mod 2**8;

   function X (Char : Ada.Streams.Stream_Element) return Digit;
   function X (Char : Ada.Streams.Stream_Element) return Small;
   function X (First, Second : Ada.Streams.Stream_Element) return Small;
   function X (Item : Digit) return Ada.Streams.Stream_Element;

   pragma Inline (X);

   subtype Element_Array is Ada.Streams.Stream_Element_Array;
   subtype Offset is Ada.Streams.Stream_Element_Count;

   subtype Positive_Index is Offset range 1 .. Offset'Last;

   type Buffer is array (Positive_Index range <>) of
     aliased Ada.Streams.Stream_Element;

   Plus  : constant Ada.Streams.Stream_Element := Character'Pos ('+');
   Minus : constant Ada.Streams.Stream_Element := Character'Pos ('-');

   procedure Add
     (Left   : in     Buffer;
      Right  : in     Buffer;
      Result :    out Buffer;
      Last   :    out Offset;
      Mult   : in     Digit := 1);

   procedure Add
     (Left   : in     Buffer;
      Right  : in     Buffer;
      Result :    out Buffer;
      Last   :    out Offset;
      Mult   : in     Digit := 1;
      Carry  :    out Digit);

   procedure Subtract
     (Left   : in     Buffer;
      Right  : in     Buffer;
      Result :    out Buffer;
      Last   :    out Offset;
      Mult   : in     Digit := 1);

   procedure Subtract
     (Left   : in     Buffer;
      Right  : in     Buffer;
      Result :    out Buffer;
      Last   :    out Offset;
      Mult   : in     Digit := 1;
      Carry  :    out Digit);

   procedure Multiply
     (Left   : in     Buffer;
      Right  : in     Buffer;
      Result : in out Buffer;
      Last   :    out Offset);

   procedure Normalize_For_Devide
     (Left   : in out Buffer;
      Right  : in out Buffer;
      Mult   :    out Digit);

   procedure Devide
     (Left   : in out Buffer;
      Right  : in     Buffer;
      Result :    out Digit);

   procedure Devide
     (Left   : in     Value;
      Right  : in     Value;
      Result :    out Value;
      Rest   : in     Boolean := False);

   procedure Initialize
     (Text   : in     String;
      Result : in out Buffer;
      Last   :    out Offset;
      Base   : in     Digit := 10);

   function Signed_Add
     (Left_Text   : Element_Array;
      Right_Text  : Element_Array)
      return Value;

   function Less
     (Left_Text   : Buffer;
      Right_Text  : Buffer)
      return Boolean;

   function To_Small (Text : Buffer) return Small;

   procedure Change_Sign (Sign : in out Ada.Streams.Stream_Element);

   procedure Fast_Devide
     (Left   : in     Buffer;
      Right  : in     Digit;
      Result :    out Buffer;
      Last   :    out Offset;
      Rest   :    out Digit);

   function Get_Last (Text : Buffer) return Offset;
   function Get_Last (Text : Element_Array) return Offset;

   function Get_Sign (Left, Right : Value) return Ada.Streams.Stream_Element;

   function To_Value (Text : Element_Array) return Value;
   function To_Buffer (Text : Value) return Buffer;

   generic
      with function Op (Left, Right : Digit) return Digit;
   function Logic (Left, Right : Value) return Value;

   Buffer_Overflow : exception;

   function Simple_Literal
     (Text : String;
      Base : Positive := 10)
      return Value;

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Value) return Value is
      Left_Text   : constant Buffer := To_Buffer (Left);
      Right_Text  : constant Buffer := To_Buffer (Right);
      Result_Last : Offset := 0;
      Result_Text : Element_Array (0 .. Left_Text'Length + Right_Text'Length);
   begin
      Result_Text (0) := Get_Sign (Left, Right);

      Multiply
        (Left   => Left_Text,
         Right  => Right_Text,
         Result => Buffer (Result_Text (1 .. Result_Text'Last)),
         Last   => Result_Last);

      return To_Value (Result_Text (0 .. Result_Last));
   end "*";

   ----------
   -- "**" --
   ----------

   function "**" (Left, Right : Value) return Value is
      Right_Text  : constant Buffer := To_Buffer (Right);
      Power       : Small := To_Small (Right_Text);
      Last        : constant Offset := (Left.Length) * Offset (Power) + 1;
      Mult_Last   : Offset := Left.Length;
      Result_Last : Offset := 1;
      Result_Text : Element_Array (0 .. Offset'Max (Last, 1));
      Temp_Text   : Buffer (1 .. Last);
      Mult_Text   : Buffer (1 .. Offset'Max (Last, Mult_Last));
   begin
      if Right.Element (1) = Minus then
         raise Constraint_Error;
      elsif (Power and 1) /= 0 and then Left.Element (1) = Minus then
         Result_Text (0) := Minus;
      else
         Result_Text (0) := Plus;
      end if;

      Result_Text (1) := X (1);
      Mult_Text (1 .. Mult_Last) := To_Buffer (Left);

      while Power > 0 loop
         if (Power and 1) /= 0 then
            Temp_Text (1 .. Result_Last) :=
              Buffer (Result_Text (1 .. Result_Last));

            Multiply
              (Left   => Temp_Text (1 .. Result_Last),
               Right  => Mult_Text (1 .. Mult_Last),
               Result => Buffer (Result_Text (1 .. Result_Text'Last)),
               Last   => Result_Last);

            Power := Power - 1;
         else
            Temp_Text (1 .. Mult_Last) := Mult_Text (1 .. Mult_Last);

            Multiply
              (Left   => Temp_Text (1 .. Mult_Last),
               Right  => Temp_Text (1 .. Mult_Last),
               Result => Mult_Text (1 .. Mult_Text'Last),
               Last   => Mult_Last);

            Power := Power / 2;
         end if;
      end loop;

      return To_Value (Result_Text (0 .. Result_Last));
   end "**";

   ------------
   -- RSASP1 --
   ------------

   function RSASP1 (M, D, N : Value) return Value is
      Power  : Buffer := To_Buffer (D);
      Mult   : Value := M;
      Result : Value := One;
      Mask   : Ada.Streams.Stream_Element := 1;
      J      : Offset := Power'First;
   begin
      while J <= Power'Last loop
         if (Power (J) and Mask) = 0 then
            Mult := (Mult * Mult) mod N;

            if Mask = 128 then
               J := J + 1;
               Mask := 1;
            else
               Mask := Mask * 2;
            end if;
         else
            Result := (Result * Mult) mod N;
            Power (J) := Power (J) and not Mask;
         end if;
      end loop;

      return Result;
   end RSASP1;

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Value) return Value is
      Left_Text  : constant Element_Array := Left.To_Stream_Element_Array;
      Right_Text : constant Element_Array := Right.To_Stream_Element_Array;
   begin
      return Signed_Add (Left_Text, Right_Text);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Value) return Value is
      Left_Text : Element_Array := Left.To_Stream_Element_Array;
   begin
      Change_Sign (Left_Text (0));
      return To_Value (Left_Text);
   end "-";

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Value) return Value is
      Left_Text  : constant Element_Array := Left.To_Stream_Element_Array;
      Right_Text : Element_Array := Right.To_Stream_Element_Array;
   begin
      Change_Sign (Right_Text (0));
      return Signed_Add (Left_Text, Right_Text);
   end "-";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Value) return Value is
      Result : Value;
   begin
      Devide (Left, Right, Result);
      return Result;
   end "/";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Value) return Boolean is
      Left_Text  : constant Element_Array := Left.To_Stream_Element_Array;
      Right_Text : constant Element_Array := Right.To_Stream_Element_Array;
   begin
      if Left_Text (0) = Plus and Right_Text (0) = Plus then
         return Less (Buffer (Left_Text (1 .. Left_Text'Last)),
                      Buffer (Right_Text (1 .. Right_Text'Last)));
      elsif Left_Text (0) = Plus and Right_Text (0) = Minus then
         return False;
      elsif Left_Text (0) = Minus and Right_Text (0) = Plus then
         return True;
      else
         return Less (Buffer (Right_Text (1 .. Right_Text'Last)),
                      Buffer (Left_Text (1 .. Left_Text'Last)));
      end if;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Value) return Boolean is
   begin
      return Left = Right or else Left < Right;
   end "<=";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Value) return Boolean is
      use League.Stream_Element_Vectors;
   begin
      return Stream_Element_Vector (Left) = Stream_Element_Vector (Right);
   end "=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Value) return Boolean is
   begin
      return not (Left <= Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Value) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   -----------
   -- "abs" --
   -----------

   function "abs" (Left : Value) return Value is
      Sign : constant Ada.Streams.Stream_Element := Left.Element (1);
   begin
      if Sign = Minus then
         return -Left;
      else
         return Left;
      end if;
   end "abs";

   -----------
   -- Logic --
   -----------

   function Logic (Left, Right : Value) return Value is
      Left_Text   : constant Buffer := To_Buffer (Left);
      Right_Text  : constant Buffer := To_Buffer (Right);
      Length      : constant Offset :=
        Offset'Max (Left_Text'Last, Right_Text'Last);
      Temp        : Digit;
      Result_Last : Offset := 0;
      Result_Text : Element_Array (0 .. Length);
   begin
      if Left.Element (1) /= Plus or Right.Element (1) /= Plus then
         raise Constraint_Error;
      end if;

      Result_Text (0) := Plus;

      for J in 1 .. Length loop
         if J in Right_Text'Range then
            Temp := X (Right_Text (J));
         else
            Temp := 0;
         end if;

         if J in Left_Text'Range then
            Temp := Op (X (Left_Text (J)), Temp);
         else
            Temp := Op (0, Temp);
         end if;

         Result_Text (J) := X (Temp);
      end loop;

      Result_Last := Get_Last (Result_Text);

      return To_Value (Result_Text (0 .. Result_Last));
   end Logic;

   -----------
   -- "and" --
   -----------

   function And_Logic is new Logic ("and");

   function "and" (Left, Right : Value) return Value
     renames And_Logic;

   -----------
   -- "mod" --
   -----------

   function "mod" (Left, Right : Value) return Value is
      Result : Value;
   begin
      Devide (Left, Right, Result, True);

      if Left.Element (1) = Right.Element (1) or Result = Zero then
         return Result;
      else
         return Result + Right;
      end if;
   end "mod";

   -----------
   -- "not" --
   -----------

   function "not" (Left : Value) return Value is
      Left_Text : Element_Array := Left.To_Stream_Element_Array;
      Left_Last : Offset := 0;
   begin
      if Left_Text (0) /= Plus then
         raise Constraint_Error;
      end if;

      for J in 1 .. Left_Text'Last loop
         Left_Text (J) := X (not X (Left_Text (J)));
      end loop;

      Left_Last := Get_Last (Left_Text);

      return To_Value (Left_Text (0 .. Left_Last));
   end "not";

   ----------
   -- "or" --
   ----------

   function Or_Logic is new Logic ("or");

   function "or" (Left, Right : Value) return Value
     renames Or_Logic;

   -----------
   -- "rem" --
   -----------

   function "rem" (Left, Right : Value) return Value is
      Result : Value;
   begin
      Devide (Left, Right, Result, True);
      return Result;
   end "rem";

   -----------
   -- "xor" --
   -----------

   function Xor_Logic is new Logic ("xor");

   function "xor" (Left, Right : Value) return Value
     renames Xor_Logic;

   ---------
   -- Add --
   ---------

   procedure Add
     (Left   : in     Buffer;
      Right  : in     Buffer;
      Result :    out Buffer;
      Last   :    out Offset;
      Mult   : in     Digit := 1)
   is
      Carry : Digit;
   begin
      Add (Left, Right, Result, Last, Mult, Carry);

      if Carry /= 0 then
         Last := Last + 1;

         if Last > Result'Last then
            raise Buffer_Overflow;
         end if;

         Result (Last) := X (Carry);
      end if;
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Left   : in     Buffer;
      Right  : in     Buffer;
      Result :    out Buffer;
      Last   :    out Offset;
      Mult   : in     Digit := 1;
      Carry  :    out Digit)
   is
      Temp  : Small;
      Max   : constant Offset := Offset'Max (Left'Length, Right'Length);
   begin
      if Max > Result'Length then
         raise Buffer_Overflow;
      end if;

      Carry := 0;

      for J in 0 .. Max - 1 loop
         if J < Right'Length then
            Temp := X (Right (Right'First + J)) * Small (Mult);
         else
            Temp := 0;
         end if;

         if J < Left'Length then
            Temp := X (Left (Left'First + J)) + Temp;
         end if;

         Temp := Temp + Small (Carry);
         Result (Result'First + J) := X (Digit (Temp mod Digit'Modulus));
         Carry := Digit (Temp / Digit'Modulus);
      end loop;

      Last := Result'First + Max - 1;
   end Add;

   -----------------
   -- Change_Sign --
   -----------------

   procedure Change_Sign (Sign : in out Ada.Streams.Stream_Element) is
   begin
      if Sign = Plus then
         Sign := Minus;
      else
         Sign := Plus;
      end if;
   end Change_Sign;

   ------------
   -- Devide --
   ------------

   procedure Devide
     (Left   : in out Buffer;
      Right  : in     Buffer;
      Result :    out Digit)
   is
      function Get_Digit (Left, Right : Buffer) return Digit;

      function Get_Digit (Left, Right : Buffer) return Digit is
         Temp   : Small;
         Result : Small;
      begin
         if Left (Left'Last) = Right (Right'Last) then
            Result := 255;
         else
            Result := X (Left (Left'Last), Left (Left'Last - 1));
            Result := Result / X (Right (Right'Last));
         end if;

         loop
            Temp := X (Left (Left'Last), Left (Left'Last - 1));
            Temp := Temp - Result * X (Right (Right'Last));
            exit when Temp >= 256;

            Temp := Temp * 256 + X (Left (Left'Last - 2));
            exit when Result * X (Right (Right'Last - 1)) <= Temp;

            Result := Result - 1;
         end loop;

         return Digit (Result);
      end Get_Digit;

      Carry : Digit;
      Last  : Offset;
   begin
      Result := Get_Digit (Left, Right);

      Subtract
        (Left   => Left,
         Right  => Right,
         Result => Left,
         Last   => Last,
         Mult   => Result,
         Carry  => Carry);

      if Carry /= 0 then
         Result := Result - 1;

         Add
           (Left   => Left,
            Right  => Right,
            Result => Left,
            Last   => Last,
            Carry  => Carry);
      end if;
   end Devide;

   ------------
   -- Devide --
   ------------

   procedure Devide
     (Left   : in     Value;
      Right  : in     Value;
      Result :    out Value;
      Rest   : in     Boolean := False)
   is
      Left_Length : constant Offset :=
        Offset'Max (Left.Length, Right.Length) + 1;
      Left_Text   : Buffer (1 .. Left_Length);
      Right_Text  : Buffer := To_Buffer (Right);
      Index       : Offset := Left_Text'Length - Right_Text'Length;
      Last        : Offset;
      Temp        : Digit;
      Mult        : Digit;
   begin
      Left_Text (1 .. Left.Length) := To_Buffer (Left);
      Left_Text (Left.Length + 1 .. Left_Length) := (others => 0);

      if Right_Text'Length = 0 then
         raise Buffer_Overflow;
      elsif Right_Text'Length = 1 then
         declare
            Result_Text : Element_Array (0 .. Left_Text'Last);
         begin
            Fast_Devide
              (Left   => Left_Text (1 .. Left_Text'Last),
               Right  => X (Right_Text (1)),
               Result => Buffer (Result_Text (1 .. Result_Text'Last)),
               Last   => Last,
               Rest   => Temp);

            if Rest then
               Result_Text (0) := Left.Element (1);
               Result_Text (1) := X (Temp);
               Result := To_Value (Result_Text (0 .. 1));
            else
               Result_Text (0) := Get_Sign (Left, Right);
               Result := To_Value (Result_Text (0 .. Last));
            end if;

            return;
         end;
      end if;

      Normalize_For_Devide
        (Left   => Left_Text,
         Right  => Right_Text,
         Mult   => Mult);

      declare
         Result_Text : Element_Array (0 .. Left_Length) := (others => 0);
      begin
         while Index >= 1 loop
            Devide
              (Left   => Left_Text (Index .. Index + Right_Text'Length),
               Right  => Right_Text,
               Result => Temp);

            Result_Text (Index) := X (Temp);
            Index := Index - 1;
         end loop;

         if Rest then
            Result_Text := (others => 0);

            Fast_Devide
              (Left   => Left_Text (1 .. Right_Text'Length + 1),
               Right  => Mult,
               Result => Buffer (Result_Text (1 .. Result_Text'Last)),
               Last   => Last,
               Rest   => Temp);

            Result_Text (0) := Left.Element (1);
         else
            Result_Text (0) := Get_Sign (Left, Right);
         end if;

         Last := Get_Last (Result_Text);
         Result := To_Value (Result_Text (0 .. Last));
      end;
   end Devide;

   -----------------
   -- Fast_Devide --
   -----------------

   procedure Fast_Devide
     (Left   : in     Buffer;
      Right  : in     Digit;
      Result :    out Buffer;
      Last   :    out Offset;
      Rest   :    out Digit)
   is
      Temp   : Small := 0;
   begin
      for J in reverse Left'Range loop
         Temp := Temp * 256 + X (Left (J));
         Result (J) := X (Digit (Temp / Small (Right)));
         Temp := Temp mod Small (Right);
      end loop;

      Rest := Digit (Temp);
      Last := Get_Last (Result);
   end Fast_Devide;

   --------------
   -- Get_Last --
   --------------

   function Get_Last (Text : Element_Array) return Offset is
   begin
      return Get_Last (Buffer (Text (1 .. Text'Last)));
   end Get_Last;

   --------------
   -- Get_Last --
   --------------

   function Get_Last (Text : Buffer) return Offset is
   begin
      for J in reverse Text'Range loop
         if Text (J) /= X (0) then
            return J;
         end if;
      end loop;

      return Text'First - 1;
   end Get_Last;

   --------------
   -- Get_Sign --
   --------------

   function Get_Sign (Left, Right : Value) return Ada.Streams.Stream_Element is
   begin
      if Left.Element (1) = Right.Element (1) then
         return Plus;
      else
         return Minus;
      end if;
   end Get_Sign;

   ----------
   -- Hash --
   ----------

   function Hash (Left : Value) return Ada.Containers.Hash_Type is
   begin
      return League.Stream_Element_Vectors.Hash
        (League.Stream_Element_Vectors.Stream_Element_Vector (Left));
   end Hash;

   -----------
   -- Image --
   -----------

   function Image (Left : Value) return String is
      Text        : Buffer := To_Buffer (Left);
      Text_Last   : Offset := Text'Last;
      Result_Text : String (1 .. Text'Length * 3);
      First       : Positive := Result_Text'Last + 1;
      Rest        : Digit;
   begin
      if Left.Length = 0 then
         return "0";
      end if;

      while Text_Last >= 1 loop
         Fast_Devide
           (Left   => Text (1 .. Text_Last),
            Right  => 10,
            Result => Text (1 .. Text_Last),
            Last   => Text_Last,
            Rest   => Rest);

         First := First - 1;
         Result_Text (First) := Character'Val (X (Rest + Character'Pos ('0')));
      end loop;

      if Left.Element (1) = Minus then
         First := First - 1;
         Result_Text (First) := '-';
      end if;

      return Result_Text (First .. Result_Text'Last);
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Text   : in     String;
      Result : in out Buffer;
      Last   :    out Offset;
      Base   : in     Digit := 10)
   is
      Left   : Buffer (Result'First .. Result'First);
      Temp   : Character;
      Stop   : Offset := Result'First - 1;
      Skip   : Boolean := False;
   begin
      for J in Text'Range loop
         Temp := Text (J);

         case Temp is
            when '0' .. '9' =>
               Left (Result'First) :=
                 X (Character'Pos (Temp) - Character'Pos ('0'));
            when 'a' .. 'f' =>
               Left (Result'First) :=
                 X (Character'Pos (Temp) - Character'Pos ('a') + 10);
            when 'A' .. 'F' =>
               Left (Result'First) :=
                 X (Character'Pos (Temp) - Character'Pos ('A') + 10);
            when '_' | '.' | '#' | ':' =>
               Skip := True;
            when others =>
               raise Constraint_Error;
         end case;

         if Skip then
            Skip := False;
         else
            Add (Left, Result (Result'First .. Stop), Result, Stop, Base);
         end if;
      end loop;
      Last := Stop;
   end Initialize;

   ------------
   -- Length --
   ------------

   function Length (Left : Value) return Ada.Streams.Stream_Element_Count is
   begin
      return League.Stream_Element_Vectors.Stream_Element_Vector
        (Left).Length - 1;
   end Length;

   ----------
   -- Less --
   ----------

   function Less
     (Left_Text   : Buffer;
      Right_Text  : Buffer)
      return Boolean is
   begin
      if Left_Text'Length = Right_Text'Length then
         for J in reverse Left_Text'Range loop
            if Left_Text (J) < Right_Text (J) then
               return True;
            elsif Left_Text (J) > Right_Text (J) then
               return False;
            end if;
         end loop;
         return False;
      elsif Left_Text'Length > Right_Text'Length then
         if Left_Text (Left_Text'Last) = X (0) then
            raise Constraint_Error;
         end if;
         return False;
      else
         if Right_Text (Right_Text'Last) = X (0) then
            raise Constraint_Error;
         end if;
         return True;
      end if;
   end Less;

   --------------------
   -- Simple_Literal --
   --------------------

   function Simple_Literal
     (Text : String;
      Base : Positive := 10)
      return Value
   is
      Result_Text : Element_Array (0 .. Text'Length / 2 + 2);
      Result_Last : Offset;
      J           : constant Positive := Text'First;
   begin
      if Text (J) = '-' then
         Result_Text (0) := Minus;

         Initialize
           (Text (Text'First + 1 .. Text'Last),
            Buffer (Result_Text (1 .. Result_Text'Last)),
            Result_Last,
            Digit (Base));
      elsif Text (J) in '+' | ' ' then
         Result_Text (0) := Plus;

         Initialize
           (Text (Text'First + 1 .. Text'Last),
            Buffer (Result_Text (1 .. Result_Text'Last)),
            Result_Last,
            Digit (Base));
      else
         Result_Text (0) := Plus;

         Initialize
           (Text,
            Buffer (Result_Text (1 .. Result_Text'Last)),
            Result_Last,
            Digit (Base));
      end if;
      return To_Value (Result_Text (0 .. Result_Last));
   end Simple_Literal;

   -------------
   -- Literal --
   -------------

   function Literal (Text : String) return Value is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps.Constants;

      Base   : Positive := 10;
      Base_V : Value := Ten;
      Sharp  : Natural := Index (Text, "#");
      E      : Natural := Text'First;
      Result : Value;
      Exp    : Value;
   begin
      if Sharp = 0 then
         Sharp := Index (Text, ":");
      end if;

      if Sharp /= 0 then
         Base   := Positive'Value (Text (Text'First .. Sharp - 1));
         Base_V := Simple_Literal (Text (Text'First .. Sharp - 1));
         E := Index (Text, (1 => Text (Sharp)), From => Sharp + 1);
      else
         Sharp := Text'First - 1;
      end if;

      E := Index (Text, "E", Mapping => Upper_Case_Map, From => E);

      if E /= 0 then
         Exp := Simple_Literal (Text (E + 1 .. Text'Last));
      else
         E := Text'Last + 1;
         Exp := Zero;
      end if;

      Result := Simple_Literal (Text (Sharp + 1 .. E - 1), Base);

      if Exp > Zero then
         Exp := Base_V ** Exp;
         Result := Result * Exp;
      elsif Exp < Zero then
         Exp := Base_V ** (-Exp);
         Result := Result / Exp;
      end if;

      return Result;
   end Literal;

   --------------
   -- Multiply --
   --------------

   procedure Multiply
     (Left   : in     Buffer;
      Right  : in     Buffer;
      Result : in out Buffer;
      Last   :    out Offset)
   is
      Temp   : Digit;
      Length : Offset := Result'First - 1;
      Mult   : Buffer (Result'Range);
   begin
      for J in Left'Range loop
         Temp := X (Left (J));
         Mult (J .. J + Right'Length - 1) := Right;
         Mult (Mult'First .. J - 1) := (others => X (0));

         Add
           (Left   => Result (Result'First .. Length),
            Right  => Mult (Mult'First .. J + Right'Length - 1),
            Result => Result,
            Last   => Length,
            Mult   => Temp);
      end loop;

      Last := Length;
   end Multiply;

   --------------------------
   -- Normalize_For_Devide --
   --------------------------

   procedure Normalize_For_Devide
     (Left   : in out Buffer;
      Right  : in out Buffer;
      Mult   :    out Digit)
   is
      Zero : constant Buffer (Left'First .. Left'First - 1) :=
        (others => X (0));
      Last : Offset;
   begin
      Mult := Digit (Small'(256) / (X (Right (Right'Last)) + 1));
      if Mult = 1 then
         Left (Left'Last) := X (0);
      else
         Add
           (Left   => Zero,
            Right  => Left,
            Result => Left,
            Last   => Last,
            Mult   => Mult);

         Add
           (Left   => Zero,
            Right  => Right,
            Result => Right,
            Last   => Last,
            Mult   => Mult);
      end if;
   end Normalize_For_Devide;

   ----------------
   -- Signed_Add --
   ----------------

   function Signed_Add
     (Left_Text   : Element_Array;
      Right_Text  : Element_Array)
      return Value
   is
      Length      : constant Offset :=
        Offset'Max (Left_Text'Last, Right_Text'Last) + 1;
      Result_Last : Offset;
      Result_Text : Element_Array (0 .. Length);
   begin
      if Left_Text (0) = Right_Text (0) then
         Result_Text (0) := Left_Text (0);

         Add
           (Left   => Buffer (Left_Text (1 .. Left_Text'Last)),
            Right  => Buffer (Right_Text (1 .. Right_Text'Last)),
            Result => Buffer (Result_Text (1 .. Result_Text'Last)),
            Last   => Result_Last);
      else
         if Less (Buffer (Left_Text (1 .. Left_Text'Last)),
                  Buffer (Right_Text (1 .. Right_Text'Last)))
         then
            Result_Text (0) := Right_Text (0);
            Subtract
              (Left   => Buffer (Right_Text  (1 .. Right_Text'Last)),
               Right  => Buffer (Left_Text   (1 .. Left_Text'Last)),
               Result => Buffer (Result_Text (1 .. Result_Text'Last)),
               Last   => Result_Last);
         else
            Result_Text (0) := Left_Text (0);
            Subtract
              (Left   => Buffer (Left_Text (1 .. Left_Text'Last)),
               Right  => Buffer (Right_Text (1 .. Right_Text'Last)),
               Result => Buffer (Result_Text (1 .. Result_Text'Last)),
               Last   => Result_Last);
         end if;
      end if;

      return To_Value (Result_Text (0 .. Result_Last));
   end Signed_Add;

   --------------
   -- Subtract --
   --------------

   procedure Subtract
     (Left   : in     Buffer;
      Right  : in     Buffer;
      Result :    out Buffer;
      Last   :    out Offset;
      Mult   : in     Digit := 1)
   is
      Carry : Digit;
   begin
      Subtract (Left, Right, Result, Last, Mult, Carry);
      if Carry /= 0 then
         Last := Last + 1;
         if Last > Result'Last then
            raise Buffer_Overflow;
         end if;
         Result (Last) := X (-Carry);
      end if;
   end Subtract;

   --------------
   -- Subtract --
   --------------

   procedure Subtract
     (Left   : in     Buffer;
      Right  : in     Buffer;
      Result :    out Buffer;
      Last   :    out Offset;
      Mult   : in     Digit := 1;
      Carry  :    out Digit)
   is
      Temp  : Small;
      Max   : constant Offset := Offset'Max (Left'Length, Right'Length);
   begin
      if Max > Result'Length then
         raise Buffer_Overflow;
      end if;

      Carry := 0;

      for J in 0 .. Max - 1 loop
         if J < Right'Length then
            Temp := X (Right (Right'First + J)) * Small (Mult);
         else
            Temp := 0;
         end if;

         if J < Left'Length then
            Temp := X (Left (Left'First + J)) - Temp;
         else
            Temp := -Temp;
         end if;

         Temp := Temp - Small (Carry);
         Result (Result'First + J) := X (Digit (Temp mod Digit'Modulus));
         Carry := -Digit (Temp / Digit'Modulus);
      end loop;

      Last := Result'First + Max - 1;
   end Subtract;

   ---------------
   -- To_Value --
   ---------------

   function To_Value (Text : Element_Array) return Value is
      Last : constant Offset := Get_Last (Text);
   begin
      if Text'First >= Last then
         return Zero;
      else
         return To_Stream_Element_Vector (Text (Text'First .. Last));
      end if;
   end To_Value;

   --------------
   -- To_Small --
   --------------

   function To_Small (Text : Buffer) return Small is
      Result : Small := 0;
   begin
      for J in reverse Text'Range loop
         Result := Result * 256 + X (Text (J));
      end loop;
      return Result;
   end To_Small;

   -------
   -- X --
   -------

   function X (Char : Ada.Streams.Stream_Element) return Digit is
   begin
      return Ada.Streams.Stream_Element'Pos (Char);
   end X;

   -------
   -- X --
   -------

   function X (Char : Ada.Streams.Stream_Element) return Small is
   begin
      return Ada.Streams.Stream_Element'Pos (Char);
   end X;

   -------
   -- X --
   -------

   function X (First, Second : Ada.Streams.Stream_Element) return Small is
   begin
      return Small (First) * 256 + Small (Second);
   end X;

   -------
   -- X --
   -------

   function X (Item : Digit) return Ada.Streams.Stream_Element is
   begin
      return Ada.Streams.Stream_Element'Val (Item);
   end X;

   function Zero return Value is
      Result : constant Element_Array := (0 => Plus);
   begin
      return To_Stream_Element_Vector (Result);
   end Zero;

   function One  return Value is
      Result : constant Element_Array := (Plus, 1);
   begin
      return To_Stream_Element_Vector (Result);
   end One;

   function Two  return Value is
      Result : constant Element_Array := (Plus, 2);
   begin
      return To_Stream_Element_Vector (Result);
   end Two;

   function Ten  return Value is
      Result : constant Element_Array := (Plus, 10);
   begin
      return To_Stream_Element_Vector (Result);
   end Ten;

   function To_Buffer (Text : Value) return Buffer is
   begin
      return Buffer (Text.To_Stream_Element_Array (1 .. Text.Length));
   end To_Buffer;

   function BER_Value (Data : Ada.Streams.Stream_Element_Array) return Value is
      Buf : Ada.Streams.Stream_Element_Array (0 .. Data'Length);
   begin
      Buf (0) := Plus;

      for J in 1 .. Buf'Last loop
         Buf (J) := Data (Data'Last - J + 1);
      end loop;

      return To_Value (Buf);
   end BER_Value;

   function To_BER (Data : Value) return Ada.Streams.Stream_Element_Array is
      V : constant Ada.Streams.Stream_Element_Array :=
        Data.To_Stream_Element_Array;
   begin
      return Result : Ada.Streams.Stream_Element_Array (1 .. V'Last) do
         for J in Result'Range loop
            Result (J) := V (V'Last - J + 1);
         end loop;
      end return;
   end To_BER;
end JWS.Integers;
