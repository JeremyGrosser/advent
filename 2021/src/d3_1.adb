with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;
with Ada.Containers.Vectors;
with Interfaces; use Interfaces;

procedure D3_1 is
   package Unsigned_IO is new Advent_IO.Generic_Numbers
      (Number => Unsigned_32);
   use Unsigned_IO;

   package Unsigned_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Unsigned_32);
   use Unsigned_Vectors;

   function Get_Word
      (Size : out Natural)
       return Unsigned_32
   is
      Line : constant String := Read_Until (Input, Whitespace);
      X    : Unsigned_32;
   begin
      Size := Line'Length;
      X := 0;
      for I in Line'Range loop
         X := X or Shift_Left (Unsigned_32'Value (String'(1 => Line (I))), Line'Last - I);
      end loop;
      return X;
   end Get_Word;

   type Popcount is array (Unsigned_32 range 0 .. 1) of Natural;

   function Get_Popcount
      (V        : Vector;
       Position : Natural)
       return Popcount
   is
      Counts : Popcount := (others => 0);
      Bit    : Unsigned_32;
   begin
      for X of V loop
         Bit := Shift_Right (X, Position) and 1;
         Counts (Bit) := Counts (Bit) + 1;
      end loop;
      return Counts;
   end Get_Popcount;

   Inputs  : Vector := Empty_Vector;
   Size    : Positive;
   Counts  : Popcount;
   Gamma   : Unsigned_32 := 0;
   Epsilon : Unsigned_32 := 0;
begin
   while not End_Of_Input loop
      Inputs.Append (Get_Word (Size));
   end loop;

   for Position in 0 .. Size - 1 loop
      Counts := Get_Popcount (Inputs, Position);
      if Counts (0) > Counts (1) then
         Gamma := Gamma or Shift_Left (1, Position);
      else
         Epsilon := Epsilon or Shift_Left (1, Position);
      end if;
   end loop;

   Put (Output, Gamma * Epsilon);
   New_Line (Output);
end D3_1;
