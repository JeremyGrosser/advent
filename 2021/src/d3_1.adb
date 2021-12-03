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

   Inputs           : Vector := Empty_Vector;
   Gamma, Epsilon   : Unsigned_32;
   Count_0, Count_1 : Natural := 0;
   Word_Size : Positive;
begin
   while not End_Of_Input loop
      declare
         Line : constant String := Read_Until (Input, Whitespace);
         X : Unsigned_32;
      begin
         Word_Size := Line'Length;
         X := 0;
         for I in Line'Range loop
            X := X or Shift_Left (Unsigned_32'Value (String'(1 => Line (I))), Line'Last - I);
         end loop;
         Inputs.Append (X);
      end;
   end loop;

   for I in 0 .. Word_Size - 1 loop
      Count_0 := 0;
      Count_1 := 0;
      for X of Inputs loop
         if (Shift_Right (X, I) and 1) = 0 then
            Count_0 := Count_0 + 1;
         else
            Count_1 := Count_1 + 1;
         end if;
      end loop;

      if Count_1 > Count_0 then
         Gamma := Gamma or Shift_Left (1, I);
      else
         Epsilon := Epsilon or Shift_Left (1, I);
      end if;
   end loop;

   Put (Output, Gamma * Epsilon);
   New_Line (Output);
end D3_1;
