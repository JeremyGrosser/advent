with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;
with Ada.Containers.Generic_Array_Sort;

procedure D7_2 is
   package Integer_IO is new Advent_IO.Generic_Numbers
      (Number => Integer);
   use Integer_IO;

   function Maximum
      (N : Numbers)
      return Integer
   is
      A : Integer := -1;
   begin
      for B of N loop
         if B > A then
            A := B;
         end if;
      end loop;
      return A;
   end Maximum;

   function Cost
      (Positions : Numbers;
       Alignment : Integer)
       return Integer
   is
      Total    : Integer := 0;
      Distance : Natural;
   begin
      for P of Positions loop
         Distance := abs (P - Alignment);
         for I in 1 .. Distance loop
            Total := Total + I;
         end loop;
      end loop;
      return Total;
   end Cost;

   Positions      : constant Numbers := Get_Numbers (Input, Delimiter => Comma);
   Lowest_Cost    : Integer := Integer'Last;
   C              : Integer;
begin
   for Alignment in 1 .. Maximum (Positions) loop
      C := Cost (Positions, Alignment);
      if C < Lowest_Cost then
         Lowest_Cost := C;
      end if;
   end loop;

   Integer_IO.Put (Output, Lowest_Cost);
   New_Line (Output);
end D7_2;
