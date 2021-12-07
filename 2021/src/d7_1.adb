with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;
with Ada.Containers.Generic_Array_Sort;

procedure D7_1 is
   package Integer_IO is new Advent_IO.Generic_Numbers
      (Number => Integer);
   use Integer_IO;

   procedure Sort is new Ada.Containers.Generic_Array_Sort
      (Index_Type   => Positive,
       Element_Type => Integer,
       Array_Type   => Numbers);

   function Median
      (N : Numbers)
      return Integer
   is
      A : Numbers := N;
   begin
      Sort (A);
      return A (A'Length / 2);
   end Median;

   function Cost
      (Positions : Numbers;
       Alignment : Integer)
       return Integer
   is
      Total : Integer := 0;
   begin
      for P of Positions loop
         Total := Total + (abs (P - Alignment));
      end loop;
      return Total;
   end Cost;

   Positions : constant Numbers := Get_Numbers (Input, Delimiter => Comma);
begin
   Integer_IO.Put (Output, Cost (Positions, Median (Positions)));
   New_Line (Output);
end D7_1;
