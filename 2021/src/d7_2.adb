with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;
with Ada.Containers.Generic_Array_Sort;

procedure D7_2 is
   package Integer_IO is new Advent_IO.Generic_Numbers
      (Number => Integer);
   use Integer_IO;

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
         Total := Total + (Distance * (Distance + 1)) / 2;
      end loop;
      return Total;
   end Cost;

   Positions : constant Numbers := Get_Numbers (Input, Delimiter => Comma);
   Alignment : Integer := 1;
   Previous  : Integer := Integer'Last;
   C         : Integer;
begin
   loop
      C := Cost (Positions, Alignment);
      exit when C > Previous;
      Previous := C;
      Alignment := Alignment + 1;
   end loop;
   Integer_IO.Put (Output, Previous);
   New_Line (Output);
end D7_2;
