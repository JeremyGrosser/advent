with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;
with Ada.Containers.Vectors;

procedure Day1_2 is
   package Natural_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Natural);
   use Natural_Vectors;

   package Natural_Sorting is new Natural_Vectors.Generic_Sorting;
   use Natural_Sorting;

   V : Vector := Empty_Vector;
   Total : Natural;
begin
   while not End_Of_Input loop
      declare
         Line : constant String := Read_Until (Input, CRLF);
      begin
         if Line'Length = 0 then
            Append (V, Total);
            Total := 0;
         else
            Total := Total + Natural'Value (Line);
         end if;
      end;
   end loop;

   Append (V, Total);

   Sort (V);
   Reverse_Elements (V);

   for Element of V loop
      String'Write (Error, Element'Image);
      New_Line (Error);
   end loop;

   Total := V (1) + V (2) + V (3);

   Put (Output, Total);
   New_Line (Output);
end Day1_2;
