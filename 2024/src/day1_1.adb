with Ada.Containers.Vectors;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day1_1 is
   package Natural_Vectors is new Ada.Containers.Vectors (Positive, Natural);
   package Sorting is new Natural_Vectors.Generic_Sorting ("<");
   use Sorting;
   use Natural_Vectors;
   Left, Right : Natural_Vectors.Vector;
   Sum : Natural := 0;
begin
   while not Input.End_Of_Input loop
      declare
         L, R : Natural;
      begin
         L := Natural'Value (Input.Read_Until (" "));
         R := Natural'Value (Input.Read_Until (ASCII.LF));
         Append (Left, L);
         Append (Right, R);
      end;
   end loop;

   Sort (Left);
   Sort (Right);

   for I in 1 .. Positive (Length (Left)) loop
      Sum := Sum + (abs (Left (I) - Right (I)));
   end loop;

   Output.Put (Sum);
end Day1_1;
