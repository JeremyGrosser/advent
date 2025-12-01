pragma Extensions_Allowed (On);
with Ada.Containers.Vectors;
with Advent.Input;
with Advent.Output;

procedure Day1_1
   (Input  : in out Advent.Input.Buffer;
    Output : Advent.Output.Buffer)
is
   package Natural_Vectors is new Ada.Containers.Vectors (Positive, Natural);
   package Sorting is new Natural_Vectors.Generic_Sorting ("<");
   use Sorting;
   use Natural_Vectors;
   Left, Right : Natural_Vectors.Vector;
   Sum : Natural := 0;
begin
   while not Input.End_Of_Input loop
      Input.Skip_Whitespace;
      declare
         L, R : Natural;
      begin
         Input.Get_Integer (L);
         Input.Get_Integer (R);
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
