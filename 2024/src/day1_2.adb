with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day1_2 is
   package Natural_Vectors is new Ada.Containers.Vectors (Positive, Natural);
   use Natural_Vectors;

   package Natural_Maps is new Ada.Containers.Ordered_Maps (Natural, Natural);
   use Natural_Maps;

   Left  : Natural_Vectors.Vector;
   Right : Natural_Maps.Map;
   Sum : Natural := 0;
begin
   while not Input.End_Of_Input loop
      declare
         L, R : Natural;
      begin
         L := Natural'Value (Input.Read_Until (" "));
         R := Natural'Value (Input.Read_Until (ASCII.LF));
         Append (Left, L);

         if not Contains (Right, R) then
            Include (Right, R, 1);
         else
            Replace (Right, R, Element (Right, R) + 1);
         end if;
      end;
   end loop;

   for Item of Left loop
      if Contains (Right, Item) then
         Sum := Sum + (Item * Element (Right, Item));
      end if;
   end loop;

   Output.Put (Sum);
end Day1_2;
