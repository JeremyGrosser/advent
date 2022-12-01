with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;

procedure Day1_1 is
   Total    : Natural := 0;
   Highest  : Natural := 0;
begin
   while not End_Of_Input loop
      declare
         Line : constant String := Read_Until (Input, CRLF);
      begin
         if Line'Length = 0 then
            if Total > Highest then
               Highest := Total;
            end if;
            Total := 0;
         else
            Total := Total + Natural'Value (Line);
         end if;
      end;
   end loop;

   if Total > Highest then
      Highest := Total;
   end if;

   Put (Output, Highest);
   New_Line (Output);
end Day1_1;
