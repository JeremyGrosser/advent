with Advent_IO.Integers; use Advent_IO.Integers;
with Advent_IO; use Advent_IO;

procedure Day1_1 is
   Ch : Character;
   Count : Natural := 0;
begin
   while not End_Of_Input loop
      Character'Read (Input, Ch);
      Count := Count + 1;
   end loop;

   Put (Output, Count);
   New_Line (Output);
end Day1_1;
