pragma Style_Checks (Off);
with Advent_IO; use Advent_IO;
with Ada.Command_Line;

with Day1_1;
with Day1_2;
with Day2_1;
with Day2_2;

procedure Advent2022 is
   Puzzle : constant String := Ada.Command_Line.Argument (1);
begin
   if Puzzle = "1.1" then Day1_1; return; end if;
   if Puzzle = "1.2" then Day1_2; return; end if;
   if Puzzle = "2.1" then Day2_1; return; end if;
   if Puzzle = "2.2" then Day2_2; return; end if;

   String'Write (Error, "Unknown Puzzle: ");
   String'Write (Error, Puzzle);
   New_Line (Error);
end Advent2022;
