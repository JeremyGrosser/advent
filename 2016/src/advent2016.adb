pragma Style_Checks (Off);
with Advent_IO; use Advent_IO;
with Ada.Command_Line;

with Day1_1;

procedure Advent2016 is
   Puzzle   : constant String := Ada.Command_Line.Argument (1);
   Filename : constant String := Ada.Command_Line.Argument (2);
begin
   Input := Stream (Filename);
   if Puzzle = "1.1" then Day1_1; return; end if;

   String'Write (Error, "Unknown Puzzle: ");
   String'Write (Error, Puzzle);
   New_Line (Error);
end Advent2016;
