pragma Style_Checks (Off);
with Advent_IO; use Advent_IO;
with Ada.Command_Line;

with Day1_1;
with Day1_2;
with Day2_1;
with Day2_2;
with Day3_1;
with Day3_2;
with Day4_1;
with Day4_2;
with Day5_1;
with Day5_2;
with Day6_1;
with Day6_2;
with Day7_1;
with Day7_2;
with Day8_1;

procedure Advent2022 is
   Puzzle   : constant String := Ada.Command_Line.Argument (1);
   Filename : constant String := Ada.Command_Line.Argument (2);
begin
   Input := Stream (Filename);
   if Puzzle = "1.1" then Day1_1; return; end if;
   if Puzzle = "1.2" then Day1_2; return; end if;
   if Puzzle = "2.1" then Day2_1; return; end if;
   if Puzzle = "2.2" then Day2_2; return; end if;
   if Puzzle = "3.1" then Day3_1; return; end if;
   if Puzzle = "3.2" then Day3_2; return; end if;
   if Puzzle = "4.1" then Day4_1; return; end if;
   if Puzzle = "4.2" then Day4_2; return; end if;
   if Puzzle = "5.1" then Day5_1; return; end if;
   if Puzzle = "5.2" then Day5_2; return; end if;
   if Puzzle = "6.1" then Day6_1; return; end if;
   if Puzzle = "6.2" then Day6_2; return; end if;
   if Puzzle = "7.1" then Day7_1; return; end if;
   if Puzzle = "7.2" then Day7_2; return; end if;
   if Puzzle = "8.1" then Day8_1; return; end if;

   String'Write (Error, "Unknown Puzzle: ");
   String'Write (Error, Puzzle);
   New_Line (Error);
end Advent2022;
