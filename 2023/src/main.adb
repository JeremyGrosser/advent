with Ada.Environment_Variables;
with Ada.Command_Line;
with Ada.Text_IO;

with Advent_IO;
with Day1_1;
with Day1_2;
with Day2_1;
with Day2_2;
with Day3_1;
with Day3_2;
with Day4_1;
with Day4_2;

with Ada.Real_Time; use Ada.Real_Time;

procedure Main is
   package TIO renames Ada.Text_IO;
   package CLI renames Ada.Command_Line;
   package Env renames Ada.Environment_Variables;
begin
   CLI.Set_Exit_Status (CLI.Success);
   if CLI.Argument_Count >= 2 then
      declare
         Puzzle   : constant String := CLI.Argument (1);
         Filename : constant String := CLI.Argument (2);
         Start    : constant Time := Clock;
         Elapsed  : Natural;
      begin
         Advent_IO.Open (Filename);
         if Puzzle = "1.1" then
            Day1_1;
         elsif Puzzle = "1.2" then
            Day1_2;
         elsif Puzzle = "2.1" then
            Day2_1;
         elsif Puzzle = "2.2" then
            Day2_2;
         elsif Puzzle = "3.1" then
            Day3_1;
         elsif Puzzle = "3.2" then
            Day3_2;
         elsif Puzzle = "4.1" then
            Day4_1;
         elsif Puzzle = "4.2" then
            Day4_2;
         else
            TIO.Put ("Unknown Puzzle: ");
            TIO.Put (Puzzle);
            TIO.New_Line;
            CLI.Set_Exit_Status (CLI.Failure);
         end if;

         if Env.Exists ("ADVENT_PROFILE") then
            Elapsed := Natural (To_Duration (Clock - Start) * 1_000_000.0);
            TIO.Put (Filename);
            TIO.Put (":");
            TIO.Put (Elapsed'Image);
            TIO.Put (" micros");
            TIO.New_Line;
         end if;
      end;
   else
      TIO.Put ("Usage:");
      TIO.Put (Ada.Command_Line.Command_Name);
      TIO.Put (" <puzzle> <filename>");
      TIO.New_Line;
      CLI.Set_Exit_Status (CLI.Failure);
   end if;
end Main;
