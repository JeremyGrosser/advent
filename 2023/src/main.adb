with Advent_IO; use Advent_IO;
with Ada.Command_Line;
with Ada.Environment_Variables;

with Day1_1;
with Day1_2;
with Day2_1;
with Day2_2;

with Ada.Real_Time; use Ada.Real_Time;

procedure Main is
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
         Input := Stream (Filename);
         if Puzzle = "1.1" then
            Day1_1;
         elsif Puzzle = "1.2" then
            Day1_2;
         elsif Puzzle = "2.1" then
            Day2_1;
         elsif Puzzle = "2.2" then
            Day2_2;
         else
            String'Write (Error, "Unknown Puzzle: ");
            String'Write (Error, Puzzle);
            New_Line (Error);
            CLI.Set_Exit_Status (CLI.Failure);
         end if;

         if Env.Exists ("ADVENT_PROFILE") then
            Elapsed := Natural (To_Duration (Clock - Start) * 1_000_000.0);
            String'Write (Error, Filename);
            String'Write (Error, ":");
            String'Write (Error, Elapsed'Image);
            String'Write (Error, " micros");
            New_Line (Error);
         end if;
      end;
   else
      String'Write (Error, "Usage:");
      String'Write (Error, Ada.Command_Line.Command_Name);
      String'Write (Error, " <puzzle> <filename>");
      New_Line (Error);
      CLI.Set_Exit_Status (CLI.Failure);
   end if;
end Main;
