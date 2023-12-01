with Advent_IO; use Advent_IO;
with Ada.Command_Line;

with Day1_1;
with Day1_2;

procedure Main is
   package CLI renames Ada.Command_Line;
begin
   CLI.Set_Exit_Status (CLI.Success);
   if CLI.Argument_Count >= 2 then
      declare
         Puzzle   : constant String := CLI.Argument (1);
         Filename : constant String := CLI.Argument (2);
      begin
         Input := Stream (Filename);
         if Puzzle = "1.1" then
            Day1_1;
            return;
         end if;

         if Puzzle = "1.2" then
            Day1_2;
            return;
         end if;

         String'Write (Error, "Unknown Puzzle: ");
         String'Write (Error, Puzzle);
         New_Line (Error);
         CLI.Set_Exit_Status (CLI.Failure);
      end;
   else
      String'Write (Error, "Usage:");
      String'Write (Error, Ada.Command_Line.Command_Name);
      String'Write (Error, " <puzzle> <filename>");
      New_Line (Error);
      CLI.Set_Exit_Status (CLI.Failure);
   end if;
end Main;
