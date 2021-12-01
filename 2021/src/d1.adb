with Advent_IO; use Advent_IO;
with Ada.Command_Line;

procedure D1 is
   procedure Part_1 is
      Line : constant String := Read_Until (ASCII.LF);
   begin
      String'Write (Standard_Output, Line);
      New_Line;
      Close;
   end Part_1;
begin
   Part_1;
   Ada.Command_Line.Set_Exit_Status (123);
end D1;
