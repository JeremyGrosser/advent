with Ada.Command_Line;

with Intcode;

procedure Advent5 is
begin
    Intcode.Load_From_File(Ada.Command_Line.Argument (1));
    Intcode.Run;
end Advent5;
