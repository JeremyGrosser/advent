with Ada.Command_Line;

with Intcode;

procedure Advent5 is
    M : Intcode.Machine;
begin
    M.Load_From_File(Ada.Command_Line.Argument (1));
    M.Run;
end Advent5;
