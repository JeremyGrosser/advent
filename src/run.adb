with Ada.Command_Line;
with Intcode;

procedure Run is
    M : Intcode.Machine;
begin
    M.Load_From_File(Ada.Command_Line.Argument (1));
    M.Reset;
    M.Run;
end Run;
