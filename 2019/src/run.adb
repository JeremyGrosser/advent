with Ada.Command_Line;
with Intcode;

procedure Run is
    M : Intcode.Machine;
begin
    Intcode.Load_From_File(M, Ada.Command_Line.Argument (1));
    Intcode.Reset (M);
    Intcode.Run (M);
end Run;
