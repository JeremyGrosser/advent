with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Intcode;

procedure Run is
    M : Intcode.Machine;
    Input, Output : Intcode.Word;
begin
    M.Load_From_File(Ada.Command_Line.Argument (1));
    M.Reset;
    loop
       begin
          M.Step;
          if M.Has_Output then
             M.Read_Output (Output);
             Put_Line ("<" & Output'Image);
          end if;
       exception
          when Intcode.Buffer_Underrun =>
             Put ("> ");
             Input := Intcode.Word'Value (Get_Line);
             M.Write_Input (Input);
          when Intcode.Halted =>
             Put_Line ("HALT");
             M.Print_Summary;
             return;
       end;
    end loop;
end Run;
