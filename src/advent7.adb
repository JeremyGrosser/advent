with Ada.Command_Line;

with Intcode;

procedure Advent7 is
    type Amplifier_ID is (A, B, C, D, E);
    Amp_Control : array (Amplifier_ID'Range) of Intcode.Machine;
begin
    for AC of Amp_Control loop
        AC.Load_From_File(Ada.Command_Line.Argument (1));
        AC.Reset;
    end loop;

    loop
        for AC of Amp_Control loop
            AC.Step;
        end loop;
    end loop;
end Advent7;
