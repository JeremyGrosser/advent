with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Ordered_Sets;
with Ada.Command_Line;
with Intcode;

procedure Advent7 is
    type Amplifier_Id is (A, B, C, D, E);
    subtype Phase is Intcode.Word range 0 .. 4;
    type Phase_Array is array (Amplifier_Id) of Phase;

    function Test (Filename : String; Phases : Phase_Array) return Natural is
       M        : Intcode.Machine;
       Output   : Intcode.Word := 0;
    begin
       for Id in Phases'Range loop
          M.Reset;
          M.Clear_Memory;
          M.Load_From_File (Filename);
          M.Reset;
          M.Write_Input (Phases (Id));
          M.Write_Input (Output);
          M.Run_Until_Output;
          M.Read_Output (Output);
       end loop;
       return Natural (Output);
    end Test;

    function Repeating (P : Phase_Array) return Boolean is
       package Phase_Sets is new Ada.Containers.Ordered_Sets (Phase);
       S : Phase_Sets.Set;
    begin
       for I in P'Range loop
          if S.Contains (P (I)) then
             return True;
          else
             S.Include (P (I));
          end if;
       end loop;
       return False;
    end Repeating;

    Best     : Natural := 0;
    Best_Setting : Phase_Array;
    Setting  : Phase_Array;
    Result   : Natural;
begin
   Result := Test ("input/advent7-example1", (4, 3, 2, 1, 0));
   if Result /= 43210 then
      Put_Line ("7.1 expected 43210, got " & Result'Image);
      return;
   end if;

   Result := Test ("input/advent7-example2", (0, 1, 2, 3, 4));
   if Result /= 54321 then
      Put_Line ("7.1 expected 54321, got " & Result'Image);
      return;
   end if;

   Result := Test ("input/advent7-example3", (1, 0, 4, 3, 2));
   if Result /= 65210 then
      Put_Line ("7.1 expected 65210, got " & Result'Image);
      return;
   end if;

   for PA in Phase'Range loop
      for PB in Phase'Range loop
         for PC in Phase'Range loop
            for PD in Phase'Range loop
               for PE in Phase'Range loop
                  Setting := (PA, PB, PC, PD, PE);
                  if not Repeating (Setting) then
                     Result := Test ("input/advent7", Setting);
                     if Result > Best then
                        Best := Result;
                        Best_Setting := Setting;
                     end if;
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;

   Put_Line (Best'Image);
   for I in Best_Setting'Range loop
      Put (Best_Setting (I)'Image);
   end loop;
   Put_Line ("");
exception
   when Intcode.Halted => Put_Line ("HALT");
end Advent7;
