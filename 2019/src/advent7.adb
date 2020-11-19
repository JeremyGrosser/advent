with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Ordered_Sets;
with Ada.Command_Line;
with Intcode;

procedure Advent7 is
    type Amplifier_Id is (A, B, C, D, E);
    subtype Phase is Intcode.Word range 0 .. 4;
    type Phase_Array is array (Amplifier_Id) of Phase;
    subtype Phase_2 is Intcode.Word range 5 .. 9;
    type Phase_2_Array is array (Amplifier_Id) of Phase_2;

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

    function Repeating (P : Phase_2_Array) return Boolean is
       package Phase_Sets is new Ada.Containers.Ordered_Sets (Phase_2);
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

   procedure Part_1 is
      Best         : Natural := 0;
      Best_Setting : Phase_Array;
      Setting      : Phase_Array;
      Result       : Natural;
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
   end Part_1;


   function Test_2 (Filename : String; Phases : Phase_2_Array) return Natural is
      type Machine_Array is array (Amplifier_Id) of Intcode.Machine;

      Amp        : Machine_Array;
      Value      : Intcode.Word := 0;
      Halting    : Boolean := False;
   begin
      for Id in Amp'Range loop
         Amp (Id).Load_From_File (Filename);
         Amp (Id).Reset;
         Amp (Id).Write_Input (Phases (Id));
      end loop;

      loop
         for Id in Amp'Range loop
            begin
               Amp (Id).Write_Input (Value);
               loop
                  exit when Amp (Id).Has_Output;
                  Amp (Id).Step;
               end loop;
               Amp (Id).Read_Output (Value);
            exception
               when Intcode.Halted => Halting := True;
            end;
         end loop;
         exit when Halting;
      end loop;
      return Natural (Value);
   end Test_2;

   procedure Part_2 is
      Best         : Natural := 0;
      Best_Setting : Phase_2_Array;
      Setting      : Phase_2_Array;
      Result       : Natural;
   begin
      Result := Test_2 ("input/advent7.2-example1", (9, 8, 7, 6, 5));
      if Result /= 139629729 then
         Put_Line ("7.2 expected 139629729, got " & Result'Image);
         return;
      end if;

      Result := Test_2 ("input/advent7.2-example2", (9, 7, 8, 5, 6));
      if Result /= 18216 then
         Put_Line ("7.2 expected 18216, got " & Result'Image);
         return;
      end if;

      for PA in Phase_2'Range loop
         for PB in Phase_2'Range loop
            for PC in Phase_2'Range loop
               for PD in Phase_2'Range loop
                  for PE in Phase_2'Range loop
                     Setting := (PA, PB, PC, PD, PE);
                     if not Repeating (Setting) then
                        Result := Test_2 ("input/advent7", Setting);
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
   end Part_2;
begin
   Part_1;
   Part_2;
end Advent7;
