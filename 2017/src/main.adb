pragma Style_Checks ("M120");
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Exceptions;
with Ada.Text_IO;

with Advent.Input;
with Advent.Output;
with Puzzle;

procedure Main is
   Advent_Error : exception;

   use type Puzzle.Any_Solution;
   Solution : Puzzle.Any_Solution := null;

   procedure Set_Puzzle
      (Name : String)
   is
      Split : constant Natural := Ada.Strings.Fixed.Index (Name, ".");
   begin
      if Split = 0 then
         raise Advent_Error with "No '.' in puzzle name";
      end if;

      declare
         Day  : constant Puzzle.Day_Type := Puzzle.Day_Type'Value (Name (Name'First .. Split - 1));
         Part : constant Puzzle.Part_Type := Puzzle.Part_Type'Value (Name (Split + 1 .. Name'Last));
      begin
         Solution := Puzzle.Get_Solution (Day, Part);
         if Solution = null then
            raise Advent_Error with "No solution available for Day" & Day'Image & " Part" & Part'Image;
         end if;
      end;
   exception
      when Constraint_Error =>
         raise Advent_Error with "Invalid puzzle name: " & Name;
   end Set_Puzzle;

   package CLI renames Ada.Command_Line;
   type Argument_Type is (Puzzle, Filename, No_More);
   Required : Argument_Type := Puzzle;
begin
   CLI.Set_Exit_Status (255);
   for I in 1 .. CLI.Argument_Count loop
      declare
         Arg : constant String := CLI.Argument (I);
      begin
         if Arg'Length > 2 and then Arg (1 .. 2) = "--" then
            if Arg (3 .. Arg'Last) = "verbose" then
               Advent.Output.Enable_Log;
            end if;
         elsif Arg'Length > 1 and then Arg (1) = '-' then
            case Arg (2) is
               when 'v' =>
                  Advent.Output.Enable_Log;
               when others =>
                  raise Advent_Error with "Unknown flag: " & Arg (1 .. 2);
            end case;
         else
            case Required is
               when Puzzle =>
                  Set_Puzzle (Arg);
                  Required := Argument_Type'Succ (Required);
               when Filename =>
                  Advent.Input.Open (Arg);
                  Required := Argument_Type'Succ (Required);
               when No_More =>
                  raise Advent_Error with "Too many arguments!";
            end case;
         end if;
      end;
   end loop;

   if Required /= Argument_Type'Last then
      raise Advent_Error with "Required arguments missing";
   end if;

   if Solution = null then
      raise Advent_Error with "No solution selected";
   end if;

   if not Advent.Input.Is_Open then
      raise Advent_Error with "Unable to open input file";
   end if;

   Solution.all;
   CLI.Set_Exit_Status (0);
exception
   when E : Advent_Error =>
      Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, "ERROR: ");
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Ada.Exceptions.Exception_Message (E));
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Usage: advent17 [--verbose] <puzzle> <filename>");
      CLI.Set_Exit_Status (1);
end Main;
