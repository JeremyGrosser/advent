with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;
with Ada.Containers.Vectors;

procedure D10_1 is
   package Integer_IO is new Advent_IO.Generic_Numbers (Integer);
   use Integer_IO;
   package Character_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Character);

   type Parse_Error is (No_Error, Incomplete, Corrupted);
   type Parse_State is record
      Error    : Parse_Error;
      Position : Integer;
      Stack    : Character_Vectors.Vector;
   end record;

   procedure Parse
      (Line  : String;
       State : out Parse_State);

   function Is_Open
      (C : Character)
      return Boolean
   is (C in '(' | '[' | '{' | '<');

   function Is_Close
      (C : Character)
      return Boolean
   is (C in ')' | ']' | '}' | '>');

   function Inverse
      (C : Character)
      return Character
   is
   begin
      case C is
         when '(' => return ')';
         when '[' => return ']';
         when '{' => return '}';
         when '<' => return '>';
         when ')' => return '(';
         when ']' => return '[';
         when '}' => return '{';
         when '>' => return '<';
         when others => return ASCII.NUL;
      end case;
   end Inverse;

   procedure Parse
      (Line  : String;
       State : out Parse_State)
   is
      use Character_Vectors;
      C : Character;
   begin
      State.Error := Incomplete;
      State.Position := Line'First;
      State.Stack := Empty_Vector;

      while State.Position <= Line'Last loop
         C := Line (State.Position);
         if Is_Open (C) then
            Append (State.Stack, C);
         elsif Is_Close (C) then
            if Last_Element (State.Stack) = Inverse (C) then
               Delete_Last (State.Stack);
            else
               State.Error := Corrupted;
               return;
            end if;
         else
            State.Error := Corrupted;
            return;
         end if;
         State.Position := State.Position + 1;
      end loop;

      if Is_Empty (State.Stack) then
         State.Error := No_Error;
      end if;
   end Parse;

   Line_Number : Positive := 1;
   Score : Natural := 0;
begin
   while not End_Of_Input loop
      declare
         Line  : constant String := Read_Until (Input, CRLF);
         State : Parse_State;
      begin
         Parse (Line, State);
         if State.Error = Corrupted then
            case Line (State.Position) is
               when ')' => Score := Score + 3;
               when ']' => Score := Score + 57;
               when '}' => Score := Score + 1197;
               when '>' => Score := Score + 25137;
               when others =>
                  String'Write (Error, "Unexpected character '" & Line (State.Position) & "' at line ");
                  Put (Error, Line_Number);
                  String'Write (Error, ", position ");
                  Put (Error, State.Position);
                  New_Line (Error);
            end case;
         end if;
      end;
      Line_Number := Line_Number + 1;
   end loop;

   Put (Output, Score);
   New_Line (Output);
end D10_1;
