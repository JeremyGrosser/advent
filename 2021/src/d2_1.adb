with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;

procedure D2_1 is
   package Integer_IO is new Advent_IO.Generic_Numbers
      (Number => Integer);
   use Integer_IO;

   type Position is record
      Horizontal : Integer;
      Depth      : Integer;
   end record;

   type Command is (Forward, Up, Down);

   P : Position := (0, 0);
   C : Command;
   N : Integer;
begin
   while not End_Of_Input loop
      C := Command'Value (Read_Until (Input, Whitespace));
      N := Get (Input);
      case C is
         when Forward =>
            P.Horizontal := P.Horizontal + N;
         when Up =>
            P.Depth := P.Depth - N;
         when Down =>
            P.Depth := P.Depth + N;
      end case;
   end loop;
   Put (Output, P.Horizontal * P.Depth);
   New_Line (Output);
end D2_1;
