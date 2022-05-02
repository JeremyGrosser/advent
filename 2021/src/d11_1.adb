with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;

procedure D11_1 is
   package Integer_IO is new Advent_IO.Generic_Numbers (Integer);
   use Integer_IO;
begin
   while not End_Of_Input loop
      declare
         Line  : constant String := Read_Until (Input, CRLF);
      begin
         null;
      end;
   end loop;

   Put (Output, -1);
   New_Line (Output);
end D11_1;
