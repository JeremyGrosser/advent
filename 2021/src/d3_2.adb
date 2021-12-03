with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;

procedure D3_2 is
   package Integer_IO is new Advent_IO.Generic_Numbers
      (Number => Integer);
   use Integer_IO;
begin
   null;
   Put (Output, -1);
   New_Line (Output);
end D3_2;
