with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;

procedure D1_1 is
   package Integer_IO is new Advent_IO.Generic_Numbers
      (Number => Integer);
   use Integer_IO;

   Current, Previous : Integer;
   Increases : Natural := 0;
begin
   Previous := Get (Input);

   while not End_Of_Input loop
      Current := Get (Input);
      if Current > Previous then
         Increases := Increases + 1;
      end if;
      Previous := Current;
   end loop;

   Put (Output, Increases);
   New_Line (Output);
end D1_1;
