with Advent_IO.Integers; use Advent_IO.Integers;
with Advent_IO; use Advent_IO;

procedure Day1_1 is
   function First_Number
      (Line : String)
      return Natural
   is
   begin
      for Ch of Line loop
         if Ch in '0' .. '9' then
            return Character'Pos (Ch) - Character'Pos ('0');
         end if;
      end loop;
      return 0;
   end First_Number;

   function Last_Number
      (Line : String)
      return Natural
   is
   begin
      for Ch of reverse Line loop
         if Ch in '0' .. '9' then
            return Character'Pos (Ch) - Character'Pos ('0');
         end if;
      end loop;
      return 0;
   end Last_Number;

   Sum : Natural := 0;
begin
   while not End_Of_Input loop
      declare
         Line : constant String := Read_Until (Input, CRLF);
      begin
         Sum := Sum + (First_Number (Line) * 10) + Last_Number (Line);
      end;
   end loop;

   Put (Output, Sum);
   New_Line (Output);
end Day1_1;
