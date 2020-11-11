with Ada.Text_IO; use Ada.Text_IO;
with Solutions.Day_1;

procedure Advent2018 is
   Result : Integer;
begin
   if Solutions.Day_1.Part_1 ("input/day1.1a") /= 3 then
      return;
   end if;
   Result := Solutions.Day_1.Part_1 ("input/day1");
   Put_Line ("Solution 1.1: " & Result'Image);

   if Solutions.Day_1.Part_2 ("input/day1.2a") /= 0 then
      return;
   end if;

   if Solutions.Day_1.Part_2 ("input/day1.2b") /= 10 then
      return;
   end if;

   if Solutions.Day_1.Part_2 ("input/day1.2c") /= 5 then
      return;
   end if;
   
   if Solutions.Day_1.Part_2 ("input/day1.2d") /= 14 then
      return;
   end if;
   Result := Solutions.Day_1.Part_2 ("input/day1");
   Put_Line ("Solution 1.2: " & Result'Image);

end Advent2018;
