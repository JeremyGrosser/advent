with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;
with Str;

procedure Day1_2 is
   Top : array (1 .. 3) of Natural := (others => 0);
   Total : Natural := 0;

   procedure Add_Elf
      (N : Natural)
   is
   begin
      if N > Top (1) then
         Top (1) := Top (2);
         Top (2) := Top (3);
         Top (3) := N;
      end if;
   end Add_Elf;

   Line : String (1 .. 6);
   Last : Natural;
begin
   while not End_Of_Input loop
      Read_Until (Input, ASCII.LF, Line, Last);
      if Last = 0 then
         Add_Elf (Total);
         Total := 0;
      else
         Total := Total + Str.To_Natural (Line (1 .. Last));
      end if;
   end loop;

   Add_Elf (Total);

   Total := Top (1) + Top (2) + Top (3);

   Put (Output, Total);
   New_Line (Output);
end Day1_2;
