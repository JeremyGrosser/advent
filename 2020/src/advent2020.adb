with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Advent.D1;
with Advent.D2;
with Advent.D3;

procedure Advent2020 is
begin
   if Argument_Count < 1 then
      return;
   end if;

   case Positive'Value (Argument (1)) is
      when 1 => Advent.D1.Run;
      when 2 => Advent.D2.Run;
      when 3 => Advent.D3.Run;
      when others => Put_Line ("Unimplemented.");
   end case;
end Advent2020;
