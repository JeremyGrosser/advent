with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Advent.D1;
with Advent.D2;
with Advent.D3;
with Advent.D4;
with Advent.D5;
with Advent.D6;
with Advent.D7;
with Advent.D8;
with Advent.D9;
with Advent.D10;
with Advent.D11;
with Advent.D12;
with Advent.D13;

procedure Advent2020 is
begin
   if Argument_Count < 1 then
      return;
   end if;

   case Positive'Value (Argument (1)) is
      when 1 => Advent.D1.Run;
      when 2 => Advent.D2.Run;
      when 3 => Advent.D3.Run;
      when 4 => Advent.D4.Run;
      when 5 => Advent.D5.Run;
      when 6 => Advent.D6.Run;
      when 7 => Advent.D7.Run;
      when 8 => Advent.D8.Run;
      when 9 => Advent.D9.Run;
      when 10 => Advent.D10.Run;
      when 11 => Advent.D11.Run;
      when 12 => Advent.D12.Run;
      when 13 => Advent.D13.Run;
      when others => Put_Line ("Unimplemented.");
   end case;
end Advent2020;
