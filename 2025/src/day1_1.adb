pragma Style_Checks ("M120");
with Advent.Input;
with Advent.Output;
with Advent; use Advent;

procedure Day1_1 is
begin
   while not Input.End_Of_Input loop
      Input.Seek (1);
   end loop;
   Output.Put (0);
end Day1_1;
