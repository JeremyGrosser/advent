with Advent_IO; use Advent_IO;
with Handheld;

procedure Day10_2 is
   VM : Handheld.CPU;
begin
   VM.Initialize (Input);
   while not End_Of_Input loop
      VM.Tick;
   end loop;
   delay 5.0;
   VM.Finalize;
end Day10_2;
