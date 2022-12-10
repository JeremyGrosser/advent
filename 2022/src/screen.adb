with Advent_IO; use Advent_IO;
with AnsiAda;

package body Screen is

   procedure Initialize
      (This : in out CRT)
   is
   begin
      String'Write (Output, AnsiAda.Clear_Screen);
   end Initialize;

   procedure Update
      (This  : in out CRT;
       Cycle : Positive;
       X     : Integer)
   is
      Raster_Start_Y : constant := 4;
      Beam_X : constant Natural := (Cycle - 1) mod Width;
      Beam_Y : constant Natural := ((Cycle - 1) / Width) mod Height;
   begin
      String'Write (Output, AnsiAda.Position (1, 1) & AnsiAda.Clear_To_End_Of_Line);
      String'Write (Output, "Cycle " & Cycle'Image & ASCII.LF);
      String'Write (Output, "X=" & X'Image & ASCII.LF);

      String'Write (Output, AnsiAda.Position
         (Row    => Raster_Start_Y + Beam_Y,
          Column => Beam_X + 1));

      if Beam_X in X - 1 .. X + 1 then
         Character'Write (Advent_IO.Output, '#');
      else
         Character'Write (Advent_IO.Output, '.');
      end if;

      delay 0.01;
   end Update;

end Screen;
