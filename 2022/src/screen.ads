with Ada.Real_Time;
with Notcurses;

package Screen is

   Width             : constant := 40;
   Height            : constant := 6;

   Frames_Per_Second : constant := 1.0;
   Pixel_Clock       : constant Ada.Real_Time.Time_Span := Ada.Real_Time.To_Time_Span
      (1.0 / Frames_Per_Second / (Width * Height));

   type CRT is tagged record
      Top, Debug, Output : Notcurses.Notcurses_Plane;
      HSync : Ada.Real_Time.Time;
   end record;

   procedure Initialize
      (This : in out CRT);

   procedure Update
      (This  : in out CRT;
       Cycle : Positive;
       X     : Integer);

   procedure Finalize
      (This : in out CRT);

end Screen;
