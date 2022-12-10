with Notcurses.Channel; use Notcurses.Channel;
with Notcurses.Plane; use Notcurses.Plane;
with Notcurses.Context;

package body Screen is

   procedure Initialize
      (This : in out CRT)
   is
   begin
      Notcurses.Context.Initialize;

      This.Top := Standard_Plane;

      This.Debug := Create_Sub_Plane
         (Plane      => This.Top,
          Position   => (X => 0,  Y => 0),
          Size       => (X => 80, Y => 1));

      This.Output := Create_Sub_Plane
         (Plane      => This.Top,
          Position   => (X => 0, Y => 1),
          Size       => (X => Width, Y => Height));

      This.HSync := Ada.Real_Time.Clock;
   end Initialize;

   procedure Update
      (This  : in out CRT;
       Cycle : Positive;
       X     : Integer)
   is
      use Ada.Real_Time;
      Beam_X : constant Natural := (Cycle - 1) mod Width;
      Beam_Y : constant Natural := ((Cycle - 1) / Width) mod Height;
      Lit : constant Notcurses_Channel :=
         (Use_Palette   => False,
          Not_Default   => True,
          Alpha         => Opaque,
          R             => 0,
          G             => 192,
          B             => 0);
      Dark : constant Notcurses_Channel :=
         (Use_Palette   => False,
          Not_Default   => True,
          Alpha         => Opaque,
          R             => 0,
          G             => 48,
          B             => 0);
   begin
      Erase (This.Debug);
      Put (This.Debug, "Cycle");
      Put (This.Debug, Cycle'Wide_Wide_Image, Y => 0, X => 6);

      Put (This.Debug, "X",    Y => 0, X => 12);
      Put (This.Debug, X'Wide_Wide_Image, Y => 0, X => 14);

      if Beam_X in X - 1 .. X + 1 then
         Set_Foreground (This.Output, Lit);
      else
         Set_Foreground (This.Output, Dark);
      end if;
      Put (This.Output, "" & Wide_Wide_Character'Val (16#2588#), Y => Beam_Y, X => Beam_X);

      Notcurses.Context.Render (Context (This.Top));

      This.HSync := This.HSync + Pixel_Clock;
      delay until This.HSync;
   end Update;

   procedure Finalize
      (This : in out CRT)
   is
      pragma Unreferenced (This);
   begin
      Notcurses.Context.Stop;
   end Finalize;

end Screen;
