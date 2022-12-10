package Screen is

   Width  : constant := 40;
   Height : constant := 6;

   type CRT is tagged null record;

   procedure Initialize
      (This : in out CRT);

   procedure Tick
      (This  : in out CRT;
       Cycle : Positive;
       X     : Integer);

end Screen;
