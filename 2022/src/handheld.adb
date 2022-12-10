package body Handheld is

   procedure Initialize
      (This  : in out CPU;
       Input : not null Advent_IO.Stream_Access)
   is
   begin
      This.X := 1;
      This.Cycle := 1;
      This.Op := noop;
      This.Input := Input;
      Integer_Stacks.Clear (This.Args);
      This.Argument_Count := 0;
      This.Output.Initialize;
   end Initialize;

   procedure Tick
      (This : in out CPU)
   is
   begin
      This.Fetch_1;
      This.Decode;
      This.Fetch_2;
      This.Execute;
   end Tick;

   procedure Fetch_1
      (This : in out CPU)
   is
      S : constant String := Advent_IO.Read_Until (This.Input, Advent_IO.Whitespace);
   begin
      This.Op := Opcode'Value (S);
   end Fetch_1;

   procedure Decode
      (This : in out CPU)
   is
   begin
      case This.Op is
         when noop =>
            This.Argument_Count := 0;
         when addx =>
            This.Argument_Count := 1;
      end case;
   end Decode;

   procedure Fetch_2
      (This : in out CPU)
   is
      use Integer_Stacks;
   begin
      for I in 1 .. This.Argument_Count loop
         if Is_Full (This.Args) then
            raise Program_Error with "Argument stack overflow";
         end if;

         Push (This.Args, Integer'Value
            (Advent_IO.Read_Until (This.Input, Advent_IO.Whitespace)));

         This.Output.Update (This.Cycle, This.X);
         This.Cycle := This.Cycle + 1;
         This.Output.Update (This.Cycle, This.X);
      end loop;
   end Fetch_2;

   procedure Execute
      (This : in out CPU)
   is
   begin
      case This.Op is
         when noop =>
            null;
         when addx =>
            This.X := This.X + Integer_Stacks.Pop (This.Args);
      end case;

      This.Cycle := This.Cycle + 1;
      This.Output.Update (This.Cycle, This.X);
   end Execute;

end Handheld;
