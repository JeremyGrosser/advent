private with Chests.Stacks;
private with Screen;
with Advent_IO;

package Handheld is

   type CPU is tagged private;

   --  Reset CPU state
   procedure Initialize
      (This  : in out CPU;
       Input : not null Advent_IO.Stream_Access);

   --  Fetch, Decode, Execute a single cycle
   procedure Tick
      (This : in out CPU);

private

   package Integer_Stacks is new Chests.Stacks
      (Element_Type  => Integer,
       Capacity      => 1); --  Max Argument_Count

   type Opcode is (noop, addx);

   type CPU is tagged record
      X        : Integer;
      Cycle    : Positive;
      Op       : Opcode;
      Args     : Integer_Stacks.Stack;
      Input    : Advent_IO.Stream_Access;
      Argument_Count : Natural;
      Output   : Screen.CRT;
   end record;

   --  Read the Opcode
   procedure Fetch_1
      (This : in out CPU);

   --  Populate This.Argument_Count
   procedure Decode
      (This : in out CPU);

   --  Read arguments and push to the Args Stack
   procedure Fetch_2
      (This : in out CPU);

   --  Execute the Op loaded by Fetch_1 using the arguments from Fetch_2
   procedure Execute
      (This : in out CPU);

end Handheld;
