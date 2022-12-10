with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;
with Ada.Streams;

procedure Day10_1 is
   Sum : Natural := 0;

   type Machine is tagged record
      Cycle : Positive := 1;
      X     : Integer := 1;
      Signal_Strength : Integer := 1;
   end record;

   type Opcode is (addx, noop);

   type Instruction is record
      Op : Opcode;
      Arg : Integer;
   end record;

   procedure Machine_Write
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : Machine);
   for Machine'Write use Machine_Write;

   procedure Machine_Write
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : Machine)
   is
   begin
      String'Write (Stream, Item.Cycle'Image & " : X=" & Item.X'Image & " Signal_Strength=" & Item.Signal_Strength'Image);
   end Machine_Write;

   procedure Fetch
      (This : in out Machine;
       Insn : out Instruction)
   is
      Op_String : constant String := Read_Until (Input, Whitespace);
   begin
      Insn.Op := Opcode'Value (Op_String);
      if Insn.Op = addx then
         Insn.Arg := Get (Input);
      end if;
   end Fetch;

   function Cycle_Count
      (Insn : Instruction)
      return Positive
   is
   begin
      case Insn.Op is
         when addx => return 2;
         when noop => return 1;
      end case;
   end Cycle_Count;

   procedure Execute
      (This   : in out Machine;
       Insn   : Instruction)
   is
      Num_Cycles : constant Positive := Cycle_Count (Insn);
   begin

      for C in 1 .. Num_Cycles loop
         This.Signal_Strength := This.Cycle * This.X;

         if C = Num_Cycles then
            case Insn.Op is
               when addx =>
                  This.X := This.X + Insn.Arg;
               when noop =>
                  null;
            end case;
         end if;


         if This.Cycle = 20 or else (This.Cycle - 20) mod 40 = 0 then
            Sum := Sum + This.Signal_Strength;
         end if;

         This.Cycle := This.Cycle + 1;
      end loop;

   end Execute;

   Interest_List : constant array (1 .. 6) of Natural :=
      (20, 60, 100, 140, 180, 220);
   VM   : Machine;
   Insn : Instruction;
begin
   while not End_Of_Input loop
      Fetch (VM, Insn);
      Execute (VM, Insn);
   end loop;

   Put (Output, Sum);
   New_Line (Output);
end Day10_1;
