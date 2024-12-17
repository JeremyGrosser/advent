pragma Ada_2022;
with Ada.Containers.Vectors;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day17_1 is
   type UInt3 is mod 2 ** 3;
   type Int is new Integer;

   function "xor"
      (Left, Right : Int)
      return Int
   is
      --  yikes.
      type UInt32 is mod 2 ** 32;
      A : UInt32 with Import, Address => Left'Address;
      B : UInt32 with Import, Address => Right'Address;
      C : aliased UInt32;
      Result : Int with Address => C'Address;
   begin
      C := A xor B;
      return Result;
   end "xor";

   type Program_Offset is new Natural;

   package Program_Vectors is new Ada.Containers.Vectors (Program_Offset, UInt3);
   use Program_Vectors;

   type Opcode is
      (Op_adv,    --  A := A / 2 ** op_combo
       Op_bxl,    --  B := B xor op_literal
       Op_bst,    --  B := op_combo mod 8
       Op_jnz,    --  (if A /= 0 then IP := op_literal else nop)
       Op_bxc,    --  B := B xor C
       Op_out,    --  Output := op_combo mod 8
       Op_bdv,    --  B := A / 2 ** op_combo
       Op_cdv);   --  C := A / 2 ** op_combo
   for Opcode use
      (Op_adv => 0,
       Op_bxl => 1,
       Op_bst => 2,
       Op_jnz => 3,
       Op_bxc => 4,
       Op_out => 5,
       Op_bdv => 6,
       Op_cdv => 7);

   type Register is (A, B, C);
   type Registers is array (Register) of Int;

   type Machine is record
      Reg      : Registers;
      IP       : Program_Offset := 0;
      Program  : Program_Vectors.Vector;
      Halt     : Boolean := False;
   end record;

   type Instruction is record
      Op  : Opcode;
      Arg : UInt3;
   end record;

   procedure Load
      (M : in out Machine)
   is
      procedure Read_Register is
         Ch : Character;
      begin
         loop
            Input.Get (Ch);
            exit when Ch = ' ';
         end loop;

         declare
            Name  : constant String := Input.Read_Until (':');
            R     : constant Register := Register'Value (Name);
            Val   : constant Int := Int (Input.Get_Integer);
         begin
            M.Reg (R) := Val;
         end;
      end Read_Register;

      procedure Read_Program is
         Ch : Character;
      begin
         loop
            Input.Get (Ch);
            exit when Ch = ' ';
         end loop;

         loop
            Append (M.Program, UInt3 (Input.Get_Integer));
            exit when Input.Peek /= ',';
            Input.Seek (1);
         end loop;
      end Read_Program;

      Ch : Character;
   begin
      while not Input.End_Of_Input loop
         Input.Get (Ch);
         case Ch is
            when 'P' =>
               Read_Program;
            when 'R' =>
               Read_Register;
            when ASCII.LF =>
               null;
            when others =>
               raise Program_Error with "Unexpected character in input: " & Ch;
         end case;
      end loop;
   end Load;

   procedure Fetch
      (M    : in out Machine;
       Inst : out Instruction)
   is
   begin
      if (M.IP + 1) > Last_Index (M.Program) then
         M.Halt := True;
      else
         declare
            Op : constant UInt3 := M.Program (M.IP);
         begin
            Inst.Op := Opcode'Enum_Val (Natural (Op));
            Inst.Arg := M.Program (M.IP + 1);
         end;
      end if;
   end Fetch;

   function Decode_Combo
      (M    : Machine;
       Arg  : UInt3)
       return Int
   is
   begin
      if Arg in 0 .. 3 then
         return Int (Arg);
      elsif Arg = 4 then
         return M.Reg (A);
      elsif Arg = 5 then
         return M.Reg (B);
      elsif Arg = 6 then
         return M.Reg (C);
      else
         raise Program_Error with "Combo operand 7 is reserved";
      end if;
   end Decode_Combo;

   First_Output : Boolean := True;

   procedure Write_Out
      (N : Int)
   is
   begin
      if not First_Output then
         Output.Put (",", False);
      end if;
      Output.Put (Integer (N), False);
      First_Output := False;
   end Write_Out;

   procedure Execute
      (M    : in out Machine;
       Inst : Instruction)
   is
      IP_Increment : Program_Offset := 2;
      Arg : Int;
   begin
      Output.Log (Inst'Image);
      case Inst.Op is
         when Op_adv =>
            Arg := Decode_Combo (M, Inst.Arg);
            M.Reg (A) := M.Reg (A) / 2 ** Integer (Arg);
         when Op_bxl =>
            M.Reg (B) := M.Reg (B) xor Int (Inst.Arg);
         when Op_bst =>
            Arg := Decode_Combo (M, Inst.Arg);
            M.Reg (B) := Arg mod 8;
         when Op_jnz =>
            if M.Reg (A) /= 0 then
               M.IP := Program_Offset (Inst.Arg);
               IP_Increment := 0;
            end if;
         when Op_bxc =>
            M.Reg (B) := M.Reg (B) xor M.Reg (C);
         when Op_out =>
            Arg := Decode_Combo (M, Inst.Arg);
            Write_Out (Arg mod 8);
         when Op_bdv =>
            Arg := Decode_Combo (M, Inst.Arg);
            M.Reg (B) := M.Reg (A) / 2 ** Integer (Arg);
         when Op_cdv =>
            Arg := Decode_Combo (M, Inst.Arg);
            M.Reg (C) := M.Reg (A) / 2 ** Integer (Arg);
      end case;

      M.IP := M.IP + IP_Increment;
   end Execute;

   M    : Machine;
   Inst : Instruction;
begin
   Load (M);

   loop
      Output.Log (M'Image);
      Fetch (M, Inst);
      exit when M.Halt;
      Execute (M, Inst);
   end loop;

end Day17_1;
