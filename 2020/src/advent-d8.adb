with Str; use Str;

package body Advent.D8 is
   procedure Dump_Registers
      (This : Machine)
   is
   begin
      Put_Line ("ACC=" & This.Accumulator'Image & " PC=" & This.PC'Image);
   end Dump_Registers;

   function Parse
      (Text : String)
      return Instruction
   is
      Op  : constant Operation := Operation'Value (Split (Text, ' ', 0));
      Arg : constant Word := Word'Value (Split (Text, ' ', 1));
   begin
      return (Op, Arg);
   end Parse;

   function Read
      (Input : File_Type)
      return Program
   is
      P : Program;
   begin
      loop
         exit when End_Of_File (Input);
         P.Append (Parse (Get_Line (Input)));
      end loop;
      return P;
   end Read;

   procedure Execute
      (This : in out Machine;
       I    : Instruction)
   is
      Order : Address := This.PC + 1;
   begin
      --  Put (I);
      case I.Op is
         when acc =>
            This.Accumulator := This.Accumulator + I.Arg;
            This.PC := This.PC + 1;
         when jmp =>
            This.PC := This.PC + I.Arg;
         when nop =>
            This.PC := This.PC + 1;
      end case;
      --  Put (" : ");
      --  Dump_Registers (This);
   end Execute;

   procedure Put
      (I : Instruction)
   is
   begin
      Put (I.Op'Image & ' ' & I.Arg'Image);
   end Put;

   procedure Run_Program
      (This : in out Machine;
       Code : Program)
   is
   begin
      loop
         Execute (This, Code (This.PC));
         if This.PC > Address (Code.Last_Index) then
            --  raise Halt with "Program Counter overflow";
            return;
         end if;
      end loop;
   end Run_Program;

   function Does_Terminate
      (Code : Program)
      return Boolean
   is
      Executed : array (0 .. Integer (Code.Length)) of Boolean := (others => False);
      I : Instruction;
      M : Machine;
   begin
      loop
         if Executed (M.PC) = True then
            return False;
         end if;
         Executed (M.PC) := True;
         I := Code (M.PC);
         Execute (M, I);
         if M.PC > Address (Code.Last_Index) then
            return True;
         end if;
      end loop;
   end Does_Terminate;

   function Part_1
      (Filename : String)
      return Integer
   is
      Input : File_Type;
      M     : Machine;
      I     : Instruction;
      Code  : Program;
   begin
      Open (Input, In_File, Filename);
      Code := Read (Input);
      Close (Input);

      declare
         Executed : array (0 .. Integer (Code.Length)) of Boolean := (others => False);
      begin
         loop
            exit when Executed (M.PC) = True;
            Executed (M.PC) := True;
            I := Code (M.PC);
            Execute (M, I);
         end loop;
      end;
      return M.Accumulator;
   end Part_1;

   function Modify_Program
      (Code   : Program;
       Offset : Address)
      return Program
   is
      Modified : Program := Code;
   begin
      if Modified (Offset).Op = nop then
         Modified (Offset).Op := jmp;
      elsif Modified (Offset).Op = jmp then
         Modified (Offset).Op := nop;
      end if;
      return Modified;
   end Modify_Program;

   procedure Print
      (Code : Program)
   is
   begin
      for I of Code loop
         Put (I);
         Put_Line ("");
      end loop;
   end Print;

   function Part_2
      (Filename : String)
      return Integer
   is
      Input     : File_Type;
      Code      : Program;
      Candidate : Program;
      M         : Machine;
   begin
      Open (Input, In_File, Filename);
      Code := Read (Input);
      Close (Input);

      for I in 0 .. Address (Code.Last_Index) loop
         Candidate := Modify_Program (Code, I);
         if Does_Terminate (Candidate) then
            Run_Program (M, Candidate);
            return M.Accumulator;
         end if;
      end loop;
      raise No_Answer;
   end Part_2;

   procedure Run is
   begin
      Test (Part_1'Access, "8.1", "input/d8-test", 5);
      Put_Line ("8.1 solution: " & Part_1 ("input/d8")'Image);

      Test (Part_2'Access, "8.2", "input/d8-test2", 8);
      Put_Line ("8.2 solution: " & Part_2 ("input/d8")'Image);
   end Run;
end Advent.D8;
