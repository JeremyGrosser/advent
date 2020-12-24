with Ada.Text_IO; use Ada.Text_IO;

package Advent.D8 is
   Halt : exception;

   subtype Word is Integer;
   subtype Address is Word range 0 .. 8192;
   --  type Memory_Array is array (Address) of Word;

   type Operation is
      (nop, acc, jmp);

   type Instruction is record
      Op  : Operation;
      Arg : Word;
   end record;

   package Instruction_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Address,
       Element_Type => Instruction);

   subtype Program is Instruction_Vectors.Vector;

   type Machine is record
      --  Memory      : Memory_Array := (others => 0);
      PC          : Address := 0;
      Accumulator : Word := 0;
   end record;

   function Parse
      (Text : String)
      return Instruction;

   function Read
      (Input : File_Type)
      return Program;

   procedure Execute
      (This : in out Machine;
       I    : Instruction);

   procedure Run_Program
      (This : in out Machine;
       Code : Program);

   function Does_Terminate
      (Code : Program)
      return Boolean;

   function Modify_Program
      (Code   : Program;
       Offset : Address)
      return Program;

   procedure Print
      (Code : Program);

   procedure Dump_Registers
      (This : Machine);

   procedure Put
      (I : Instruction);

   function Part_1
      (Filename : String)
      return Integer;

   function Part_2
      (Filename : String)
      return Integer;

   procedure Run;

end Advent.D8;
