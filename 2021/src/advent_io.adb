with Ada.Containers.Vectors;

package body Advent_IO is
   use Ada.Streams.Stream_IO;

   STDIN, STDOUT, STDERR : File_Type;

   function Standard_Input
      return Stream_Access
   is (Stream (STDIN));

   function Standard_Output
      return Stream_Access
   is (Stream (STDOUT));

   function Standard_Error
      return Stream_Access
   is (Stream (STDERR));

   function End_Of_File
      return Boolean
   is (Ada.Streams.Stream_IO.End_Of_File (STDIN));

   function Read_Until
      (Stop : Ada.Strings.Maps.Character_Set)
       return String
   is
      Ch : Character;
   begin
      while not End_Of_File loop
         Character'Read (Standard_Input, Ch);
         exit when Ada.Strings.Maps.Is_In (Ch, Stop);
         return Ch & Read_Until (Stop);
      end loop;
      return "";
   end Read_Until;

   function Read_Until
      (C : Character)
       return String
   is (Read_Until (Ada.Strings.Maps.To_Set ("" & C)));

   function Get_Integer
      return Integer
   is
      S : constant String := Read_Until (Whitespace);
   begin
      if S'Length > 1 then
         return Integer'Value (S);
      else
         raise End_Of_Input;
      end if;
   end Get_Integer;

   function Get_Integers
      return Integers
   is
      package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type => Positive,
          Element_Type => Integer);
      use Integer_Vectors;
      V : Vector := Empty_Vector;
   begin
      while not End_Of_File loop
         Append (V, Get_Integer);
      end loop;

      declare
         X : Integers (1 .. Positive (Length (V)));
      begin
         for I in X'Range loop
            X (I) := V (I);
         end loop;
         return X;
      end;
   end Get_Integers;

   procedure Put
      (I : Integer)
   is
   begin
      String'Write (Standard_Output, I'Image);
   end Put;

   procedure New_Line is
   begin
      String'Write (Standard_Output, "" & ASCII.LF);
   end New_Line;

   procedure Flush is
   begin
      Ada.Streams.Stream_IO.Flush (STDOUT);
   end Flush;
begin
   --  So, technically the stdio streams are opened automatically as FD 0,1,2
   --  when the process starts, but we can't get references to those without
   --  making calls into Interfaces.C_Streams. /dev/std{in,out,err} are
   --  symlinks to /proc/$PID/fd/{0,1,2}. Opening them clones the descriptors
   --  as 3,4,5 but they still point to the same memory in the kernel. It's a
   --  little wasteful, but far simpler than making syscalls.
   Open (STDIN, In_File, "/dev/stdin");
   Open (STDOUT, Out_File, "/dev/stdout");
   Open (STDERR, Out_File, "/dev/stderr");
end Advent_IO;
