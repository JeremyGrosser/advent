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

   function Read_Until
      (Stop : Ada.Strings.Maps.Character_Set)
       return String
   is
      Ch : Character;
   begin
      loop
         Character'Read (Standard_Input, Ch);
         exit when Ada.Strings.Maps.Is_In (Ch, Stop);
         return Ch & Read_Until (Stop);
      end loop;
      return "";
   exception
      when End_Error => return "";
   end Read_Until;

   function Read_Until
      (C : Character)
       return String
   is (Read_Until (Ada.Strings.Maps.To_Set ("" & C)));

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
