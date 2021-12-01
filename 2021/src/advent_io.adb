package body Advent_IO is
   use Ada.Streams.Stream_IO;

   STDIN, STDOUT, STDERR : File_Type;

   function Input
      return Stream_Access
   is (Stream (STDIN));

   function Output
      return Stream_Access
   is (Stream (STDOUT));

   function Error
      return Stream_Access
   is (Stream (STDERR));

   function End_Of_Input
      return Boolean
   is (Ada.Streams.Stream_IO.End_Of_File (STDIN));

   function Read_Until
      (S : not null access Ada.Streams.Root_Stream_Type'Class;
       Stop : Ada.Strings.Maps.Character_Set)
       return String
   is
      Ch : Character;
   begin
      while not End_Of_Input loop
         Character'Read (S, Ch);
         exit when Ada.Strings.Maps.Is_In (Ch, Stop);
         return Ch & Read_Until (S, Stop);
      end loop;
      return "";
   end Read_Until;

   function Read_Until
      (S : not null access Ada.Streams.Root_Stream_Type'Class;
       C : Character)
       return String
   is (Read_Until (S, Ada.Strings.Maps.To_Set ("" & C)));

   procedure New_Line
      (S : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      String'Write (Output, "" & ASCII.LF);
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
