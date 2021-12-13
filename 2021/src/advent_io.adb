with Ada.Text_IO;

package body Advent_IO is
   Input_Buffer : aliased String (1 .. 64);

   STDIN  : aliased FD_Stream :=
      FD_Stream'(Root_Stream_Type with FD => Interfaces.C_Streams.stdin);
   STDOUT : aliased FD_Stream :=
      FD_Stream'(Root_Stream_Type with FD => Interfaces.C_Streams.stdout);
   STDERR : aliased FD_Stream :=
      FD_Stream'(Root_Stream_Type with FD => Interfaces.C_Streams.stderr);

   overriding
   procedure Read
      (Stream : in out FD_Stream;
       Item   : out Stream_Element_Array;
       Last   : out Stream_Element_Offset)
   is
      use Interfaces.C_Streams;
      Status : Integer;
   begin
      Ada.Text_IO.Put_Line ("Read " & Item'Length'Image);
      Status := Integer
         (fread
            (buffer => voids (Item'Address),
             size   => size_t (Item'Length),
             count  => 1,
             stream => Stream.FD));
      Ada.Text_IO.Put_Line ("Read Status=" & Status'Image);
      Last := Stream_Element_Offset (Status);
   end Read;

   overriding
   procedure Write
      (Stream : in out FD_Stream;
       Item   : Stream_Element_Array)
   is
      use Interfaces.C_Streams;
      Length : Stream_Element_Offset;
   begin
      Length := Stream_Element_Offset (fwrite
         (buffer => voids (Item'Address),
          size   => size_t (Item'Length),
          count  => 1,
          stream => Stream.FD));
      if Length > 0 and Length < Item'Length then
         Write (Stream, Item (Item'First + Length .. Item'Last));
      end if;
      Ada.Text_IO.Put_Line ("Write Length=" & Length'Image);
   end Write;

   function Input
      return Stream_Access
   is (STDIN'Access);

   function Output
      return Stream_Access
   is (STDOUT'Access);

   function Error
      return Stream_Access
   is (STDERR'Access);

   function End_Of_Input
      return Boolean
   is
      Status : Integer;
   begin
      Status := Integer (Interfaces.C_Streams.feof (Interfaces.C_Streams.stdin));
      Ada.Text_IO.Put_Line ("feof=" & Status'Image);
      if Status /= 0 then
         return True;
      end if;

      Status := Integer (Interfaces.C_Streams.ferror (Interfaces.C_Streams.stdin));
      Ada.Text_IO.Put_Line ("ferror =" & Status'Image);
      return Status /= 0;
   end End_Of_Input;

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
      String'Write (S, "" & ASCII.LF);
   end New_Line;

   procedure Flush is
      use Interfaces.C_Streams;
      Status : Integer
         with Unreferenced;
   begin
      Status := Integer (fflush (NULL_Stream));
   end Flush;

begin
   declare
      use Interfaces.C_Streams;
      Status : Integer with Unreferenced;
   begin
      Status := Integer (setvbuf
         (Interfaces.C_Streams.stdin,
          Input_Buffer'Address,
          IOFBF,
          size_t (Input_Buffer'Length)));
   end;
end Advent_IO;
