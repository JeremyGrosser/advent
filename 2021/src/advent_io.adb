package body Advent_IO is
   Input_Buffer : aliased String (1 .. 64);

   STD_IN  : aliased FD_Stream :=
      FD_Stream'(Root_Stream_Type with FD => Interfaces.C_Streams.stdin);
   STD_OUT : aliased FD_Stream :=
      FD_Stream'(Root_Stream_Type with FD => Interfaces.C_Streams.stdout);
   STD_ERR : aliased FD_Stream :=
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
      Status := Integer
         (fread
            (buffer => voids (Item'Address),
             size   => size_t (Item'Length),
             count  => 1,
             stream => Stream.FD));
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
   end Write;

   function Input
      return Stream_Access
   is (STD_IN'Access);

   function Output
      return Stream_Access
   is (STD_OUT'Access);

   function Error
      return Stream_Access
   is (STD_ERR'Access);

   function End_Of_Input
      return Boolean
   is
      use Interfaces.C_Streams;
      Current : constant long := ftell (stdin);
      Last    : long;
      Status  : int;
   begin
      if feof (stdin) /= 0 then
         return True;
      else
         Status := fseek (stdin, 0, SEEK_END);
         Last := ftell (stdin);
         Status := fseek (stdin, Current, SEEK_SET);
         return Long_Integer (Current) = Long_Integer (Last);
      end if;
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
