pragma Warnings (Off, """System.Mmap"" is an internal GNAT unit");
pragma Warnings (Off, "use of this unit is non-portable and version-dependent");
private with System.Mmap;
with Ada.Text_IO.Text_Streams;
with Ada.Text_IO;
with Ada.Strings.Maps;
with Ada.Streams;

package Advent_IO is

   type Mapped_Stream is new Ada.Streams.Root_Stream_Type with private;
   type Stream_Access is access Mapped_Stream;

   overriding
   procedure Read
      (Stream : in out Mapped_Stream;
       Item   : out Ada.Streams.Stream_Element_Array;
       Last   : out Ada.Streams.Stream_Element_Offset);

   overriding
   procedure Write
      (Stream : in out Mapped_Stream;
       Item   : Ada.Streams.Stream_Element_Array)
   is null;
   --  not supported

   type Seek_From is (Seek_Start, Seek_Current, Seek_End);
   subtype Seek_Offset is Integer;
   procedure Seek
      (Stream : not null Stream_Access;
       Offset : Seek_Offset;
       From   : Seek_From := Seek_Current)
   with Pre => Stream = Input; --  TODO: support other streams

   function Tell
      (Stream : not null Stream_Access)
      return Seek_Offset
   with Pre => Stream = Input; --  TODO: support other streams

   function Read_Until
      (Stream : not null Stream_Access;
       Stop   : Character)
       return String;

   function Read_Until
      (Stream : not null Stream_Access;
       Stop   : Ada.Strings.Maps.Character_Set)
       return String;

   procedure Read_Until
      (Stream : not null Stream_Access;
       Stop   : Character;
       Item   : out String;
       Last   : out Natural);

   function End_Of_File
      (Stream : not null Stream_Access)
      return Boolean;

   function Stream
      (Filename : String)
      return Stream_Access;

   Whitespace : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set
         (ASCII.HT & ASCII.LF & ASCII.CR & ' ');
   CRLF       : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set
         (ASCII.CR & ASCII.LF);

   Input  : Stream_Access := null;
   Output : constant Ada.Text_IO.Text_Streams.Stream_Access :=
      Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Output);
   Error  : constant Ada.Text_IO.Text_Streams.Stream_Access :=
      Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Error);

   function End_Of_Input
      return Boolean;

   procedure New_Line
      (Stream : not null Ada.Text_IO.Text_Streams.Stream_Access);

private

   type Mapped_Stream is new Ada.Streams.Root_Stream_Type with record
      File   : System.Mmap.Mapped_File;
      Region : System.Mmap.Mapped_Region;
      Offset : System.Mmap.File_Size;
      Last   : System.Mmap.File_Size;
   end record;

   function Stream
      (File : System.Mmap.Mapped_File)
      return Stream_Access;

end Advent_IO;
