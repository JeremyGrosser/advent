with Ada.Strings.Fixed;

package body Advent_IO is
   use type System.Mmap.File_Size;

   function Stream
      (File : System.Mmap.Mapped_File)
      return Stream_Access
   is
      MS : constant not null Stream_Access := new Mapped_Stream;
   begin
      MS.File := File;
      MS.Offset := 0;
      MS.Last := System.Mmap.Length (Stream.File);
      MS.Region := System.Mmap.Read
         (File    => MS.File,
          Offset  => MS.Offset,
          Length  => MS.Last,
          Mutable => False);
      return MS;
   end Stream;

   function Stream
      (Filename : String)
      return Stream_Access
   is (Stream (System.Mmap.Open_Read (Filename)));

   overriding
   procedure Read
      (Stream : in out Mapped_Stream;
       Item   : out Ada.Streams.Stream_Element_Array;
       Last   : out Ada.Streams.Stream_Element_Offset)
   is
      use Ada.Streams;
      Data       : constant System.Mmap.Str_Access := System.Mmap.Data (Stream.Region);
      Data_Last  : constant Natural := Natural (Stream.Last);
      Data_Index : Natural;
   begin
      Last := Item'First - 1;
      for I in Item'Range loop
         Data_Index := Natural (Stream.Offset) + 1;
         exit when Data_Index = Data_Last;
         Last := Last + 1;
         Item (I) := Stream_Element (Character'Pos (Data (Data_Index)));
         Stream.Offset := Stream.Offset + 1;
      end loop;
   end Read;

   procedure Seek
      (Stream : not null Stream_Access;
       Offset : Seek_Offset;
       From   : Seek_From := Seek_Current)
   is
      Off : constant System.Mmap.File_Size := System.Mmap.File_Size (Offset);
   begin
      case From is
         when Seek_Start =>
            Stream.Offset := Off;
         when Seek_Current =>
            Stream.Offset := Stream.Offset + Off;
         when Seek_End =>
            Stream.Offset := Stream.Last - Off;
      end case;
   end Seek;

   function Tell
      (Stream : not null Stream_Access)
      return Seek_Offset
   is (Seek_Offset (Stream.Offset));

   function Index
      (S : String;
       C : Character)
       return Natural
   is
   begin
      for I in S'Range loop
         if S (I) = C then
            return I;
         end if;
      end loop;
      return 0;
   end Index;

   procedure Read_Until
      (Stream : not null Stream_Access;
       Stop   : Character;
       Item   : out String;
       Last   : out Natural)
   is
      Data       : constant System.Mmap.Str_Access := System.Mmap.Data (Stream.Region);
      Data_First : constant Natural := Natural (Stream.Offset) + 1;
      Data_Last  : Natural := Natural (Stream.Last);
   begin
      Data_Last := Index (String (Data (Data_First .. Data_Last)), Stop);
      if Data_Last = 0 then
         --  Stop does not occur in remaining Data, return everything and
         --  advance offset past the end of file
         Data_Last := Natural (Stream.Last);
         Stream.Offset := Stream.Last + 1;
      else
         --  Advance the offset to Last
         Stream.Offset := System.Mmap.File_Size (Data_Last);
         Data_Last := Data_Last - 1;
      end if;

      Last := Item'First + (Data_Last - Data_First);
      Item (Item'First .. Last) := String (Data (Data_First .. Data_Last));
   end Read_Until;

   function Read_Until
      (Stream : not null Stream_Access;
       Stop   : Character)
       return String
   is
      Data  : constant System.Mmap.Str_Access := System.Mmap.Data (Stream.Region);
      First : constant Natural := Natural (Stream.Offset) + 1;
      Last  : Natural := Natural (Stream.Last);
   begin
      Last := Index (String (Data (First .. Last)), Stop);
      if Last = 0 then
         Last := Natural (Stream.Last);
         Stream.Offset := Stream.Last + 1;
      else
         Stream.Offset := System.Mmap.File_Size (Last);
         Last := Last - 1;
      end if;

      return String (Data (First .. Last));
   end Read_Until;

   function Read_Until
      (Stream : not null Stream_Access;
       Stop   : Ada.Strings.Maps.Character_Set)
       return String
   is
      Data  : constant System.Mmap.Str_Access := System.Mmap.Data (Stream.Region);
      First : constant Natural := Natural (Stream.Offset) + 1;
      Last  : Natural := Natural (Stream.Last);
   begin
      Last := Ada.Strings.Fixed.Index (String (Data (First .. Last)), Stop);
      if Last = 0 then
         Last := Natural (Stream.Last);
         Stream.Offset := Stream.Last + 1;
      else
         Stream.Offset := System.Mmap.File_Size (Last);
         Last := Last - 1;
      end if;

      return String (Data (First .. Last));
   end Read_Until;

   function End_Of_File
      (Stream : not null Stream_Access)
      return Boolean
   is
      use System.Mmap;
   begin
      return Stream.Offset >= Stream.Last - 1;
   end End_Of_File;

   function End_Of_Input
      return Boolean
   is (End_Of_File (Input));

   procedure New_Line
      (Stream : not null Ada.Text_IO.Text_Streams.Stream_Access)
   is
   begin
      Character'Write (Stream, ASCII.LF);
   end New_Line;

   function Character_Count
      (Stream : not null Stream_Access;
       Ch     : Character)
      return Natural
   is
      Data  : constant System.Mmap.Str_Access := System.Mmap.Data (Stream.Region);
      First : constant Natural := Natural (Stream.Offset) + 1;
      Last  : constant Natural := Natural (Stream.Last);
      Count : Natural := 0;
   begin
      for D of Data (First .. Last) loop
         if D = Ch then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Character_Count;

   function Length
      (Stream : not null Stream_Access)
      return Natural
   is (Natural (Stream.Last));

   function Peek
      (Stream : not null Stream_Access)
      return Character
   is
      Data  : constant System.Mmap.Str_Access := System.Mmap.Data (Stream.Region);
      First : constant Natural := Natural (Stream.Offset) + 1;
   begin
      return Data.all (First);
   end Peek;

   function Lookahead
      (Stream : not null Stream_Access;
       N      : Positive)
      return String
   is
      Data  : constant System.Mmap.Str_Access := System.Mmap.Data (Stream.Region);
      First : constant Natural := Natural (Stream.Offset) + 1;
      Last  : Natural := First + N - 1;
   begin
      if Last > Natural (Stream.Last) then
         Last := Natural (Stream.Last);
      end if;
      return String (Data.all (First .. Last));
   end Lookahead;

end Advent_IO;
