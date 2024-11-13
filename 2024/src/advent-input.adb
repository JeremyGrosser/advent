pragma Warnings (Off, """System.Mmap"" is an internal GNAT unit");
pragma Warnings (Off, "use of this unit is non-portable and version-dependent");
with System.Mmap;

package body Advent.Input
   with SPARK_Mode => Off
is
   use type System.Mmap.File_Size;
   use type System.Mmap.Mapped_File;

   File     : System.Mmap.Mapped_File := System.Mmap.Invalid_Mapped_File;
   Region   : System.Mmap.Mapped_Region := System.Mmap.Invalid_Mapped_Region;
   G_Offset : System.Mmap.File_Size := 0;
   G_Last   : System.Mmap.File_Size := 0;

   procedure Open
      (Filename : String)
   is
   begin
      File := System.Mmap.Open_Read_No_Exception (Filename);
      if File /= System.Mmap.Invalid_Mapped_File then
         Region := System.Mmap.Read (File);
         G_Last := System.Mmap.Length (File);
      end if;
   end Open;

   function Is_Open
      return Boolean
   is (File /= System.Mmap.Invalid_Mapped_File);

   procedure Get
      (Item : out Character)
   is
      Data : constant System.Mmap.Str_Access := System.Mmap.Data (Region);
   begin
      G_Offset := G_Offset + 1;
      Item := Data (Natural (G_Offset));
   end Get;

   procedure Seek
      (Offset : Seek_Offset;
       From   : Seek_From := Current)
   is
      Off : constant System.Mmap.File_Size := System.Mmap.File_Size (Offset);
   begin
      case From is
         when First =>
            G_Offset := Off;
         when Current =>
            G_Offset := G_Offset + Off;
         when Last =>
            G_Offset := G_Last - Off;
      end case;
   end Seek;

   function Tell
      return Seek_Offset
   is (Seek_Offset (G_Offset));

   function Index
      (S       : String;
       Charset : String)
       return Natural
   is
   begin
      for I in S'Range loop
         for C of Charset loop
            if S (I) = C then
               return I;
            end if;
         end loop;
      end loop;
      return 0;
   end Index;

   procedure Read_Until
      (Stop : String;
       Item : out String;
       Last : out Natural)
   is
      Data       : constant System.Mmap.Str_Access := System.Mmap.Data (Region);
      Data_First : constant Natural := Natural (G_Offset) + 1;
      Data_Last  : Natural := Natural (G_Last);
   begin
      Data_Last := Index (String (Data (Data_First .. Data_Last)), Stop);
      if Data_Last = 0 then
         --  Stop does not occur in remaining Data, return everything and
         --  advance offset past the end of file
         Data_Last := Natural (G_Last);
         G_Offset := G_Last + 1;
      else
         --  Advance the offset to Last
         G_Offset := System.Mmap.File_Size (Data_Last);
         Data_Last := Data_Last - 1;
      end if;

      Last := Item'First + (Data_Last - Data_First);
      Item (Item'First .. Last) := String (Data (Data_First .. Data_Last));
   end Read_Until;

   procedure Read_Until
      (Stop : Character;
       Item : out String;
       Last : out Natural)
   is
   begin
      Read_Until ("" & Stop, Item, Last);
   end Read_Until;

   function Read_Until
      (Stop : String)
       return String
   is
      Data  : constant System.Mmap.Str_Access := System.Mmap.Data (Region);
      First : constant Natural := Natural (G_Offset) + 1;
      Last  : Natural := Natural (G_Last);
   begin
      Last := Index (String (Data (First .. Last)), Stop);
      if Last = 0 then
         Last := Natural (G_Last);
         G_Offset := System.Mmap.File_Size (Last + 1);
      else
         G_Offset := System.Mmap.File_Size (Last);
         Last := Last - 1;
      end if;

      return String (Data (First .. Last));
   end Read_Until;

   function Read_Until
      (Stop : Character)
      return String
   is (Read_Until ("" & Stop));

   function End_Of_Input
      return Boolean
   is
      use System.Mmap;
   begin
      return G_Offset >= G_Last - 1;
   end End_Of_Input;

   function Length
      return Natural
   is (Natural (G_Last) - 1);

   function Peek
      (Offset : Positive := 1)
       return Character
   is
      Data  : constant System.Mmap.Str_Access := System.Mmap.Data (Region);
      First : constant Natural := Natural (G_Offset);
   begin
      return Data.all (First + Offset);
   end Peek;

   function Lookahead
      (N : Positive)
      return String
   is
      Data  : constant System.Mmap.Str_Access := System.Mmap.Data (Region);
      First : constant Natural := Natural (G_Offset) + 1;
      Last  : Natural := First + N - 1;
   begin
      if Last > Natural (G_Last) then
         Last := Natural (G_Last);
      end if;
      return String (Data.all (First .. Last));
   end Lookahead;
end Advent.Input;
