pragma Style_Checks ("M120");

package body Advent.Input
   with SPARK_Mode => Off
is
   use type System.Mmap.File_Size;
   use type System.Mmap.Mapped_File;

   procedure Open
      (This     : in out Buffer;
       Filename : String)
   is
   begin
      This.File := System.Mmap.Open_Read_No_Exception (Filename);
      if This.File /= System.Mmap.Invalid_Mapped_File then
         This.Region := System.Mmap.Read (This.File);
         This.Last := System.Mmap.Length (This.File);
      end if;
   end Open;

   function Is_Open
      (This : Buffer)
       return Boolean
   is (This.File /= System.Mmap.Invalid_Mapped_File);

   procedure Get
      (This : in out Buffer;
       Item : out Character)
   is
      Data : constant System.Mmap.Str_Access := System.Mmap.Data (This.Region);
   begin
      This.Offset := This.Offset + 1;
      Item := Data (Natural (This.Offset));
   end Get;

   procedure Seek
      (This   : in out Buffer;
       Offset : Seek_Offset;
       From   : Seek_From := Seek_Current)
   is
      Off : constant System.Mmap.File_Size := System.Mmap.File_Size (Offset);
   begin
      case From is
         when Seek_Start =>
            This.Offset := Off;
         when Seek_Current =>
            This.Offset := This.Offset + Off;
         when Seek_End =>
            This.Offset := This.Last - Off;
      end case;
   end Seek;

   function Tell
      (This : Buffer)
       return Seek_Offset
   is (Seek_Offset (This.Offset));

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
      (This : in out Buffer;
       Stop : String;
       Item : out String;
       Last : out Natural)
   is
      Data       : constant System.Mmap.Str_Access := System.Mmap.Data (This.Region);
      Data_First : constant Natural := Natural (This.Offset) + 1;
      Data_Last  : Natural := Natural (This.Last);
   begin
      Item := (others => NUL);
      Data_Last := Index (String (Data (Data_First .. Data_Last)), Stop);
      if Data_Last = 0 then
         --  Stop does not occur in remaining Data, return everything and
         --  advance offset past the end of file
         Data_Last := Natural (This.Last);
         This.Offset := This.Last + 1;
      else
         --  Advance the offset to Last
         This.Offset := System.Mmap.File_Size (Data_Last);
         Data_Last := Data_Last - 1;
      end if;

      Last := Item'First + (Data_Last - Data_First);
      Item (Item'First .. Last) := String (Data (Data_First .. Data_Last));
   end Read_Until;

   procedure Read_Until
      (This : in out Buffer;
       Stop : Character;
       Item : out String;
       Last : out Natural)
   is
   begin
      Read_Until (This, "" & Stop, Item, Last);
   end Read_Until;

   function End_Of_Input
      (This : Buffer)
       return Boolean
   is
      use System.Mmap;
   begin
      return This.Last = 0 or else This.Offset >= This.Last - 1;
   end End_Of_Input;

   function Length
      (This : Buffer)
       return Natural
   is (Natural (This.Last) - 1);

   function Line_Length
      (This : Buffer)
       return Natural
   is
      Remaining : constant Natural := Length (This) - Tell (This);
   begin
      for I in 1 .. Remaining loop
         if Peek (This, I) = ASCII.LF then
            return I - 1;
         end if;
      end loop;
      return Remaining;
   end Line_Length;

   function Peek
      (This   : Buffer;
       Offset : Positive := 1)
       return Character
   is
      Data  : constant System.Mmap.Str_Access := System.Mmap.Data (This.Region);
      First : constant Natural := Natural (This.Offset);
   begin
      return Data.all (First + Offset);
   end Peek;

   function Lookahead
      (This : Buffer;
       N    : Positive)
       return String
   is
      Data  : constant System.Mmap.Str_Access := System.Mmap.Data (This.Region);
      First : constant Natural := Natural (This.Offset) + 1;
      Last  : Natural := First + N - 1;
   begin
      if Last > Natural (This.Last) then
         Last := Natural (This.Last);
      end if;
      return String (Data.all (First .. Last));
   end Lookahead;

   procedure Get_Integer
      (This : in out Buffer;
       N    : out Integer)
   is
      Digit  : constant array (Character range '0' .. '9') of Natural := (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
      Negate : Boolean := False;
      Ch     : Character;
   begin
      N := 0;
      Skip_Whitespace (This);

      if Peek (This) = '-' then
         Negate := True;
         Seek (This, 1);
      end if;

      while not End_Of_Input (This) and then Peek (This) in '0' .. '9' loop
         Get (This, Ch);
         N := N * 10 + Digit (Ch);
      end loop;

      if Negate then
         N := N * (-1);
      end if;
   end Get_Integer;

   procedure Get_Long
      (This : in out Buffer;
       N    : out Long_Long_Integer)
   is
      Digit  : constant array (Character range '0' .. '9') of Long_Long_Integer := (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
      Negate : Boolean := False;
      Ch     : Character;
   begin
      N := 0;
      Skip_Whitespace (This);

      if Peek (This) = '-' then
         Negate := True;
         Seek (This, 1);
      end if;

      while not End_Of_Input (This) and then Peek (This) in '0' .. '9' loop
         Get (This, Ch);
         N := N * 10 + Digit (Ch);
      end loop;

      if Negate then
         N := N * (-1);
      end if;
   end Get_Long;

   procedure Skip_Whitespace
      (This : in out Buffer)
   is
      Ch : Character;
   begin
      while not End_Of_Input (This) loop
         Ch := Peek (This);
         case Ch is
            when ' ' | HT | CR | LF =>
               Seek (This, 1);
            when others =>
               exit;
         end case;
      end loop;
   end Skip_Whitespace;

   procedure Expect
      (This   : in out Buffer;
       Prefix : String)
   is
   begin
      if Lookahead (This, Prefix'Length) = Prefix then
         Seek (This, Prefix'Length);
      else
         raise Program_Error with "Expected """ & Prefix & """, got """ & Lookahead (This, Prefix'Length) & """";
      end if;
   end Expect;

   procedure Expect
      (This : in out Buffer;
       Ch   : Character)
   is
   begin
      Expect (This, Ch & "");
   end Expect;

end Advent.Input;
