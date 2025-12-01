pragma Style_Checks ("M120");
pragma Warnings (Off, """System.Mmap"" is an internal GNAT unit");
pragma Warnings (Off, "use of this unit is non-portable and version-dependent");
private with System.Mmap;

package Advent.Input is

   type Buffer is private;

   procedure Open
      (This     : in out Buffer;
       Filename : String);

   function Is_Open
      (This : Buffer)
       return Boolean;

   function Peek
      (This   : Buffer;
       Offset : Positive := 1)
       return Character;
   --  Returns the Character at Offset distance from the cursor.
   --  Defaults to the current cursor location (1 based)

   function Lookahead
      (This : Buffer;
       N    : Positive)
       return String;
   --  Returns the next N Characters starting from the cursor

   procedure Get
      (This : in out Buffer;
       Item : out Character);
   --  Returns the Character at the current cursor position, then increments the cursor position.

   type Seek_From is (Seek_Start, Seek_Current, Seek_End);
   subtype Seek_Offset is Integer;
   procedure Seek
      (This   : in out Buffer;
       Offset : Seek_Offset;
       From   : Seek_From := Seek_Current);

   function Tell
      (This : Buffer)
       return Seek_Offset;

   procedure Read_Until
      (This : in out Buffer;
       Stop : String; --  matches "any" character in Stop
       Item : out String;
       Last : out Natural);

   procedure Read_Until
      (This : in out Buffer;
       Stop : Character;
       Item : out String;
       Last : out Natural);

   function Read_Until
      (This : in out Buffer;
       Stop : Character)
       return String;

   function Read_Until
      (This : in out Buffer;
       Stop : String)
      return String;

   function End_Of_Input
      (This : Buffer)
       return Boolean;

   function Length
      (This : Buffer)
       return Natural;

   function Get_Integer
      (This : in out Buffer)
       return Integer;

   function Get_Long
      (This : in out Buffer)
       return Long_Long_Integer;

   procedure Skip_Whitespace
      (This : in out Buffer);

   function Match
      (This   : in out Buffer;
       Prefix : String)
       return Boolean;
   --  If the N characters equal Prefix, Match advances the cursor and returns True.
   --  Otherwise, it returns False and does not modify the cursor position.

   procedure Expect
      (This   : in out Buffer;
       Prefix : String);
   --  If the next N characters don't equal Prefix, Expect throws a Program_Error exception

   function Match
      (This : in out Buffer;
       Ch   : Character)
      return Boolean;

   procedure Expect
      (This : in out Buffer;
       Ch   : Character);

private

   type Buffer is record
      File   : System.Mmap.Mapped_File := System.Mmap.Invalid_Mapped_File;
      Region : System.Mmap.Mapped_Region := System.Mmap.Invalid_Mapped_Region;
      Offset : System.Mmap.File_Size := 0;
      Last   : System.Mmap.File_Size := 0;
   end record;

end Advent.Input;
