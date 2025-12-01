pragma Style_Checks ("M120");
pragma Warnings (Off, """System.Mmap"" is an internal GNAT unit");
pragma Warnings (Off, "use of this unit is non-portable and version-dependent");
private with System.Mmap;

package Advent.Input
   with SPARK_Mode => On
is

   type Buffer is private;

   procedure Open
      (This     : in out Buffer;
       Filename : String)
   with Global => null;

   function Is_Open
      (This : Buffer)
       return Boolean
   with Global => null;

   function Peek
      (This   : Buffer;
       Offset : Positive := 1)
       return Character
   with Global => null;
   --  Returns the Character at Offset distance from the cursor.
   --  Defaults to the current cursor location (1 based)

   function Lookahead
      (This : Buffer;
       N    : Positive)
       return String
   with Global => null;
   --  Returns the next N Characters starting from the cursor

   procedure Get
      (This : in out Buffer;
       Item : out Character)
   with Global => null;
   --  Returns the Character at the current cursor position, then increments the cursor position.

   type Seek_From is (Seek_Start, Seek_Current, Seek_End);
   subtype Seek_Offset is Integer;
   procedure Seek
      (This   : in out Buffer;
       Offset : Seek_Offset;
       From   : Seek_From := Seek_Current)
   with Global => null;

   function Tell
      (This : Buffer)
       return Seek_Offset
   with Global => null;

   procedure Read_Until
      (This : in out Buffer;
       Stop : String; --  matches "any" character in Stop
       Item : out String;
       Last : out Natural)
   with Global => null;

   procedure Read_Until
      (This : in out Buffer;
       Stop : Character;
       Item : out String;
       Last : out Natural)
   with Global => null;

   function End_Of_Input
      (This : Buffer)
       return Boolean
   with Global => null;

   function Length
      (This : Buffer)
       return Natural
   with Global => null;

   procedure Get_Integer
      (This : in out Buffer;
       N    : out Integer)
   with Global => null;

   procedure Get_Long
      (This : in out Buffer;
       N    : out Long_Long_Integer)
   with Global => null;

   procedure Skip_Whitespace
      (This : in out Buffer)
   with Global => null;

   procedure Expect
      (This   : in out Buffer;
       Prefix : String)
   with Global => null;
   --  If the next N characters don't equal Prefix, Expect throws a Program_Error exception

   procedure Expect
      (This : in out Buffer;
       Ch   : Character)
   with Global => null;

private

   type Buffer is record
      File   : System.Mmap.Mapped_File := System.Mmap.Invalid_Mapped_File;
      Region : System.Mmap.Mapped_Region := System.Mmap.Invalid_Mapped_Region;
      Offset : System.Mmap.File_Size := 0;
      Last   : System.Mmap.File_Size := 0;
   end record;

end Advent.Input;
