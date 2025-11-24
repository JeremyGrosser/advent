pragma Style_Checks ("M120");

package Advent.Input
   with Elaborate_Body,
        SPARK_Mode => On
is
   procedure Open
      (Filename : String);

   function Is_Open
      return Boolean;

   function Peek
      (Offset : Positive := 1)
      return Character;
   --  Returns the Character at Offset distance from the cursor.
   --  Defaults to the current cursor location (1 based)

   function Lookahead
      (N : Positive)
       return String;
   --  Returns the next N Characters starting from the cursor

   procedure Get
      (Item : out Character);
   --  Returns the Character at the current cursor position, then increments the cursor position.

   type Seek_From is (Seek_Start, Seek_Current, Seek_End);
   subtype Seek_Offset is Integer;
   procedure Seek
      (Offset : Seek_Offset;
       From   : Seek_From := Seek_Current);

   function Tell
      return Seek_Offset;

   procedure Read_Until
      (Stop : String; --  matches "any" character in Stop
       Item : out String;
       Last : out Natural);

   procedure Read_Until
      (Stop : Character;
       Item : out String;
       Last : out Natural);

   function Read_Until
      (Stop : Character)
       return String;

   function Read_Until
      (Stop : String)
      return String;

   function End_Of_Input
      return Boolean;

   function Length
      return Natural;

   function Get_Integer
      return Integer;

   function Get_Long
      return Long_Long_Integer;

   procedure Skip_Whitespace;

   function Match
      (Prefix : String)
      return Boolean;
   --  If the N characters equal Prefix, Match advances the cursor and returns True.
   --  Otherwise, it returns False and does not modify the cursor position.

   procedure Expect
      (Prefix : String);
   --  If the next N characters don't equal Prefix, Expect throws a Program_Error exception

   function Match
      (Ch : Character)
      return Boolean;

   procedure Expect
      (Ch : Character);

end Advent.Input;
