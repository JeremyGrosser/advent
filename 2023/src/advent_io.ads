package Advent_IO
   with Elaborate_Body,
        SPARK_Mode => On
is

   procedure Open
      (Filename : String);

   procedure Get
      (Item : out Character);

   type Seek_From is (Seek_Start, Seek_Current, Seek_End);
   subtype Seek_Offset is Integer;
   procedure Seek
      (Offset : Seek_Offset;
       From   : Seek_From := Seek_Current);

   function Tell
      return Seek_Offset;

   CRLF : constant String := ASCII.CR & ASCII.LF;

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

   function Input_Length
      return Natural;

   function Peek
      (Offset : Positive := 1)
      return Character;

   function Lookahead
      (N : Positive)
       return String;

end Advent_IO;
