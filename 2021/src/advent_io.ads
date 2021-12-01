with Ada.Streams.Stream_IO;
with Ada.Streams;
with Ada.Strings.Maps;

package Advent_IO is

   End_Of_Input : exception;

   Whitespace : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set
      (ASCII.HT & ASCII.LF & ASCII.CR & ' ');

   CRLF : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set
      (ASCII.CR & ASCII.LF);

   function Standard_Input
      return Ada.Streams.Stream_IO.Stream_Access;

   function Standard_Output
      return Ada.Streams.Stream_IO.Stream_Access;

   function Standard_Error
      return Ada.Streams.Stream_IO.Stream_Access;

   function End_Of_File
      return Boolean;

   function Read_Until
      (Stop : Ada.Strings.Maps.Character_Set)
      return String;

   function Read_Until
      (C : Character)
      return String;

   function Get_Integer
      return Integer;

   type Integers is array (Positive range <>) of Integer;
   function Get_Integers
      return Integers;

   procedure Put
      (I : Integer);

   procedure New_Line;
   procedure Flush;

end Advent_IO;
