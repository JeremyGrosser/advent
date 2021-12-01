with Ada.Streams.Stream_IO;
with Ada.Streams;
with Ada.Strings.Maps;

package Advent_IO is

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

   function Read_Until
      (Stop : Ada.Strings.Maps.Character_Set)
      return String;

   function Read_Until
      (C : Character)
      return String;

   procedure New_Line;
   procedure Flush;

end Advent_IO;
