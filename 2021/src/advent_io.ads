with Ada.Streams.Stream_IO;
with Ada.Streams;
with Ada.Strings.Maps;

package Advent_IO is

   --  These streams wrap stdio
   function Input
      return Ada.Streams.Stream_IO.Stream_Access;

   function Output
      return Ada.Streams.Stream_IO.Stream_Access;

   function Error
      return Ada.Streams.Stream_IO.Stream_Access;

   function End_Of_Input
      return Boolean;

   Whitespace : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set
      (ASCII.HT & ASCII.LF & ASCII.CR & ' ');
   CRLF       : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set
      (ASCII.CR & ASCII.LF);

   function Read_Until
      (S    : not null access Ada.Streams.Root_Stream_Type'Class;
       Stop : Ada.Strings.Maps.Character_Set)
      return String;

   function Read_Until
      (S : not null access Ada.Streams.Root_Stream_Type'Class;
       C : Character)
      return String;

   procedure New_Line
      (S : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Flush;

end Advent_IO;
