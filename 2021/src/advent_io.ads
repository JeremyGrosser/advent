with Ada.Streams; use Ada.Streams;
with Interfaces.C_Streams;
with Ada.Strings.Maps;

package Advent_IO is

   type FD_Stream is new Root_Stream_Type with private;
   type Stream_Access is access all Root_Stream_Type'Class;

   overriding
   procedure Read
      (Stream : in out FD_Stream;
       Item   : out Stream_Element_Array;
       Last   : out Stream_Element_Offset);

   overriding
   procedure Write
      (Stream : in out FD_Stream;
       Item   : Stream_Element_Array);

   --  These streams wrap stdio
   function Input
      return Stream_Access;

   function Output
      return Stream_Access;

   function Error
      return Stream_Access;

   function End_Of_Input
      return Boolean;

   Whitespace : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set
      (ASCII.HT & ASCII.LF & ASCII.CR & ' ');
   Comma      : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set
      (',');
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

private

   type FD_Stream is new Root_Stream_Type with record
      FD : Interfaces.C_Streams.FILEs;
   end record;

end Advent_IO;
