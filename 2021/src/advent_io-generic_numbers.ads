with Ada.Containers.Vectors;

generic
   type Number is (<>);
package Advent_IO.Generic_Numbers is

   Format_Error : exception;
   --  Raised when multiple consecutive whitespace characters are encountered
   --  or the input ends unexpectedly.

   function Get
      (S : not null access Ada.Streams.Root_Stream_Type'Class)
      return Number;

   package Number_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Number);

   function Get
      (S : not null access Ada.Streams.Root_Stream_Type'Class)
      return Number_Vectors.Vector;

   type Numbers is array (Positive range <>) of Number;

   function Get
      (S : not null access Ada.Streams.Root_Stream_Type'Class)
      return Numbers;

   procedure Put
      (S : not null access Ada.Streams.Root_Stream_Type'Class;
       N : Number);

end Advent_IO.Generic_Numbers;
