with Ada.Streams;

generic
   type Number is (<>);
package Generic_Number_Stream_IO is

   Not_A_Number : exception;

   function Get
      (S : not null access Ada.Streams.Root_Stream_Type'Class)
      return Number;

   type Numbers is array (Positive range <>) of Number;

   function Get
      (S : not null access Ada.Streams.Root_Stream_Type'Class)
      return Numbers;

   procedure Put
      (S : not null access Ada.Streams.Root_Stream_Type'Class;
       N : Number);
end Generic_Number_Stream_IO;
