with Advent_IO; use Advent_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Strings;

package body Generic_Number_Stream_IO is
   function Get
      (S : not null access Ada.Streams.Root_Stream_Type'Class)
      return Number
   is
      X : constant String := Read_Until (S, Whitespace);
   begin
      if X'Length = 0 then
         raise Format_Error with "Encountered an empty string or unexpected end of input";
      else
         return Number'Value (X);
      end if;
   end Get;

   function Get
      (S : not null access Ada.Streams.Root_Stream_Type'Class)
      return Numbers
   is
      package Number_Vectors is new Ada.Containers.Vectors
         (Index_Type => Positive,
          Element_Type => Number);
      use Number_Vectors;
      V : Vector := Empty_Vector;
      N : Number;
   begin
      while not End_Of_Input loop
         N := Get (S);
         Append (V, N);
      end loop;

      declare
         X : Numbers (1 .. Positive (Length (V)));
      begin
         for I in X'Range loop
            X (I) := V (I);
         end loop;
         return X;
      end;
   end Get;

   procedure Put
      (S : not null access Ada.Streams.Root_Stream_Type'Class;
       N : Number)
   is
      use Ada.Strings.Fixed;
      use Ada.Strings;
   begin
      String'Output (S, Trim (N'Image, Left));
   end Put;
end Generic_Number_Stream_IO;
