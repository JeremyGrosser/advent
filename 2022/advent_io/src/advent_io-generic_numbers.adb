with Ada.Containers;
with Ada.Strings.Fixed;
with Ada.Strings;

package body Advent_IO.Generic_Numbers is
   function Get
      (S : not null Stream_Access;
       Delimiter : Ada.Strings.Maps.Character_Set := Whitespace)
      return Number
   is
      X : constant String := Ada.Strings.Fixed.Trim
         (Source => Read_Until (S, Delimiter),
          Left   => Whitespace,
          Right  => Whitespace);
   begin
      if X'Length = 0 then
         return Get (S, Delimiter);
      else
         return Number'Value (X);
      end if;
   end Get;

   function Get_Vector
      (S : not null Stream_Access;
       Delimiter : Ada.Strings.Maps.Character_Set := Whitespace;
       Initial_Capacity : Natural := 0)
      return Number_Vectors.Vector
   is
      use Number_Vectors;
      V : Vector := Empty_Vector;
      N : Number;
   begin
      Reserve_Capacity (V, Ada.Containers.Count_Type (Initial_Capacity));
      while not End_Of_Input loop
         N := Get (S, Delimiter);
         Append (V, N);
      end loop;
      return V;
   end Get_Vector;

   function Get_Numbers
      (S : not null Stream_Access;
       Delimiter : Ada.Strings.Maps.Character_Set := Whitespace)
      return Numbers
   is
      use Number_Vectors;
      V : constant Vector := Get_Vector (S, Delimiter);
      N : Numbers (1 .. Natural (Length (V)));
   begin
      for I in N'Range loop
         N (I) := V (I);
      end loop;
      return N;
   end Get_Numbers;

   procedure Put
      (S : not null access Ada.Streams.Root_Stream_Type'Class;
       N : Number)
   is
      use Ada.Strings.Fixed;
      use Ada.Strings;
   begin
      String'Write (S, Trim (N'Image, Left));
   end Put;
end Advent_IO.Generic_Numbers;
