with Ada.Containers.Vectors;

package Advent.D9 is
   package Integer_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Integer);

   function Valid
      (Numbers : Integer_Vectors.Vector;
       N       : Integer)
       return Boolean;

   function Part_1
      (Filename        : String;
       Preamble_Length : Positive)
      return Integer;

   procedure Run;
end Advent.D9;
