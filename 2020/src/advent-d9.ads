with Ada.Containers.Vectors;

package Advent.D9 is
   package Long_Integer_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Long_Integer);

   function Valid_1
      (Numbers : Long_Integer_Vectors.Vector;
       N       : Long_Integer)
       return Boolean;

   function Part_1
      (Filename        : String;
       Preamble_Length : Positive)
      return Long_Integer;

   type Int_Array is array (Positive range <>) of Long_Integer;

   function Read_Numbers
      (Filename : String)
      return Int_Array;

   type Result_Type is record
      Sum      : Long_Integer := 0;
      Smallest : Long_Integer := Long_Integer'Last;
      Largest  : Long_Integer := Long_Integer'First;
   end record;

   function Valid_2
      (Numbers     : Int_Array;
       First, Last : Positive)
       return Result_Type;

   function Part_2
      (Filename        : String;
       Preamble_Length : Positive)
      return Long_Integer;

   procedure Run;
end Advent.D9;
