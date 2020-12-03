with Ada.Containers.Vectors;

package Advent is
   type Test_Function is access function (Filename : in String) return Integer;
   Test_Failure : exception;
   No_Answer    : exception;

   procedure Test
      (F        : Test_Function;
       Name     : String;
       Filename : String;
       Expected : Integer);

   package Integer_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Integer);

   subtype Count_Type is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   function Read_Integers
      (Filename : String)
      return Integer_Vectors.Vector;
end Advent;
