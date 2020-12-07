package Advent.D6 is
   subtype Index_Type is Character range 'a' .. 'z';
   type Flag_Array is array (Index_Type) of Natural;

   --  This isn't anything like a popcount, but it was the only name that came
   --  to me at the time.
   function Popcount
      (FA    : Flag_Array;
       Match : Natural := 1)
      return Natural;

   function Part_1
      (Filename : String)
      return Integer;

   function Part_2
      (Filename : String)
      return Integer;

   procedure Run;
end Advent.D6;
