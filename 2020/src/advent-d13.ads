package Advent.D13 is
   function Part_1
      (Filename : String)
      return Integer;

   function Part_2
      (Filename : String)
      return Integer;

   procedure Run;

private

   subtype Bus_Id is Positive;
   subtype Timestamp is Natural;

   package Bus_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Bus_Id);

   function Parse_Buses
      (S : String)
      return Bus_Vectors.Vector;

end Advent.D13;
