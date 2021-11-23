package Advent.D13 is
   function Part_1
      (Filename : String)
      return Long_Long_Integer;

   function Part_2
      (Filename : String)
      return Long_Long_Integer;

   procedure Run;

private

   subtype Bus_Id is Long_Long_Integer;
   subtype Timestamp is Long_Long_Integer;

   package Bus_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Bus_Id);

   function Parse_Buses
      (S : String)
      return Bus_Vectors.Vector;

   function Is_Valid
      (Buses : Bus_Vectors.Vector;
       I     : Timestamp)
       return Boolean;

end Advent.D13;
