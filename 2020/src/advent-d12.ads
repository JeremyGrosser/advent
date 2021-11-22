package Advent.D12 is
   function Part_1
      (Filename : String)
      return Integer;

   function Part_2
      (Filename : String)
      return Long_Long_Integer;

   procedure Run;

private
   type Coordinate is record
      X, Y : Integer;
   end record;

   type Degrees is mod 360;
   subtype Radians is Float;

   function To_Radians
      (D : Degrees)
      return Radians;

   function To_Degrees
      (R : Radians)
      return Degrees;

   function "+"
      (Left  : Degrees;
       Right : Integer)
       return Degrees;

   procedure Move_Toward
      (Point : in out Coordinate;
       To    : Coordinate;
       Count : Positive);

   function Distance_To
      (From, To : Coordinate)
      return Integer;
end Advent.D12;
