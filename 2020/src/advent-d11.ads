package Advent.D11 is
   function Part_1
      (Filename : String)
      return Integer;

   function Part_2
      (Filename : String)
      return Integer;

   procedure Run;

private

   type Seat is (Floor, Empty, Occupied);
   type Seats is array (Positive range <>, Positive range <>) of Seat;

   function Read_Seats
      (Filename : String)
      return Seats;

   function Adjacent_Seats
      (S : Seats;
       Y, X : Positive)
       return Seats;

   function Occupancy
      (S : Seats)
      return Natural;

   function Next_State
      (S : Seats;
       Y, X : Positive)
       return Seat;

   procedure Print
      (S : Seats);

end Advent.D11;
