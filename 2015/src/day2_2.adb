with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day2_2 is
   type Dimension is (Length, Width, Height);
   type Box is array (Dimension) of Positive;

   function Min (A, B, C : Positive) return Positive
   is (if    A <= B and then A <= C then A
       elsif B <= A and then B <= C then B
       else  C);

   function Shortest_Distance
      (B : Box)
      return Positive
   is (Min (B (Length) * 2 + B (Width) * 2,
            B (Width) * 2 + B (Height) * 2,
            B (Height) * 2 + B (Length) * 2));

   function Smallest_Perimeter
      (B : Box)
      return Positive
   is (Min (2 * (B (Length) + B (Width)),
            2 * (B (Width) + B (Height)),
            2 * (B (Height) + B (Length))));

   function Volume
      (B : Box)
      return Positive
   is (B (Length) * B (Width) * B (Height));

   B        : Box;
   Ribbon   : Positive;
   Bow      : Positive;
   Sum      : Natural := 0;
begin
   while not Input.End_Of_Input loop
      B (Length) := Positive'Value (Input.Read_Until ('x'));
      B (Width)  := Positive'Value (Input.Read_Until ('x'));
      B (Height) := Positive'Value (Input.Read_Until (CRLF));
      Ribbon := Min (Shortest_Distance (B), Smallest_Perimeter (B), Positive'Last);
      Bow := Volume (B);
      Sum := Sum + Ribbon + Bow;
   end loop;
   Output.Put (Sum);
end Day2_2;
