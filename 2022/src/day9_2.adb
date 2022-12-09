with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;
with Ada.Unchecked_Conversion;
with Ada.Containers.Hashed_Sets;
with Ada.Containers;
with Str;

procedure Day9_2 is
   type Position is record
      Y, X : Integer;
   end record;

   function Hash (Element : Position)
      return Ada.Containers.Hash_Type
   is
      use Ada.Containers;
      function To_Hash_Type is new Ada.Unchecked_Conversion
         (Integer, Hash_Type);
   begin
      return (To_Hash_Type (Element.X) * 2 ** 16) xor To_Hash_Type (Element.Y);
   end Hash;

   package Position_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type => Position,
       Hash => Hash,
       Equivalent_Elements => "=");
   use Position_Sets;

   Visited  : Set := Empty_Set;
   Knots    : array (0 .. 9) of Position := (others => (0, 0));

   procedure Update_Knot
      (Head : Position;
       Tail : in out Position)
   is
      function Adjacent
         return Boolean
      is (Tail.X in Head.X - 1 .. Head.X + 1 and then
          Tail.Y in Head.Y - 1 .. Head.Y + 1);
   begin
      if not Adjacent then
         if Tail.X < Head.X then
            Tail.X := Tail.X + 1;
         elsif Tail.X > Head.X then
            Tail.X := Tail.X - 1;
         end if;

         if Tail.Y < Head.Y then
            Tail.Y := Tail.Y + 1;
         elsif Tail.Y > Head.Y then
            Tail.Y := Tail.Y - 1;
         end if;
      end if;
   end Update_Knot;

   Direction : Character;
   Count : Natural;
   Num  : String (1 .. 2);
   Last : Natural;
begin
   --  Allocate much more space than we need. One big malloc is faster than
   --  lots of small ones.
   Reserve_Capacity (Visited, Ada.Containers.Count_Type (Length (Input)));

   while not End_Of_Input loop
      Character'Read (Input, Direction);
      Seek (Input, 1, Seek_Current); --  skip space
      Read_Until (Input, ASCII.LF, Num, Last);
      Count := Str.To_Natural (Num (1 .. Last));

      for I in 1 .. Count loop
         case Direction is
            when 'U' => Knots (0).Y := Knots (0).Y + 1;
            when 'D' => Knots (0).Y := Knots (0).Y - 1;
            when 'L' => Knots (0).X := Knots (0).X - 1;
            when 'R' => Knots (0).X := Knots (0).X + 1;
            when others =>
               raise Program_Error with "Invalid direction";
         end case;

         for I in 1 .. 9 loop
            Update_Knot (Knots (I - 1), Knots (I));
         end loop;
         Include (Visited, Knots (Knots'Last));
      end loop;
   end loop;

   Put (Output, Natural (Length (Visited)));
   New_Line (Output);
end Day9_2;
