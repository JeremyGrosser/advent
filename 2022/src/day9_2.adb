with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;
with Ada.Containers.Hashed_Sets;
with Ada.Containers;

procedure Day9_2 is
   type Position is record
      Y, X : Integer;
   end record;

   function Hash (Element : Position)
      return Ada.Containers.Hash_Type
   is
      use Ada.Containers;
      H : Hash_Type;
   begin
      H := Hash_Type (abs Element.X);
      H := H * (2 ** 32);
      H := H or Hash_Type (abs Element.Y);
      return H;
   end Hash;

   package Position_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type => Position,
       Hash => Hash,
       Equivalent_Elements => "=");
   use Position_Sets;

   Visited  : Set := Empty_Set;
   Head     : Position := (0, 0);
   Knots    : array (1 .. 9) of Position := (others => Head);

   procedure Update_Knot
      (Head : Position;
       Tail : in out Position)
   is
      function Adjacent
         return Boolean
      is (Tail.X in Head.X - 1 .. Head.X + 1 and then Tail.Y in Head.Y - 1 .. Head.Y + 1);
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
begin
   Include (Visited, Knots (Knots'Last));
   while not End_Of_Input loop
      declare
         Direction : Character;
         Count : Natural;
      begin
         Character'Read (Input, Direction);
         Seek (Input, 1, Seek_Current); --  skip space
         Count := Get (Input);

         for I in 1 .. Count loop
            case Direction is
               when 'U' => Head.Y := Head.Y + 1;
               when 'D' => Head.Y := Head.Y - 1;
               when 'L' => Head.X := Head.X - 1;
               when 'R' => Head.X := Head.X + 1;
               when others =>
                  raise Program_Error with "Invalid direction";
            end case;

            for I in Knots'Range loop
               if I = Knots'First then
                  Update_Knot (Head, Knots (I));
               else
                  Update_Knot (Knots (I - 1), Knots (I));
               end if;
            end loop;
            Include (Visited, Knots (Knots'Last));
            --  Print_State;
         end loop;
      end;
   end loop;

   Put (Output, Natural (Length (Visited)));
   New_Line (Output);
end Day9_2;
