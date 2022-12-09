with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;
with Ada.Unchecked_Conversion;
with Ada.Containers.Hashed_Sets;
with Ada.Containers;

procedure Day9_1 is
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

   Visited : Set := Empty_Set;
   Head, Tail : Position := (0, 0);

   procedure Update_Tail is
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
   end Update_Tail;
begin
   Include (Visited, Tail);
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

            Update_Tail;
            Include (Visited, Tail);
         end loop;
      end;
   end loop;

   Put (Output, Natural (Length (Visited)));
   New_Line (Output);
end Day9_1;
