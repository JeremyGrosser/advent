with Ada.Containers.Hashed_Sets;
with Ada.Containers;

with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day3_1 is
   type Direction is (North, East, South, West);
   type Point is record
      X, Y : Integer;
   end record;

   function Hash (P : Point)
      return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (P.X * P.Y mod 65536));

   package Point_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type => Point,
       Equivalent_Elements => "=",
       Hash         => Hash);
   use Point_Sets;

   Visited : Point_Sets.Set := Empty_Set;
   Santa   : Point := (0, 0);

   procedure Move
      (Dir : Direction)
   is
   begin
      case Dir is
         when North =>
            Santa.Y := Santa.Y + 1;
         when East =>
            Santa.X := Santa.X + 1;
         when South =>
            Santa.Y := Santa.Y - 1;
         when West =>
            Santa.X := Santa.X - 1;
      end case;
   end Move;

   Ch : Character;
begin
   Include (Visited, Santa);

   while not Input.End_Of_Input loop
      Input.Get (Ch);
      case Ch is
         when '<' =>
            Move (West);
         when '>' =>
            Move (East);
         when '^' =>
            Move (North);
         when 'v' =>
            Move (South);
         when others =>
            raise Program_Error with "Unknown character in input: " & Ch;
      end case;
      if not Contains (Visited, Santa) then
         Include (Visited, Santa);
      end if;
   end loop;
   Output.Put (Natural (Length (Visited)));
end Day3_1;
