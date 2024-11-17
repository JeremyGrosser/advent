with Ada.Containers.Hashed_Sets;
with Ada.Containers;

with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day3_2 is
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

   type Santa_Type is (Regular, Robot);
   Santa : array (Santa_Type) of Point := (others => (0, 0));
   Turn : Santa_Type := Regular;

   procedure Move
      (P : in out Point;
       Dir : Direction)
   is
   begin
      case Dir is
         when North =>
            P.Y := P.Y + 1;
         when East =>
            P.X := P.X + 1;
         when South =>
            P.Y := P.Y - 1;
         when West =>
            P.X := P.X - 1;
      end case;
   end Move;

   Ch : Character;
begin
   Include (Visited, Santa (Turn));

   while not Input.End_Of_Input loop
      Input.Get (Ch);
      case Ch is
         when '<' =>
            Move (Santa (Turn), West);
         when '>' =>
            Move (Santa (Turn), East);
         when '^' =>
            Move (Santa (Turn), North);
         when 'v' =>
            Move (Santa (Turn), South);
         when others =>
            raise Program_Error with "Unknown character in input: " & Ch;
      end case;

      if not Contains (Visited, Santa (Turn)) then
         Include (Visited, Santa (Turn));
      end if;

      if Turn = Robot then
         Turn := Regular;
      else
         Turn := Robot;
      end if;
   end loop;
   Output.Put (Natural (Length (Visited)));
end Day3_2;
