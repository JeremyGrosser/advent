pragma Ada_2022;
with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day15_2 is
   type Real is delta 0.5 range -10_000.0 .. +10_000.0
      with Small => 0.5;
   type Coordinate is record
      Y : Integer;
      X : Real;
   end record;

   function "+" (Left, Right : Coordinate) return Coordinate
   is ((Y => Left.Y + Right.Y, X => Left.X + Right.X));

   type Direction is (Up, Down, Left, Right);

   Step : constant array (Direction) of Coordinate :=
      (Up      => (-1, 0.0),
       Down    => (+1, 0.0),
       Left    => (0, -0.5),
       Right   => (0, +0.5));

   function To_Direction
      (Ch : Character)
      return Direction
   is (case Ch is
      when '^' => Up,
      when 'v' => Down,
      when '<' => Left,
      when '>' => Right,
      when others => raise Program_Error with Ch & " is not a direction");


   function Hash (Item : Coordinate) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Item.Y * 1024 + Integer (Item.X * 2.0)));

   package Coordinate_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type         => Coordinate,
       Element_Type     => Character,
       Hash             => Hash,
       Equivalent_Keys  => "=");
   use Coordinate_Maps;

   procedure Print
      (M       : Coordinate_Maps.Map;
       Extents : Coordinate)
   is
      Pos : Coordinate;
   begin
      Pos.Y := 0;
      loop
         exit when Pos.Y > Extents.Y;
         Pos.X := 0.0;
         loop
            exit when Pos.X > Extents.X;
            if Contains (M, Pos) then
               Output.Log ("" & Element (M, Pos), False);
            else
               Output.Log (".", False);
            end if;
            Pos.X := Pos.X + 0.5;
         end loop;
         Pos.Y := Pos.Y + 1;
         Output.Log ("");
      end loop;
      Output.Log ("");
   end Print;

   function Can_Move
      (M   : Coordinate_Maps.Map;
       Pos : Coordinate;
       Dir : Direction)
       return Boolean
   is
      Next : constant Coordinate := Pos + Step (Dir);
      Ch : Character;
   begin
      if Next.X < 0.0 or else Next.Y < 0 then
         return False;
      end if;

      if not Contains (M, Next) then
         return True;
      end if;

      Ch := Element (M, Next);
      case Ch is
         when '[' =>
            if Dir in Up .. Down then
               return Can_Move (M, Next + Step (Right), Dir) and then Can_Move (M, Next, Dir);
            else
               return Can_Move (M, Next, Dir);
            end if;
         when ']' =>
            if Dir in Up .. Down then
               return Can_Move (M, Next + Step (Left), Dir) and then Can_Move (M, Next, Dir);
            else
               return Can_Move (M, Next, Dir);
            end if;
         when '#' =>
            return False;
         when others =>
            raise Program_Error with "Unexpected character in map: " & Ch;
      end case;
   end Can_Move;

   procedure Move
      (M       : in out Coordinate_Maps.Map;
       Pos     : in out Coordinate;
       Dir     : Direction;
       Recurse : Natural := 0)
   is
      P2 : Coordinate;
      Next : Coordinate := Pos + Step (Dir);
      Ch : Character;
   begin
      if not Contains (M, Pos) then
         raise Program_Error with "There is nothing at " & Pos'Image;
      end if;

      if Contains (M, Next) then
         Move (M, Next, Dir);
      end if;

      Ch := Element (M, Pos);
      if Recurse < 1 and then Dir in Up .. Down then
         if Ch = '[' then
            P2 := Pos + Step (Right);
            Move (M, P2, Dir, Recurse + 1);
         elsif Ch = ']' then
            P2 := Pos + Step (Left);
            Move (M, P2, Dir, Recurse + 1);
         end if;
      end if;

      Delete (M, Pos);
      Pos := Pos + Step (Dir);
      Insert (M, Pos, Ch);
   end Move;

   function GPS_Coordinate
      (Pos : Coordinate)
      return Natural
   is (Integer (Pos.Y) * 100 + Integer (Pos.X * 2.0));

   Warehouse : Coordinate_Maps.Map;
   Extents   : Coordinate := (0, 0.0);
   Robot     : Coordinate;

   Sum : Natural := 0;
   Prev, Ch : Character := ASCII.NUL;
   Pos : Coordinate := (0, 0.0);
   Dir : Direction;
begin
   loop
      Input.Get (Ch);
      case Ch is
         when ASCII.LF =>
            exit when Prev = ASCII.LF;
            Extents := Pos;
            Pos.Y := Pos.Y + 1;
            Pos.X := 0.0;
         when '#' =>
            Insert (Warehouse, Pos, Ch);
            Pos.X := Pos.X + 0.5;
            Insert (Warehouse, Pos, Ch);
            Pos.X := Pos.X + 0.5;
         when 'O' =>
            Insert (Warehouse, Pos, '[');
            Pos.X := Pos.X + 0.5;
            Insert (Warehouse, Pos, ']');
            Pos.X := Pos.X + 0.5;
         when '.' =>
            Pos.X := Pos.X + 1.0;
         when '@' =>
            Insert (Warehouse, Pos, Ch);
            Robot := Pos;
            Pos.X := Pos.X + 1.0;
         when others =>
            raise Program_Error with "Unexpected character in map: " & Ch;
      end case;
      Prev := Ch;
   end loop;

   Extents.X := Extents.X - 0.5;

   loop
      Print (Warehouse, Extents);
      exit when Input.End_Of_Input;
      Input.Get (Ch);
      Output.Log (Ch & "");
      if Ch /= ASCII.LF then
         Dir := To_Direction (Ch);
         if Can_Move (Warehouse, Robot, Dir) then
            Move (Warehouse, Robot, Dir);
         end if;
      end if;
   end loop;

   for Cursor in Iterate (Warehouse) loop
      Pos := Key (Cursor);
      Ch := Element (Cursor);
      if Ch = '[' then
         Sum := Sum + GPS_Coordinate (Pos);
      end if;
   end loop;

   Output.Put (Sum);
end Day15_2;
