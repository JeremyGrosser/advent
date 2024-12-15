with Ada.Containers.Hashed_Maps;
with Ada.Containers;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day12_1 is
   Columns : constant Natural := Input.Read_Until (ASCII.LF)'Length;
   Rows    : constant Natural := Input.Length / Columns;

   type Coordinate is record
      Y, X : Integer;
   end record;

   type Direction is (Up, Down, Left, Right);

   Move : constant array (Direction) of Coordinate :=
      (Up      => (-1, 0),
       Down    => (+1, 0),
       Left    => (0, -1),
       Right   => (0, +1));

   function "+" (Left, Right : Coordinate) return Coordinate
   is ((Left.Y + Right.Y, Left.X + Right.X));

   function Hash
      (Item : Coordinate)
      return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Item.Y * Columns + Item.X));

   package Coordinate_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type => Coordinate,
       Hash         => Hash);
   use Coordinate_Sets;

   package Region_Vectors is new Ada.Containers.Vectors (Positive, Coordinate_Sets.Set);
   use Region_Vectors;

   type Plant is new Character;

   package Plant_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type         => Coordinate,
       Element_Type     => Plant,
       Hash             => Hash,
       Equivalent_Keys  => "=");
   use Plant_Maps;

   function Is_Edge
      (M : Plant_Maps.Map;
       Pos : Coordinate)
   is
      P : constant Plant := Element (M, Pos);
      Check : Position;
   begin
      for Dir in Direction'Range loop
         Check := Pos + Move (Dir);
         if not Contains (M, Check) or else Element (M, Check) /= P then
            return True;
         end if;
      end loop;
      return False;
   end Is_Edge;

   procedure Add_Coordinate
      (V   : Region_Vectors.Vector;
       Pos : Coordinate)
   is
   begin
      for Cursor in Iterate (V) loop
         if Contains (Reference (Region), Pos) then
            return;
         end if;
      end loop;
   end Add_Coordinate;

   type Plant_Array is array (Plant) of Natural;
   Perimeter, Area : Plant_Array := (others => 0);

   Regions : Region_Vectors.Vector;

   Sum   : Natural := 0;
   Price : Natural;
   Ch    : Character;
   M     : Plant_Maps.Map;
begin
   Input.Seek (0, Input.Seek_Start);
   for Y in 1 .. Rows loop
      for X in 1 .. Columns loop
         Input.Get (Ch);
         Insert (M, (Y, X), Plant (Ch));
      end loop;
      Input.Skip_Whitespace;
   end loop;

   for Cursor in Iterate (M) loop
      declare
         Pos   : constant Coordinate := Key (Cursor);
         Ray   : Coordinate := Pos;
         P     : constant Plant := Element (Cursor);
      begin
         Add_Coordinate (Regions, Pos);
         for Region of Regions loop
            if Contains (Region, Pos) then
               exit;
            end if;
         end loop;
         end loop;
         loop
            exit when Is_Edge (M, Ray);
            Ray := Ray + Up;
         end loop;

         loop
            exit when Is_Edge (M, Ray);

         Area (P) := Area (P) + 1;
         for Dir in Direction'Range loop
            Check := Pos + Move (Dir);
            if not Contains (M, Check) or else Element (M, Check) /= P then
               Perimeter (P) := Perimeter (P) + 1;
            end if;
         end loop;
      end;
   end loop;

   for P in Plant'Range loop
      Price := Perimeter (P) * Area (P);
      if Price > 0 then
         Output.Log (P'Image, False);
         Output.Log (" area=", False);
         Output.Log (Area (P)'Image, False);
         Output.Log (" perimeter=", False);
         Output.Log (Perimeter (P)'Image, False);
         Output.Log (" price ", False);
         Output.Log (Price'Image);
      end if;
      Sum := Sum + Price;
   end loop;

   Output.Put (Sum);
end Day12_1;
