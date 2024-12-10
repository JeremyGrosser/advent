with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Containers;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day10_2 is
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

   package Coordinate_Vectors is new Ada.Containers.Vectors (Positive, Coordinate);
   use Coordinate_Vectors;
   Trailheads, Peaks : Coordinate_Vectors.Vector;

   function Hash (Item : Coordinate) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Item.Y * Columns + Item.X));

   package Coordinate_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type         => Coordinate,
       Element_Type     => Natural,
       Hash             => Hash,
       Equivalent_Keys  => "=");
   use Coordinate_Maps;

   Num_Paths : Natural := 0;

   function Longest_Path
      (M : Coordinate_Maps.Map;
       From, To : Coordinate)
      return Coordinate_Vectors.Vector
   is
      Best_Distance  : Natural := 0;
      Best_Path      : Coordinate_Vectors.Vector := Empty_Vector;

      Next_Step   : Coordinate := (-1, -1);
      Slope       : Integer := 10;
      Next_Distance : Natural := 0;
      Path : Coordinate_Vectors.Vector := Empty_Vector;
   begin
      for Dir in Direction'Range loop
         Next_Step := From + Move (Dir);
         if Contains (M, Next_Step) then
            Slope := Element (M, Next_Step) - Element (M, From);
            if Slope = 1 then
               Clear (Path);
               Append (Path, From);
               Append (Path, Next_Step);

               if Next_Step = To then
                  Num_Paths := Num_Paths + 1;
               else
                  Append (Path, Longest_Path (M, Next_Step, To));
                  if Last_Element (Path) = To then
                     Num_Paths := Num_Paths + 1;
                  end if;
                  Next_Distance := Natural (Length (Path));
                  if Next_Distance > Best_Distance then
                     Best_Distance := Next_Distance;
                     Best_Path := Path;
                  end if;
               end if;
            end if;
         end if;
      end loop;

      if not Is_Empty (Best_Path) and then Last_Element (Best_Path) = To then
         return Best_Path;
      else
         return Empty_Vector;
      end if;
   end Longest_Path;

   procedure Print
      (H : Coordinate_Maps.Map;
       Path : Coordinate_Vectors.Vector)
   is
      M : array (1 .. Rows, 1 .. Columns) of Character := (others => (others => '.'));
      Num : constant array (0 .. 9) of Character := "0123456789";
   begin
      for Step of Path loop
         M (Step.Y, Step.X) := Num (Element (H, Step) mod 10);
      end loop;

      for Y in M'Range (1) loop
         for X in M'Range (2) loop
            Output.Log ("" & M (Y, X), False);
         end loop;
         Output.Log ("");
      end loop;
      Output.Log ("");
   end Print;

   HM    : Coordinate_Maps.Map;
   Ch    : Character;
   Pos   : Coordinate;
   Sum   : Natural := 0;
begin
   Input.Seek (0, Input.Seek_Start);
   for Y in 1 .. Rows loop
      for X in 1 .. Columns loop
         Input.Get (Ch);
         if Ch /= '.' then
            Pos := (Y, X);
            Include (HM, Pos, Natural'Value (Ch & ""));

            if Ch = '0' then
               Append (Trailheads, Pos);
            end if;

            if Ch = '9' then
               Append (Peaks, Pos);
            end if;
         end if;
      end loop;
      Input.Get (Ch);

      if Y < Rows and then Ch /= ASCII.LF then
         raise Program_Error with "Expected LF";
      end if;
   end loop;

   for Start of Trailheads loop
      for Finish of Peaks loop
         declare
            Path : constant Coordinate_Vectors.Vector := Longest_Path (HM, Start, Finish);
         begin
            if not Is_Empty (Path) then
               Print (HM, Path);
               Sum := Sum + 1;
            end if;
         end;
      end loop;
   end loop;

   Output.Put (Num_Paths);
end Day10_2;
