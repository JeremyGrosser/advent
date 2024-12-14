pragma Ada_2022;
with AnsiAda;
with Ada.Containers.Vectors;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day14_2 is
   Columns : constant Positive := Positive'Value (Input.Read_Until ('x'));
   Rows    : constant Positive := Positive'Value (Input.Read_Until (ASCII.LF));

   type Coordinate is record
      Y, X : Integer;
   end record;

   function "+" (Left, Right : Coordinate) return Coordinate
   is
      Result : Coordinate;
   begin
      Result.Y := (Left.Y + Right.Y) mod Rows;
      Result.X := (Left.X + Right.X) mod Columns;
      return Result;
   end "+";

   type Robot is record
      P : Coordinate;
      V : Coordinate;
   end record;

   package Robot_Vectors is new Ada.Containers.Vectors (Positive, Robot);
   Robots : Robot_Vectors.Vector;
   use Robot_Vectors;

   function Step
      (Previous : Robot_Vectors.Vector)
      return Robot_Vectors.Vector
   is
      Next : Robot_Vectors.Vector;
      Next_R : Robot;
   begin
      for R of Previous loop
         Next_R := R;
         Next_R.P := R.P + R.V;
         Append (Next, Next_R);
      end loop;
      return Next;
   end Step;

   type Quadrant is range 0 .. 3;

   Split_X : constant Positive := Columns / 2;
   Split_Y : constant Positive := Rows / 2;
   function Get_Quadrant (C : Coordinate) return Quadrant
   is (if    C.X < Split_X and then C.Y < Split_Y then 0
       elsif C.X < Split_X and then C.Y > Split_Y then 1
       elsif C.X > Split_X and then C.Y < Split_Y then 2
       elsif C.X > Split_X and then C.Y > Split_Y then 3
       else raise Program_Error with "Coordinate on zero line.");

   procedure Print
      (V : Robot_Vectors.Vector)
   is
      M : array (0 .. Rows, 0 .. Columns) of Natural := (others => (others => 0));
      Num : constant array (0 .. 9) of Character := "0123456789";
   begin
      for R of V loop
         M (R.P.Y, R.P.X) := @ + 1;
      end loop;

      for Y in M'Range (1) loop
         for X in M'Range (2) loop
            if M (Y, X) = 0 then
               Output.Log (" ", False);
            else
               Output.Log (Num (M (Y, X)) & "", False);
            end if;
         end loop;
         Output.Log ("");
      end loop;
      Output.Log ("");
   end Print;

   function Safety_Factor
      (V : Robot_Vectors.Vector)
      return Natural
   is
      Count : array (Quadrant) of Natural := (others => 0);
      Quad : Quadrant;
   begin
      for R of V loop
         if R.P.X /= Split_X and then R.P.Y /= Split_Y then
            Quad := Get_Quadrant (R.P);
            Count (Quad) := Count (Quad) + 1;
         end if;
      end loop;

      return Count (0) * Count (1) * Count (2) * Count (3);
   end Safety_Factor;

   Min_Safety : Natural := Natural'Last;
   Min_Safety_I : Natural;
   SF : Natural;

   Sum : Natural := 0;
   R : Robot;
begin
   loop
      exit when Input.End_Of_Input;
      Input.Seek (2); --  "p="
      R.P.X := Integer'Value (Input.Read_Until (','));
      R.P.Y := Integer'Value (Input.Read_Until (' '));
      Input.Seek (2); --  "v="
      R.V.X := Integer'Value (Input.Read_Until (','));
      R.V.Y := Integer'Value (Input.Read_Until (ASCII.LF));
      Robot_Vectors.Append (Robots, R);
   end loop;

   for I in 1 .. 10000 loop
      Robots := Step (Robots);
      SF := Safety_Factor (Robots);
      if SF < Min_Safety then
         Min_Safety := SF;
         Min_Safety_I := I;
         Print (Robots);
         Output.Log (I'Image);
         Output.Log ("---------------------------");
      end if;
   end loop;

   Output.Put (Min_Safety_I);
end Day14_2;
