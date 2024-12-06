with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day4_2 is
   type Map is array (Positive range <>, Positive range <>) of Character;
   type Coordinate is record
      Y, X : Integer;
   end record;

   function Read_Map
      (Width, Height : Positive)
      return Map
   is
      M : Map (1 .. Height, 1 .. Width);
   begin
      Input.Seek (0, Input.Seek_Start);
      for Y in M'Range (1) loop
         for X in M'Range (2) loop
            Input.Get (M (Y, X));
         end loop;

         if Input.Peek /= ASCII.LF then
            raise Program_Error with "Expected LF";
         end if;
         Input.Seek (1);
      end loop;
      return M;
   end Read_Map;

   type Direction is (North, North_East, East, South_East, South, South_West, West, North_West);

   function "+"
      (Left  : Coordinate;
       Right : Direction)
       return Coordinate
   is
   begin
      case Right is
         when North =>
            return Coordinate'(Left.Y - 1, Left.X);
         when North_East =>
            return Coordinate'(Left.Y - 1, Left.X + 1);
         when East =>
            return Coordinate'(Left.Y, Left.X + 1);
         when South_East =>
            return Coordinate'(Left.Y + 1, Left.X + 1);
         when South =>
            return Coordinate'(Left.Y + 1, Left.X);
         when South_West =>
            return Coordinate'(Left.Y + 1, Left.X - 1);
         when West =>
            return Coordinate'(Left.Y, Left.X - 1);
         when North_West =>
            return Coordinate'(Left.Y - 1, Left.X - 1);
      end case;
   end "+";

   function In_Bounds
      (M : Map;
       P : Coordinate)
       return Boolean
   is (P.Y in M'Range (1) and then P.X in M'Range (2));

   function To_String
      (M : Map;
       P : Coordinate;
       D : Direction)
       return String
   is
      S : String (1 .. 3);
      PP : Coordinate := P;
   begin
      for I in S'Range loop
         if not In_Bounds (M, PP) then
            return "...";
         end if;
         S (I) := M (PP.Y, PP.X);
         PP := PP + D;
      end loop;
      return S;
   end To_String;

   Columns : constant Natural := Input.Read_Until (ASCII.LF)'Length;
   Rows    : constant Natural := Input.Length / Columns;
   M       : constant Map := Read_Map (Columns, Rows);
   A, B    : String (1 .. 3);
   Sum     : Natural := 0;
begin
   for Y in 1 .. Rows - 2 loop
      for X in 1 .. Columns - 2 loop
         A := To_String (M, Coordinate'(Y, X), South_East);
         B := To_String (M, Coordinate'(Y + 2, X), North_East);
         if (A = "MAS" or else A = "SAM") and then (B = "MAS" or else B = "SAM") then
            Sum := Sum + 1;
         end if;
      end loop;
   end loop;

   Output.Put (Sum);
end Day4_2;
