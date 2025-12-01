pragma Extensions_Allowed (On);
with Advent.Input;
with Advent.Output;

procedure Day4_1
   (Input  : in out Advent.Input.Buffer;
    Output : Advent.Output.Buffer)
is
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

   Columns : constant Natural := Input.Line_Length;
   Rows    : constant Natural := Input.Length / Columns;
   M       : constant Map := Read_Map (Columns, Rows);
   P1, P2, P3, P4 : Coordinate;
   Sum     : Natural := 0;
begin
   for Y in M'Range (1) loop
      for X in M'Range (2) loop
         P1 := Coordinate'(Y, X);
         if M (P1.Y, P1.X) = 'X' then
            for Dir in Direction'Range loop
               P2 := P1 + Dir;
               P3 := P2 + Dir;
               P4 := P3 + Dir;
               if In_Bounds (M, P2) and then M (P2.Y, P2.X) = 'M' and then
                  In_Bounds (M, P3) and then M (P3.Y, P3.X) = 'A' and then
                  In_Bounds (M, P4) and then M (P4.Y, P4.X) = 'S'
               then
                  Sum := Sum + 1;
               end if;
            end loop;
         end if;
      end loop;
   end loop;

   Output.Put (Sum);
end Day4_1;
