pragma Style_Checks ("-t");
pragma Extensions_Allowed (On);
with Advent.Input;
with Advent.Output;

procedure Day6_1
   (Input  : in out Advent.Input.Buffer;
    Output : Advent.Output.Buffer)
is
   type Map is array (Positive range <>, Positive range <>) of Character;

   type Coordinate is record
      Y, X : Integer;
   end record;

   function "+"
      (Left, Right : Coordinate)
      return Coordinate
   is (Coordinate'(Y => Left.Y + Right.Y, X => Left.X + Right.X));

   type Direction is (North, North_East, East, South_East, South, South_West, West, North_West);

   type Pawn is record
      Position : Coordinate;
      Facing   : Direction;
   end record;

   Guard : Pawn;

   function Read_Map
      (Width, Height : Positive)
      return Map
   is
      M : Map (1 .. Height, 1 .. Width);
   begin
      Input.Seek (0, Advent.Input.Seek_Start);
      for Y in M'Range (1) loop
         for X in M'Range (2) loop
            Input.Get (M (Y, X));
            case M (Y, X) is
               when '^' =>
                  Guard.Position := (Y, X);
                  Guard.Facing := North;
               when '>' =>
                  Guard.Position := (Y, X);
                  Guard.Facing := East;
               when 'v' =>
                  Guard.Position := (Y, X);
                  Guard.Facing := South;
               when '<' =>
                  Guard.Position := (Y, X);
                  Guard.Facing := West;
               when others =>
                  null;
            end case;
         end loop;

         if Input.Peek /= ASCII.LF then
            raise Program_Error with "Expected LF";
         end if;
         Input.Seek (1);
      end loop;
      return M;
   end Read_Map;

   function In_Bounds
      (M : Map;
       P : Coordinate)
       return Boolean
   is (P.Y in M'Range (1) and then P.X in M'Range (2));

   procedure Turn_Right
      (Dir : in out Direction)
   is
   begin
      if Dir = Direction'Last then
         Dir := Direction'First;
      else
         Dir := Direction'Succ (Dir);
      end if;
   end Turn_Right;

   Move : constant array (Direction) of Coordinate :=
      (North      => (-1,  0),
       North_East => (-1,  1),
       East       => ( 0,  1),
       South_East => ( 1,  1),
       South      => ( 1,  0),
       South_West => ( 1, -1),
       West       => ( 0, -1),
       North_West => (-1, -1));

   Columns : constant Natural := Input.Line_Length;
   Rows    : constant Natural := Input.Length / Columns;
   M       : Map := Read_Map (Columns, Rows);
   Next    : Coordinate;
   Sum     : Natural := 0;
begin
   loop
      M (Guard.Position.Y, Guard.Position.X) := 'X';
      Next := Guard.Position + Move (Guard.Facing);
      exit when not In_Bounds (M, Next);
      if M (Next.Y, Next.X) = '#' then
         Turn_Right (Guard.Facing); --  45 degrees
         Turn_Right (Guard.Facing); --  90 degrees
      else
         Guard.Position := Next;
      end if;
   end loop;

   for Y in M'Range (1) loop
      for X in M'Range (2) loop
         if M (Y, X) = 'X' then
            Sum := Sum + 1;
         end if;
      end loop;
   end loop;
   Output.Put (Sum);
end Day6_1;
