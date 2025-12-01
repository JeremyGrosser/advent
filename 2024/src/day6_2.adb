pragma Style_Checks ("-t");
pragma Extensions_Allowed (On);
with Advent.Input;
with Advent.Output;

procedure Day6_2
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

   type Direction is (North, East, South, West);

   Move : constant array (Direction) of Coordinate :=
      (North      => (-1,  0),
       East       => ( 0,  1),
       South      => ( 1,  0),
       West       => ( 0, -1));

   type Pawn is record
      Position : Coordinate;
      Facing   : Direction;
   end record;

   Start : Pawn;

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
                  Start.Position := (Y, X);
                  Start.Facing := North;
               when '>' =>
                  Start.Position := (Y, X);
                  Start.Facing := East;
               when 'v' =>
                  Start.Position := (Y, X);
                  Start.Facing := South;
               when '<' =>
                  Start.Position := (Y, X);
                  Start.Facing := West;
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

   function Is_Stuck
      (M : Map)
       return Boolean
   is
      Guard : Pawn := Start;
      Next  : Coordinate;
      Steps : Natural := 0;
   begin
      loop
         if Steps > (M'Last (1) * M'Last (2)) then
            --  If we've moved more than the number of tiles in the map, it's
            --  probably a loop.
            return True;
         end if;

         Steps := Steps + 1;
         Next := Guard.Position + Move (Guard.Facing);
         if not In_Bounds (M, Next) then
            return False;
         end if;

         if M (Next.Y, Next.X) = '#' then
            Turn_Right (Guard.Facing); --  90 degrees
         else
            Guard.Position := Next;
         end if;
      end loop;
   end Is_Stuck;

   Columns : constant Natural := Input.Line_Length;
   Rows    : constant Natural := Input.Length / Columns;
   M       : Map := Read_Map (Columns, Rows);
   Sum     : Natural := 0;
begin
   for Y in M'Range (1) loop
      for X in M'Range (2) loop
         if M (Y, X) = '.' then
            M (Y, X) := '#';
            if Is_Stuck (M) then
               Sum := Sum + 1;
            end if;
            M (Y, X) := '.';
         end if;
      end loop;
   end loop;
   Output.Put (Sum);
end Day6_2;
