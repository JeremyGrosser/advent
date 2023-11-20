with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;
with Ada.Containers.Ordered_Maps;

procedure Day12_1 is
   type Coordinate is record
      Y, X : Positive;
   end record;

   Undefined : constant Coordinate := (Positive'Last, Positive'Last);

   type Position is record
      Height   : Character;
      Visited  : Boolean;
      Distance : Natural := Natural'Last;
      Previous : Coordinate := Undefined;
   end record;

   package Position_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type      => Coordinate,
       Element_Type  => Position);

   M : Position_Maps.Map := Position_Maps.Empty_Map;
   Start_Coordinate, End_Coordinate : Coordinate;
   Current_Coordinate   : Coordinate := (1, 1);
   Current_Position     : Position := (ASCII.NUL, False);
   Ch : Character;
begin
   while not End_Of_Input loop
      Character'Read (Input, Ch);

      if Ch = ASCII.LF then
         Current_Coordinate.Y := Current_Coordinate.Y + 1;
      else
         if Ch = 'S' then
            Start_Coordinate := Current_Coordinate;
            Ch := 'a';
         elsif Ch = 'E' then
            End_Coordinate := Current_Coordinate;
            Ch := 'z';
         end if;

         Current_Position.Height := Ch;
         M.Include (Current_Coordinate, Current_Position);
         Current_Coordinate.X := Current_Coordinate.X + 1;
      end if;
   end loop;

   Put (Output, 0);
   New_Line (Output);
end Day12_1;
