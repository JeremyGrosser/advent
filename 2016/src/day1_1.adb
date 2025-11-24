with Advent.Input;
with Advent.Output;
with Advent; use Advent;

procedure Day1_1 is

   type Facing is mod 4;

   type Dimension is (X, Y);
   type Position is array (Dimension) of Integer;

   function Distance
      (From, To : Position)
      return Natural
   is ((abs (To (X) - From (X))) + (abs (To (Y) - From (Y))));

   Start    : constant Position := (0, 0);
   Heading  : Facing := 0;
   Pos      : Position := Start;
   Steps    : Integer;

   procedure Move
      (H : Facing;
       S : Integer)
   is
   begin
      case H is
         when 0 => --  North
            Pos (Y) := Pos (Y) + S;
         when 1 => --  East
            Pos (X) := Pos (X) + S;
         when 2 => --  South
            Pos (Y) := Pos (Y) - S;
         when 3 => --  West
            Pos (X) := Pos (X) - S;
      end case;
   end Move;

   function Read_Natural
      return Natural
   is
      N : Natural := 0;
      Ch : Character;
   begin
      while not Input.End_Of_Input loop
         Ch := Input.Peek;
         exit when Ch not in '0' .. '9';
         Input.Seek (1);
         N := N * 10 + Character'Pos (Ch) - Character'Pos ('0');
      end loop;
      return N;
   end Read_Natural;

   Ch : Character;
begin
   while not Input.End_Of_Input loop
      Input.Get (Ch);
      case Ch is
         when 'L' =>
            Heading := Facing'Pred (Heading);
            Steps := Read_Natural;
            Move (Heading, Steps);
         when 'R' =>
            Heading := Facing'Succ (Heading);
            Steps := Read_Natural;
            Move (Heading, Steps);
         when ' ' | ',' =>
            null;
         when others =>
            raise Program_Error with "Unknown Character: " & Ch;
      end case;
   end loop;

   Output.Put (Distance (Start, Pos));
end Day1_1;
