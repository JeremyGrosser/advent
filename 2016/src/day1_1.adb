with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;

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

   Next_Ch : Character := ASCII.NUL;
   Ch      : Character := ASCII.NUL;

   procedure Advance is
   begin
      Ch := Next_Ch;
      if not End_Of_Input then
         Character'Read (Input, Next_Ch);
      else
         Next_Ch := ASCII.NUL;
      end if;
   end Advance;

   function Peek
      return Character
   is (Next_Ch);

   function Number
      return Integer
   is
      subtype Digit is Character range '0' .. '9';
      Val : constant array (Digit) of Integer := (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
      N   : Integer := 0;
   begin
      while Peek in Digit'Range loop
         Advance;
         N := (N * 10) + Val (Ch);
      end loop;

      return N;
   end Number;

begin
   Advance;
   while not End_Of_Input loop
      Advance;
      case Ch is
         when 'L' =>
            Heading := Facing'Pred (Heading);
            Steps := Number;
            Move (Heading, Steps);
         when 'R' =>
            Heading := Facing'Succ (Heading);
            Steps := Number;
            Move (Heading, Steps);
         when ' ' | ',' =>
            null;
         when others =>
            raise Program_Error with "Unknown Character: " & Ch;
      end case;
   end loop;

   Put (Output, Distance (Start, Pos));
   New_Line (Output);
end Day1_1;
