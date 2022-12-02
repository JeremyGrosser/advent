with Advent_IO.Integers; use Advent_IO.Integers;
with Advent_IO; use Advent_IO;

procedure Day2_2 is
   type Shape is (Rock, Paper, Scissors);
   type Round_Result is (Win, Draw, Loss);

   type Round is record
      Opponent, Self : Shape;
      Expect : Round_Result;
   end record;

   procedure Round_Read
      (Stream : not null Stream_Access;
       Item   : out Round)
   is
      Theirs : constant String := Read_Until (Stream, ' ');
      Ours   : constant String := Read_Until (Stream, ASCII.LF);
   begin
      case Theirs (1) is
         when 'A' => Item.Opponent := Rock;
         when 'B' => Item.Opponent := Paper;
         when 'C' => Item.Opponent := Scissors;
         when others =>
            raise Program_Error with "Invalid data";
      end case;

      case Ours (1) is
         when 'X' => Item.Expect := Loss;
         when 'Y' => Item.Expect := Draw;
         when 'Z' => Item.Expect := Win;
         when others =>
            raise Program_Error with "Invalid data";
      end case;
   end Round_Read;

   function Result
      (R : Round)
      return Round_Result
   is
   begin
      if (R.Opponent = Rock and R.Self = Paper) or
         (R.Opponent = Paper and R.Self = Scissors) or
         (R.Opponent = Scissors and R.Self = Rock)
      then
         return Win;
      elsif
         (R.Opponent = Rock and R.Self = Rock) or
         (R.Opponent = Paper and R.Self = Paper) or
         (R.Opponent = Scissors and R.Self = Scissors)
      then
         return Draw;
      else
         return Loss;
      end if;
   end Result;

   function Score
      (R : Round)
      return Natural
   is
      S : Natural;
   begin
      case Result (R) is
         when Win =>
            S := 6;
         when Draw =>
            S := 3;
         when Loss =>
            S := 0;
      end case;

      case R.Self is
         when Rock =>
            S := S + 1;
         when Paper =>
            S := S + 2;
         when Scissors =>
            S := S + 3;
      end case;
      return S;
   end Score;

   function Next_Shape
      (S : Shape)
      return Shape
   is
   begin
      if S = Shape'Last then
         return Shape'First;
      else
         return Shape'Succ (S);
      end if;
   end Next_Shape;

   procedure Choose_Move
      (R : in out Round)
   is
   begin
      case R.Expect is
         when Win =>
            case R.Opponent is
               when Rock => R.Self := Paper;
               when Paper => R.Self := Scissors;
               when Scissors => R.Self := Rock;
            end case;
         when Draw =>
            R.Self := R.Opponent;
         when Loss =>
            case R.Opponent is
               when Rock => R.Self := Scissors;
               when Paper => R.Self := Rock;
               when Scissors => R.Self := Paper;
            end case;
      end case;
   end Choose_Move;

   R : Round;
   Sum : Natural := 0;
begin
   while not End_Of_Input loop
      Round_Read (Input, R);
      Choose_Move (R);
      Sum := Sum + Score (R);
   end loop;

   Put (Output, Sum);
   New_Line (Output);
end Day2_2;
