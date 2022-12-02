with Advent_IO.Integers; use Advent_IO.Integers;
with Advent_IO; use Advent_IO;

procedure Day2_1 is
   type Shape is (Rock, Paper, Scissors);

   type Round is record
      Opponent, Self : Shape;
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
         when 'X' => Item.Self := Rock;
         when 'Y' => Item.Self := Paper;
         when 'Z' => Item.Self := Scissors;
         when others =>
            raise Program_Error with "Invalid data";
      end case;
   end Round_Read;

   type Round_Result is (Win, Draw, Loss);

   function Result
      (R : Round)
      return Round_Result
   is
   begin
      if (R.Opponent = Rock and then R.Self = Paper) or else
         (R.Opponent = Paper and then R.Self = Scissors) or else
         (R.Opponent = Scissors and then R.Self = Rock)
      then
         return Win;
      elsif
         (R.Opponent = Rock and then R.Self = Rock) or else
         (R.Opponent = Paper and then R.Self = Paper) or else
         (R.Opponent = Scissors and then R.Self = Scissors)
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

   R : Round;
   Sum : Natural := 0;
begin
   while not End_Of_Input loop
      Round_Read (Input, R);
      Sum := Sum + Score (R);
   end loop;

   Put (Output, Sum);
   New_Line (Output);
end Day2_1;
