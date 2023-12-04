with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Containers.Ordered_Sets;
with Advent_IO; use Advent_IO;

procedure Day4_1 is
   function Get_Natural
      return Natural
   is
      Val : Natural := 0;
      Ch : Character;
   begin
      loop
         Ch := Peek;
         exit when Ch not in '0' .. '9';
         Seek (1);
         Val := Val * 10 + Character'Pos (Ch) - Character'Pos ('0');
      end loop;
      return Val;
   end Get_Natural;

   procedure Skip_Whitespace is
      Ch : Character;
   begin
      loop
         Ch := Peek;
         exit when Ch /= ' ' and then Peek /= ASCII.CR and then Peek /= ASCII.LF;
         Seek (1);
      end loop;
   end Skip_Whitespace;

   type Input_Modes is (Winning_Numbers, Numbers_You_Have);
   Input_Mode : Input_Modes := Winning_Numbers;

   package Integer_Sets is new Ada.Containers.Ordered_Sets
      (Element_Type => Integer);
   use Integer_Sets;
   Winning, Have : Integer_Sets.Set := Integer_Sets.Empty_Set;
   Sum : Natural := 0;

   function Score
      return Natural
   is
      Matches : Natural;
      S : Natural := 0;
   begin
      Matches := Natural (Length (Intersection (Winning, Have)));
      for I in 1 .. Matches loop
         if I = 1 then
            S := 1;
         else
            S := S * 2;
         end if;
      end loop;
      return S;
   end Score;

   procedure End_Of_Card is
   begin
      Sum := Sum + Score;
      Clear (Winning);
      Clear (Have);
   end End_Of_Card;

   Card_Id : Natural;
   N : Natural;
begin
   while not End_Of_Input loop
      case Peek is
         when '|' =>
            Seek (1);
            Input_Mode := Numbers_You_Have;
         when '0' .. '9' =>
            N := Get_Natural;
            case Input_Mode is
               when Winning_Numbers =>
                  Include (Winning, N);
               when Numbers_You_Have =>
                  Include (Have, N);
            end case;
         when ' ' =>
            Seek (1);
            null;
         when 'C' =>
            Seek (4); --  "Card"
            Skip_Whitespace;
            Card_Id := Get_Natural;
            Seek (1); --  ":"
            Input_Mode := Winning_Numbers;
         when ASCII.CR | ASCII.LF =>
            Seek (1);
            End_Of_Card;
         when others =>
            raise Program_Error with "Unexpected character in input: " & Peek;
      end case;
   end loop;
   End_Of_Card;

   Ada.Integer_Text_IO.Put (Sum, Width => 0);
end Day4_1;
