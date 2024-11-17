with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day1_2 is
   Floor : Integer := 0;
   Ch : Character;
begin
   while not Input.End_Of_Input loop
      Input.Get (Ch);
      case Ch is
         when '(' =>
            Floor := Floor + 1;
         when ')' =>
            Floor := Floor - 1;
         when others =>
            raise Program_Error with "Unknown character in input: " & Ch;
      end case;
      exit when Floor < 0;
   end loop;
   Output.Put (Integer (Input.Tell));
end Day1_2;
