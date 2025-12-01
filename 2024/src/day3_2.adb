pragma Extensions_Allowed (On);
with Ada.Containers.Vectors;
with Advent.Input;
with Advent.Output;

procedure Day3_2
   (Input  : in out Advent.Input.Buffer;
    Output : Advent.Output.Buffer)
is
   type Token_Type is (T_MUL, T_Left_Paren, T_Right_Paren, T_Number, T_Comma, T_Extra, T_Do, T_Dont);

   type Token is record
      Typ : Token_Type;
      Sub : String (1 .. 5);
      Len : Positive := 1;
   end record;

   package Token_Vectors is new Ada.Containers.Vectors (Positive, Token);
   use Token_Vectors;
   Tokens : Token_Vectors.Vector;

   procedure Add_Token
      (Typ : Token_Type;
       Len : Positive := 1)
   is
      T : Token;
   begin
      T.Typ := Typ;
      T.Len := Len;
      for I in 1 .. Len loop
         Input.Get (T.Sub (I));
      end loop;
      Append (Tokens, T);
   end Add_Token;

   function To_String
      (T : Token)
      return String
   is (T.Sub (1 .. T.Len));

   function Next_Token
      return Token
   is
      T : constant Token := First_Element (Tokens);
   begin
      --  Output.Log (T.Typ'Image & " " & To_String (T));
      Delete_First (Tokens);
      return T;
   end Next_Token;

   procedure Expect
      (Typ : Token_Type)
   is
      T : constant Token := Next_Token;
   begin
      if T.Typ /= Typ then
         raise Program_Error with "Expected " & Typ'Image & " got " & T.Typ'Image & " instead";
      end if;
   end Expect;

   function Parse_Number
      return Natural
   is
      T : constant Token := Next_Token;
   begin
      if T.Typ /= T_Number then
         raise Program_Error with "Expected T_Number, got " & T.Typ'Image;
      else
         return Natural'Value (To_String (T));
      end if;
   end Parse_Number;

   Sum : Natural := 0;
   Left, Right : Natural;
   Len : Natural;
   Enabled : Boolean := True;
begin
   while not Input.End_Of_Input loop
      case Input.Peek is
         when 'm' =>
            if Input.Lookahead (3) = "mul" then
               Add_Token (T_MUL, 3);
            else
               Add_Token (T_Extra);
            end if;
         when 'd' =>
            if Input.Lookahead (5) = "don't" then
               Add_Token (T_Dont, 5);
            elsif Input.Lookahead (2) = "do" then
               Add_Token (T_Do, 2);
            else
               Add_Token (T_Extra);
            end if;
         when '(' =>
            Add_Token (T_Left_Paren);
         when ')' =>
            Add_Token (T_Right_Paren);
         when ',' =>
            Add_Token (T_Comma);
         when '0' .. '9' =>
            Len := 0;
            for I in 1 .. 3 loop --  max 3 digits
               exit when Input.Peek (I) not in '0' .. '9';
               Len := Len + 1;
            end loop;
            Add_Token (T_Number, Len);
         when others =>
            Add_Token (T_Extra);
      end case;
   end loop;

   while not Is_Empty (Tokens) loop
      case Next_Token.Typ is
         when T_MUL =>
            if Enabled then
               begin
                  Expect (T_Left_Paren);
                  Left := Parse_Number;
                  Expect (T_Comma);
                  Right := Parse_Number;
                  Expect (T_Right_Paren);
                  --  Output.Log (Left'Image & " * " & Right'Image);
                  Sum := Sum + Left * Right;
               exception
                  when Program_Error =>
                     null;
                     --  Output.Log ("Error: " & Ada.Exceptions.Exception_Message (E));
               end;
            end if;
         when T_Do =>
            Expect (T_Left_Paren);
            Expect (T_Right_Paren);
            Enabled := True;
         when T_Dont =>
            Expect (T_Left_Paren);
            Expect (T_Right_Paren);
            Enabled := False;
         when others =>
            null;
      end case;
   end loop;

   Output.Put (Sum);
end Day3_2;
