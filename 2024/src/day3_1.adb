pragma Ada_2022;
with Ada.Exceptions;
with Ada.Containers.Vectors;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day3_1 is
   type Token_Type is (T_MUL, T_Left_Paren, T_Right_Paren, T_Number, T_Comma, T_Extra);

   type Token is record
      Typ : Token_Type;
      Sub : String (1 .. 3);
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
begin
   while not Input.End_Of_Input loop
      case Input.Peek is
         when 'm' =>
            if Input.Lookahead (3) = "mul" then
               Add_Token (T_MUL, 3);
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
      if Next_Token.Typ = T_MUL then
         begin
            Expect (T_Left_Paren);
            Left := Parse_Number;
            Expect (T_Comma);
            Right := Parse_Number;
            Expect (T_Right_Paren);
            --  Output.Log (Left'Image & " * " & Right'Image);
            Sum := Sum + Left * Right;
         exception
            when E : Program_Error =>
               null;
               --  Output.Log ("Error: " & Ada.Exceptions.Exception_Message (E));
               --  Delete_First (Tokens);
         end;
      else
         --  skip garbage
         null;
      end if;
   end loop;

   Output.Put (Sum);
end Day3_1;
