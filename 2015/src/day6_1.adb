pragma Ada_2022;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day6_1 is
   type Action_Type is (Turn_On, Turn_Off, Toggle);
   type Coordinate is record
      X, Y : Natural;
   end record;
   type Token_Type is (T_Action, T_Coordinate, T_Through, T_Whitespace);
   type Token
      (Typ : Token_Type)
   is record
      case Typ is
         when T_Action =>
            Action : Action_Type;
         when T_Coordinate =>
            Point : Coordinate;
         when T_Through | T_Whitespace =>
            null;
      end case;
   end record;

   function Read_Number
      return Natural
   is
      Num : String (1 .. 3);
      I : Natural := 0;
   begin
      while I <= Num'Last loop
         exit when Input.Peek not in '0' .. '9';
         I := I + 1;
         Input.Get (Num (I));
      end loop;
      return Natural'Value (Num (1 .. I));
   end Read_Number;

   function Next_Token
      return Token
   is
   begin
      if Input.Peek in '0' .. '9' then
         declare
            Point : Coordinate;
            Comma : Character;
         begin
            Point.X := Read_Number;
            Input.Get (Comma);
            if Comma /= ',' then
               raise Program_Error with "Expected ',' in coordinate";
            end if;
            Point.Y := Read_Number;
            return Token'(Typ => T_Coordinate, Point => Point);
         end;
      else
         declare
            Word : constant String := Input.Read_Until (" " & CRLF);
         begin
            Output.Log (Word);
            if Word = "turn" then
               declare
                  Onoff : constant String := Input.Read_Until (' ');
               begin
                  if Onoff = "on" then
                     return Token'(Typ => T_Action, Action => Turn_On);
                  elsif Onoff = "off" then
                     return Token'(Typ => T_Action, Action => Turn_Off);
                  else
                     raise Program_Error with "Expected on or off after turn";
                  end if;
               end;
            elsif Word = "toggle" then
               return Token'(Typ => T_Action, Action => Toggle);
            elsif Word = "through" then
               return Token'(Typ => T_Through);
            elsif Word = "" then
               return Token'(Typ => T_Whitespace);
            else
               raise Program_Error with "Bad parse: '" & Word & "'";
            end if;
         end;
      end if;
   end Next_Token;

   Lights : array (0 .. 999, 0 .. 999) of Boolean := (others => (others => False));

   procedure Perform
      (Action   : Action_Type;
       From, To : Coordinate)
   is
   begin
      case Action is
         when Turn_On =>
            for X in From.X .. To.X loop
               for Y in From.Y .. To.Y loop
                  Lights (X, Y) := True;
               end loop;
            end loop;
         when Turn_Off =>
            for X in From.X .. To.X loop
               for Y in From.Y .. To.Y loop
                  Lights (X, Y) := False;
               end loop;
            end loop;
         when Toggle =>
            for X in From.X .. To.X loop
               for Y in From.Y .. To.Y loop
                  Lights (X, Y) := not Lights (X, Y);
               end loop;
            end loop;
      end case;
   end Perform;

   Sum : Natural := 0;
   Action : Action_Type;
   From, To : Coordinate;
   Has_From : Boolean := False;
   Has_To : Boolean := False;
begin
   while not Input.End_Of_Input loop
      declare
         T : constant Token := Next_Token;
      begin
         case T.Typ is
            when T_Action =>
               Action := T.Action;
            when T_Coordinate =>
               if Has_From then
                  To := T.Point;
                  Has_To := True;
               else
                  From := T.Point;
                  Has_From := True;
               end if;
            when T_Through | T_Whitespace =>
               null;
         end case;

         if Has_From and then Has_To then
            Perform (Action, From, To);
            Has_From := False;
            Has_To := False;
         end if;
      end;
   end loop;

   for X in Lights'Range (1) loop
      for Y in Lights'Range (2) loop
         if Lights (X, Y) then
            Sum := Sum + 1;
         end if;
      end loop;
   end loop;

   Output.Put (Sum);
end Day6_1;
