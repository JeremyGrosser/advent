with Ada.Integer_Text_IO;
with Advent_IO; use Advent_IO;

procedure Day2_2 is
   type Color_Type is (Red, Green, Blue);
   type Group_Type is array (Color_Type) of Natural;
   Current_Color  : Color_Type;
   Current_Number : Positive;
   Current_Group  : Group_Type;
   Max_Observed   : Group_Type;

   procedure Push_Color is
   begin
      Current_Group (Current_Color) := Current_Group (Current_Color) + Current_Number;
   end Push_Color;

   procedure Push_Group is
   begin
      for Color in Current_Group'Range loop
         if Current_Group (Color) > Max_Observed (Color) then
            Max_Observed (Color) := Current_Group (Color);
         end if;
      end loop;
      Current_Group := (others => 0);
   end Push_Group;

   function Power
      (G : Group_Type)
      return Natural
   is (G (Red) * G (Green) * G (Blue));

   Sum : Natural := 0;
begin
   while not End_Of_Input loop
      Current_Group := (others => 0);
      Max_Observed := (others => 0);

      declare
         Game_Id : Positive with Unreferenced;
         Ch : Character;
         Num : String (1 .. 4);
         Num_Last : Natural := 0;
      begin
         Seek (5); --  "Game "
         Game_Id := Positive'Value (Read_Until (':'));
         Seek (1); --  ' '
         loop
            Ch := Peek;
            case Ch is
               when ASCII.CR | ASCII.LF =>
                  Push_Color;
                  Push_Group;
                  Seek (1);
                  exit;
               when '0' .. '9' =>
                  Num_Last := Num_Last + 1;
                  Num (Num_Last) := Ch;
                  Seek (1);
               when 'r' =>
                  Current_Color := Red;
                  Seek (3);
               when 'g' =>
                  Current_Color := Green;
                  Seek (5);
               when 'b' =>
                  Current_Color := Blue;
                  Seek (4);
               when ',' =>
                  Push_Color;
                  Seek (1);
               when ';' =>
                  Push_Color;
                  Push_Group;
                  Seek (1);
               when ' ' =>
                  if Num_Last > 0 then
                     Current_Number := Positive'Value (Num (1 .. Num_Last));
                     Num_Last := 0;
                  end if;
                  Seek (1);
               when others =>
                  raise Program_Error with "Unexpected character: '" & Ch & "'";
            end case;
         end loop;

         Sum := Sum + Power (Max_Observed);
      end;
   end loop;
   Ada.Integer_Text_IO.Put (Sum, Width => 0);
end Day2_2;
