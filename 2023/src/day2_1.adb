with Advent_IO.Integers; use Advent_IO.Integers;
with Advent_IO; use Advent_IO;
with Ada.Containers.Ordered_Sets;

procedure Day2_1 is
   type Color_Type is (Red, Green, Blue);
   type Group_Type is array (Color_Type) of Natural with Default_Component_Value => 0;
   Current_Color  : Color_Type;
   Current_Number : Positive;
   Current_Group  : Group_Type;

   package Group_Sets is new Ada.Containers.Ordered_Sets
      (Element_Type => Group_Type);
   use Group_Sets;

   Groups : Set := Empty_Set;

   procedure Push_Color is
   begin
      Current_Group (Current_Color) := Current_Group (Current_Color) + Current_Number;
   end Push_Color;

   procedure Push_Group is
   begin
      Include (Groups, Current_Group);
      Current_Group := (others => 0);
   end Push_Group;

   function Is_Possible
      return Boolean
   is
      Max_Allowed : constant Group_Type :=
         (Red   => 12,
          Green => 13,
          Blue  => 14);
   begin
      for G of Groups loop
         for Color in G'Range loop
            if G (Color) > Max_Allowed (Color) then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Is_Possible;

   Sum : Natural := 0;
begin
   while not End_Of_Input loop
      Clear (Groups);
      Current_Group := (others => 0);

      declare
         Game_Id : Positive;
         Ch : Character;
         Num : String (1 .. 32) := (others => ASCII.NUL);
         Num_Last : Natural := 0;
      begin
         Seek (Input, 5); --  "Game "
         Game_Id := Positive'Value (Read_Until (Input, ':'));
         Seek (Input, 1); --  ' '
         loop
            Ch := Peek (Input);
            case Ch is
               when ASCII.CR | ASCII.LF =>
                  Push_Color;
                  Push_Group;
                  Seek (Input, 1);
                  exit;
               when '0' .. '9' =>
                  Num_Last := Num_Last + 1;
                  Num (Num_Last) := Ch;
                  Seek (Input, 1);
               when 'r' =>
                  Current_Color := Red;
                  Seek (Input, 3);
               when 'g' =>
                  Current_Color := Green;
                  Seek (Input, 5);
               when 'b' =>
                  Current_Color := Blue;
                  Seek (Input, 4);
               when ',' =>
                  Push_Color;
                  Seek (Input, 1);
               when ';' =>
                  Push_Color;
                  Push_Group;
                  Seek (Input, 1);
               when ' ' =>
                  if Num_Last > 0 then
                     Current_Number := Positive'Value (Num (1 .. Num_Last));
                     Num_Last := 0;
                  end if;
                  Seek (Input, 1);
               when others =>
                  String'Write (Error, "Unexpected character: '" & Ch & "'");
                  New_Line (Error);
            end case;
         end loop;

         if Is_Possible then
            Sum := Sum + Game_Id;
         end if;
      end;
   end loop;
   Put (Output, Sum);
   New_Line (Output);
end Day2_1;
