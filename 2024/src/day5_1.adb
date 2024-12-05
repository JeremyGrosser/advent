pragma Ada_2022;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day5_1 is
   Sum : Natural := 0;

   type Page_Number is new Natural;

   package Page_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Page_Number);
   use Page_Vectors;

   type Rule is record
      Before : Page_Number;
   end record;

   package Rules_Vector is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Rule);
   use Rules_Vector;

   function "<"
      (Left, Right : Rule)
      return Boolean
   is (Left.Before < Right.Before);

   package Sorting is new Rules_Vector.Generic_Sorting ("<");
   use Sorting;

   package Rules_Map is new Ada.Containers.Ordered_Maps
      (Key_Type      => Page_Number,
       Element_Type  => Rules_Vector.Vector);
   use Rules_Map;

   Rules : Rules_Map.Map;

   function In_Order
      (Update : Page_Vectors.Vector)
      return Boolean
   is
      Item : Page_Number;
   begin
      for I in First_Index (Update) .. Last_Index (Update) loop
         Item := Element (Update, I);
         if Contains (Rules, Item) then
            for R of Element (Rules, Item) loop
               for J in First_Index (Update) .. I - 1 loop
                  if Element (Update, J) = R.Before then
                     return False;
                  end if;
               end loop;
            end loop;
         end if;
      end loop;
      return True;
   end In_Order;

   function Middle_Page
      (Update : Page_Vectors.Vector)
      return Page_Number
   is (Element (Update, Last_Index (Update) / 2 + 1));

   Update : Page_Vectors.Vector;
   Page : Page_Number;
   R : Rule;
begin
   while not Input.End_Of_Input and then Input.Peek /= ASCII.LF loop
      Page := Page_Number'Value (Input.Read_Until ('|'));
      R.Before := Page_Number'Value (Input.Read_Until (ASCII.LF));
      if not Contains (Rules, Page) then
         Include (Rules, Page, Rules_Vector.Empty_Vector);
      end if;
      Append (Reference (Rules, Page), R);
   end loop;

   Input.Skip_Whitespace;
   Page := 0;

   while not Input.End_Of_Input loop
      case Input.Peek is
         when '0' .. '9' =>
            Page := Page * 10 + Page_Number'Value (Input.Peek & "");
         when ',' =>
            Append (Update, Page);
            Page := 0;
         when ASCII.LF =>
            Append (Update, Page);
            Page := 0;
            Output.Log (Update'Image);
            if In_Order (Update) then
               Sum := Sum + Natural (Middle_Page (Update));
            end if;
            Clear (Update);
         when others =>
            raise Program_Error with "Unexpected character " & Input.Peek & " in update";
      end case;
      Input.Seek (1);
   end loop;

   for Cursor in Iterate (Rules) loop
      Sort (Reference (Rules, Cursor));
   end loop;

   Output.Put (Sum);
end Day5_1;
