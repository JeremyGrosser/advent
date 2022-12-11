with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Fixed;

procedure Day11_2 is
   subtype Number is Long_Integer;
   package Number_IO is new Advent_IO.Generic_Numbers (Number);
   use Number_IO;
   type Monkey_Id is new Number;

   type Monkey_Op is (Add, Multiply);
   type Monkey is record
      Id             : Monkey_Id;
      Test_Div       : Number;
      Op             : Monkey_Op;
      Old_Arg        : Boolean; --  True if "old" value should be used instead of Arg
      Arg            : Number;
      True_Target    : Monkey_Id;
      False_Target   : Monkey_Id;
      Inspected      : Number := 0;
   end record;

   procedure Read_Monkey
      (Stream : not null Stream_Access;
       M      : out Monkey;
       Items  : out Number_Vectors.Vector)
   is
      function Parse_Numbers
         return Number_Vectors.Vector
      is
         use Number_Vectors;
         V  : Vector := Empty_Vector;
         Ch : Character;
         S  : String (1 .. 32) := (others => ASCII.NUL);
         I  : Positive := S'First;
      begin
         loop
            Character'Read (Stream, Ch);
            if Ch in '0' .. '9' then
               S (I) := Ch;
               I := I + 1;
            else
               if I > S'First then
                  Append (V, Number'Value (S (S'First .. I - 1)));
               end if;
               I := S'First;
            end if;
            exit when Ch = ASCII.LF;
         end loop;
         return V;
      end Parse_Numbers;

      Ignore : String (1 .. 32);
      Last   : Natural;
   begin
      --  skip to next 'M'onkey
      Read_Until (Stream, 'M', Ignore, Last);

      --  Monkey 0:
      Read_Until (Stream, ' ', Ignore, Last);
      M.Id := Monkey_Id'Value (Read_Until (Stream, ':'));
      Read_Until (Stream, ASCII.LF, Ignore, Last);

      --  Starting items: 75, 63
      Read_Until (Stream, ':', Ignore, Last);
      Seek (Stream, 1, Seek_Current);
      Items := Parse_Numbers;

      --  Operation: new = old * 3
      Read_Until (Stream, ':', Ignore, Last);
      Seek (Input, 1, Seek_Current);
      Read_Until (Stream, '=', Ignore, Last); --  "new ="
      Seek (Input, 1, Seek_Current);
      Read_Until (Stream, ' ', Ignore, Last); --  "old "
      declare
         Ch : Character;
      begin
         Character'Read (Stream, Ch);
         case Ch is
            when '*' =>
               M.Op := Multiply;
            when '+' =>
               M.Op := Add;
            when others =>
               raise Program_Error with "Unknown operation : " & Ch;
         end case;
      end;

      declare
         Arg : constant String := Read_Until (Stream, ASCII.LF);
      begin
         if Ada.Strings.Fixed.Index (Arg, "old") /= 0 then
            M.Old_Arg := True;
         else
            M.Old_Arg := False;
            M.Arg := Number'Value (Arg);
         end if;
      end;

      --  Test: divisible by 2
      Read_Until (Stream, 'y', Ignore, Last);
      Seek (Stream, 1, Seek_Current);
      M.Test_Div := Number'Value (Read_Until (Stream, ASCII.LF));

      --  If true: throw to monkey 7
      Read_Until (Stream, 'y', Ignore, Last);
      Seek (Stream, 1, Seek_Current);
      M.True_Target := Monkey_Id'Value (Read_Until (Stream, ASCII.LF));

      --  If false: throw to monkey 2
      Read_Until (Stream, 'y', Ignore, Last);
      Seek (Stream, 1, Seek_Current);
      M.False_Target := Monkey_Id'Value (Read_Until (Stream, ASCII.LF));
   end Read_Monkey;

   procedure Inspect
      (M          : in out Monkey;
       Item       : Number;
       New_Worry  : out Number)
   is
      Arg : Number := M.Arg;
   begin
      if M.Old_Arg then
         Arg := Item;
      end if;

      case M.Op is
         when Add =>
            New_Worry := (Item + Arg);
         when Multiply =>
            New_Worry := (Item * Arg);
      end case;

      M.Inspected := M.Inspected + 1;
   end Inspect;

   function Test
      (M     : Monkey;
       Worry : Number)
       return Boolean
   is (Worry mod M.Test_Div = 0);

   package Monkey_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type     => Monkey_Id,
       Element_Type => Monkey);

   package Monkey_Numbers_Map is new Ada.Containers.Ordered_Maps
      (Key_Type      => Monkey_Id,
       Element_Type  => Number_Vectors.Vector,
       "="           => Number_Vectors."=");

   procedure Remove_Item
      (This : in out Monkey_Numbers_Map.Map;
       Id   : Monkey_Id;
       Item : Number)
   is
      use Number_Vectors;
      V : constant Monkey_Numbers_Map.Reference_Type := Monkey_Numbers_Map.Reference (This, Id);
   begin
      for I in First_Index (V) .. Last_Index (V) loop
         if Element (V, I) = Item then
            Delete (V, I);
            return;
         end if;
      end loop;
   end Remove_Item;

   procedure Append_Item
      (This : in out Monkey_Numbers_Map.Map;
       Id   : Monkey_Id;
       Item : Number)
   is
      use Number_Vectors;
      V : constant Monkey_Numbers_Map.Reference_Type := Monkey_Numbers_Map.Reference (This, Id);
   begin
      Append (V, Item);
   end Append_Item;

   All_Monkeys : Monkey_Maps.Map := Monkey_Maps.Empty_Map;
   All_Items   : Monkey_Numbers_Map.Map := Monkey_Numbers_Map.Empty_Map;

   M : Monkey;
   Items : Number_Vectors.Vector;

   Throw_To  : Monkey_Id;
   New_Worry : Number;
   Limit     : Number := 1;
begin
   while not End_Of_Input loop
      Read_Monkey (Input, M, Items);
      Limit := Limit * M.Test_Div;
      Monkey_Maps.Insert (All_Monkeys, M.Id, M);
      Monkey_Numbers_Map.Insert (All_Items, M.Id, Items);
   end loop;

   for Round in 1 .. 10_000 loop
      for M of All_Monkeys loop
         Items := Monkey_Numbers_Map.Element (All_Items, M.Id);

         for Item of Items loop
            Remove_Item (All_Items, M.Id, Item);

            Inspect (Monkey_Maps.Reference (All_Monkeys, M.Id), Item, New_Worry);
            New_Worry := New_Worry mod Limit;
            if Test (M, New_Worry) then
               Throw_To := M.True_Target;
            else
               Throw_To := M.False_Target;
            end if;

            Append_Item (All_Items, Throw_To, New_Worry);
         end loop;
      end loop;
   end loop;

   declare
      use Number_Vectors;
      package Number_Sorting is new Number_Vectors.Generic_Sorting
         ("<" => ">");
      use Number_Sorting;

      Most_Active : Vector := Empty_Vector;
   begin
      for M of All_Monkeys loop
         Append (Most_Active, M.Inspected);
      end loop;

      Sort (Most_Active);
      Put (Output, Most_Active (1) * Most_Active (2));
      New_Line (Output);
   end;
end Day11_2;
