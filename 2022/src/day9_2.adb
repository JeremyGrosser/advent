with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;
with Ada.Containers.Hashed_Sets;
with Ada.Containers;

procedure Day9_2 is
   type Position is record
      Y, X : Integer;
   end record;

   procedure Write
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : Position);
   for Position'Write use Write;

   procedure Write
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : Position)
   is
   begin
      String'Write (Stream, "(");
      String'Write (Stream, Item.X'Image);
      String'Write (Stream, ",");
      String'Write (Stream, Item.Y'Image);
      String'Write (Stream, ")");
   end Write;

   function Hash (Element : Position)
      return Ada.Containers.Hash_Type
   is
      use Ada.Containers;
      H : Hash_Type;
   begin
      H := Hash_Type (abs Element.X);
      H := H * (2 ** 32);
      H := H or Hash_Type (abs Element.Y);
      return H;
   end Hash;

   package Position_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type => Position,
       Hash => Hash,
       Equivalent_Elements => "=");
   use Position_Sets;

   Visited  : Set := Empty_Set;
   Head     : Position := (0, 0);
   Knots    : array (1 .. 9) of Position := (others => Head);

   procedure Update_Knot
      (Head : Position;
       Tail : in out Position)
   is
      function Adjacent
         return Boolean
      is (Tail.X in Head.X - 1 .. Head.X + 1 and then Tail.Y in Head.Y - 1 .. Head.Y + 1);
   begin
      if not Adjacent then
         if Tail.X < Head.X then
            Tail.X := Tail.X + 1;
         elsif Tail.X > Head.X then
            Tail.X := Tail.X - 1;
         end if;

         if Tail.Y < Head.Y then
            Tail.Y := Tail.Y + 1;
         elsif Tail.Y > Head.Y then
            Tail.Y := Tail.Y - 1;
         end if;
      end if;
   end Update_Knot;

   procedure Print_State is
      Ch : Character;
   begin
      for Y in reverse 0 .. 21 loop
         for X in 0 .. 26 loop
            Ch := '.';

            if Contains (Visited, (Y, X)) then
               Ch := '#';
            end if;

            for I in reverse Knots'Range loop
               if Knots (I).Y = Y and then Knots (I).X = X then
                  Ch := Character'Val (Character'Pos ('0') + I);
               end if;
            end loop;

            if Head.X = X and then Head.Y = Y then
               Ch := 'H';
            end if;

            Character'Write (Error, Ch);
         end loop;
         New_Line (Error);
      end loop;
      New_Line (Error);
   end Print_State;
begin
   Include (Visited, Knots (Knots'Last));
   while not End_Of_Input loop
      declare
         Direction : Character;
         Count : Natural;
      begin
         Character'Read (Input, Direction);
         Seek (Input, 1, Seek_Current); --  skip space
         Count := Get (Input);

         for I in 1 .. Count loop
            case Direction is
               when 'U' => Head.Y := Head.Y + 1;
               when 'D' => Head.Y := Head.Y - 1;
               when 'L' => Head.X := Head.X - 1;
               when 'R' => Head.X := Head.X + 1;
               when others =>
                  raise Program_Error with "Invalid direction";
            end case;

            for I in Knots'Range loop
               if I = Knots'First then
                  Update_Knot (Head, Knots (I));
               else
                  Update_Knot (Knots (I - 1), Knots (I));
               end if;
            end loop;
            Include (Visited, Knots (Knots'Last));
            --  Print_State;
         end loop;
      end;
   end loop;

   Put (Output, Natural (Length (Visited)));
   New_Line (Output);
end Day9_2;
