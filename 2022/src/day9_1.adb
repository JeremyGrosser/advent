with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;
with Ada.Containers.Hashed_Sets;
with Ada.Containers;

procedure Day9_1 is
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
   --  return int((x + y) * (x + y +1)/2 + y)

   package Position_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type => Position,
       Hash => Hash,
       Equivalent_Elements => "=");
   use Position_Sets;

   Visited : Set := Empty_Set;
   Head, Tail : Position := (0, 0);
   Min, Max : Position := (0, 0);

   procedure Update_Tail is
      function Adjacent
         return Boolean
      is (Tail.X in Head.X - 1 .. Head.X + 1 and then Tail.Y in Head.Y - 1 .. Head.Y + 1);
   begin
      String'Write (Error, "Head ");
      Position'Write (Error, Head);
      New_Line (Error);

      String'Write (Error, "Tail ");
      Position'Write (Error, Tail);
      String'Write (Error, " -> ");

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
         Position'Write (Error, Tail);
      else
         String'Write (Error, "Adjacent");
      end if;

      New_Line (Error);
   end Update_Tail;

   procedure Print_State is
   begin
      for Y in reverse 0 .. 4 loop
         for X in 0 .. 5 loop
            if Head.X = X and Head.Y = Y then
               Character'Write (Error, 'H');
            elsif Tail.X = X and Tail.Y = Y then
               Character'Write (Error, 'T');
            elsif Contains (Visited, (Y, X)) then
               Character'Write (Error, '#');
            else
               Character'Write (Error, '.');
            end if;
         end loop;
         New_Line (Error);
      end loop;
   end Print_State;
begin
   Include (Visited, Tail);
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

            Update_Tail;
            if not Contains (Visited, Tail) then
               Insert (Visited, Tail);
            end if;

            if Tail.X < Min.X then
               Min.X := Tail.X;
            end if;

            if Tail.X > Max.X then
               Max.X := Tail.X;
            end if;

            if Tail.Y < Min.Y then
               Min.Y := Tail.Y;
            end if;

            if Tail.Y > Max.Y then
               Max.Y := Tail.Y;
            end if;

            --  Print_State;
         end loop;
      end;
   end loop;

   Put (Output, Natural (Length (Visited)));
   New_Line (Output);
end Day9_1;
