with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;
with Str;

procedure Day4_2 is
   type Section is record
      First, Last : Natural;
   end record;

   type Group is array (1 .. 2) of Section;

   procedure Read_Group
      (Stream : not null Stream_Access;
       Item   : out Group)
   is
      X    : String (1 .. 2);
      Last : Natural;
   begin
      for I in Group'Range loop
         Read_Until (Stream, '-', X, Last);
         Item (I).First := Str.To_Natural (X (1 .. Last));

         if I = Group'Last then
            Read_Until (Stream, ASCII.LF, X, Last);
         else
            Read_Until (Stream, ',', X, Last);
         end if;
         Item (I).Last := Str.To_Natural (X (1 .. Last));
      end loop;
   end Read_Group;

   function Overlaps
      (A, B : Section)
      return Boolean
   is (A.First in B.First .. B.Last or else A.Last in B.First .. B.Last);

   G : Group;
   Sum : Natural := 0;
begin
   while not End_Of_Input loop
      Read_Group (Input, G);
      if Overlaps (G (1), G (2)) or else Overlaps (G (2), G (1)) then
         Sum := Sum + 1;
      end if;
   end loop;

   Put (Output, Sum);
   New_Line (Output);
end Day4_2;
