with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;

procedure Day4_1 is
   type Section is record
      First, Last : Natural;
   end record;

   type Group is array (1 .. 2) of Section;

   procedure Read_Group
      (Stream : not null Stream_Access;
       Item   : out Group)
   is
      Str  : String (1 .. 2);
      Last : Natural;
   begin
      for I in Group'Range loop
         Read_Until (Stream, '-', Str, Last);
         Item (I).First := Natural'Value (Str (1 .. Last));

         if I = Group'Last then
            Read_Until (Stream, ASCII.LF, Str, Last);
         else
            Read_Until (Stream, ',', Str, Last);
         end if;
         Item (I).Last := Natural'Value (Str (1 .. Last));
      end loop;
   end Read_Group;

   function Fully_Contained
      (A, B : Section)
      return Boolean
   is (A.First >= B.First and then A.Last <= B.Last);

   G : Group;
   Sum : Natural := 0;
begin
   while not End_Of_Input loop
      Read_Group (Input, G);
      if Fully_Contained (G (1), G (2)) or else Fully_Contained (G (2), G (1)) then
         Sum := Sum + 1;
      end if;
   end loop;

   Put (Output, Sum);
   New_Line (Output);
end Day4_1;
