with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

procedure Day7_2 is
   function Strip
      (S : String)
      return String
   is
      Last : Natural := S'Last;
   begin
      while Last >= S'First and then S (Last) = '/' loop
         Last := Last - 1;
      end loop;
      return S (S'First .. Last);
   end Strip;

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
      (Index_Type => Positive,
       Element_Type => String);
   subtype Path is String_Vectors.Vector;

   function To_String
      (P : Path)
      return String
   is
   begin
      return P'Image;
   end To_String;

   function "<"
      (Left, Right : Path)
      return Boolean
   is
      use String_Vectors;
   begin
      if Natural (Length (Left)) < Natural (Length (Right)) then
         return True;
      else
         for I in First_Index (Right) .. Last_Index (Right) loop
            if Element (Left, I) /= Element (Right, I) and then Element (Left, I) < Element (Right, I) then
               return True;
            end if;
         end loop;
         return False;
      end if;
   end "<";

   type File is record
      Is_Dir : Boolean;
      Size : Natural;
   end record;

   package Path_File_Maps is new Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type     => Path,
       Element_Type => File);
   use Path_File_Maps;

   Files : Path_File_Maps.Map := Empty_Map;
   Working_Dir : Path := String_Vectors.Empty_Vector;

   procedure Change_Directory
      (Name : String)
   is
      use String_Vectors;
   begin
      if Name (Name'First) = '/' then
         Clear (Working_Dir);
      end if;

      if Name = ".." then
         Delete_Last (Working_Dir);
      else
         Append (Working_Dir, Name);
         Include (Files, Working_Dir, (Is_Dir => True, Size => 0));
      end if;
   end Change_Directory;

   procedure Execute_Command
      (Arg : String)
   is
      use Ada.Strings.Fixed;
      Command : constant String := Arg (Arg'First .. Index (Arg, " ") - 1);
   begin
      if Command = "cd" then
         Change_Directory (Arg (Index (Arg, " ") + 1 .. Arg'Last));
      elsif Command = "ls" then
         --  Expect listing of Working_Dir
         null;
      end if;
   end Execute_Command;

   procedure Add_Directory
      (Name : String)
   is
      use Ada.Strings.Fixed;
      use String_Vectors;
      P : Path := Working_Dir;
      N : constant String := Strip (Name);
   begin
      Append (P, N);
      Include (Files, P, (Is_Dir => True, Size => 0));
   end Add_Directory;

   procedure Add_File
      (Arg : String)
   is
      use Ada.Strings.Fixed;
      use String_Vectors;
      Size : constant Natural := Natural'Value (Arg (Arg'First .. Index (Arg, " ") - 1));
      Name : constant String := Arg (Index (Arg, " ") + 1 .. Arg'Last);
      P : Path := Working_Dir & Name;
   begin
      Insert (Files, P, (Is_Dir => False, Size => Size));
   end Add_File;

   function Starts_With
      (P : Path;
       Prefix : Path)
       return Boolean
   is
      use type Ada.Containers.Count_Type;
      use String_Vectors;
   begin
      if Length (P) < Length (Prefix) then
         return False;
      else
         for I in First_Index (Prefix) .. Last_Index (Prefix) loop
            if Element (P, I) /= Element (Prefix, I) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Starts_With;

   function Size
      (P : Path)
      return Natural
   is
      Sum : Natural := 0;
   begin
      for I in Iterate (Files) loop
         if Starts_With (Key (I), P) then
            Sum := Sum + Element (I).Size;
         end if;
      end loop;
      return Sum;
   end Size;
begin
   while not End_Of_Input loop
      declare
         use Ada.Strings.Fixed;
         Line : constant String := Read_Until (Input, ASCII.LF);
      begin
         case Line (Line'First) is
            when '$' =>
               Execute_Command (Line (Index (Line, " ", Line'First) + 1 .. Line'Last));
            when 'd' =>
               null;
               --  Add_Directory (Line (Index (Line, " ", Line'First) + 1 .. Line'Last));
            when '0' .. '9' =>
               Add_File (Line);
            when others =>
               raise Program_Error with "Invalid input line: " & Line;
         end case;
      end;
   end loop;

   declare
      Total    : constant Natural := 70_000_000;
      Used     : constant Natural := Size (["/"]);
      Needed   : constant Natural := 30_000_000;
      Best_Sum : Natural := Total;
      Dir_Sum  : Natural := 0;
   begin
      for I in Iterate (Files) loop
         if Element (I).Is_Dir then
            Dir_Sum := Size (Key (I));
            if (Total - (Used - Dir_Sum)) >= Needed and then Dir_Sum < Best_Sum then
               Best_Sum := Dir_Sum;
            end if;
         end if;
      end loop;
      Put (Output, Best_Sum);
      New_Line (Output);
   end;

end Day7_2;
