with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Str; use Str;

package body Solutions.Day_6 is
   Not_Found : exception;

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
      (Index_Type   => Positive,
       Element_Type => String);
   use type String_Vectors.Vector;

   package String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type     => String,
       Element_Type => String_Vectors.Vector);

   procedure Print (M : String_Maps.Map) is
      V : String_Vectors.Vector;
   begin
      for Cursor in M.Iterate loop
         Put (String_Maps.Key (Cursor) & " ( ");
         V := String_Maps.Element (Cursor);
         for V_Cursor in V.Iterate loop
            Put (String_Vectors.Element (V_Cursor) & ",");
         end loop;
         Put_Line ("");
      end loop;
   end Print;

   function Direct_Orbits
      (M   : String_Maps.Map;
       Key : String)
      return Natural
   is
      use type Ada.Containers.Count_Type;
      Vec : String_Vectors.Vector;
   begin
      if not M.Contains (Key) then
         return 0;
      end if;
      Vec := String_Maps.Element (M, Key);
      return Natural (Vec.Length);
   end Direct_Orbits;

   function Indirect_Orbits
      (M   : String_Maps.Map;
       Key : String)
      return Natural
   is
      Vec   : String_Vectors.Vector;
      Total : Natural := 0;
   begin
      if not M.Contains (Key) then
         return 0;
      end if;

      Vec := String_Maps.Element (M, Key);
      for Cursor in Vec.Iterate loop
         declare
            B : String := String_Vectors.Element (Cursor);
         begin
            Total := Total + Direct_Orbits (M, B) + Indirect_Orbits (M, B);
         end;
      end loop;

      return Total;
   end Indirect_Orbits;

   function Read
      (Filename : String)
      return String_Maps.Map
   is
      Input    : File_Type;
      Star_Map : String_Maps.Map;
   begin
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         declare
            Line : constant String := Get_Line (Input);
            A : constant String := Split (Line, ')', 0);
            B : constant String := Split (Line, ')', 1);
            -- B orbits A
            Vec : String_Vectors.Vector;
         begin
            if not Star_Map.Contains (A) then
               Star_Map.Insert (A, String_Vectors.Empty_Vector);
            end if;
            Vec := Star_Map.Element (A);
            Vec.Append (B);
            Star_Map.Replace (A, Vec);
         end;
      end loop;
      Close (Input);
      return Star_Map;
   end Read;


   function Part_1 (Filename : String)
      return Natural
   is
      Star_Map : String_Maps.Map := Read (Filename);
      Total    : Natural := 0;
   begin
      for Cursor in Star_Map.Iterate loop
         declare
            Key : String := String_Maps.Key (Cursor);
         begin
            Total := Total + Direct_Orbits (Star_Map, Key) + Indirect_Orbits (Star_Map, Key);
         end;
      end loop;
      return Total;
   end Part_1;

   function Path
      (M            : String_Maps.Map;
       Origin, Dest : String)
       return String_Vectors.Vector
   is
      Vec : String_Vectors.Vector;
   begin
      if not M.Contains (Origin) then
         return String_Vectors.Empty_Vector;
      else
         Vec := M.Element (Origin);
      end if;

      if Vec.Contains (Dest) then
         return Vec;
      else
         for Cursor in Vec.Iterate loop
            declare
               use type Ada.Containers.Count_Type;
               Key : String := String_Vectors.Element (Cursor);
               V   : String_Vectors.Vector := Path (M, Key, Dest);
            begin
               if V.Length > 0 then
                  return Vec & V;
               end if;
            end;
         end loop;
         return String_Vectors.Empty_Vector;
      end if;
   end Path;

   procedure Print (V : String_Vectors.Vector) is
   begin
      for Cursor in V.Iterate loop
         Put (String_Vectors.Element (Cursor) & ",");
      end loop;
      Put_Line ("");
   end Print;

   function Part_2 (Filename : String)
      return Natural
   is
      use type Ada.Containers.Count_Type;
      Star_Map : String_Maps.Map := Read (Filename);
      From     : String_Vectors.Vector := Path (Star_Map, "COM", "YOU");
      To       : String_Vectors.Vector := Path (Star_Map, "COM", "SAN");
   begin
      --Put ("From: ");
      --Print (From);
      --Put ("To:   ");
      --Print (To);
      return Natural (abs (From.Length - To.Length));
   end Part_2;

   procedure Run is
      Result : Natural;
   begin
      Result := Part_1 ("input/advent6-example");
      if Result /= 42 then
         Put_Line ("6.1 test failed, expected 42, got" & Result'Image);
         return;
      end if;

      Result := Part_1 ("input/advent6");
      Put_Line ("Solution 6.1:" & Result'Image);

      Result := Part_2 ("input/advent6-example2");
      if Result /= 4 then
         Put_Line ("6.2 test failed, expected 4, got" & Result'Image);
         return;
      end if;

      Result := Part_2 ("input/advent6");
      Put_Line ("Solution 6.2:" & Result'Image);
   end Run;
end Solutions.Day_6;
