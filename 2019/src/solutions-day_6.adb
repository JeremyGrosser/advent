with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Str; use Str;
with Graphs;

package body Solutions.Day_6 is
   Not_Found : exception;

   type Index_Type is new Positive;
   package String_Vectors is new Ada.Containers.Indefinite_Vectors
      (Index_Type   => Index_Type,
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

   procedure Print (V : String_Vectors.Vector) is
   begin
      for Cursor in V.Iterate loop
         Put (String_Vectors.Element (Cursor) & ",");
      end loop;
      Put_Line ("");
   end Print;

   function Part_2
      (Filename : String)
      return Natural
   is
      package Index_Graph is new Graphs (Index_Type);
      Input  : File_Type;
      Names  : String_Vectors.Vector;
      G      : Index_Graph.Graph;
      Result : Index_Graph.Element_Vectors.Vector;
   begin
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         declare
            Line : constant String := Get_Line (Input);
            A    : constant String := Split (Line, ')', 0);
            B    : constant String := Split (Line, ')', 1);
            AI, BI : Index_Type;
         begin
            if not Names.Contains (A) then
               Names.Append (A);
            end if;
            AI := Names.Find_Index (A);

            if not Names.Contains (B) then
               Names.Append (B);
            end if;
            BI := Names.Find_Index (B);

            G.Add (AI, BI);
            G.Add (BI, AI);
         end;
      end loop;
      Close (Input);

      Result := G.Path (Names.Find_Index ("YOU"), Names.Find_Index ("SAN"));
      --for Cursor in Result.Iterate loop
      --   Put (Names (Index_Graph.Element_Vectors.Element (Cursor)) & " ");
      --end loop;
      --Put_Line ("");
      G.Clear;
      return Natural (Result.Length) - 1;
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
