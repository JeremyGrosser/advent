with Ada.Text_IO; use Ada.Text_IO;
with Priority_Queues;

package body Graphs is
   function Contains
      (This : Graph;
       Item : Element_Type)
      return Boolean is
   begin
      return This.Adjacent.Contains (Item);
   end Contains;

   function Neighbors
      (This : Graph;
       Item : Element_Type)
      return Element_Vector_Access
   is
   begin
      return This.Adjacent.Element (Item);
   end Neighbors;

   function Length
      (This : Graph)
      return Natural
   is
   begin
      return Natural (This.Adjacent.Length);
   end Length;
   
   procedure Add
      (This     : in out Graph;
       From, To : Element_Type)
   is
      Edges : Element_Vector_Access;
   begin
      if not This.Adjacent.Contains (From) then
         This.Adjacent.Insert (From, new Element_Vectors.Vector);
      end if;
      if not This.Adjacent.Contains (To) then
         This.Adjacent.Insert (To, new Element_Vectors.Vector);
      end if;
      Edges := This.Adjacent.Element (From);
      Edges.Append (To);
   end Add;

   function Path
      (This     : Graph;
       From, To : Element_Type)
      return Element_Vectors.Vector
   is
      type Cost is new Natural;
      package Element_Queues is new Priority_Queues (Element_Type, Cost);
      package Element_Maps is new Ada.Containers.Ordered_Maps
         (Key_Type     => Element_Type,
          Element_Type => Element_Type);
      package Cost_Maps is new Ada.Containers.Ordered_Maps
         (Key_Type     => Element_Type,
          Element_Type => Cost);
      Frontier : Element_Queues.Queue;
      Came_From : Element_Maps.Map;
      Cost_So_Far : Cost_Maps.Map;
      New_Cost : Cost;
      Current, Next : Element_Type;
      Result : Element_Vectors.Vector;
   begin
      Frontier.Push (From, Cost'First);
      Cost_So_Far.Insert (From, Cost'First);

      while not Frontier.Is_Empty loop
         Frontier.Pop (Current);
         exit when Current = To;
         for Cursor in This.Neighbors (Current).all.Iterate loop
            Next := Element_Vectors.Element (Cursor);
            New_Cost := Cost_So_Far.Element (Current) + 1;
            if not Cost_So_Far.Contains (Next) or else New_Cost < Cost_So_Far.Element (Next) then
               Cost_So_Far.Insert (Next, New_Cost);
               Frontier.Push (Next, New_Cost);
               Came_From.Insert (Next, Current);
            end if;
         end loop;
      end loop;

      Current := To;
      while Current /= From loop
         Result.Append (Current);
         Current := Came_From.Element (Current);
      end loop;
      --Result.Append (From);
      Result.Delete_First;
      Result.Reverse_Elements;

      return Result;
   end Path;

   procedure Clear
      (This : in out Graph)
   is
      Edges : Element_Vector_Access;
   begin
      for Cursor in This.Adjacent.Iterate loop
         Edges := Element_Vector_Maps.Element (Cursor);
         Free (Edges);
      end loop;
      This.Adjacent.Clear;
   end Clear;
end Graphs;
