with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

generic
   type Element_Type is private;
   with function "<" (Left, Right : Element_Type) return Boolean is <>;
package Graphs is
   type Graph is tagged private;
   type Index_Type is new Positive;

   package Element_Vectors is new Ada.Containers.Vectors
      (Index_Type, Element_Type);
   type Element_Vector_Access is access Element_Vectors.Vector;

   function Contains
      (This : Graph;
       Item : Element_Type)
      return Boolean;

   function Neighbors
      (This : Graph;
       Item : Element_Type)
      return Element_Vector_Access
      with Pre => This.Contains (Item);

   function Length
      (This : Graph)
      return Natural;
   
   procedure Add
      (This     : in out Graph;
       From, To : Element_Type)
      with Post => This.Contains (From) and
                   This.Contains (To) and
                   This.Neighbors (From).Contains (To);

   procedure Clear
      (This : in out Graph);

   function Path
      (This     : Graph;
       From, To : Element_Type)
      return Element_Vectors.Vector
      with Pre  => This.Contains (From) and This.Contains (To);

private
   procedure Free is new Ada.Unchecked_Deallocation
      (Element_Vectors.Vector, Element_Vector_Access);

   package Element_Vector_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type     => Element_Type,
       Element_Type => Element_Vector_Access);

   type Graph is tagged record
      Adjacent : Element_Vector_Maps.Map;
   end record;

end Graphs;
