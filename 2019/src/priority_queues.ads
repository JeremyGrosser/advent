with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Unchecked_Deallocation;

generic
   type Element_Type is private;
   type Priority_Type is private;
   with function "<" (Left, Right : Priority_Type) return Boolean is <>;
package Priority_Queues is
   type Queue is tagged private;

   function Is_Empty (Q : Queue) return Boolean;

   procedure Push
      (Q        : in out Queue;
       Item     : Element_Type;
       Priority : Priority_Type);

   procedure Pop
      (Q    : in out Queue;
       Item : out Element_Type)
   with Pre => not Q.Is_Empty;

private
   type Index_Type is new Positive;
   package Element_Vectors is new Ada.Containers.Vectors (Index_Type, Element_Type);
   type Element_Vector_Access is access Element_Vectors.Vector;
   procedure Free is new Ada.Unchecked_Deallocation (Element_Vectors.Vector, Element_Vector_Access);

   package Element_Priority_Maps is new Ada.Containers.Ordered_Maps (Priority_Type, Element_Vector_Access);

   type Queue is tagged record
      Map : Element_Priority_Maps.Map;
   end record;
end Priority_Queues;
