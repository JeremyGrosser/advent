with Ada.Containers.Vectors;

generic
   type Element_Type is private;
package Queues is
   type Queue is tagged private;

   function Is_Empty (Q : Queue) return Boolean;

   procedure Push
      (Q    : in out Queue;
       Item : Element_Type);

   procedure Pop
      (Q    : in out Queue;
       Item : out Element_Type)
   with Pre => not Q.Is_Empty;

private
   type Index_Type is new Positive;
   package Element_Vectors is new Ada.Containers.Vectors (Index_Type, Element_Type);

   type Queue is tagged record
      Vec : Element_Vectors.Vector;
   end record;
end Queues;
