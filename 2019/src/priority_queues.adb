package body Priority_Queues is
   function Is_Empty (Q : Queue)
      return Boolean
   is
   begin
      return Q.Map.Is_Empty;
   end Is_Empty;

   procedure Push
      (Q        : in out Queue;
       Item     : Element_Type;
       Priority : Priority_Type)
   is
      Vec : Element_Vector_Access;
   begin
      if not Q.Map.Contains (Priority) then
         Q.Map.Include (Priority, new Element_Vectors.Vector);
      end if;
      Vec := Q.Map.Element (Priority);
      Vec.Append (Item);
   end Push;

   procedure Pop
      (Q    : in out Queue;
       Item : out Element_Type)
   is
      Vec : Element_Vector_Access := Q.Map.First_Element;
   begin
      Item := Vec.First_Element;
      Vec.Delete_First;
      if Vec.Is_Empty then
         Q.Map.Delete_First;
         Free (Vec);
      end if;
   end Pop;
end Priority_Queues;
