package body Queues is
   procedure Push (Q : in out Queue; Item : Element_Type) is
   begin
      Q.Vec.Append (Item);
   end Push;

   procedure Pop (Q : in out Queue; Item : out Element_Type) is
   begin
      Item := Q.Vec.First_Element;
      Q.Vec.Delete_First;
   end Pop;

   function Is_Empty (Q : Queue) return Boolean is
   begin
      return Q.Vec.Is_Empty;
   end Is_Empty;
end Queues;
