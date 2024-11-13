package Debug is
   procedure Enable;

   procedure Log
      (Item     : String;
       New_Line : Boolean := True);

   procedure Log
      (Item : Integer;
       New_Line : Boolean := True);
end Debug;
