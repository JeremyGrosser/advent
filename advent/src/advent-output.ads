package Advent.Output is

   procedure Put
      (Item     : Integer;
       New_Line : Boolean := True);

   procedure Put_Long
      (Item     : Long_Long_Integer;
       New_Line : Boolean := True);

   procedure Put
      (Item     : String;
       New_Line : Boolean := True);

   procedure Log
      (Item     : String;
       New_Line : Boolean := True);

   procedure Log
      (Item : Integer;
       New_Line : Boolean := True);

   procedure Enable_Log;

end Advent.Output;
