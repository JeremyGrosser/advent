package Advent.Output is

   type Buffer is private;

   procedure Put
      (This     : Buffer;
       Item     : Integer;
       New_Line : Boolean := True);

   procedure Put_Long
      (This     : Buffer;
       Item     : Long_Long_Integer;
       New_Line : Boolean := True);

   procedure Put
      (This     : Buffer;
       Item     : String;
       New_Line : Boolean := True);

   procedure Log
      (This     : Buffer;
       Item     : String;
       New_Line : Boolean := True);

   procedure Log
      (This     : Buffer;
       Item     : Integer;
       New_Line : Boolean := True);

   procedure Enable_Log
      (This : in out Buffer);

private

   type Buffer is record
      Log_Enabled : Boolean := False;
   end record;

end Advent.Output;
