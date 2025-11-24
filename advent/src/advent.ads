package Advent
   with Pure
is
   HT : constant Character := Character'Val (16#09#);
   LF : constant Character := Character'Val (16#0A#);
   CR : constant Character := Character'Val (16#0D#);
   CRLF : constant String := CR & LF;
end Advent;
