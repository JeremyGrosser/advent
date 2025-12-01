package Advent
   with Pure
is
   NUL   : constant Character := Character'Val (16#00#);
   HT    : constant Character := Character'Val (16#09#);
   LF    : constant Character := Character'Val (16#0A#);
   CR    : constant Character := Character'Val (16#0D#);
   CRLF  : constant String := CR & LF;
end Advent;
