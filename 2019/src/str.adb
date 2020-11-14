package body Str is
   function Find
      (S : String;
       C : Character)
       return Natural
   is
   begin
      for I in S'Range loop
         if S (I) = C then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   function Split
      (S         : String;
       Delimiter : Character;
       Skip      : Natural)
       return String
   is
      First : Natural := S'First;
      Last  : Natural := S'Last;
   begin
      if Skip > 0 then
         for I in 1 .. Skip loop
            First := Find (S (First .. Last), Delimiter) + 1;
         end loop;
      end if;
      Last := Find (S (First .. Last), Delimiter);
      if Last = 0 then
         Last := S'Last;
      else
         Last := Last - 1;
      end if;
      return S (First .. Last);
   end Split;

   function Strip
      (S : String;
       C : Character)
       return String
   is
   begin
      if S (S'First) = C then
         return Strip (S (S'First + 1 .. S'Last), C);
      elsif S (S'Last) = C then
         return Strip (S (S'First .. S'Last - 1), C);
      else
         return S;
      end if;
   end Strip;
end Str;
