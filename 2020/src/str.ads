package Str is
   pragma Pure;

   function Find
      (S : String;
       C : Character)
       return Natural;

   function Split
      (S         : String;
       Delimiter : Character;
       Skip      : Natural)
       return String;

   function Strip
      (S : String;
       C : Character)
       return String;
end Str;
