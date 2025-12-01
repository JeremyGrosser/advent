pragma Extensions_Allowed (On);
with Ada.Containers.Bounded_Vectors;
with Advent.Input;
with Advent.Output;

procedure Day7_2
   (Input  : in out Advent.Input.Buffer;
    Output : Advent.Output.Buffer)
is
   Max_Operands : constant := 16;
   subtype Int is Long_Long_Integer;
   Sum : Int := 0;

   package Int_Vectors is new Ada.Containers.Bounded_Vectors (Positive, Int);
   use Int_Vectors;

   type Operation is (Add, Mul, Concat);
   package Operation_Vectors is new Ada.Containers.Bounded_Vectors (Positive, Operation);
   use Operation_Vectors;

   procedure Generate
      (Test_Value : Int;
       Operands : Int_Vectors.Vector)
   is
      function Solve
         (Operations : Operation_Vectors.Vector)
         return Boolean
      is
         Result : Int := Operands (1);
      begin
         for I in 2 .. Last_Index (Operands) loop
            case Element (Operations, I - 1) is
               when Add =>
                  Result := Result + Operands (I);
               when Mul =>
                  Result := Result * Operands (I);
               when Concat =>
                  if Operands (I) in Int (0) .. Int (9) then
                     Result := Result * 10 + Operands (I);
                  elsif Operands (I) in Int (10) .. Int (99) then
                     Result := Result * 100 + Operands (I);
                  elsif Operands (I) in Int (100) .. Int (999) then
                     Result := Result * 1000 + Operands (I);
                  else
                     raise Program_Error with "we only expect 3 digit operands to concat";
                  end if;
            end case;
         end loop;

         return Result = Test_Value;
      end Solve;

      function Inner
         (Current   : in out Operation_Vectors.Vector;
          Remaining : Natural)
          return Boolean
      is
      begin
         if Remaining = 0 then
            return Solve (Current);
         else
            for Op in Operation'Range loop
               Append (Current, Op);
               if Inner (Current, Remaining - 1) then
                  return True;
               end if;
               Delete_Last (Current);
            end loop;
            return False;
         end if;
      end Inner;

      N : constant Natural := Natural (Length (Operands)) - 1;
      X : Operation_Vectors.Vector (Capacity => Max_Operands);
   begin
      if Inner (X, N) then
         Sum := Sum + Test_Value;
      end if;
   end Generate;

   Test_Value : Int;
   Operands : Int_Vectors.Vector (Capacity => Max_Operands);
   N : Int;
begin
   while not Input.End_Of_Input loop
      case Input.Peek is
         when '0' .. '9' =>
            Input.Get_Long (N);
            Append (Operands, N);
         when ASCII.LF =>
            Input.Seek (1);
            Test_Value := First_Element (Operands);
            Delete_First (Operands);
            Generate (Test_Value, Operands);
            Clear (Operands);
         when ' ' | ':' =>
            Input.Seek (1);
         when others =>
            raise Program_Error with "Unexpected character in input: '" & Input.Peek & "'";
      end case;
   end loop;
   Output.Put_Long (Sum);
end Day7_2;
