pragma Ada_2022;
pragma Extensions_Allowed (On);
with Ada.Containers.Vectors;
with Advent.Input;
with Advent.Output;

procedure Day7_1
   (Input  : in out Advent.Input.Buffer;
    Output : Advent.Output.Buffer)
is
   subtype Int is Long_Long_Integer;
   Sum : Int := 0;

   package Int_Vectors is new Ada.Containers.Vectors (Positive, Int);
   use Int_Vectors;

   type Operation is (Add, Mul);
   package Operation_Vectors is new Ada.Containers.Vectors (Positive, Operation);
   use Operation_Vectors;

   procedure Generate
      (Test_Value : Int;
       Operands : Int_Vectors.Vector)
   is
      N : constant Natural := Natural (Length (Operands)) - 1;

      function Solve
         (Operations : Operation_Vectors.Vector)
         return Boolean
      is
         Result : Int := First_Element (Operands);
         I : Positive := 2;
         J : Positive := 1;
      begin
         --  Output.Log (Test_Value'Image);
         --  Output.Log (Operands'Image);
         --  Output.Log (Operations'Image);

         while I <= Last_Index (Operands) loop
            case Operation_Vectors.Element (Operations, J) is
               when Add =>
                  Result := Result + Operands (I);
               when Mul =>
                  Result := Result * Operands (I);
            end case;
            I := I + 1;
            J := J + 1;
         end loop;

         return Result = Test_Value;
      end Solve;

      function Inner
         (Current   : Operation_Vectors.Vector;
          Remaining : Natural)
          return Boolean
      is
         X : Operation_Vectors.Vector := Operation_Vectors.Copy (Current);
      begin
         if Remaining = 0 then
            return Solve (Current);
         else
            for Op in Operation'Range loop
               Append (X, Op);
               if Inner (X, Remaining - 1) then
                  return True;
               end if;
               Delete_Last (X);
            end loop;
            return False;
         end if;
      end Inner;
   begin
      if Inner ([], N) then
         Output.Log (Test_Value'Image);
         Sum := Sum + Test_Value;
      end if;
   end Generate;

   Test_Value : Int;
   Operands : Int_Vectors.Vector;
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
end Day7_1;
