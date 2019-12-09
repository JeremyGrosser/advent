generic
    type Element_Type is private;
    Max_Items : Positive := 4;
package Stack is
    type Stack is tagged private;

    subtype Size_Type is Natural range 0 .. Max_Items;

    procedure Push (Container   : in out Stack;
                    Item        : in Element_Type);
    procedure Pop  (Container   : in out Stack;
                    Item        : out Element_Type);
    function  Top  (Container   : Stack) return Element_Type;
    function  Size (Container   : Stack) return Size_Type;
    function  Empty(Container   : Stack) return Boolean;
    Stack_Overflow, Stack_Underflow : exception;
private
    type Stack_Array is array (1 .. Max_Items) of Element_Type;
    subtype Stack_Pointer is Natural range Natural'First .. Max_Items;
    type Stack is tagged record
        Value : Stack_Array;
        Pointer : Stack_Pointer := Stack_Pointer'First;
    end record;
end Stack;
