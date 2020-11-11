package body Stack is
    procedure Push (Container : in out Stack;
                    Item      : in Element_Type) is
    begin
        Container.Pointer := Container.Pointer + 1;
        Container.Value (Container.Pointer) := Item;
    exception
        when Constraint_Error =>
            raise Stack_Overflow;
    end Push;

    procedure Pop (Container : in out Stack;
                   Item      : out Element_Type) is
    begin
        Item := Top (Container);
        Container.Pointer := Container.Pointer - 1;
    end Pop;

    function Top (Container : Stack) return Element_Type is
    begin
        return Container.Value (Container.Pointer);
    exception
        when Constraint_Error =>
            raise Stack_Underflow;
    end Top;

    function Size (Container : Stack) return Size_Type is
    begin
        return Container.Pointer;
    end Size;

    function Empty (Container : Stack) return Boolean is
    begin
        return Size (Container) = 0;
    end Empty;
end Stack;
