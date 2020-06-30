package Star_Map is
    type Map_Type is array (Positive, Positive) of Boolean;

    procedure Read_Map (Stream : in Ada.Streams.Root_Stream_Type'Class;
                        Map    : out Map_Type);
end Star_Map;
