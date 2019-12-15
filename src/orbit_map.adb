with Ada.Containers;

package body Orbit_Map is
    procedure Add (
        A : in Orbital;
        B : in Orbital) is
        V1, V2 : Orbital_Vector.Vector;
    begin
        if not Bodies.Contains (B) then
            Bodies.Insert (B, V1);
        end if;
        if not Bodies.Contains (A) then
            Bodies.Insert (A, V2);
        end if;
        Bodies (A).Append (B);
    end Add;

    function Count (A : in Orbital) return Natural is
        Sum : Natural := 0;
    begin
        if not Bodies.Contains (A) then
            return 0;
        end if;

        for O of Bodies (A) loop
            Sum := Sum + Count (A) + 1;
        end loop;
        return Sum;
    end Count;

    function "=" (Left, Right : Orbital_Vector.Vector) return Boolean is
        use Ada.Containers;
    begin
        if Left.Length /= Right.Length then
            return False;
        end if;
        for X of Left loop
            for Y of Right loop
                if X /= Y then
                    return False;
                end if;
            end loop;
        end loop;
        return True;
    end "=";
end Orbit_Map;
