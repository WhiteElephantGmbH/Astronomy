private package Gui.Layout is

  type Direction is (Horizontal, Vertical);

  type Size is record
    Width  : Natural := 0;
    Height : Natural := 0;
  end record;

  type Rectangle is record
    X, Y          : Integer := 0;
    Width, Height : Natural := 0;
  end record;

  type Node is private;
  type Element is access Node;

  function Group (Arrangement : Direction;
                  Gap         : Natural := 0;
                  Margin      : Natural := 0;
                  Weight      : Natural := 1;
                  Equal_Cells : Boolean := False) return Element;

  function Control (Minimum : Size;
                    Weight  : Natural := 0) return Element;

  procedure Append (Parent, Child : Element);
  function Minimum (Item : Element) return Size;
  procedure Arrange (Item : Element; Area : Rectangle);
  function Bounds (Item : Element) return Rectangle;

private

  type Node is record
    Is_Group    : Boolean := False;
    Arrangement : Direction := Vertical;
    Gap         : Natural := 0;
    Margin      : Natural := 0;
    Weight      : Natural := 0;
    Equal_Cells : Boolean := False;
    Required    : Size;
    Area        : Rectangle;
    Parent      : Element;
    First       : Element;
    Last        : Element;
    Next        : Element;
  end record;

end Gui.Layout;
