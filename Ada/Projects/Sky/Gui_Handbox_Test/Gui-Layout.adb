package body Gui.Layout is

  function Group (Arrangement : Direction;
                  Gap         : Natural := 0;
                  Margin      : Natural := 0;
                  Weight      : Natural := 1;
                  Equal_Cells : Boolean := False) return Element is
  begin
    return new Node'(Is_Group => True, Arrangement => Arrangement,
                     Gap => Gap, Margin => Margin, Weight => Weight,
                     Equal_Cells => Equal_Cells, others => <>);
  end Group;

  function Control (Minimum : Size;
                    Weight  : Natural := 0) return Element is
  begin
    return new Node'(Required => Minimum, Weight => Weight, others => <>);
  end Control;

  procedure Append (Parent, Child : Element) is
    Ancestor : Element := Parent;
  begin
    if not Parent.Is_Group or else Child.Parent /= null then
      raise Program_Error;
    end if;
    while Ancestor /= null loop
      if Ancestor = Child then
        raise Program_Error;
      end if;
      Ancestor := Ancestor.Parent;
    end loop;
    Child.Parent := Parent;
    if Parent.Last = null then
      Parent.First := Child;
    else
      Parent.Last.Next := Child;
    end if;
    Parent.Last := Child;
  end Append;

  function Along (Item : Size; Arrangement : Direction) return Natural is
    (if Arrangement = Horizontal then Item.Width else Item.Height);

  function Across (Item : Size; Arrangement : Direction) return Natural is
    (if Arrangement = Horizontal then Item.Height else Item.Width);

  function Minimum (Item : Element) return Size is
    Child : Element := Item.First;
    Count, Total, Largest, Cross : Natural := 0;
    Required : Size;
  begin
    if not Item.Is_Group then
      return Item.Required;
    end if;
    while Child /= null loop
      Required := Minimum (Child);
      Count := Count + 1;
      Total := Total + Along (Required, Item.Arrangement);
      Largest := Natural'Max (Largest, Along (Required, Item.Arrangement));
      Cross := Natural'Max (Cross, Across (Required, Item.Arrangement));
      Child := Child.Next;
    end loop;
    if Item.Equal_Cells then
      Total := Count * Largest;
    end if;
    Total := Total + 2 * Item.Margin;
    Cross := Cross + 2 * Item.Margin;
    if Count > 0 then
      Total := Total + (Count - 1) * Item.Gap;
    end if;
    if Item.Arrangement = Horizontal then
      return (Total, Cross);
    else
      return (Cross, Total);
    end if;
  end Minimum;

  procedure Arrange (Item : Element; Area : Rectangle) is
    Required : constant Size := Minimum (Item);
    Child : Element := Item.First;
    Count, Total_Weight, Largest : Natural := 0;
    Extra, Share, Extent, Cross, Offset, Used_Weight, Distributed : Natural := 0;
    Available : Natural;
    Child_Area : Rectangle;
    Child_Size : Size;
  begin
    if Area.Width < Required.Width or else Area.Height < Required.Height then
      raise Constraint_Error with "layout below minimum size";
    end if;
    Item.Area := Area;
    if not Item.Is_Group then
      return;
    end if;
    Available := Along ((Area.Width, Area.Height), Item.Arrangement);
    Cross := Across ((Area.Width, Area.Height), Item.Arrangement) - 2 * Item.Margin;
    Extra := Available - Along (Required, Item.Arrangement);
    Offset := Item.Margin;
    while Child /= null loop
      Count := Count + 1;
      Total_Weight := Total_Weight + Child.Weight;
      Largest := Natural'Max (Largest, Along (Minimum (Child), Item.Arrangement));
      Child := Child.Next;
    end loop;
    if Item.Equal_Cells then
      Total_Weight := Count;
    end if;
    Child := Item.First;
    while Child /= null loop
      Child_Size := Minimum (Child);
      if Item.Equal_Cells then
        Extent := Largest;
        Used_Weight := Used_Weight + 1;
      else
        Extent := Along (Child_Size, Item.Arrangement);
        Used_Weight := Used_Weight + Child.Weight;
      end if;
      if Total_Weight > 0 then
        Share := Natural (Long_Long_Integer (Extra) * Long_Long_Integer (Used_Weight)
                          / Long_Long_Integer (Total_Weight));
        Extent := Extent + Share - Distributed;
        Distributed := Share;
      end if;
      if Item.Arrangement = Horizontal then
        Child_Area := (Area.X + Offset, Area.Y + Item.Margin, Extent, Cross);
      else
        Child_Area := (Area.X + Item.Margin, Area.Y + Offset, Cross, Extent);
      end if;
      Arrange (Child, Child_Area);
      Child := Child.Next;
      if Child /= null then
        Offset := Offset + Extent + Item.Gap;
      end if;
    end loop;
  end Arrange;

  function Bounds (Item : Element) return Rectangle is (Item.Area);

end Gui.Layout;
