package body Gui.Layout is

  function Group (Arrangement : Direction;
                  Gap         : Natural := 0;
                  Margin      : Natural := 0;
                  Weight      : Natural := 1;
                  Equal_Cells : Boolean := False;
                  Aspect_Ratio : Float := 0.0;
                  Minimum_Width : Natural := 0) return Element is
  begin
    if Aspect_Ratio < 0.0 then
      raise Constraint_Error;
    end if;
    return new Node'(Is_Group => True, Arrangement => Arrangement,
                     Gap => Gap, Margin => Margin, Weight => Weight,
                     Equal_Cells => Equal_Cells, Aspect_Ratio => Aspect_Ratio,
                     Required => (Minimum_Width, 0), others => <>);
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

    function Required_Size (Extent : Natural; Other : Natural; Width : Boolean) return Natural is
      Margin : constant Natural := 2 * Item.Margin;
      Interior : constant Float := Float (Other - Margin);
      Scaled : constant Float := (if Width then Interior * Item.Aspect_Ratio
                                 else Interior / Item.Aspect_Ratio);
    begin
      return Natural'max (Extent, Margin + Natural (Float'ceiling (Scaled)));
    end Required_Size;

  begin -- Minimum
    if not Item.Is_Group then
      return Item.Required;
    end if;
    while Child /= null loop
      Required := Minimum (Child);
      Count := Count + 1;
      Total := Total + Along (Required, Item.Arrangement);
      Largest := Natural'max (Largest, Along (Required, Item.Arrangement));
      Cross := Natural'max (Cross, Across (Required, Item.Arrangement));
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
      Required := (Total, Cross);
    else
      Required := (Cross, Total);
    end if;
    Required.Width := Natural'max (Required.Width, Item.Required.Width);
    if Item.Aspect_Ratio > 0.0 then
      Required.Width := Required_Size (Required.Width, Required.Height, Width => True);
      Required.Height := Required_Size (Required.Height, Required.Width, Width => False);
    end if;
    return Required;
  end Minimum;


  procedure Arrange (Item : Element; Area : Rectangle) is

    Required : constant Size := Minimum (Item);
    Child : Element := Item.First;
    Count, Total_Weight, Largest, Total : Natural := 0;
    Extra, Share, Extent, Cross, Offset, Used_Weight, Distributed : Natural := 0;
    Available : Natural;
    Child_Area : Rectangle;
    Child_Size : Size;
    Fit : Rectangle := Area;
    Reserved, Limit, Bonus : Natural := 0;

    function Fitted_Size (Group : Element; Other : Natural; Width : Boolean) return Natural is
      Margin : constant Natural := 2 * Group.Margin;
      Interior : constant Float := Float (Other - Margin);
      Scaled : constant Float := (if Width then Interior * Group.Aspect_Ratio
                                 else Interior / Group.Aspect_Ratio);
    begin
      return Margin + Natural (Float'floor (Scaled));
    end Fitted_Size;


    function Capacity (Child : Element; Cross_Size : Natural) return Natural is
      Base : constant Natural := Along (Minimum (Child), Item.Arrangement);
      Target : Natural;
      Member : Element;
      Result : Natural := 0;
    begin
      if Child.Aspect_Ratio > 0.0 then
        Target := Fitted_Size (Child, Cross_Size, Width => Item.Arrangement = Horizontal);
        return Natural'max (Base, Target) - Base;
      elsif Child.Is_Group and then Child.Weight = 0 and then
        Child.Arrangement = Item.Arrangement and then not Child.Equal_Cells
      then
        -- A fixed group may need extra space to fit its aspect-constrained children.
        Member := Child.First;
        while Member /= null loop
          Result := Result + Capacity (Member, Cross_Size - 2 * Child.Margin);
          Member := Member.Next;
        end loop;
      end if;
      return Result;
    end Capacity;

  begin -- Arrange
    if Area.Width < Required.Width or else Area.Height < Required.Height then
      raise Constraint_Error with "layout below minimum size";
    end if;
    if Item.Aspect_Ratio > 0.0 then
      if Float (Area.Width - 2 * Item.Margin) > Float (Area.Height - 2 * Item.Margin) * Item.Aspect_Ratio then
        Fit.Width := Natural'max (Required.Width, Fitted_Size (Item, Area.Height, Width => True));
        Fit.X := Area.X + (Area.Width - Fit.Width) / 2;
      else
        Fit.Height := Natural'max (Required.Height, Fitted_Size (Item, Area.Width, Width => False));
      end if;
    end if;
    Item.Area := Fit;
    if not Item.Is_Group then
      return;
    end if;
    Available := Along ((Fit.Width, Fit.Height), Item.Arrangement);
    Cross := Across ((Fit.Width, Fit.Height), Item.Arrangement) - 2 * Item.Margin;
    Offset := Item.Margin;
    while Child /= null loop
      Count := Count + 1;
      Child_Size := Minimum (Child);
      Total := Total + Along (Child_Size, Item.Arrangement);
      Largest := Natural'max (Largest, Along (Child_Size, Item.Arrangement));
      Child := Child.Next;
    end loop;
    if Item.Equal_Cells then
      Total := Count * Largest;
      Total_Weight := Count;
    end if;
    Total := Total + 2 * Item.Margin;
    if Count > 0 then
      Total := Total + (Count - 1) * Item.Gap;
    end if;
    Extra := Available - Total;
    if not Item.Equal_Cells then
      Child := Item.First;
      while Child /= null loop
        if Child.Aspect_Ratio > 0.0 or else Child.Weight = 0 then
          Reserved := Reserved + Natural'min (Extra - Reserved, Capacity (Child, Cross));
        else
          Total_Weight := Total_Weight + Child.Weight;
        end if;
        Child := Child.Next;
      end loop;
    end if;
    Extra := Extra - Reserved;
    Limit := Reserved;
    Child := Item.First;
    while Child /= null loop
      Child_Size := Minimum (Child);
      if Item.Equal_Cells then
        Extent := Largest;
        Used_Weight := Used_Weight + 1;
      else
        Extent := Along (Child_Size, Item.Arrangement);
        if Child.Aspect_Ratio > 0.0 or else Child.Weight = 0 then
          Bonus := Natural'min (Limit, Capacity (Child, Cross));
          Limit := Limit - Bonus;
          Extent := Extent + Bonus;
        else
          Used_Weight := Used_Weight + Child.Weight;
        end if;
      end if;
      if Total_Weight > 0 then
        Share := Natural (Long_Long_Integer (Extra) * Long_Long_Integer (Used_Weight)
                          / Long_Long_Integer (Total_Weight));
        Extent := Extent + Share - Distributed;
        Distributed := Share;
      end if;
      if Item.Arrangement = Horizontal then
        Child_Area := (Fit.X + Offset, Fit.Y + Item.Margin, Extent, Cross);
      else
        Child_Area := (Fit.X + Item.Margin, Fit.Y + Offset, Cross, Extent);
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
