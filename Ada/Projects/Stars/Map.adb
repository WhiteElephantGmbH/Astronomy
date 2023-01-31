-- *********************************************************************************************************************
-- *                       (c) 2010 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
-->Style: White_Elephant

with Exceptions;
with Ada.Numerics.Generic_Elementary_Functions;
with Constellation;

package body Map is

  Ecliptic_Pole : constant Coordinate.Polar := Coordinate.Location_Of (Dec => Coordinate.Dec_Of ("66°33'43"""),
                                                                       Ra  => Coordinate.Ra_Of ("18h00m00.0s"));
  The_Size : Long_Float;

  The_Constellation_Limit : Coordinate.Declination;
  The_Visibility_Limit    : Coordinate.Declination;

  use type Eps.Value;
  use type Coordinate.Declination;


  function Size return Long_Float is
  begin
    return The_Size;
  end Size;


  function Offset return Eps.Value is
  begin
    return Eps.Value(The_Size / 2.0);
  end Offset;


  function Scaled (Item : Coordinate.Position) return Eps.Value is
  begin
    return Eps.Value(Long_Float(Item) * Size * 90.0 / (abs(Long_Float(The_Visibility_Limit)) + 90.0));
  end Scaled;


  function X_Value_Of (Item : Coordinate.Position) return Eps.Value is
  begin
    return Offset - Scaled(Item);
  end X_Value_Of;


  function Y_Value_Of (Item : Coordinate.Position) return Eps.Value is
  begin
    return Scaled(Item) + Offset;
  end Y_Value_Of;


  function Location_Of (Point : Coordinate.Cartesian) return Eps.Location is
  begin
    return (X => X_Value_Of (Coordinate.X_Of (Point)),
            Y => Y_Value_Of (Coordinate.Y_Of (Point)));
  end Location_Of;


  function Size_Of (Magnitude  : Star.Magnitude;
                    Brightness : Star_Brightness) return Eps.Value is
    M    : constant Float := Float(Magnitude);
    Mmin : constant Float := Float(Brightness.Limit);
    Mmax : constant Float := Float(Brightness.Filter);
    Bmin : constant Float := Float(Brightness.Min);
    Bmax : constant Float := Float(Brightness.Max);
  begin
    if M <= Mmin then
      return Brightness.Max;
    elsif M >= Mmax then
      return Brightness.Min;
    end if;
    return Eps.Value(Bmin + ((Bmax - Bmin) / (Mmin - Mmax)) * (M - Mmax));
  end Size_Of;


  procedure Draw (Declination              : Coordinate.Declination;
                  Margin                   : Eps.Value;
                  Map_Size                 : Eps.Value;
                  Constellation_Line_Width : Eps.Value;
                  Ecliptic_Line_Width      : Eps.Value;
                  Equator_Line_Width       : Eps.Value;
                  Visibility_Line_Width    : Eps.Value;
                  Brightness               : Star_Brightness) is

    procedure Draw_Center is
      Cross_Size : constant Eps.Value := Brightness.Max;
    begin
      Eps.Set_Color (Eps.Red);
      Eps.Set_Line (Eps.Solid);
      Eps.Set_Line (Width => 0.1);
      Eps.Add_Line (From => (X => Offset - Cross_Size, Y => Offset),
                    To   => (X => Offset + Cross_Size, Y => Offset));
      Eps.Add_Line (From => (X => Offset, Y => Offset - Cross_Size),
                    To   => (X => Offset, Y => Offset + Cross_Size));
    end Draw_Center;


    procedure Draw_Border is
    begin
      Eps.Set_Color (Eps.Red);
      Eps.Set_Line (Eps.Solid);
      Eps.Set_Line (Width => 0.1);
      Eps.Add_Circle (To        => (X => X_Value_Of(0.0), Y => Y_Value_Of (0.0)),
                      Radius    => Offset,
                      Is_Filled => False);
    end Draw_Border;


    procedure Draw_Visible_Area (Location : Coordinate.Polar;
                                 Fill     : Boolean := False) is

      package Math is new Ada.Numerics.Generic_Elementary_Functions (Coordinate.Value);

      Location_Ra  : constant Coordinate.Right_Ascension := Coordinate.Ra_Of (Location);
      Location_Dec : constant Coordinate.Declination     := Coordinate.Dec_Of (Location);

      use type Coordinate.Right_Ascension;

      function Dec_Of (Angle : Coordinate.Right_Ascension) return Coordinate.Declination is

        use type Coordinate.Value;

        function Sqr  (X : Coordinate.Value) return Coordinate.Value is
        begin
          return X * X;
        end Sqr;

      begin -- Dec_Of
        if Coordinate.Orientation * Location_Dec = 90.0 then
          return 0.0;
        else
          declare
            B : constant Coordinate.Value := Coordinate.Radian_Of (Coordinate.Orientation * Location_Dec);
            G : constant Coordinate.Value := Coordinate.Radian_Of (Angle);
            D : constant Coordinate.Declination
              := Coordinate.Arc_Of (Math.Arcsin (Math.Sqrt(1.0 / (1.0 + (Sqr(Math.Tan(B)) / Sqr(Math.Cos(G)))))));
          begin
            if abs((Angle) - 12.0) > 6.0 then
              return -D;
            end if;
            return D;
          end;
        end if;
      end Dec_Of;

      The_Ra  : Coordinate.Right_Ascension := 0.0;
      The_Dec : Coordinate.Declination;

      function Point return Eps.Location is
      begin
        The_Dec := Coordinate.Orientation * Dec_Of (The_Ra);
        if Coordinate.Is_Outside (Location_Dec) then
          The_Dec := - The_Dec;
        end if;
        return Location_Of (Coordinate.Point_Of (Dec => The_Dec, Ra => Location_Ra - The_Ra));
      end Point;

    begin -- Draw_Visible_Area
      Eps.Start_Line (From => Point);
      loop
        The_Ra := The_Ra + 0.01;
        Eps.Continue_Line (To => Point);
        exit when The_Ra >= 24.0;
      end loop;
      if Fill then
        Eps.Save;
        if Coordinate.Is_Outside (Declination) then
           Eps.Set_Gray;
        else
           Eps.Set_White;
        end if;
        Eps.Fill;
        Eps.Restore;
      end if;
      Eps.Draw_Line;
    end Draw_Visible_Area;


    procedure Draw_Star_Map (The_Brightness : Star_Brightness) is

      procedure Draw_Constellations is

        Filename : constant String := "Constellations.csv";

        procedure Draw (From, To: Natural) is
          From_Point : constant Eps.Location := Location_Of (Star.Point_Of (From));
          To_Point   : constant Eps.Location := Location_Of (Star.Point_Of (To));
        begin
          Eps.Add_Line (From => From_Point,
                        To   => To_Point);
        end Draw;

        function Is_Visible (Item : Natural) return Boolean is
        begin
          return Coordinate.Orientation * Star.Dec_Of (Item) > The_Constellation_Limit;
        end Is_Visible;

        procedure Draw_Line (Part : Constellation.Part) is
        begin
          if Is_Visible (Part.From) and then Is_Visible (Part.To) then
            Draw (Part.From, Part.To);
          end if;
        end Draw_Line;

        procedure Draw_Lines is new Constellation.Itterate (Draw_Line);

      begin -- Draw_Constellations
        Eps.Set_Color (Eps.Green);
        Eps.Set_Line (Width => Constellation_Line_Width);
        Eps.Set_Line (Eps.Solid);
        if Constellation.Read (Filename) then
          Draw_Lines;
        else
          Error (Filename & " not found");
        end if;
      end Draw_Constellations;

      use type Star.Magnitude;

    begin -- Draw_Star_Map
      if Map_Size > Eps.Value'last - Margin then
        Error ("'Map Size' + 'Margin' must be smaller then" & Eps.Value'image(Eps.Value'last));
        raise Aborted;
      end if;
      Eps.Create ("Star_Map.Eps",
                  Lower_Left  => (X => Margin,   Y => Margin),
                  Upper_Right => (X => Map_Size, Y => Map_Size));
      Draw_Border;

      Eps.Set_Color (Eps.Magenta);
      Eps.Set_Line (Width => Equator_Line_Width);
      Eps.Set_Line (Eps.Dashed);
      Eps.Add_Circle (To        => (X => X_Value_Of(0.0), Y => Y_Value_Of (0.0)),
                      Radius    => Scaled (0.5),
                      Is_Filled => False);

      Eps.Set_Color (Eps.Light_Blue);
      Eps.Set_Line (Width => Ecliptic_Line_Width);
      Eps.Set_Line (Eps.Dashed);
      Draw_Visible_Area (Ecliptic_Pole);
      Star.Read;

      Draw_Constellations;

      Eps.Set_Color (Eps.Black);
      while Star.Next loop
        if Coordinate.Orientation * Coordinate.Dec_Of (Star.Location) > The_Visibility_Limit then
          if Star.Mag <= The_Brightness.Filter or else Constellation.Is_Used (Star.HR_Id) then
            Eps.Add_Circle (To        => Location_Of (Star.Point),
                            Radius    => Size_Of (Star.Mag, The_Brightness) / 2.0,
                            Is_Filled => True);
          end if;
        end if;
      end loop;

      Draw_Center;
      Eps.Close;
    end Draw_Star_Map;


    procedure Draw_Visibility (The_Declination : Coordinate.Declination) is
    begin
      Eps.Create ("Star_View.Eps",
                  Lower_Left  => (X => Margin,   Y => Margin),
                  Upper_Right => (X => Map_Size, Y => Map_Size));
      if not Coordinate.Is_Outside (The_Declination) then
        Eps.Set_Gray;
        Eps.Add_Circle (To        => (X => X_Value_Of(0.0), Y => Y_Value_Of (0.0)),
                        Radius    => Offset,
                        Is_Filled => True);
      end if;
      Draw_Border;
      Eps.Set_Color (Eps.Black);
      Eps.Set_Line (Width => Visibility_Line_Width);
      Eps.Set_Line (Eps.Solid);
      Draw_Visible_Area (Coordinate.Location_Of (Dec => The_Declination, Ra => 0.0),
                         Fill => True);
      Draw_Center;
      Eps.Close;
    end Draw_Visibility;

  begin -- Draw
    The_Size := Long_Float(Map_Size);
    if Coordinate.Is_Outside (Declination) then
      The_Visibility_Limit := -90.0;
    else
      The_Visibility_Limit := Coordinate.Orientation * Declination - 90.0;
    end if;
    The_Constellation_Limit := The_Visibility_Limit;
    if The_Constellation_Limit < -80.0 then
      The_Constellation_Limit := -80.0;
    end if;
    Draw_Star_Map (Brightness);
    Draw_Visibility (Declination);
  exception
  when Aborted =>
    raise;
  when Item: others =>
    Error ("Exception " & Exceptions.Information_Of (Item));
  end Draw;

end Map;