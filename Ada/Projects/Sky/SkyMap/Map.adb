-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *                                                                                                                   *
-- *    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General     *
-- *    Public License as published by the Free Software Foundation; either version 2 of the License, or               *
-- *    (at your option) any later version.                                                                            *
-- *                                                                                                                   *
-- *    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the     *
-- *    implied warranty of MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE. See the GNU General Public License    *
-- *    for more details.                                                                                              *
-- *                                                                                                                   *
-- *    You should have received a copy of the GNU General Public License along with this program; if not, write to    *
-- *    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Numerics.Generic_Elementary_Functions;
with Angle;
with Constellation;
with Earth;
with Error;
with Eps;
with Parameter;
with Solar;
with Star;
with Traces;
with User;

package body Map is

  package Log is new Traces ("Map");

  subtype Value is Angle.Degrees;

  use type Value;

  package Math is new Ada.Numerics.Generic_Elementary_Functions (Value);


  procedure Draw (Ut     : Time.Ut;
                  Format : Paper_Format) is

    use type Eps.Value;

    type Dimension_Table is array (Paper_Format) of Eps.Dimension;

    Dimensions : constant Dimension_Table := [
      A0 => (Width => 2384.0, Height => 3370.0),
      A1 => (Width => 1684.0, Height => 2384.0),
      A2 => (Width => 1191.0, Height => 1684.0),
      A3 => (Width =>  842.0, Height => 1191.0),
      A4 => (Width =>  595.0, Height =>  842.0),
      A5 => (Width =>  420.0, Height =>  595.0),
      A6 => (Width =>  298.0, Height =>  420.0)];

    Time_Image : constant String := Time.Image_Of (Ut);
    Title      : constant String := "Sternkarte";
    Date_Time  : constant String := "Datum: " & Time_Image;

    function Moon_Phase return String is ("Mond Phase:" & Solar.Moon_Phase'image & '%');

    Dimension    : constant Eps.Dimension := Dimensions(Format);
    Margin       : constant Eps.Value := Eps.Value(Natural(Dimension.Width / 24)) / 2;
    Size         : constant Eps.Value := Dimension.Width - 2 * Margin;
    Earth_Radius : constant Eps.Value := Size / 2;
    X_Offset     : constant Eps.Value := Margin + Earth_Radius;
    Y_Offset     : constant Eps.Value := Dimension.Height / 2;

    Horizon_Margin : constant Angle.Degrees := Angle.Degrees(Margin / Earth_Radius) * 90.0;

    Format_Factor : constant Float := Float(Dimension.Width) / Float(Dimensions(A4).Width);

    Radius_Factor : constant Eps.Value := Eps.Value(Float(Dimension.Width) / Float(Dimensions(A4).Width)) / 2;

    Header_Text_Size     : constant Eps.Text_Size := Eps.Text_Size (24.75 * Format_Factor);
    Footer_Text_Size     : constant Eps.Text_Size := Eps.Text_Size (15.0 * Format_Factor);
    Text_Offset          : constant Eps.Value := (Dimension.Height - Size) / 4;
    Header_Offset        : constant Eps.Value := Dimension.Height - Text_Offset - Eps.Value(Header_Text_Size) / 2;
    Footer_Offset_1      : constant Eps.Value := Text_Offset - Eps.Value(Footer_Text_Size) * 3 / 2;
    Footer_Offset_2      : constant Eps.Value := Text_Offset + Eps.Value(Footer_Text_Size) / 2;
    Top_Center_Text      : constant Eps.Location := (X => X_Offset, Y => Header_Offset);
    Bottom_Center_Text_1 : constant Eps.Location := (X => X_Offset, Y => Footer_Offset_1);
    Bottom_Center_Text_2 : constant Eps.Location := (X => X_Offset, Y => Footer_Offset_2);

    subtype Position is Value range -2.0 .. 2.0;

    function Scaled (Item : Position) return Eps.Value is (Eps.Value(Value(Item) * Value(Earth_Radius)));

    function X_Of (Item : Position) return Eps.Value is (X_Offset - Scaled(Item)); -- '-' => west positive

    function Y_Of (Item : Position) return Eps.Value is (Scaled(Item) + Y_Offset);


    Below_Horizon : exception;

    function Position_Of (Item : Star.Direction) return Eps.Location is
      Alt, Az, D : Value;
      X, Y       : Position;
      use type Angle.Signed;
      use type Angle.Value;
    begin
      if not Earth.Direction_Is_Known (Item) then
        raise Below_Horizon;
      end if;
      Alt := +Angle.Signed'(+Earth.Alt_Of (Item));
      Az := +Earth.Az_Of (Item);
      D := (90.0 - Alt) / 90.0;
      X := Math.Cos (90.0 - Az, Cycle => 360.0) * D;
      Y := Math.Sin (90.0 - Az, Cycle => 360.0) * D;
      return (X => X_Of (X), Y => Y_Of (Y));
    end Position_Of;


    function Size_Of (Magnitude  : Star.Magnitude) return Eps.Value is
      M    : constant Float := Float(Magnitude);
      Mmin : constant Float := Float(Parameter.Magnitude_Min);
      Mmax : constant Float := Float(Parameter.Magnitude_Max);
      Bmin : constant Float := Float(Parameter.Star_Min) * Format_Factor;
      Bmax : constant Float := Float(Parameter.Star_Max) * Format_Factor;
    begin
      if M <= Mmin then
        return Eps.Value(Bmax);
      elsif M >= Mmax then
        return Eps.Value(Bmin);
      end if;
      return Eps.Value(Bmin + ((Bmax - Bmin) / (Mmin - Mmax)) * (M - Mmax));
    end Size_Of;


    procedure Draw_Border is
      Center : constant Eps.Location := (X => X_Of(0.0), Y => Y_Of (0.0));
    begin
      Eps.Set_Color (Parameter.Earth_Color);
      Eps.Add_Circle (To        => Center,
                      Radius    => Earth_Radius * 2,
                      Is_Filled => True);
      if Solar.Is_Day_Light then
        Eps.Set_Color (Parameter.Day_Sky_Color);
      else
        Eps.Set_Color (Parameter.Night_Sky_Color);
      end if;
      Eps.Add_Circle (To        => Center,
                      Radius    => Earth_Radius,
                      Is_Filled => True);

      Eps.Set_Black;
      Eps.Define_Font ("Helvetica", Header_Text_Size);
      Eps.Add_Text (Item   => Title,
                    Center => Top_Center_Text);

      Eps.Set_Black;
      Eps.Define_Font ("Helvetica", Footer_Text_Size);
      Eps.Add_Text (Item   => Moon_Phase,
                    Center => Bottom_Center_Text_1);
      Eps.Add_Text (Item   => Date_Time,
                    Center => Bottom_Center_Text_2);

      Eps.Start_Circle (To     => Center,
                        Radius => Earth_Radius);
      Eps.Clip;
    end Draw_Border;


    procedure Draw_Constellations is
    begin
      Log.Write ("Draw Constellations");
      Eps.Set_Color (Parameter.Line_Color);
      Eps.Set_Line (Width => Parameter.Line_Size * Eps.Value(Format_Factor));
      Eps.Set_Line (Eps.Solid);
      for The_Constellation of Constellation.Visible loop
        for Line of Constellation.Visible_Lines_Of (The_Constellation)loop
          Eps.Add_Line (From => Position_Of (Line.From.Direction),
                        To   => Position_Of (Line.To.Direction));
        end loop;
      end loop;
    end Draw_Constellations;


    function Color_Class_Of (Class : Star.Spectral_Class) return Eps.Color_Class is
      use type Eps.Color_Class;
    begin
      return Eps.Color_Class'first + Eps.Color_Class (Star.Spectral_Class'pos(Class));
    end Color_Class_Of;


    procedure Draw_Stars is
      use type Star.Magnitude;
    begin
      Eps.Set_Line (Eps.Solid);
      for The_Star of Star.Data_List loop
        if The_Star.Mag <= Parameter.Magnitude_Max or else Constellation.Is_Used (The_Star.Id) then
          Eps.Add_Filled_Circle (To     => Position_Of (The_Star.Loc),
                                 Radius => Size_Of (The_Star.Mag) / 2.0,
                                 Class  => Color_Class_Of (The_Star.Class));
        end if;
      end loop;
    end Draw_Stars;


    procedure Draw_Planets is
    begin
      for The_Planet in Solar.Planet loop
        declare
          Direction : constant Earth.Direction := Solar.Direction_For (The_Planet);
        begin
          if not Earth.Is_Below_Horizon (Direction) then
            Log.Write ("Draw " & Solar.Image_Of (The_Planet));
            Eps.Set_Color (Parameter.Color_Of (The_Planet));
            Eps.Add_Circle (To        => Position_Of (Direction),
                            Radius    => Radius_Factor * Parameter.Size_Of (The_Planet),
                            Is_Filled => True);
          end if;
        end;
      end loop;
    end Draw_Planets;


    procedure Draw_Sun is
    begin
      if Solar.Is_Day_Light then
        Log.Write ("Draw Sun");
        Eps.Set_Color (Parameter.Sun_Color);
        Eps.Add_Circle (To        => Position_Of (Solar.Sun_Direction),
                        Radius    => Radius_Factor * Parameter.Sun_Size,
                        Is_Filled => True);
      end if;
    end Draw_Sun;


    procedure Draw_Moon is
      Direction : constant Earth.Direction := Solar.Moon_Direction;
    begin
      if not Earth.Is_Below_Horizon (Direction) then
        Log.Write ("Draw Moon");
        declare
          Center : constant Eps.Location := Position_Of (Direction);
          Phase  : constant Solar.Phase := Solar.Moon_Phase;
          B      : constant Eps.Value := Radius_Factor * Parameter.Moon_Size * 2;
          A      : constant Eps.Value := abs (B * Eps.Value(Math.Cos (Angle.Degrees(Solar.Moon_Phase * 3.6), 360.0)));
          Radius : constant Eps.Value := B / 2.0;
          use type Solar.Phase;
        begin
          if Solar.Is_Day_Light then
            Eps.Set_Color (Parameter.Day_Sky_Color);
          else
            Eps.Set_Color (Parameter.Dark_Moon_Color);
          end if;
          Eps.Add_Circle (To        => Center,
                          Radius    => Radius,
                          Is_Filled => True);
          Eps.Set_Color (Parameter.Bright_Moon_Color);
          if Phase = 0.0 or Phase = 100.0 then
            Log.Write ("new moon");
          elsif Phase < 25.0 then
            Log.Write ("moon increasing to half");
            Eps.Start_Elliptical_Arc (Center => Center,
                                      A      => A,
                                      B      => B,
                                      From   => 270.0,
                                      To     => 90.0);
            Eps.Continue_Arc_N (Center => Center,
                                Radius => Radius,
                                From   => 90.0,
                                To     => 270.0);
            Eps.Fill;
          elsif Phase = 25.0 then
            Log.Write ("half moon increasing");
            Eps.Add_Arc (Center    => Center,
                         Radius    => Radius,
                         From      => 270.0,
                         To        => 90.0,
                         Is_Filled => True);
          elsif Phase < 50.0 then
            Log.Write ("moon increasing to full");
            Eps.Start_Elliptical_Arc (Center => Center,
                                      A      => A,
                                      B      => B,
                                      From   => 90.0,
                                      To     => 270.0);
            Eps.Continue_Arc (Center => Center,
                              Radius => Radius,
                              From   => 270.0,
                              To     => 90.0);
            Eps.Fill;
          elsif Phase = 50.0 then
            Log.Write ("full moon");
            Eps.Add_Circle (To        => Center,
                            Radius    => Radius,
                            Is_Filled => True);
          elsif Phase < 75.0 then
            Log.Write ("moon decreasing to half");
            Eps.Start_Elliptical_Arc (Center => Center,
                                      A      => A,
                                      B      => B,
                                      From   => 270.0,
                                      To     => 90.0);
            Eps.Continue_Arc (Center => Center,
                              Radius => Radius,
                              From   => 90.0,
                              To     => 270.0);
            Eps.Fill;
          elsif Phase = 75.0 then
            Log.Write ("half moon decreasing");
            Eps.Add_Arc (Center    => Center,
                         Radius    => Radius,
                         From      => 90.0,
                         To        => 270.0,
                         Is_Filled => True);
          elsif Phase < 100.0 then
            Log.Write ("moon decreesing to new");
            Eps.Start_Elliptical_Arc (Center => Center,
                                      A      => A,
                                      B      => B,
                                      From   => 90.0,
                                      To     => 270.0);
            Eps.Continue_Arc_N (Center => Center,
                                Radius => Radius,
                                From   => 270.0,
                                To     => 90.0);
            Eps.Fill;
          else
            raise Program_Error;
          end if;
        end;
      end if;
    end Draw_Moon;


  begin -- Draw
    Log.Write ("Draw at " & Time_Image);
    if Size > Eps.Value'last - Margin then
      Error.Raise_With ("'Map Size' + 'Margin' must be smaller then" & Eps.Value'image(Eps.Value'last));
    end if;
    Star.Read (Ut);
    Solar.Prepare (Ut);
    Constellation.Prepare (Horizon_Margin);
    Eps.Create ("SkyMap.Eps", Dimension, Eps.Class_Colors(Parameter.Star_Colors));
    Draw_Border;
    Draw_Constellations;
    Draw_Stars;
    Draw_Planets;
    Draw_Sun;
    Draw_Moon;
    Eps.Close;
    User.Set_Status ("Karte generiert");
  exception
  when Error.Occurred =>
    raise;
  when Item: others =>
    Log.Termination (Item);
    Error.Raise_With ("Internal Error");
  end Draw;

end Map;
