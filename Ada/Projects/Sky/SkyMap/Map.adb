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
with Solar_System;
with Traces;
with User;

package body Map is

  package Log is new Traces ("Map");

  subtype Value is Angle.Degrees;

  use type Value;

  package Math is new Ada.Numerics.Generic_Elementary_Functions (Value);


  procedure Draw (Size          : Eps.Value;
                  Margin        : Eps.Value;
                  Star_Min      : Eps.Value;
                  Star_Max      : Eps.Value;
                  Magnitude_Min : Star.Magnitude;
                  Magnitude_Max : Star.Magnitude) is

    use type Eps.Value;

    Offset : constant Eps.Value := Size / 2.0;

    subtype Position is Value range -1.0 .. 1.0;

    function Scaled (Item : Position) return Eps.Value is (Eps.Value(Value(Item) * Value(Offset)));

    function X_Of (Item : Position) return Eps.Value is (Offset - Scaled(Item));

    function Y_Of (Item : Position) return Eps.Value is (Scaled(Item) + Offset);


    Below_Horizon : exception;

    function Position_Of (Item : Star.Direction) return Eps.Location is
      Alt, Az, D : Value;
      X, Y       : Position;
      use type Angle.Value;
    begin
      if not Earth.Direction_Is_Known (Item) then
        raise Below_Horizon;
      end if;
      Alt := +Earth.Alt_Of (Item);
      Log.Write ("Alt: " & Earth.Alt_Image_Of (Item));
      Az := +Earth.Az_Of (Item);
      Log.Write ("Az : " & Earth.Az_Image_Of (Item));
      D := (90.0 - Alt) / 90.0;
      X := Math.Cos (90.0 - Az, Cycle => 360.0) * D;
      Y := Math.Sin (90.0 - Az, Cycle => 360.0) * D;
      return (X => X_Of (X), Y => Y_Of (Y));
    end Position_Of;


    function Size_Of (Magnitude  : Star.Magnitude) return Eps.Value is
      M    : constant Float := Float(Magnitude);
      Mmin : constant Float := Float(Magnitude_Min);
      Mmax : constant Float := Float(Magnitude_Max);
      Bmin : constant Float := Float(Star_Min);
      Bmax : constant Float := Float(Star_Max);
    begin
      if M <= Mmin then
        return Star_Max;
      elsif M >= Mmax then
        return Star_Min;
      end if;
      return Eps.Value(Bmin + ((Bmax - Bmin) / (Mmin - Mmax)) * (M - Mmax));
    end Size_Of;


    procedure Draw_Border is
    begin
      Eps.Set_Color (Eps.Green);
      Eps.Set_Line (Eps.Solid);
      Eps.Set_Line (Width => 1.0);
      Eps.Add_Circle (To        => (X => X_Of(0.0), Y => Y_Of (0.0)),
                      Radius    => Offset,
                      Is_Filled => False);
    end Draw_Border;


    procedure Draw_Constellations is
      function Position_Of (Id : Star.Number) return Eps.Location is (Position_Of (Star.Location_Of (Id)));
    begin
      Eps.Set_Color (Eps.Magenta);
      Eps.Set_Line (Width => 1.0);
      Eps.Set_Line (Eps.Solid);
      Constellation.Start;
      while not Constellation.At_End loop
        declare
          Next_Constellation : constant Constellation.List := Constellation.Next;
          Is_Complete        : Boolean := True;
        begin
          for Part of Next_Constellation loop
            declare
              Unused : Eps.Location;
            begin
              Unused := Position_Of (Part.From);
              Unused := Position_Of (Part.To);
            exception
            when Below_Horizon =>
              Is_Complete := False;
              exit;
            end;
          end loop;
          if Is_Complete then
            for Part of Next_Constellation loop
              Eps.Add_Line (From => Position_Of (Part.From),
                            To   => Position_Of (Part.To));
            end loop;
          end if;
        end;
      end loop;
    end Draw_Constellations;


    procedure Draw_Stars is
      use type Star.Magnitude;
    begin
      Eps.Set_Color (Eps.Yellow);
      Eps.Set_Line (Eps.Solid);
      for The_Star of Star.Data_List loop
        if The_Star.Mag <= Magnitude_Max or else Constellation.Is_Used (The_Star.Id) then
          Eps.Add_Circle (To        => Position_Of (The_Star.Loc),
                          Radius    => Size_Of (The_Star.Mag) / 2.0,
                          Is_Filled => True);
        end if;
      end loop;
    end Draw_Stars;

    procedure Draw_Sun is
      Radius    : constant Eps.Value := Size / 120.0;
      Direction : constant Earth.Direction := Solar_System.Direction_Of (Solar_System.Sun, Star.Ut);
    begin
      if not Earth.Is_Below_Horizon (Direction) then
        Eps.Set_Color (Eps.Yellow);
        Eps.Set_Line (Eps.Solid);
        Eps.Add_Circle (To        => Position_Of (Direction),
                        Radius    => Radius,
                        Is_Filled => True);
      end if;
    end Draw_Sun;

  begin -- Draw
    if Size > Eps.Value'last - Margin then
      Error.Raise_With ("'Map Size' + 'Margin' must be smaller then" & Eps.Value'image(Eps.Value'last));
    end if;
    Eps.Create ("SkyMap.Eps",
                Lower_Left  => (X => Margin,   Y => Margin),
                Upper_Right => (X => Size + Margin, Y => Size + Margin));
    Draw_Constellations;
    Draw_Stars;
    Draw_Sun;
    Draw_Border;
    Eps.Close;
    User.Set_Status ("Map generated");
  exception
  when Error.Occurred =>
    raise;
  when Item: others =>
    Log.Termination (Item);
    Error.Raise_With ("Internal Error");
  end Draw;

end Map;
