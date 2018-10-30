-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Astro;
with Matrix;
with Parameter;
with Refraction;

package body Numerics is

  function Interpolation_Of (Az, Az1, Az2, Alt1, Alt2 : Angle.Value) return Angle.Value is

    use type Angle.Degrees;

    function Normalized (A : Angle.Degrees) return Angle.Degrees is
    begin
      if A < 0.0 then
        return A + 360.0;
      else
        return A;
      end if;
    end Normalized;

    use type Angle.Value;

    Daz  : constant Angle.Degrees := Normalized (Az2 - Az1);
    Dalt : constant Angle.Degrees := Alt2 - Alt1;

  begin -- Interpolation_Of
    if Daz = 0.0 then
      return Alt1;
    else
      return Alt1 + Dalt * Normalized (Az - Az1) / Daz;
    end if;
  end Interpolation_Of;


  function Interpolation_Of (T, T1, T2 : Time.Ut;
                                V1, V2 : Angle.Value) return Angle.Value is
    use type Angle.Degrees;
    use type Angle.Value;
    DT : constant Angle.Degrees := Angle.Degrees(T2 - T1);
    DV : constant Angle.Degrees := V2 - V1;
  begin
    if DT = 0.0 then
      return V1;
    else
      return V1 + (Angle.Degrees(T - T1) * DV / DT);
    end if;
  end Interpolation_Of;


  function Interpolation_Of (T, T1, T2 : Time.Ut;
                                V1, V2 : Angle.Degrees) return Angle.Degrees is
    use type Angle.Value;
  begin
    return +(Interpolation_Of (T, T1, T2, +V1, +V2));
  end Interpolation_Of;


  -------------------------------------------------
  -- Convert eqatorial to horizontal coordinates --
  -------------------------------------------------

  procedure EQUHOR (DEC :     Angle.Value; -- declination
                    TAU :     Angle.Value; -- hours angle
                    PHI :     Angle.Value; -- geographic latitude
                    A   : out Angle.Value; -- altitude
                    AZ  : out Angle.Value) -- azimuth
  is
    The_Altitude : Angle.Degrees;
    The_Azimuth  : Angle.Degrees;
    use type Angle.Value;
  begin
    Astro.SPHLIB.EQUHOR (DEC => +DEC,
                         TAU => +TAU,
                         PHI => +PHI,
                         H   => The_Altitude,
                         AZ  => The_Azimuth);
    A := +The_Altitude;
    AZ := Angle.Semi_Circle + The_Azimuth;
  end EQUHOR;


  function Direction_Of (Direction : Space.Direction;
                         Lmst      : Time.Value) return Earth.Direction is
    The_Azimuth  : Angle.Value;
    The_Altitude : Angle.Value;
    use type Angle.Value;
  begin
    if Space.Direction_Is_Known (Direction) then
      EQUHOR (DEC => Space.Dec_Of (Direction),
              TAU => Lmst - Space.Ra_Of (Direction),
              PHI => Parameter.Latitude,
              A   => The_Altitude,
              AZ  => The_Azimuth);
      return Earth.Direction_Of (Alt => The_Altitude,
                                 Az  => The_Azimuth);
    else
      return Earth.Unknown_Direction;
    end if;
  end Direction_Of;


  -------------------------------------------------
  -- Convert horizontal to eqatorial coordinates --
  -------------------------------------------------

  procedure HOREQU (A   :     Angle.Value; -- altitude
                    AZ  :     Angle.Value; -- azimuth
                    PHI :     Angle.Value; -- geographic latitude
                    DEC : out Angle.Value; -- declination
                    TAU : out Angle.Value) -- hours angle
  is
    The_Declination : Angle.Degrees;
    The_Hours_Angle : Angle.Degrees;
    use type Angle.Value;
  begin
    Astro.SPHLIB.HOREQU (H   => +A,
                         AZ  => +(AZ + Angle.Semi_Circle),
                         PHI => +PHI,
                         DEC  => The_Declination,
                         TAU  => The_Hours_Angle);
    DEC := +The_Declination;
    TAU := +The_Hours_Angle;
  end HOREQU;


  function Direction_Of (Direction : Earth.Direction;
                         Ut        : Time.Ut) return Space.Direction is
    The_Declination : Angle.Value;
    The_Hours_Angle : Angle.Value;
    use type Angle.Value;
  begin
    if Earth.Direction_Is_Known (Direction) then
      HOREQU (A   => Earth.Alt_Of (Direction),
              AZ  => Earth.Az_Of (Direction),
              PHI => Parameter.Latitude,
              DEC => The_Declination,
              TAU => The_Hours_Angle);
      return Space.Direction_Of (Dec => The_Declination,
                                 Ra  => Time.Lmst_Of (Ut) - The_Hours_Angle);
    else
      return Space.Unknown_Direction;
    end if;
  end Direction_Of;


  ---------------------------------------------
  -- Motor position of eqatorial coordinates --
  ---------------------------------------------

  function Position_Of (Direction : Space.Direction;
                        Rotations : Space.Direction;
                        At_Time   : Time.Ut) return Motor.Position is

    The_Altitude     : Angle.Degrees;
    The_Azimuth      : Angle.Degrees;
    The_First_Angle  : Angle.Degrees;
    The_Second_Angle : Angle.Degrees;

    use type Angle.Value;

  begin
    if Space.Direction_Is_Known (Direction) then
      declare
        Tau : Angle.Value := Time.Lmst_Of (At_Time) - Space.Ra_Of (Direction);
        Dec : Angle.Value := Space.Dec_Of (Direction);
      begin
        if Space.Direction_Is_Known (Rotations) then
          Tau := Tau - Space.Ra_Of (Rotations);
          Dec := Dec - Space.Dec_Of (Rotations);
        end if;
        Astro.SPHLIB.EQUHOR (DEC => +Dec,
                             TAU => +Tau,
                             PHI => +Parameter.Latitude,
                             H   => The_Altitude,
                             AZ  => The_Azimuth);
      end;
      declare
        Alt : Angle.Value := +The_Altitude;
        Az  : Angle.Value := +The_Azimuth;
      begin
        Matrix.Correct (Alt        => Alt,
                        Az         => Az,
                        Is_Inverse => Motor.Is_Inverse);
        The_Altitude := +Alt;
        The_Azimuth := +Az;
      end;
      Refraction.Correct (The_Altitude);
      if Parameter.Is_Azimuthal_Mount then
        return Motor.Position_Of (First  => Angle.Semi_Circle + The_Azimuth,
                                  Second => +The_Altitude);
      else
        Astro.SPHLIB.HOREQU (H   => The_Altitude,
                             AZ  => The_Azimuth,
                             PHI => +Parameter.Pole_Height,
                             DEC  => The_Second_Angle,
                             TAU  => The_First_Angle);
        return Motor.Position_Of (First  => Angle.Semi_Circle + The_First_Angle,
                                  Second => +The_Second_Angle);
      end if;
    else
      raise Program_Error;
    end if;
  end Position_Of;


  ----------------------------------------------
  -- Motor position of horizontal coordinates --
  ----------------------------------------------

  function Position_Of (Direction : Earth.Direction) return Motor.Position is
    The_First_Angle  : Angle.Value;
    The_Second_Angle : Angle.Value;
    use type Angle.Value;
  begin
    if not Earth.Direction_Is_Known (Direction) then
      raise Program_Error;
    end if;
    if Parameter.Pole_Height = Angle.Quadrant then
      return Motor.Position_Of (First  => Earth.Az_Of (Direction),
                                Second => Earth.Alt_Of (Direction));
    else
      HOREQU (A   => Earth.Alt_Of (Direction),
              AZ  => Earth.Az_Of (Direction),
              PHI => Parameter.Pole_Height,
              DEC => The_Second_Angle,
              TAU => The_First_Angle);
      return Motor.Position_Of (First  => The_First_Angle + Angle.Semi_Circle,
                                Second => The_Second_Angle);
    end if;
  end Position_Of;


  ----------------------------------------------
  -- Horizontal coordinates of motor position
  ----------------------------------------------

  procedure Calculate_Horizontal_Coordinates_For (Data          :     Motor.Position_Data;
                                                  The_Positions : out Earth.Direction;
                                                  The_Offsets   : out Earth.Direction) is
    The_Altitude        : Angle.Value;
    The_Azimuth         : Angle.Value;
    The_Altitude_Offset : Angle.Value;
    The_Azimuth_Offset  : Angle.Value;

    use type Motor.Position;

    Original_Positions : constant Motor.Position := Data.Positions - Data.Offsets;

    use type Angle.Value;

  begin
    if Motor.Is_Defined (Data.Positions) then
      if Parameter.Is_Azimuthal_Mount then
        The_Altitude := Motor.Second_Of (Data.Positions);
        The_Azimuth := Motor.First_Of (Data.Positions);
        The_Altitude_Offset := Motor.Second_Of (Data.Offsets);
        The_Azimuth_Offset := Motor.First_Of (Data.Offsets);
      else
        EQUHOR (DEC => Motor.Second_Of (Data.Positions),
                TAU => Motor.First_Of (Data.Positions) + Angle.Semi_Circle,
                PHI => Parameter.Pole_Height,
                A   => The_Altitude,
                AZ  => The_Azimuth);
        EQUHOR (DEC => Motor.Second_Of (Original_Positions),
                TAU => Motor.First_Of (Original_Positions) + Angle.Semi_Circle,
                PHI => Parameter.Pole_Height,
                A   => The_Altitude_Offset,
                AZ  => The_Azimuth_Offset);
        The_Altitude_Offset := The_Altitude - The_Altitude_Offset;
        The_Azimuth_Offset := The_Azimuth - The_Azimuth_Offset;
      end if;
      The_Positions := Earth.Direction_Of (Alt => The_Altitude,
                                           Az  => The_Azimuth,
                                           Inv => Motor.Is_Inverse);
      The_Offsets := Earth.Direction_Of (Alt => The_Altitude_Offset,
                                         Az  => The_Azimuth_Offset,
                                         Inv => Motor.Is_Inverse);
    else
      The_Positions := Earth.Unknown_Direction;
      The_Offsets := Earth.Unknown_Direction;
    end if;
  end Calculate_Horizontal_Coordinates_For;

end Numerics;
