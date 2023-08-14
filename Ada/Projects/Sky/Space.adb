-- *********************************************************************************************************************
-- *                       (c) 2014 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Error;

package body Space is

  function Direction_Of (Dec : Angle.Value;
                         Ra  : Angle.Value) return Direction is
  begin
    return (Dec      => Dec,
            Ra       => Ra,
            Is_Known => True);
  end Direction_Of;


  function Direction_Of (Dec : Angle.Degrees;
                         Ra  : Angle.Degrees) return Direction is
  begin
    return (Dec      => +Dec,
            Ra       => +Ra,
            Is_Known => True);
  end Direction_Of;


  function Direction_Of (Dec_Image : String;
                         Ra_Image  : String) return Direction is
  begin
    return (Dec      => Angle.Value_Of (Dec_Image, Angle.In_Degrees),
            Ra       => Angle.Value_Of (Ra_Image, Angle.In_Hours),
            Is_Known => True);
  end Direction_Of;


  function Dec_Of (The_Direction : Direction) return Angle.Value is
  begin
    return The_Direction.Dec;
  end Dec_Of;


  function Ra_Of (The_Direction : Direction) return Angle.Value is
  begin
    return The_Direction.Ra;
  end Ra_Of;


  function Dec_Image_Of (The_Direction : Direction) return String is
  begin
    return Angle.Image_Of (The_Direction.Dec, Show_Signed => True);
  end Dec_Image_Of;


  function Ra_Image_Of (The_Direction : Direction) return String is
  begin
    return Angle.Image_Of (The_Direction.Ra, Unit => Angle.In_Hours, Decimals => 2);
  end Ra_Image_Of;


  function Image_Of (The_Direction : Direction) return String is
  begin
    return "- Ra: " & Ra_Image_Of (The_Direction) & " - Dec: " & Dec_Image_Of (The_Direction);
  end Image_Of;


  function Dec_Offset_Image_Of (The_Direction : Direction) return String is
  begin
    return Angle.Image_Of (The_Direction.Dec, Show_Signed => True);
  end Dec_Offset_Image_Of;


  function Ra_Offset_Image_Of (The_Direction : Direction) return String is
  begin
    return Angle.Image_Of (The_Direction.Ra, Show_Signed => True);
  end Ra_Offset_Image_Of;


  function Direction_Is_Known (The_Direction : Direction) return Boolean is
  begin
    return The_Direction.Is_Known;
  end Direction_Is_Known;


  function "-" (Left, Right : Direction) return Direction is
  begin
    return (Dec      => Left.Dec - Right.Dec,
            Ra       => Left.Ra - Right.Ra,
            Is_Known => Left.Is_Known and Right.Is_Known);
  end "-";


  function Angle_Between (Left, Right : Direction) return Angle.Degrees is

    use Astro.MATLIB;
    use type Angle.Degrees;

    Dec_1 : constant Angle.Degrees := + Left.Dec;
    Dec_2 : constant Angle.Degrees := + Right.Dec;
    Ra_1  : constant Angle.Degrees := + Left.Ra;
    Ra_2  : constant Angle.Degrees := + Right.Ra;
    D_Ra  : constant Angle.Degrees := Ra_2 - Ra_1;

  begin
    if Left.Is_Known and Right.Is_Known then
      return ACS(SN(Dec_1) * SN(Dec_2) + CS(Dec_1) * CS(Dec_2) * CS(D_Ra));
    else
      Error.Raise_With ("value unknown");
    end if;
  end Angle_Between;


  function Angle_Between (Left, Right  : Direction;
                          Smaller_Than : Angle.Degrees) return Boolean is
    use type Angle.Degrees;
    D_Dec : constant Angle.Degrees := abs(Left.Dec - Right.Dec);
  begin
    if D_Dec >= Smaller_Than then
      return False;
    end if;
    return Angle_Between (Left, Right) < Smaller_Than;
  end Angle_Between;

end Space;
