-- *********************************************************************************************************************
-- *                       (c) 2014 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package body Earth is

  function Direction_Of (Alt : Angle.Value;
                         Az  : Angle.Value;
                         Inv : Boolean := False) return Direction is
  begin
    return (Alt        => Alt,
            Az         => Az,
            Is_Known   => True,
            Is_Inverse => Inv);
  end Direction_Of;


  function Alt_Of (The_Direction : Direction) return Angle.Value is
  begin
    return The_Direction.Alt;
  end Alt_Of;


  function Az_Of (The_Direction : Direction) return Angle.Value is
  begin
    return The_Direction.Az;
  end Az_Of;


  function Alt_Image_Of (The_Direction : Direction) return String  is
  begin
    return Angle.Image_Of (The_Direction.Alt, Show_Signed => True);
  end Alt_Image_Of;


  function Az_Image_Of (The_Direction : Direction) return String is
  begin
    return Angle.Image_Of (The_Direction.Az);
  end Az_Image_Of;


  function Alt_Offset_Image_Of (The_Direction : Direction) return String is
  begin
    return Angle.Image_Of (The_Direction.Alt, Show_Signed => True);
  end Alt_Offset_Image_Of;


  function Az_Offset_Image_Of (The_Direction : Direction) return String  is
  begin
    return Angle.Image_Of (The_Direction.Az, Show_Signed => True);
  end Az_Offset_Image_Of;


  function Is_Below_Horizon (The_Direction : Direction) return Boolean is
    use type Angle.Signed;
  begin
    return Angle.Signed'(+The_Direction.Alt) < 0;
  end Is_Below_Horizon;


  function Direction_Is_Known (The_Direction : Direction) return Boolean is
  begin
    return The_Direction.Is_Known;
  end Direction_Is_Known;


  function Direction_Is_Inverse (The_Direction : Direction) return Boolean is
  begin
    return The_Direction.Is_Inverse;
  end Direction_Is_Inverse;


  function "+" (Left, Right : Direction) return Direction is
    use type Angle.Value;
  begin
    if Left.Is_Known and Right.Is_Known then
      return (Az         => Left.Az + Right.Az,
              Alt        => Left.Alt + Right.Alt,
              Is_Known   => True,
              Is_Inverse => Left.Is_Inverse);
    else
      return Unknown_Direction;
    end if;
  end "+";


  function "-" (Left, Right : Direction) return Direction is
    use type Angle.Value;
  begin
    if Left.Is_Known and Right.Is_Known then
      return (Az         => Left.Az - Right.Az,
              Alt        => Left.Alt - Right.Alt,
              Is_Known   => True,
              Is_Inverse => Left.Is_Inverse);
    else
      return Unknown_Direction;
    end if;
  end "-";


  procedure Add_Az_To (The_Direction : in out Direction;
                       The_Offset    :        Angle.Degrees) is
    use type Angle.Value;
  begin
    The_Direction.Az := The_Direction.Az + The_Offset;
    The_Direction.Is_Known := True;
  end Add_Az_To;


  procedure Add_Alt_To (The_Direction : in out Direction;
                        The_Offset    :        Angle.Degrees) is
    use type Angle.Value;
  begin
    The_Direction.Alt := The_Direction.Alt + The_Offset;
    The_Direction.Is_Known := True;
  end Add_Alt_To;


  procedure Add_To (The_Direction : in out Direction;
                    The_Offset    : in out Direction) is
    use type Angle.Degrees;
    use type Angle.Signed;
    use type Angle.Value;
  begin
    if The_Direction.Is_Known and The_Offset.Is_Known then
      declare
        The_Altitude : Angle.Degrees := +Angle.Signed'(+Angle.Value'(The_Direction.Alt + The_Offset.Alt));
      begin
        if The_Altitude > 90.0 then
          The_Offset.Alt := Angle.Quadrant - The_Direction.Alt;
          The_Altitude := 90.0;
        elsif The_Altitude < 0.0 then
          The_Offset.Alt := Angle.Zero - The_Direction.Alt;
          The_Altitude := 0.0;
        end if;
        The_Direction := (Az         => The_Direction.Az + The_Offset.Az,
                          Alt        => +The_Altitude,
                          Is_Known   => True,
                          Is_Inverse => The_Direction.Is_Inverse);
      end;
    else
      The_Direction := Unknown_Direction;
    end if;
  end Add_To;

end Earth;
