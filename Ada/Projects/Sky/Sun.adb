-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Earth;
with Objects;
with Solar_System;
with Time;
with Traces;

package body Sun is

  package Log is new Traces ("Sun");

  The_Sun_Direction : Space.Direction;
  The_Safety_Angle  : Angle.Degrees := 0.0;


  procedure Define (Safety_Angle : Angle.Degrees) is
    use type Angle.Value;
  begin
    The_Safety_Angle := Safety_Angle;
    Log.Write ("Safety Angle: " & Angle.Image_Of (+The_Safety_Angle));
  end Define;


  function Is_Visible return Boolean is
    Ut : constant Time.Ut := Time.Universal;
  begin
    The_Sun_Direction := Solar_System.Direction_Of (Solar_System.Sun, Ut);
    return not Earth.Is_Below_Horizon (Objects.Direction_Of (Direction => The_Sun_Direction,
                                                             Lmst      => Time.Lmst_Of (Ut)));
  end Is_Visible;


  function Protection_Is_Disabled return Boolean is
    use type Angle.Degrees;
  begin
    return The_Safety_Angle = 0.0 or else not Is_Visible;
  end Protection_Is_Disabled;


  function Is_In_Safe_Distance (To_Target : Space.Direction) return Boolean is
    use type Angle.Degrees;
  begin
    if The_Safety_Angle = 0.0 then
      return True;
    end if;
    return not Space.Angle_Between (The_Sun_Direction, To_Target, Smaller_Than => The_Safety_Angle);
  end Is_In_Safe_Distance;

end Sun;
