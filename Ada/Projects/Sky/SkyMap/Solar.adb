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

with Moon;
with Objects;
with Text;
with Traces;

package body Solar is

  package Log is new Traces ("Solar");

  The_Sun_Direction  : Earth.Direction;
  The_Moon_Direction : Earth.Direction;
  The_Moon_Phase     : Phase := 0.0;

  type Planet_Direction is array (Planet) of Earth.Direction;

  The_Planet_Direction : Planet_Direction;


  function Image_Of (Direction : Earth.Direction) return String is
  begin
    return " - Az: " & Earth.Az_Image_Of (Direction) & " - Alt: " & Earth.Alt_Image_Of (Direction);
  end Image_Of;


  function Sun_Direction return Earth.Direction is
  begin
    return The_Sun_Direction;
  end Sun_Direction;


  function Is_Day_Light return Boolean is
  begin
    return not Earth.Is_Below_Horizon (The_Sun_Direction);
  end Is_Day_Light;


  function Moon_Direction return Earth.Direction is
  begin
    return The_Moon_Direction;
  end Moon_Direction;


  function Direction_For (Item : Planet) return Earth.Direction is
  begin
    return The_Planet_Direction(Item);
  end Direction_For;


  function Image_Of (Item : Planet) return String is
  begin
    return Text.Legible_Of (Item'image);
  end Image_Of;


  function Moon_Phase return Phase is
  begin
    return The_Moon_Phase;
  end Moon_Phase;


  procedure Prepare (Ut : Time.Ut) is
    Before, After : Time.Ut;
    use type Time.Ut;
  begin
    The_Sun_Direction := Solar_System.Direction_Of (Solar_System.Sun, Ut);
    Log.Write ("Sun" & Image_Of (The_Sun_Direction));

    The_Moon_Direction := Objects.Direction_Of (Direction => Moon.Direction_Of (UT => Ut),
                                                Lmst      => Time.Lmst_Of (Ut));
    Log.Write ("Moon" & Image_Of (The_Moon_Direction));

    Moon.Get_New_Phase (Around    => Ut,
                        Before    => Before,
                        After     => After);
    The_Moon_Phase := Phase((Ut - Before) * 100 / (After - Before));
    Log.Write ("Moon Phase :" & The_Moon_Phase'image & '%');

    for The_Planet in Planet'range loop
      The_Planet_Direction(The_Planet) := Solar_System.Direction_Of (The_Planet, Ut);
      Log.Write (Image_Of (The_Planet) & Image_Of (The_Planet_Direction(The_Planet)));
    end loop;
  end Prepare;

end Solar;
