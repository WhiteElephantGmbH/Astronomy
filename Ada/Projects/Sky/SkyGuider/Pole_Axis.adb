-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Picture;
with Site;
with Traces;

package body Pole_Axis is

  package Log is new Traces ("Pole_Axis");

  The_Pole_Top   : Earth.Direction;
  The_Pole_Left  : Earth.Direction;
  The_Pole_Right : Earth.Direction;

  The_Cone_Error   : Angle.Value := Angle.Zero;
  The_Pole_Offsets : Earth.Direction;


  procedure Set_Alignment is
    The_Alt      : Angle.Signed := 0;
    The_Az       : Angle.Signed := 0;
    The_Coning   : Angle.Signed := 0;
    The_Left_Az  : Angle.Signed;
    The_Right_Az : Angle.Signed;
    use type Angle.Signed;
  begin
    if Earth.Direction_Is_Known (The_Pole_Top) then
      The_Alt := +Earth.Alt_Of (The_Pole_Top);
    end if;
    if Earth.Direction_Is_Known (The_Pole_Left) and Earth.Direction_Is_Known (The_Pole_Right) then
      The_Left_Az := +Earth.Az_Of (The_Pole_Left);
      The_Right_Az := +Earth.Az_Of (The_Pole_Right);
      The_Az := The_Right_Az + The_Left_Az;
      The_Az := The_Az / 2;
      The_Coning := (The_Az - The_Left_Az);
    elsif Earth.Direction_Is_Known (The_Pole_Right) then
      The_Az := +Earth.Az_Of (The_Pole_Right);
    elsif Earth.Direction_Is_Known (The_Pole_Left) then
      The_Az := +Earth.Az_Of (The_Pole_Left);
    end if;
    if (The_Az = 0) and (The_Alt = 0) then
      The_Pole_Offsets := Earth.Unknown_Direction;
    else
      if The_Alt /= 0 then
        The_Alt := The_Alt - Angle.Signed'(+Site.Latitude) - The_Coning;
      end if;
      The_Pole_Offsets := Earth.Direction_Of (Alt => +The_Alt, Az => +The_Az);
    end if;
    The_Cone_Error := Angle.Value'(+The_Coning);
  end Set_Alignment;


  procedure Clear is
  begin
    Log.Write ("Clear");
    The_Pole_Top   := Earth.Unknown_Direction;
    The_Pole_Left  := Earth.Unknown_Direction;
    The_Pole_Right := Earth.Unknown_Direction;
    Set_Alignment;
  end Clear;


  function Has_Values return Boolean is
    use type Angle.Value;
  begin
    return Earth.Direction_Is_Known (Offsets) or (Cone_Error /= Angle.Zero);
  end Has_Values;


  procedure Evaluate_Left is
  begin
    Log.Write ("Evaluate_Pole_Left");
    The_Pole_Left := Picture.Direction;
    Set_Alignment;
  end Evaluate_Left;


  procedure Evaluate_Right is
  begin
    Log.Write ("Evaluate_Pole_Right");
    The_Pole_Right := Picture.Direction;
    Set_Alignment;
  end Evaluate_Right;


  procedure Evaluate_Top is
  begin
    Log.Write ("Evaluate_Pole_Top");
    The_Pole_Top := Picture.Direction;
    Set_Alignment;
  end Evaluate_Top;


  function Cone_Error return Angle.Value is
  begin
    return The_Cone_Error;
  end Cone_Error;


  function Offsets return Earth.Direction is
  begin
    return The_Pole_Offsets;
  end Offsets;

end Pole_Axis;
