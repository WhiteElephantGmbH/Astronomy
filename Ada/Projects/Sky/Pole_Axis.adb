-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Alignment;
with Angle;
with Earth;
with Picture;
with Site;
with Space;
with Traces;

package body Pole_Axis is

  package Log is new Traces ("Pole_Axis");


  function Image_Of (Item : Angle.Value) return String is
  begin
    return Angle.Image_Of (The_Value   => Item,
                           Unit        => Angle.In_Degrees,
                           Decimals    => 3,
                           Show_Signed => True);
  end Image_Of;


  procedure Evaluate_Direction (The_Direction : in out Earth.Direction) is
    The_Count : Natural := 0;
  begin
    if Picture.Solve (Search_From => Space.North_Pole) then
      if not Site.Is_Defined then
        declare
          The_Site : Site.Data;
        begin
          The_Site.Latitude := Picture.Latitude;
          The_Site.Longitude := Picture.Longitude;
          The_Site.Elevation := Picture.Elevation;
          Site.Define (The_Site);
          Log.Write ("Elevation :" & Picture.Elevation'image & 'm');
          Log.Write ("Latitude  : " & Image_Of (Picture.Latitude));
          Log.Write ("Longitude : " & Image_Of (Picture.Longitude));
        exception
        when others =>
          Log.Error ("Unknown Site");
          return;
        end;
      end if;
      while not Picture.Solved loop
        delay 0.5;
        The_Count := The_Count + 1;
        if The_Count = 40 then
          Picture.Stop_Solving;
          Log.Write ("Evaluation timeout");
          raise Picture_Not_Solved;
        end if;
      end loop;
      The_Direction := Picture.Direction;
    else
      raise Picture_Not_Solved;
    end if;
  exception
  when Picture.File_Not_Found =>
    raise Picture_Not_Found;
  when Picture.Not_Solved =>
    raise Picture_Not_Solved;
  when Item: others =>
    Log.Termination (Item);
    raise Picture_Not_Solved;
  end Evaluate_Direction;


  The_Pol_Top   : Earth.Direction;
  The_Pol_Left  : Earth.Direction;
  The_Pol_Right : Earth.Direction;


  procedure Set_Alignment is
    The_Alt        : Angle.Degrees := 0.0;
    The_Az         : Angle.Degrees := 0.0;
    The_Cone_Error : Angle.Degrees := 0.0;
    The_Left_Az    : Angle.Degrees;
    The_Right_Az   : Angle.Degrees;
    use type Angle.Degrees;
    use type Angle.Value;
  begin
    if Earth.Direction_Is_Known (The_Pol_Top) then
      The_Alt := +Earth.Alt_Of (The_Pol_Top);
    end if;
    if Earth.Direction_Is_Known (The_Pol_Left) and Earth.Direction_Is_Known (The_Pol_Right) then
      The_Left_Az := +Earth.Az_Of (The_Pol_Left);
      The_Right_Az := +Earth.Az_Of (The_Pol_Right);
      The_Az := (The_Right_Az + The_Left_Az) / 2.0;
      The_Cone_Error := (The_Right_Az - The_Left_Az) / 2.0;
    elsif Earth.Direction_Is_Known (The_Pol_Right) then
      The_Az := +Earth.Az_Of (The_Pol_Right);
    elsif Earth.Direction_Is_Known (The_Pol_Left) then
      The_Az := +Earth.Az_Of (The_Pol_Left);
    end if;
    if (The_Az = 0.0) and (The_Alt = 0.0) then
      Alignment.Set (Pole_Offsets => Earth.Unknown_Direction);
    else
      if The_Alt /= 0.0 then
        The_Alt := The_Alt - Angle.Degrees'(+Site.Latitude) - The_Cone_Error;
      end if;
      Alignment.Set (Pole_Offsets => Earth.Direction_Of (Alt => +The_Alt, Az => +The_Az));
    end if;
    Alignment.Set (Cone_Error => Angle.Value'(+The_Cone_Error));
  end Set_Alignment;


  procedure Clear is
  begin
    Log.Write ("Clear");
    The_Pol_Top   := Earth.Unknown_Direction;
    The_Pol_Left  := Earth.Unknown_Direction;
    The_Pol_Right := Earth.Unknown_Direction;
    Set_Alignment;
  end Clear;


  procedure Evaluate_Pole_Top is
  begin
    Log.Write ("Evaluate_Pole_Top");
    Evaluate_Direction (The_Pol_Top);
    Set_Alignment;
  end Evaluate_Pole_Top;


  procedure Evaluate_Pole_Left is
  begin
    Log.Write ("Evaluate_Pole_Left");
    Evaluate_Direction (The_Pol_Left);
    Set_Alignment;
  end Evaluate_Pole_Left;


  procedure Evaluate_Pole_Right is
  begin
    Log.Write ("Evaluate_Pole_Right");
    Evaluate_Direction (The_Pol_Right);
    Set_Alignment;
  end Evaluate_Pole_Right;

end Pole_Axis;
