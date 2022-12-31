-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with M_Zero;
with Picture;
with Space;
with Traces;
with Pole_Axis;

package body Pole is

  package Log is new Traces ("Pole");

  procedure Evaluate_Direction is
    The_Count : Natural := 0;
  begin
    Log.Write ("evaluate direction");
    if Picture.Solve (Search_From => Space.Pole_Search_Direction) then
      M_Zero.Start_Solving;
      while not Picture.Solved loop
        delay 0.5;
        The_Count := The_Count + 1;
        if The_Count = 40 then
          Picture.Stop_Solving;
          Log.Write ("Evaluation timeout");
          raise Picture_Not_Solved;
        end if;
      end loop;
      M_Zero.End_Solving;
    else
      raise Picture_Not_Solved;
    end if;
  exception
  when Picture_Not_Solved =>
    raise;
  when Picture.File_Not_Found =>
    raise Picture_Not_Found;
  when Picture.Not_Solved =>
    raise Picture_Not_Solved;
  when Item: others =>
    Log.Termination (Item);
    raise Picture_Not_Solved;
  end Evaluate_Direction;


  procedure Clear is
  begin
    Pole_Axis.Clear;
  end Clear;


  function Has_Values return Boolean is (Pole_Axis.Has_Values);


  procedure Evaluate_Left is
  begin
    Evaluate_Direction;
    Pole_Axis.Evaluate_Left;
  end Evaluate_Left;


  procedure Evaluate_Right is
  begin
    Evaluate_Direction;
    Pole_Axis.Evaluate_Right;
  end Evaluate_Right;


  procedure Evaluate_Top is
  begin
    Evaluate_Direction;
    Pole_Axis.Evaluate_Top;
  end Evaluate_Top;

end Pole;
