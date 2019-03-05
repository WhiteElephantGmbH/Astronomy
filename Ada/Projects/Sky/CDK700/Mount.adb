-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Traces;

package body Mount is

  package Log is new Traces ("Mount");

  procedure Connect_Communication is
  begin
    Log.Write ("Connect_Communication");
  end Connect_Communication;


  The_State_Handler : State_Handler_Access with Unreferenced;

  procedure Open_Communication (State_Handler : State_Handler_Access) is
  begin
    Log.Write ("Open_Communication");
    The_State_Handler := State_Handler;
  end Open_Communication;


  procedure Initialize is
  begin
    Log.Write ("Initialize");
  end Initialize;


  function Image_Of (The_Direction : Space.Direction) return String is
  begin
    if Space.Direction_Is_Known (The_Direction) then
      return "RA: " & Space.Ra_Image_Of (The_Direction) & " - DEC: " & Space.Dec_Image_Of (The_Direction);
    end if;
    return "Unknown";
  end Image_Of;


  function Image_Of (The_Direction : Earth.Direction) return String is
  begin
    if Earth.Direction_Is_Known (The_Direction) then
      return "AZ: " & Earth.Az_Image_Of (The_Direction) & " - ALT: " & Earth.Alt_Image_Of (The_Direction);
    end if;
    return "Unknown";
  end Image_Of;


  procedure Goto_Target (Direction : Space.Direction) is
  begin
    Log.Write ("Goto_Target " & Image_Of (Direction));
  end Goto_Target;


  procedure Goto_Mark (Direction : Earth.Direction) is
  begin
    Log.Write ("Goto_Mark " & Image_Of (Direction));
  end Goto_Mark;


  procedure Direct (The_Drive  : Device.Drive;
                    With_Speed : Angle.Signed) is
    pragma Unreferenced (With_Speed);
  begin
    Log.Write ("Direct " & The_Drive'img);
  end Direct;


  procedure Adjust (The_Drive  : Device.Drive;
                    With_Speed : Angle.Signed) is
    pragma Unreferenced (With_Speed);
  begin
    Log.Write ("Adjust " & The_Drive'img);
  end Adjust;


  procedure Stop is
  begin
    Log.Write ("Stop");
  end Stop;


  function Actual_Direction return Space.Direction is
  begin
    Log.Write ("Actual_Direction Unknown");
    return Space.Unknown_Direction;
  end Actual_Direction;


  procedure Close_Communication is
  begin
    Log.Write ("Close_Communication");
  end Close_Communication;

end Mount;
