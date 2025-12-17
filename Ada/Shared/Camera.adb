-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Camera.Canon;
with Camera.QHYCCD;
with Camera.Raw;

with Traces;

package body Camera is

  package Log is new Traces ("Camera");

  procedure Start is
  begin
    Log.Write ("Start");
    Canon.Start_Control;
    QHYCCD.Start_Control;
  end Start;


  function Actual_Information return Information is
  begin
    return Camera_Data.Actual;
  end Actual_Information;


  procedure Capture (Filename  : String;
                     Time      : Exposure.Item    := Exposure.From_Camera;
                     Parameter : Sensitivity.Item := Sensitivity.Default) is
  begin
    Log.Write ("Capture " & Filename & " - Time: " & Time'image & " - Parameter: " & Parameter'image);
    if Canon.Is_Available then
      Canon.Capture_Picture (Filename, Time, Parameter);
    elsif QHYCCD.Is_Available then
      QHYCCD.Capture_Picture (Filename, Time, Parameter);
    else
      Camera_Data.Set_Error ("No Camera Available");
    end if;
  end Capture;


  procedure Capture (Size      : Square_Size;
                     Time      : Exposure.Item := Exposure.From_Camera;
                     Parameter : Sensitivity.Item := Sensitivity.Default) is
  begin
    Log.Write ("Capture - Size:" & Size'image & " - Time: " & Time'image & " - Parameter: " & Parameter'image);
    if Canon.Is_Available then
      Canon.Capture_Grid (Size, Time, Parameter);
    elsif QHYCCD.Is_Available then
      QHYCCD.Capture_Grid (Size, Time, Parameter);
    else
      Camera_Data.Set_Error ("No Camera Available");
    end if;
  end Capture;


  function Captured return Green_Grid is
  begin
    case Camera_Data.Actual.Camera is
    when Canon_Eos_6D | Canon_Eos_60D =>
      return Raw.Grid;
    when QHY600C =>
      return QHYCCD.Grid;
    end case;
  end Captured;


  procedure Stop is
  begin
    Log.Write ("Stop");
    case Camera_Data.Actual.Camera is
    when Canon_Eos_6D | Canon_Eos_60D =>
      Canon.Stop_Capture;
    when QHY600C =>
      QHYCCD.Stop_Capture;
    end case;
  end Stop;


  procedure Finish is
  begin
    Log.Write ("Finish");
    Canon.End_Control;
    QHYCCD.End_Control;
  end Finish;

-------------
-- Private --
-------------

  -----------
  -- Error --
  -----------
  Camera_Error : exception;

  procedure Raise_Error (Message : String) is
  begin
    Log.Error (Message);
    Camera_Data.Set_Error (Message);
    raise Camera_Error;
  end Raise_Error;


  function Error_Message return String is
  begin
    Camera_Data.Reset_Error;
    return Camera_Data.Last_Error;
  end Error_Message;


  ----------
  -- Data --
  ----------
  protected body Camera_Data is

    procedure Set (State : Status) is
    begin
      if The_Information.State /= Error then
        The_Information.State := State;
      end if;
    end Set;


    procedure Set (Item : Model) is
    begin
      The_Information.Camera := Item;
    end Set;


    procedure Set (Height : Rows) is
    begin
      The_Information.Height := Height;
    end Set;


    procedure Set (Width : Columns) is
    begin
      The_Information.Width := Width;
    end Set;


    function Actual return Information is
    begin
      return The_Information;
    end Actual;


    procedure Check (Item : Status) is
    begin
      if The_Information.State /= Item then
        The_Last_Error := ["Sequence Error - State must be " & Item'image];
        The_Information.State := Error;
        raise Camera_Error;
      end if;
    end Check;


    procedure Set_Error (Message : String) is
    begin
      The_Last_Error := [Message];
      The_Information.State := Error;
    end Set_Error;


    function Last_Error return String is
    begin
      return The_Last_Error.To_String;
    end Last_Error;


    procedure Reset_Error is
    begin
      The_Information.State := Idle;
    end Reset_Error;

  end Camera_Data;

  ----------
  -- Grid --
  ----------


end Camera;
