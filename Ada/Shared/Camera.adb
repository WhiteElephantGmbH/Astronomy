-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Directory;
with File;
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


  function Model_Image return String is
  begin
    case Actual_Information.Camera is
    when Unknown =>
      return "";
    when Canon_Eos_60D =>
      return "Canon EOS 60D";
    when Canon_Eos_6D =>
      return "Canon EOS 6D";
    when QHY600C =>
      return "QHY600C";
    end case;
  end Model_Image;


  procedure Capture (Filename  : String;
                     Time      : Exposure.Item;
                     Parameter : Sensitivity.Item) is

    function Filename_With_Extension (Default : String) return String is
      Folder : constant String := File.Containing_Directory_Of (Filename);
    begin
      if not Directory.Exists (Folder) then
        Raise_Error ("Unknown picture directory " & Folder);
      elsif File.Extension_Of (Filename) = "" then
        return Filename & '.' & Default;
      else
        return Filename;
      end if;
    end Filename_With_Extension;

  begin -- Capture
    Log.Write ("Capture " & Filename & " - Time: " & Time'image & " - Parameter: " & Parameter'image);
    Camera_Data.Set (Unknown);
    if Canon.Is_Available then
      Canon.Capture_Picture (Filename_With_Extension (Default => "CR2"), Time, Parameter);
    elsif QHYCCD.Is_Available then
      QHYCCD.Capture_Picture (Filename_With_Extension (Default => "FITS"), Time, Parameter);
    else
      Camera_Data.Set_Error ("No Camera Available");
    end if;
  exception
  when Occurrence: others =>
    Camera_Data.Set_Fatal (Occurrence);
  end Capture;


  procedure Capture (Filename : String) is
  begin
    Capture (Filename, The_Exposure_Parameter, The_Sensitivity_Parameter);
  end Capture;


  procedure Capture (Size      : Square_Size;
                     Time      : Exposure.Item;
                     Parameter : Sensitivity.Item) is
  begin
    Log.Write ("Capture - Size:" & Size'image & " - Time: " & Time'image & " - Parameter: " & Parameter'image);
    Camera_Data.Set (Unknown);
    if Canon.Is_Available then
      Canon.Capture_Grid (Size, Time, Parameter);
    elsif QHYCCD.Is_Available then
      QHYCCD.Capture_Grid (Size, Time, Parameter);
    else
      Camera_Data.Set_Error ("No Camera Available");
    end if;
  exception
  when Occurrence: others =>
    Camera_Data.Set_Fatal (Occurrence);
  end Capture;


  procedure Capture (Size : Square_Size) is
  begin
    Capture (Size, The_Exposure_Parameter, The_Sensitivity_Parameter);
  end Capture;


  function Captured return Raw_Grid is
  begin
    case Camera_Data.Actual.Camera is
    when Canon_Eos_6D | Canon_Eos_60D =>
      return Raw.Grid;
    when QHY600C =>
      return QHYCCD.Grid;
    when Unknown =>
      Camera_Data.Set_Error ("No Camera Connected");
      return [];
    end case;
  exception
  when Occurrence: others =>
    Camera_Data.Set_Fatal (Occurrence);
    return [];
  end Captured;


  procedure Stop is
  begin
    Log.Write ("Stop");
    case Camera_Data.Actual.Camera is
    when Canon_Eos_6D | Canon_Eos_60D =>
      Canon.Stop_Capture;
    when QHY600C =>
      QHYCCD.Stop_Capture;
    when Unknown =>
      null;
    end case;
  exception
  when Occurrence: others =>
    Camera_Data.Set_Fatal (Occurrence);
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

  procedure Raise_Error (Message : String) is
  begin
    Log.Error (Message);
    Camera_Data.Set_Error (Message);
    raise Camera_Error;
  end Raise_Error;


  function Has_Error return Boolean is
  begin
    return Camera_Data.Actual.State = Failed;
  end Has_Error;


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
      if The_Information.State /= Failed then
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
        The_Information.State := Failed;
        raise Camera_Error;
      end if;
    end Check;


    procedure Set_Error (Message : String) is
    begin
      The_Last_Error := [Message];
      The_Information.State := Failed;
    end Set_Error;


    procedure Set_Fatal (Item : Exceptions.Occurrence) is
    begin
      Set_Error ("Internal_Error - " & Exceptions.Name_Of (Item));
    end Set_Fatal;


    function Last_Error return String is
    begin
      return The_Last_Error.To_String;
    end Last_Error;


    procedure Reset_Error is
    begin
      The_Information.State := Idle;
    end Reset_Error;

  end Camera_Data;

end Camera;
