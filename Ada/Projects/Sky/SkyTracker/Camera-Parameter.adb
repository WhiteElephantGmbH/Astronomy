-- *********************************************************************************************************************
-- *                               (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Error;
with Section;

package body Camera.Parameter is

  Exposure_Time_Key : constant String := "Exposure Time";
  Sensitivity_Key   : constant String := "Sensitivity";


  function Exposure_For (Key : String) return Exposure.Item is
    Image : constant String := Section.String_Value_Of (Key);
  begin
    return Exposure.Value (Image);
  exception
  when others =>
    Error.Raise_With ("Incorrect Exposure Time: " & Image);
  end Exposure_For;


  function Sensitivity_For (Key : String) return Sensitivity.Item is
    Image : constant String := Section.String_Value_Of (Key);
  begin
    return Sensitivity.Value (Image);
  exception
  when others =>
    Error.Raise_With ("Incorrect Sensitivity: " & Image);
  end Sensitivity_For;


  procedure Define (Handle : Configuration.File_Handle) is
  begin
    Section.Set (Configuration.Handle_For (Handle, Camera_Id));
    if Section.Exists then
      The_Exposure_Parameter := Exposure_For (Exposure_Time_Key);
      Log.Write ("Exposure: " & The_Exposure_Parameter'image);
      The_Sensitivity_Parameter := Sensitivity_For (Sensitivity_Key);
      Log.Write ("Sensitivity: " & The_Sensitivity_Parameter'image);
    end if;
  end Define;


  procedure Defaults (Put               : access procedure (Item : String);
                      Exposure_Time     : Exposure.Item;
                      Sensitivity_Value : Sensitivity.Item) is
  begin
    Put ("[" & Camera_Id & "]");
    Put (Exposure_Time_Key & " = " & Exposure_Time'image);
    Put (Sensitivity_Key & "   = " & Sensitivity_Value'image);
  end Defaults;

end Camera.Parameter;
