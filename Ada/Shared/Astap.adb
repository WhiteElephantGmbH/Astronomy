-- *********************************************************************************************************************
-- *                               (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Configuration;
with File;
with Os.Process;
with Text;
with Traces;

package body Astap is

  package Log is new Traces ("Astap");

  The_Executable      : Text.String;
  The_Result_Filename : Text.String;
  The_Wcs_Filename    : Text.String;
  The_Process_Id      : Os.Process.Id;

  Is_Solving : Boolean := False;


  procedure Terminate_Process is
  begin
    Os.Process.Terminate_With (The_Process_Id);
  exception
  when others =>
    null;
  end Terminate_Process;


  function Result_Filename return String is
  begin
    return Text.String_Of (The_Result_Filename);
  end Result_Filename;


  function Wcs_Filename return String is
  begin
    return Text.String_Of (The_Wcs_Filename);
  end Wcs_Filename;


  procedure Cleanup is
  begin
    File.Delete (Result_Filename);
    File.Delete (Wcs_Filename);
  end Cleanup;


  procedure Define (Executable : String) is
  begin
    The_Executable := Text.String_Of (Executable);
  end Define;


  procedure Solve (Filename : String;
                   Height   : Degrees;
                   Ra       : Degrees;
                   Dec      : Degrees) is

    Directory : constant String := File.Containing_Directory_Of (Filename);
    Base_Name : constant String := File.Base_Name_Of (Filename);

    function Ra_Image return String is
      type Ra_Value is delta 0.001 range 0.0 .. 24.0;
      Value : constant Degrees := Ra / 15.0;
    begin
      return Ra_Value(Value)'img;
    end Ra_Image;

    type Value is delta 0.01 range 0.0 .. 360.0;

    function Spd_Image return String is
      Spd : Degrees := Dec + 90.0;
    begin
      if Spd > 360.0 then
        Spd := Spd - 360.0;
      end if;
      return Value(Spd)'img;
    end Spd_Image;

    Height_Image : constant String := Value(Height)'image;
    Executable   : constant String := Text.String_Of (The_Executable);

    Parameters   : constant String
      := "-f " & Filename & " -ra" & Ra_Image & " -spd" & Spd_Image & " -fov" & Height_Image & " -r 180";

  begin -- Solve
    if not File.Exists (Executable) then
      Log.Error ("Executable not found");
      raise Not_Solved;
    end if;
    Log.Write ("Parameters: " & Parameters);

    The_Result_Filename := Text.String_Of (File.Composure (Directory, Base_Name, "ini"));
    The_Wcs_Filename := Text.String_Of (File.Composure (Directory, Base_Name, "wcs"));

    Cleanup;
    The_Process_Id := Os.Process.Created (Executable     => Executable,
                                          Parameters     => Parameters,
                                          Current_Folder => Directory);
    Is_Solving := True;
  exception
  when Not_Solved =>
    Cleanup;
    raise;
  when Item: others =>
    Log.Termination (Item);
    raise Not_Solved;
  end Solve;


  function Solved (Ra  : out Degrees;
                   Dec : out Degrees) return Boolean is

    procedure Handle_Result is

      Result_Handle : constant Configuration.File_Handle := Configuration.Handle_For (Result_Filename);
      Null_Section  : constant Configuration.Section_Handle := Configuration.Handle_For (Result_Handle);
      Ra_2000       : constant String := Configuration.Value_Of (Null_Section, "CRVAL1");
      Dec_2000      : constant String := Configuration.Value_Of (Null_Section, "CRVAL2");
      Error         : constant String := Configuration.Value_Of (Null_Section, "ERROR");
      Warning       : constant String := Configuration.Value_Of (Null_Section, "WARNING");

    begin
      if Error /= "" then
        Log.Error (Error);
        raise Not_Solved;
      end if;
      if Warning /= "" then
        Log.Warning (Warning);
      end if;
      Ra := Degrees'value(Ra_2000);
      Dec := Degrees'value(Dec_2000);
    exception
    when others =>
      raise Not_Solved;
    end Handle_Result;

  begin -- Solved
    if Is_Solving then
      if File.Exists (Result_Filename) then
        Handle_Result;
        Cleanup;
        Terminate_Process;
        Is_Solving := False;
        return True;
      end if;
      return False;
    else
      raise Not_Solved;
    end if;
  exception
  when Not_Solved =>
    Is_Solving := False;
    Cleanup;
    raise Not_Solved;
  when Item: others =>
    Is_Solving := False;
    Log.Termination (Item);
    raise Not_Solved;
  end Solved;


  procedure Stop is
  begin
    Terminate_Process;
    Is_Solving := False;
  end Stop;

end Astap;
