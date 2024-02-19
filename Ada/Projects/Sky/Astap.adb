-- *********************************************************************************************************************
-- *                           (c) 2021 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                      *
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
with Traces;

package body Astap is

  package Log is new Traces ("Astap");

  use type Strings.Element;

  The_Executable : Strings.Element;
  The_Filename   : Strings.Element; -- without extension
  The_Process_Id : Os.Process.Id;

  Is_Solving : Boolean := False;


  procedure Terminate_Process is
  begin
    Os.Process.Terminate_With (The_Process_Id);
  exception
  when others =>
    null;
  end Terminate_Process;


  function Fits_Filename return String is
  begin
    return The_Filename & ".CR2.fits";
  end Fits_Filename;


  function Result_Filename return String is
  begin
    return The_Filename & ".ini";
  end Result_Filename;


  function Wcs_Filename return String is
  begin
    return The_Filename & ".wcs";
  end Wcs_Filename;


  procedure Cleanup is
  begin
    File.Delete (Fits_Filename);
    File.Delete (Result_Filename);
    File.Delete (Wcs_Filename);
  exception
  when others =>
    null;
  end Cleanup;


  procedure Define (Executable : String) is
  begin
    Log.Write ("Executable: " & Executable);
    The_Executable := [Executable];
  end Define;


  procedure Solve (Filename : String;
                   Height   : Angle.Degrees;
                   Start    : Location) is

    Directory : constant String := File.Containing_Directory_Of (Filename);
    Base_Name : constant String := File.Base_Name_Of (Filename);
    Name      : constant String := Directory & File.Folder_Separator & Base_Name; -- without extension

    use type Angle.Degrees;

    function Ra_Image return String is
      type Ra_Value is delta 0.001 range 0.0 .. 24.0;
      Value : constant Angle.Degrees := Start(Ra) / 15.0;
    begin
      return Ra_Value(Value)'img;
    end Ra_Image;

    type Value is delta 0.01 range 0.0 .. 360.0;

    function Spd_Image return String is
      Spd : Angle.Degrees := Start(Dec) + 90.0;
    begin
      if Spd > 360.0 then
        Spd := Spd - 360.0;
      end if;
      return Value(Spd)'img;
    end Spd_Image;

    Height_Image : constant String := Value(Height)'image;
    Executable   : constant String := +The_Executable;
    Parameters   : constant String
      := "-f " & Filename & " -ra" & Ra_Image & " -spd" & Spd_Image & " -fov" & Height_Image & " -r 180 -o " & Filename;

  begin -- Solve
    if not File.Exists (Executable) then
      Log.Error ("Executable not found");
      raise Not_Solved;
    end if;
    Log.Write ("Parameters: " & Parameters);
    The_Filename := [Name];
    Cleanup;
    The_Process_Id := Os.Process.Created (Executable     => Executable,
                                          Parameters     => Parameters,
                                          Current_Folder => Directory);
    Is_Solving := True;
  exception
  when Not_Solved =>
    Cleanup;
    raise;
  when Os.Process.Creation_Failure =>
    Cleanup;
    Log.Error ("Process creation failed for " & Executable);
    raise Not_Solved;
  when Item: others =>
    Log.Termination (Item);
    raise Not_Solved;
  end Solve;


  function Solved (The_Ra  : out Angle.Degrees;
                   The_Dec : out Angle.Degrees) return Boolean is
    CRVAL : Location;
    Unused_CD : Matrix;
    Unused_Size : Pixels;
    Result : constant Boolean := Solved (CRVAL, Unused_CD, Unused_Size);
  begin
    The_Ra := CRVAL(Ra);
    The_Dec := CRVAL(Dec);
    return Result;
  end Solved;


  function Solved (CRVAL : out Location;
                   CD    : out Matrix;
                   Size  : out Pixels) return Boolean is

    procedure Handle_Result is

      Result_Handle : constant Configuration.File_Handle := Configuration.Handle_For (Result_Filename);
      Null_Section  : constant Configuration.Section_Handle := Configuration.Handle_For (Result_Handle);
      CRVAL1        : constant String := Configuration.Value_Of (Null_Section, "CRVAL1");
      CRVAL2        : constant String := Configuration.Value_Of (Null_Section, "CRVAL2");
      CD1_1         : constant String := Configuration.Value_Of (Null_Section, "CD1_1");
      CD1_2         : constant String := Configuration.Value_Of (Null_Section, "CD1_2");
      CD2_1         : constant String := Configuration.Value_Of (Null_Section, "CD2_1");
      CD2_2         : constant String := Configuration.Value_Of (Null_Section, "CD2_2");
      Dimensions    : constant String := Configuration.Value_Of (Null_Section, "DIMENSIONS");
      Error         : constant String := Configuration.Value_Of (Null_Section, "ERROR");
      Warning       : constant String := Configuration.Value_Of (Null_Section, "WARNING");

    begin
      if Error /= "" then
        Log.Warning (Error);
        raise Not_Solved;
      end if;
      if Warning /= "" then
        Log.Warning (Warning);
      end if;
      CRVAL := [Angle.Degrees'value(CRVAL1), Angle.Degrees'value(CRVAL2)];
      CD := [[Angle.Degrees'value(CD1_1), Angle.Degrees'value(CD1_2)],
             [Angle.Degrees'value(CD2_1), Angle.Degrees'value(CD2_2)]];
      declare
        Dimension_Images : constant Strings.Item := Strings.Item_Of (Dimensions, Separator => 'x');
      begin
        Size := [Positive'value(Dimension_Images(Ra)), Positive'value(Dimension_Images(Dec))];
      end;
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
        Log.Write ("Solution found");
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
    Log.Warning ("No solution found");
    raise Not_Solved;
  when Item: others =>
    Is_Solving := False;
    Log.Termination (Item);
    raise Not_Solved;
  end Solved;


  procedure Stop is
  begin
    if Is_Solving then
      Log.Write ("Stop");
      Terminate_Process;
      Cleanup;
      Is_Solving := False;
    end if;
  end Stop;

end Astap;
