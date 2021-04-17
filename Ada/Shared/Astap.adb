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
with Traces;

package body Astap is

  package Log is new Traces ("Astap");


  procedure Solve (Filename :        String;
                   Height   :        Degrees;
                   Ra       : in out Degrees;
                   Dec      : in out Degrees) is

    Directory : constant String := File.Containing_Directory_Of (Filename);
    Base_Name : constant String := File.Base_Name_Of (Filename);

    Result_Filename : constant String := File.Composure (Directory, Base_Name, "ini");
    Wcs_Filename    : constant String := File.Composure (Directory, Base_Name, "wcs");

    procedure Cleanup is
    begin
      File.Delete (Result_Filename);
      File.Delete (Wcs_Filename);
    end Cleanup;

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
    Parameters   : constant String
      := "-f " & Filename & " -ra" & Ra_Image & " -spd" & Spd_Image & " -fov" & Height_Image & " -r 180";

  begin -- Solve
    Log.Write ("Parameters: " & Parameters);
    Cleanup;
    declare
      Output : constant String := Os.Process.Execution_Of (Executable => "astap",
                                                           Parameters => Parameters);
      Timeout    : constant Duration := 20.0;
      Delay_Time : constant Duration := 0.2;

      The_Time : Duration := 0.0;

    begin
      while not File.Exists (Result_Filename) loop
        delay Delay_Time;
        The_Time := The_Time + Delay_Time;
        if The_Time > Timeout then
          if Output /= "" then -- Windows has no output
            Log.Error (Output);
          else
            Log.Error ("Timeout");
          end if;
          raise Not_Solved;
        end if;
      end loop;
    end;
    Handle_Result;
    Cleanup;
  exception
  when Not_Solved =>
    Cleanup;
    raise;
  when Item: others =>
    Log.Termination (Item);
    raise Not_Solved;
  end Solve;

end Astap;
