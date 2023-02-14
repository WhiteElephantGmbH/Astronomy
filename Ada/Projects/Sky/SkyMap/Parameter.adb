-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Ada.Text_IO;
with Application;
with Configuration;
with Error;
with File;
with Strings;
with Traces;

package body Parameter is

  package Log is new Traces ("Parameter");

  Filename : constant String := Application.Composure (Application.Name, "ini");

  Stars_Id          : constant String := "Stars";
  Min_Size_Key      : constant String := "Min Size";
  Max_Size_Key      : constant String := "Max Size";
  Min_Magnitude_Key : constant String := "Min Magnitude";
  Max_Magnitude_Key : constant String := "Max Magnitude";

  Constellations_Id : constant String := "Constellations";
  Line_Size_Key     : constant String := "Line Size";

  The_Section : Configuration.Section_Handle;

  -- Stars
  The_Stars_Min_Size : Eps.Value;
  The_Stars_Max_Size : Eps.Value;
  The_Min_Magnitude  : Star.Magnitude;
  The_Max_Magnitude  : Star.Magnitude;

  -- Constellations
  The_Line_Size : Eps.Value;


  procedure Set (Section : Configuration.Section_Handle) is
  begin
    The_Section := Section;
  end Set;


  function String_Value_Of (Key : String) return String is
  begin
    return Configuration.Value_Of (The_Section, Key);
  exception
  when others =>
    return "";
  end String_Value_Of;


  function String_Of (Key : String) return String is
    Image : constant String := String_Value_Of (Key);
  begin
    if Image = "" then
      Error.Raise_With ("Parameter <" & Key & "> not defined");
    end if;
    return Image;
  end String_Of;


  function Value_Of (Key  : String) return Eps.Value is
    Item : constant String := String_Of (Key);
  begin
    return Eps.Value'value(Item);
  exception
  when others =>
    Error.Raise_With ("Incorrect " & Key & ": <" & Item & ">");
  end Value_Of;


  function Value_Of (Key  : String) return Star.Magnitude is
    Item : constant String := String_Of (Key);
  begin
    return Star.Magnitude'value(Item);
  exception
  when others =>
    Error.Raise_With ("Incorrect " & Key & ": <" & Item & ">");
  end Value_Of;


  procedure Read is

    procedure Create_Default_Parameters is

      The_File : Ada.Text_IO.File_Type;

      procedure Put (Line : String) is
      begin
        Ada.Text_IO.Put_Line (The_File, Line);
      end Put;

    begin -- Create_Default_Parameters
      begin
        Ada.Text_IO.Create (The_File, Name => Filename);
      exception
      when others =>
        Error.Raise_With ("Can't create " & Filename);
      end;
      Put (Strings.Bom_8 & "[" & Stars_Id & "]");
      Put (Min_Size_Key & "      = 1.0");
      Put (Max_Size_Key & "      = 5.0");
      Put (Min_Magnitude_Key & " =-1.0");
      Put (Max_Magnitude_Key & " = 6.0");
      Put ("");
      Put ("[" & Constellations_Id & "]");
      Put (Line_Size_Key & " = 1.0");
      Ada.Text_IO.Close (The_File);
    exception
    when Error.Occurred =>
      raise;
    when Item: others =>
      Log.Termination (Item);
      Ada.Text_IO.Delete (The_File);
      Error.Raise_With ("Internal Error - creating default parameters");
    end Create_Default_Parameters;


    procedure Read_Values is

      Handle         : constant Configuration.File_Handle    := Configuration.Handle_For (Filename);
      Stars          : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Stars_Id);
      Constellations : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Constellations_Id);

    begin -- Read_Values
      Set (Stars);
      Log.Write ("Stars");
      The_Stars_Min_Size := Value_Of (Min_Size_Key);
      Log.Write ("  Min Size      :" & The_Stars_Min_Size'image);
      The_Stars_Max_Size := Value_Of (Max_Size_Key);
      Log.Write ("  Max Size      :" & The_Stars_Max_Size'image);
      The_Min_Magnitude := Value_Of (Min_Magnitude_Key);
      Log.Write ("  Min Magnitude :" & The_Min_Magnitude'image);
      The_Max_Magnitude := Value_Of (Max_Magnitude_Key);
      Log.Write ("  Max Magnitude :" & The_Max_Magnitude'image);

      Set (Constellations);
      Log.Write ("Constellation");
      The_Line_Size := Value_Of (Line_Size_Key);
      Log.Write ("  Line Size :" & The_Line_Size'image);
    end Read_Values;

   begin -- Read
    Log.Write ("Read " & Filename);
    if not File.Exists (Filename) then
      Create_Default_Parameters;
    end if;
    Read_Values;
  end Read;


  function Line_Size return Eps.Value is (The_Line_Size);

  function Star_Min return Eps.Value is (The_Stars_Min_Size);

  function Star_Max return Eps.Value is (The_Stars_Max_Size);

  function Magnitude_Min return Star.Magnitude is (The_Min_Magnitude);

  function Magnitude_Max return Star.Magnitude is (The_Max_Magnitude);

end Parameter;
