-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Language.Parameter;
with Moon.Parameter;
with Picture.Parameter;
with Section;
with Stellarium.Parameter;
with Traces;

package body Parameter is

  package Log is new Traces ("Parameter");

  Filename : constant String := Application.Composure (Application.Name, "ini");

  M_Zero_Id      : constant String := "M-Zero";
  Ip_Address_Key : constant String := Section.Ip_Address_Key;
  Port_Key       : constant String := Section.Port_Key;

  -- M_Zero
  The_M_Zero_Ip_Address : Network.Ip_Address;
  The_M_Zero_Port       : Network.Port_Number;

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
      Language.Parameter.Defaults (Put'access); -- must be first
      Put ("");
      Put ("[" & M_Zero_Id & "]");
      Put (Ip_Address_Key & " = 192.168.4.1");
      Put (Port_Key & "       = 4040");
      Put ("");
      Moon.Parameter.Defaults (Put'access);
      Put ("");
      Picture.Parameter.Defaults (Put'access);
      Put ("");
      Stellarium.Parameter.Defaults (Put'access);
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
      Handle        : constant Configuration.File_Handle    := Configuration.Handle_For (Filename);
      M_Zero_Handle : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, M_Zero_Id);
    begin
      Language.Parameter.Define (Handle);

      Section.Set (M_Zero_Handle);
      declare
        IP_Address_Image : constant String := Section.String_Of (Ip_Address_Key);
      begin
        The_M_Zero_Ip_Address := Network.Ip_Address_Of (IP_Address_Image);
        Log.Write ("M-Zero IP Address: " & IP_Address_Image);
      exception
      when others =>
        Error.Raise_With ("Incorrect M-Zero IP Address: " & IP_Address_Image);
      end;
      begin
        The_M_Zero_Port := Network.Port_Number (Section.Value_Of (Port_Key));
        Log.Write ("M-Zero Port:" & The_M_Zero_Port'img);
      exception
      when others =>
        Error.Raise_With ("M-Zero port number out of range");
      end;
      Moon.Parameter.Define (Handle);
      Picture.Parameter.Define (Handle);
      Stellarium.Parameter.Define (Handle, With_Satellites => False);
    end Read_Values;

  begin -- Read
    if not File.Exists (Filename) then
      Create_Default_Parameters;
    end if;
    Read_Values;
  end Read;


  ------------
  -- M_Zero --
  ------------

  function M_Zero_Ip_Address return Network.Ip_Address is
  begin
    return The_M_Zero_Ip_Address;
  end M_Zero_Ip_Address;


  function M_Zero_Port return Network.Port_Number is
  begin
    return The_M_Zero_Port;
  end M_Zero_Port;

end Parameter;
