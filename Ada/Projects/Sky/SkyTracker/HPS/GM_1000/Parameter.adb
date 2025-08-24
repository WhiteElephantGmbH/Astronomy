-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Alignment.Parameter;
with Application;
with Camera.Parameter;
with Clock.Parameter;
with Configuration;
with Error;
with File;
with Focuser_Client.Parameter;
with Http_Server.Parameter;
with Language.Parameter;
with Moon.Parameter;
with Picture.Parameter;
with Stellarium.Parameter;
with Sun.Parameter;
with Ten_Micron.Parameter;
with Traces;

package body Parameter is

  package Log is new Traces ("Parameter");

  Filename : constant String := Application.Composure (Application.Name, "ini");

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
      Ten_Micron.Parameter.Defaults (Put'access, Ip_Address => "169.254.42.42", Port => 3490);
      Put ("");
      Focuser_Client.Parameter.Defaults (Put'access);
      Put ("");
       Http_Server.Parameter.Defaults (Put'access, "Handbox_HPS", Port => 9001);
      Put ("");
      Sun.Parameter.Defaults (Put'access);
      Put ("");
      Moon.Parameter.Defaults (Put'access);
      Put ("");
      Clock.Parameter.Defaults (Put'access);
      Put ("");
      Picture.Parameter.Defaults (Put'access, Height => "0.51", Width => "0.74");
      Put ("");
      Camera.Parameter.Defaults (Put'access);
      Put ("");
      Alignment.Parameter.Defaults (Put'access);
      Put ("");
      Stellarium.Parameter.Defaults (Put'access, "10micron");
      Put ("");
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
      Handle : constant Configuration.File_Handle := Configuration.Handle_For (Filename);
    begin
      Language.Parameter.Define (Handle); -- must be first
      Ten_Micron.Parameter.Define (Handle);
      Focuser_Client.Parameter.Define (Handle);
      Http_Server.Parameter.Define (Handle);
      Sun.Parameter.Define (Handle);
      Moon.Parameter.Define (Handle);
      Clock.Parameter.Define (Handle);
      Picture.Parameter.Define (Handle);
      Camera.Parameter.Define (Handle);
      Alignment.Parameter.Define (Handle);
      Stellarium.Parameter.Define (Handle);
    end Read_Values;

  begin -- Read
    if not File.Exists (Filename) then
      Create_Default_Parameters;
    end if;
    Read_Values;
  end Read;

end Parameter;
