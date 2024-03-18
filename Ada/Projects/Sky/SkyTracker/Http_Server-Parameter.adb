-- *********************************************************************************************************************
-- *                            (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                             *
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
with File;
with Os.System;
with Section;

package body Http_Server.Parameter is

  Gui_Client_Key : constant String := "GUI Client";
  Port_Key       : constant String := Section.Port_Key;


  procedure Define (Handle : Configuration.File_Handle) is
  begin
    Section.Set (Configuration.Handle_For (Handle, Id));
    The_Server_Port :=  Section.Port_For (Id);
    declare
      Client_Filename : constant String := Section.String_Value_Of (Gui_Client_Key);
    begin
      if Client_Filename /= "" then
        Log.Write ("GUI client program file: """ & Client_Filename & """");
        if not File.Exists (Client_Filename) then
          Error.Raise_With ("GUI client program file """ & Client_Filename & """ not found");
        end if;
      end if;
      The_Client_Filename := [Client_Filename];
    end;
  end Define;


  procedure Defaults (Put    : access procedure (Item : String);
                      Client : String) is
  begin
    Put ("[" & Id & "]");
    Put (Port_Key & "       = 9000");
    Put (Gui_Client_Key & " = " & Os.System.Program_Files_Folder & "White_Elephant\" & Client & ".exe");
  end Defaults;

end Http_Server.Parameter;
