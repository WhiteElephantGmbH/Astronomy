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

with AWS.Server;
with Protected_Storage;
with Os.Process;

package body Http_Server is

  package Protected_Control is new Protected_Storage (Control_Data);

  use type Text.String;

  The_Server : AWS.Server.HTTP;

  procedure Start (Callback : AWS.Response.Callback) is
  begin
    if Text.Is_Null (The_Client_Filename) then
      Log.Warning ("No GUI client");
    else
      declare
        Client_Filename : constant String := +The_Client_Filename;
      begin
        Log.Write ("Start " & Client_Filename);
        AWS.Server.Start (Web_Server => The_Server,
                          Name       => "Skytracker",
                          Callback   => Callback,
                          Port       => Natural(The_Server_Port));
        Os.Process.Create (Client_Filename);
      exception
      when others =>
        Log.Error ("GUI client not started");
      end;
    end if;
  end Start;


  procedure Set (Data : Control_Data) renames Protected_Control.Set;


  procedure Set_Control_Values (Info : JS.JSON_Value) is
    Control : constant JS.JSON_Value := JS.Create_Object;
    Data    : constant Control_Data  := Protected_Control.Data;
  begin
    JS.Set_Field (Control, "window_minimized", JS.Create (Data.Window_Minimized));
    JS.Set_Field (Info, "control", Control);
  end Set_Control_Values;


  procedure Shutdown is
  begin
    if not Text.Is_Null (The_Client_Filename) then
      Log.Write ("Shutdown");
      AWS.Server.Shutdown (The_Server);
    end if;
  exception
  when others =>
    null;
  end Shutdown;

end Http_Server;
