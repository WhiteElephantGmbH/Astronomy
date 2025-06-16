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

private with AWS.Response;
private with GNATCOLL.JSON;
private with Network;
private with Text;
private with Traces;

package Http_Server is

  type Control_Data is record
    Window_Minimized : Boolean := False;
  end record;

  procedure Set (Data : Control_Data);

  procedure Shutdown;

private

  procedure Start (Callback : AWS.Response.Callback);

  package JS renames GNATCOLL.JSON;

  procedure Set_Control_Values (Info : JS.JSON_Value);

  Id : constant String := "Http_Server";

  package Log is new Traces (Id);

  The_Server_Port     : Network.Port_Number;
  The_Client_Filename : Text.String;

end Http_Server;
