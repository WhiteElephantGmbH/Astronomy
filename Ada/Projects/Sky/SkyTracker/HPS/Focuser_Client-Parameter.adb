-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

package body Focuser_Client.Parameter is

  Ip_Address_Key    : constant String := Section.Ip_Address_Key;
  Port_Key          : constant String := Section.Port_Key;
  Home_Position_Key : constant String := "Home Position";
  Backlash_Key      : constant String := "Backlash";


  function Distance_For (Key : String) return Focuser.Distance is
    Value : constant Integer := Section.Value_Of (Key);
  begin
    return Focuser.Distance (Value);
  exception
  when others =>
    Error.Raise_With (Key & Value'image & " out of range");
  end Distance_For;


  function Lash_For (Key : String) return Focuser.Lash is
    Value : constant Integer := Section.Value_Of (Key);
  begin
    return Focuser.Lash (Value);
  exception
  when others =>
    Error.Raise_With (Key & Value'image & " out of range");
  end Lash_For;


  procedure Define (Handle : Configuration.File_Handle) is
  begin
    Section.Set (Configuration.Handle_For (Handle, Id));
    if Section.Exists then
      The_Server_Exists := True;
      The_Client_Address := Section.Ip_Address_For (Id);
      The_Client_Port := Section.Port_For (Id);
      The_Home_Position := Distance_For (Home_Position_Key);
      The_Backlash := Lash_For (Backlash_Key);
    end if;
  end Define;


  procedure Defaults (Put : access procedure (Item : String)) is
  begin
    Put ("[" & Id & "]");
    Put (Ip_Address_Key & "    = 169.254.42.44");
    Put (Port_Key & "          =" & Celestron.Focuser.Default_Port_Number'image);
    Put (Home_Position_Key & " =" & Celestron.Focuser.Default_Home_Position'image);
    Put (Backlash_Key & "      =" & Celestron.Focuser.Default_Backlash'image);
  end Defaults;

end Focuser_Client.Parameter;
