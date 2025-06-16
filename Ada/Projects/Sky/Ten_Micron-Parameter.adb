-- *********************************************************************************************************************
-- *                           (c) 2024 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                      *
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

with Section;
with Text;

package body Ten_Micron.Parameter is

  Expert_Mode_Key : constant String := "Expert Mode";
  Ip_Address_Key  : constant String := Section.Ip_Address_Key;
  Port_Key        : constant String := Section.Port_Key;

  procedure Define (Handle : Configuration.File_Handle) is
  begin
    Section.Set (Configuration.Handle_For (Handle, Id));
    Is_In_Expert_Mode := Text.Matches (Section.String_Value_Of (Expert_Mode_Key), "True");
    Log.Write ("Expert Mode: " & Is_In_Expert_Mode'image);
    The_Server_Address := Section.Ip_Address_For (Id);
    The_Server_Port := Section.Port_For (Id);
  end Define;


  procedure Defaults (Put        : access procedure (Item : String);
                      Ip_Address : String;
                      Port       : Natural) is
  begin
    Put ("[" & Id & "]");
    Put (Expert_Mode_Key & " = False");
    Put (Ip_Address_Key & "  = " & Ip_Address);
    Put (Port_Key & "        =" & Port'image);
  end Defaults;

end Ten_Micron.Parameter;
