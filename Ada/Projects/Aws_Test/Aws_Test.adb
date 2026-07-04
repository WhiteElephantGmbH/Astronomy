-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
pragma Style_Astronomy;

pragma Build (Description => "Test AWS",
              Version     => (1, 0, 0, 1),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWS"),
              Compiler    => "GNAT\14.2");
with Ada.Text_IO;
with Ada.Exceptions;
with AWS.Client;
with AWS.Default;
with AWS.Response;

procedure Aws_Test is
pragma Linker_Options ("-lssl");
pragma Linker_Options ("-lcrypto");
  Result : AWS.Response.Data;

begin
  Ada.Text_IO.Put_Line ("Before Get");
  Ada.Text_IO.Put_Line ("Client_Certificate = " & AWS.Default.Client_Certificate);
  Ada.Text_IO.Put_Line ("Trusted_CA = " & AWS.Default.Trusted_CA);
  Result := AWS.Client.Get ("https://www.google.com");
  Ada.Text_IO.Put_Line ("After Get:" & AWS.Response.Status_Code (Result)'image);
  Ada.Text_IO.Put_Line ("Content-Type = " & AWS.Response.Content_Type (Result));
exception
when E : others =>
  Ada.Text_IO.Put_Line ("Exception:");
  Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
  Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
  Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
end Aws_Test;
