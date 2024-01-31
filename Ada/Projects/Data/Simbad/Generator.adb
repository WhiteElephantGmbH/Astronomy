-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Command_Line;
with Exceptions;
with Generator.Data;
with Database;

package body Generator is

  procedure Execute is
    Max_Star_Magnitude : Database.Magnitude;
  begin
    case Ada.Command_Line.Argument_Count is
    when 0 =>
      Max_Star_Magnitude := 10.0;
    when 1 =>
      declare
        Magnitude : constant String := Ada.Command_Line.Argument(1);
      begin
        Max_Star_Magnitude := Database.Magnitude'value(Magnitude);
      exception
      when others =>
        Error ("Incorrect command line argument for Magnitude value: " & Magnitude);
      end;
    when others =>
      Error ("Too many command line arguments.");
    end case;
    Data.Read (Max_Star_Magnitude);
    Data.Generate;
  exception
  when Error_Occured =>
    null;
  when Item: others =>
    Error (Exceptions.Information_Of (Item));
  end Execute;


  procedure Error (Text : String) is
  begin
    IO.Put_Line ("*** " & Text & (if The_Line_Number = 0 then "" else " at line" & The_Line_Number'image));
    raise Error_Occured;
  end Error;

end Generator;
