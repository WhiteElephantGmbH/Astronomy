-- *********************************************************************************************************************
-- *                       (c) 2010 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Map;

package body Generator is

  package IO renames Ada.Text_IO;

  procedure Error_Handler (Message : String) is
  begin
    IO.Put_Line ("### " & Message & " ###");
  end Error_Handler;


  package Star_Map is new Map (Error_Handler);


  procedure Execute is
    use type Star_Map.Star.Magnitude;
  begin
    IO.Put_Line ("Star Map Generator");
    Star_Map.Draw (Declination              => 47.5,
                   Margin                   => 12.5,
                   Map_Size                 => 570.0,
                   Constellation_Line_Width => 0.4,
                   Ecliptic_Line_Width      => 0.8,
                   Equator_Line_Width       => 0.8,
                   Visibility_Line_Width    => 0.8,
                   Brightness    => (Limit  => -1.5,
                                     Filter => 6.0,
                                     Min    => 0.3,
                                     Max    => 4.0));
  end Execute;

end Generator;
