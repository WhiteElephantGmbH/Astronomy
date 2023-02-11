-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Eps;
with Star;

package Map is

  type Paper_Format is (A0, A1, A2, A3, A4, A5, A6);

  procedure Draw (Header_Text   : String;
                  Footer_Text   : String;
                  Format        : Paper_Format;
                  Line_Size     : Eps.Value;
                  Star_Min      : Eps.Value;
                  Star_Max      : Eps.Value;
                  Magnitude_Min : Star.Magnitude;
                  Magnitude_Max : Star.Magnitude);

end Map;
