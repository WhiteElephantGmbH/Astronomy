-- *********************************************************************************************************************
-- *                               (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package body Moon.Parameter is

  Minimum_Sun_Altitude_Key : constant String := "Minimum Sun Altitude";
  Maximum_Sun_Altitude_Key : constant String := "Maximum Sun Altitude";


  procedure Define (Handle : Configuration.File_Handle) is
    use type Angle.Degrees;
  begin
    Section.Set (Configuration.Handle_For (Handle, Id));
    Define (Min_Sun_Altitude => Section.Degrees_Of (Minimum_Sun_Altitude_Key, Minimum => -90.0, Maximum => 0.0),
            Max_Sun_Altitude => Section.Degrees_Of (Maximum_Sun_Altitude_Key, Maximum => 90.0));
  end Define;


  procedure Defaults (Put : access procedure (Item : String)) is
  begin
    Put ("[" & Id & "]");
    Put (Minimum_Sun_Altitude_Key & " = -2.0" & Angle.Degree);
    Put (Maximum_Sun_Altitude_Key & " = 15.0" & Angle.Degree);
  end Defaults;

end Moon.Parameter;
