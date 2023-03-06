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

package body Object.Star is

  function Direction_Of (Item : Id) return Space.Direction is
  begin
    return Direction_Of (Ra_J2000   => Object.Catalog.Ra_J2000_Of (Item),
                         Dec_J2000  => Object.Catalog.Dec_J2000_Of (Item),
                         Ra_Motion  => Object.Catalog.Ra_Motion_Of (Item),
                         Dec_Motion => Object.Catalog.Dec_Motion_Of (Item));
  end Direction_Of;


  function Magnitude_Of (Item : Id) return Magnitude is
  begin
    return Object.Catalog.V_Mag_Of (Item);
  end Magnitude_Of;


  function Spectral_Type_Of (Item : Id) return Spectral_Type is
  begin
    return Object.Catalog.Spec_Type_Of (Item);
  end Spectral_Type_Of;


  function Class_Of (Item : Id) return Class is
  begin
    return Spectral_Type_Of (Item).Class;
  end Class_Of;

end Object.Star;
