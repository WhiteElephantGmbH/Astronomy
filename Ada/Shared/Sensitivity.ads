-- *********************************************************************************************************************
-- *                       (c) 2025 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

private with Ada.Strings.Text_Buffers;

package Sensitivity is

  type Gain is new Natural range 0 .. 140;

  type Offset is new Natural range 0 .. 255;

  type Iso is new Natural range 6 .. 819200 with
    Static_Predicate => Iso in  6 |    12 |                     25 |                     50 |
                              100 |   125 |   160 |    200 |   250 |   320 |    400 |   500 |   640 |    800 |
                             1000 |  1250 |  1600 |   2000 |  2500 |  3200 |   4000 |  5000 |  6400 |   8000 |
                            10000 | 12800 | 16000 |  20000 | 25600 | 32000 |  40000 | 51200 | 64000 |  80000 |
                           102400 |                 204800 |                 409600 |                 819200;

  type Item is tagged private;

  function Default return Item;

  function Value (Iso_Value : Iso) return Item;

  function Value (Gain_Value   : Gain;
                  Offset_Value : Offset := 0) return Item;

  function Value (Image : String) return Item;

  function Is_Default (The_Item : Item) return Boolean;

  function Is_Iso_Or_Default (The_Item : Item) return Boolean;

  function Is_Gain_And_Offset_Or_Default (The_Item : Item) return Boolean;

  function Value (The_Item : Item) return Iso;

  function Value (The_Item : Item) return Gain;

  function Value (The_Item : Item) return Offset;

private

  type Value_Kind is (Is_Default, Is_Iso, Is_Gain_And_Offset);

  type Item is tagged record
    Kind        : Value_Kind := Is_Default;
    Value       : Natural := 26; -- Gain unity
    With_Offset : Offset := 30;
  end record
  with
    Put_Image => Put_Image;

  procedure Put_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                       V :        Item);

end Sensitivity;
