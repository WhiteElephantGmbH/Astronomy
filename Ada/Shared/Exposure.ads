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

private with Ada.Strings.Text_Buffers;

package Exposure is

  Time_Delta : constant := 0.000_001;

  type Duration is delta Time_Delta range Time_Delta .. 4194.0 with Small => Time_Delta, Size  => 32;

  type Kind is (From_Camera, Tv_Mode, Timer_Mode);

  type Tv is (
    Tv_30_S,
    Tv_25_S,
    Tv_20_S,
    Tv_15_S,
    Tv_13_S,
    Tv_10_S,
    Tv_8_S,
    Tv_6_S,
    Tv_5_S,
    Tv_4_S,
    Tv_3_2_S,
    Tv_2_5_S,
    Tv_2_S,
    Tv_1_6_S,
    Tv_1_3_S,
    Tv_1_S,
    Tv_0_8_S,
    Tv_0_6_S,
    Tv_0_5_S,
    Tv_0_4_S,
    Tv_0_3_S,
    Tv_D_4_S,
    Tv_D_5_S,
    Tv_D_6_S,
    Tv_D_8_S,
    Tv_D_10_S,
    Tv_D_13_S,
    Tv_D_15_S,
    Tv_D_20_S,
    Tv_D_25_S,
    Tv_D_30_S,
    Tv_D_40_S,
    Tv_D_50_S,
    Tv_D_60_S,
    Tv_D_80_S,
    Tv_D_100_S,
    Tv_D_125_S,
    Tv_D_160_S,
    Tv_D_200_S,
    Tv_D_250_S,
    Tv_D_320_S,
    Tv_D_400_S,
    Tv_D_500_S,
    Tv_D_640_S,
    Tv_D_800_S,
    Tv_D_1000_S,
    Tv_D_1250_S,
    Tv_D_1600_S,
    Tv_D_2000_S,
    Tv_D_2500_S,
    Tv_D_3200_S,
    Tv_D_4000_S,
    Tv_D_5000_S,
    Tv_D_6400_S,
    Tv_D_8000_S,
    TV_D_10000_S,
    TV_D_12800_S,
    TV_D_16000_S,
    TV_D_20000_S,
    TV_D_25600_S,
    TV_D_32000_S);

  type Item is tagged private;

  function Value (Image : String) return Item;

  function Value (The_Time : Duration) return Item;

  function Mode (The_Item : Item) return Kind;

  function Time_Value (The_Item : Item) return Tv;

  function Time (The_Item : Item) return Duration;

  function From_Camera return Item;

private

  type Item is tagged record
    Mode       : Kind     := From_Camera;
    Time_Value : Duration := Duration'last;
    T_Value    : Tv       := Tv'last;
  end record
  with
    Put_Image => Put_Image;

  procedure Put_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                       V :        Item);

end Exposure;
