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

with Text;

package body Exposure is

  function Value (Image : String) return Item is
    Parts        : constant Text.Strings := Text.Strings_Of (Image, Separator => '/');
    The_Duration : Duration;
  begin
    The_Duration := Duration'value(Parts(1));
    if Parts.Count = 2 then
      The_Duration := @ / Duration'value(Parts(2));
    end if;
    return The_Item : Item do
      The_Item := Value (The_Duration);
    end return;
  end Value;


  function Value (The_Time : Duration) return Item is
    Tus      : constant Natural := Natural(The_Time * 1_000_000.0); -- in Microseconds
    The_Tv   : Tv;
    The_Item : Item;
  begin
    The_Item.Time_Value := The_Time;
    case Tus is
    when  0_800_000 =>
      The_Tv := Tv_0_8_S;
    when  0_600_000 =>
      The_Tv := Tv_0_6_S;
    when  0_500_000 =>
      The_Tv := Tv_0_5_S;
    when  0_400_000 =>
      The_Tv := Tv_0_4_S;
    when  0_300_000 =>
      The_Tv := Tv_0_3_S;
    when  0_250_000 =>
      The_Tv := Tv_D_4_S;
    when  0_200_000 =>
      The_Tv := Tv_D_5_S;
    when  0_166_666 =>
      The_Tv := Tv_D_6_S;
    when  0_125_000 =>
      The_Tv := Tv_D_8_S;
    when  0_100_000 =>
      The_Tv := Tv_D_10_S;
    when  0_076_923 =>
      The_Tv := Tv_D_13_S;
    when  0_066_666 =>
      The_Tv := Tv_D_15_S;
    when  0_050_000 =>
      The_Tv := Tv_D_20_S;
    when  0_040_000 =>
      The_Tv := Tv_D_25_S;
    when  0_033_333 =>
      The_Tv := Tv_D_30_S;
    when  0_025_000 =>
      The_Tv := Tv_D_40_S;
    when  0_020_000 =>
      The_Tv := Tv_D_50_S;
    when  0_016_666 =>
      The_Tv := Tv_D_60_S;
    when  0_012_500 =>
      The_Tv := Tv_D_80_S;
    when  0_010_000 =>
      The_Tv := Tv_D_100_S;
    when  0_008_000 =>
      The_Tv := Tv_D_125_S;
    when  0_006_250 =>
      The_Tv := Tv_D_160_S;
    when  0_005_000 =>
      The_Tv := Tv_D_200_S;
    when  0_004_000 =>
      The_Tv := Tv_D_250_S;
    when  0_003_125 =>
      The_Tv := Tv_D_320_S;
    when  0_002_500 =>
      The_Tv := Tv_D_400_S;
    when  0_002_000 =>
      The_Tv := Tv_D_500_S;
    when  0_001_562 =>
      The_Tv := Tv_D_640_S;
    when  0_001_250 =>
      The_Tv := Tv_D_800_S;
    when  0_001_000 =>
      The_Tv := Tv_D_1000_S;
    when  0_000_800 =>
      The_Tv := Tv_D_1250_S;
    when  0_000_625 =>
      The_Tv := Tv_D_1600_S;
    when  0_000_500 =>
      The_Tv := Tv_D_2000_S;
    when  0_000_400 =>
      The_Tv := Tv_D_2500_S;
    when  0_000_312 =>
      The_Tv := Tv_D_3200_S;
    when  0_000_250 =>
      The_Tv := Tv_D_4000_S;
    when  0_000_200 =>
      The_Tv := Tv_D_5000_S;
    when  0_000_156 =>
      The_Tv := Tv_D_6400_S;
    when  0_000_125 =>
      The_Tv := Tv_D_8000_S;
    when  0_000_100 =>
      The_Tv := TV_D_10000_S;
    when  0_000_078 =>
      The_Tv := TV_D_12800_S;
    when  0_000_062 =>
      The_Tv := TV_D_16000_S;
    when  0_000_050 =>
      The_Tv := TV_D_20000_S;
    when  0_000_039 =>
      The_Tv := TV_D_25600_S;
    when  0_000_031 =>
      The_Tv := TV_D_32000_S;
    when others =>
      The_Item.Mode := Timer_Mode;
      return The_Item;
    end case;
    The_Item.T_Value := The_Tv;
    The_Item.Mode := Tv_Mode;
    return The_Item;
  end Value;


  function Mode (The_Item : Item) return Kind is
  begin
    return The_Item.Mode;
  end Mode;


  function Time_Value (The_Item : Item) return Tv is
  begin
    return The_Item.T_Value;
  end Time_Value;


  function Time (The_Item : Item) return Duration is
  begin
    return The_Item.Time_Value;
  end Time;


  function From_Camera return Item is
  begin
    return (Mode   => From_Camera,
            others => <>);
  end From_Camera;


  procedure Put_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                       V :        Item) is
  begin
    case V.Mode is
    when From_Camera =>
      S.Put ("[]");
    when Tv_Mode =>
      declare
        Image : constant String := V.T_Value'image;
      begin
        case V.T_Value is
        when Tv_0_8_S .. Tv_0_3_S =>
          S.Put ([Image(Image'first + 3), '.', Image(Image'last - 2)]);
        when Tv_D_4_S .. TV_D_32000_S =>
          S.Put ("1/" & Image(Image'first + 5 .. Image'last - 2));
        end case;
      exception
      when others =>
        S.Put (Image);
      end;
    when Timer_Mode =>
      declare
        Image : constant String := V.Time_Value'image;
      begin
        S.Put (Image(Image'first + 1 .. Image'last));
      end;
    end case;
  exception
  when others =>
    S.Put ("???");
  end Put_Image;

end Exposure;
