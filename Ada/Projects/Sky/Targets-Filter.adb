-- *********************************************************************************************************************
-- *                           (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Gui.Enumeration_Menu_Of;
with Lexicon;
with Traces;

package body Targets.Filter is

  package Log is new Traces ("Targets.Filter");

  function Image_Of (The_Switch : Switch) return String is
  begin
    case The_Switch is
    when Off =>
      return Lexicon.Image_Of (Lexicon.Off);
    when On =>
      return Lexicon.Image_Of (Lexicon.On);
    end case;
  end Image_Of;

  package Sort_Menu is new Gui.Enumeration_Menu_Of (Switch, Gui.Radio, Image_Of);

  procedure Sort_Handler (Sorted : Switch) is
  begin
    Log.Write ("Sorted: " & Sorted'img);
    Set (Sorted);
    Define_Catalog;
  exception
  when others =>
    Log.Error ("Sort_Handler");
  end Sort_Handler;


  User_Signal_Update : Update_Signal;


  package Selection_Menu is new Gui.Enumeration_Menu_Of (Selection, Gui.Radio, Targets.Image_Of);

  procedure Selection_Handler (The_Filter : Selection) is
  begin
    Log.Write ("Filter: " & The_Filter'img);
    Set (The_Selection => The_Filter);
    User_Signal_Update.all;
  exception
  when others =>
    Log.Error ("Selection_Handler");
  end Selection_Handler;


  type Direction is (None, North, East, South, West);

  function Image_Of (Item : Direction) return String is
  begin
    case Item is
    when None =>
      return "-";
    when North =>
      return Lexicon.Image_Of (Lexicon.North);
    when East =>
      return Lexicon.Image_Of (Lexicon.East);
    when South =>
      return Lexicon.Image_Of (Lexicon.South);
    when West =>
      return Lexicon.Image_Of (Lexicon.West);
    end case;
  end Image_Of;


  package Direction_Menu is new Gui.Enumeration_Menu_Of (Direction, Gui.Radio, Image_Of);


  procedure Direction_Handler (The_Direction : Direction) is

    type Ranges is array (Direction) of Az_Range;

    Az_Ranges : constant Ranges := [None   => (Angle.North, Angle.North),
                                    North  => (Angle.West, Angle.East),
                                    East   => (Angle.North, Angle.South),
                                    South  => (Angle.East, Angle.West),
                                    West   => (Angle.South, Angle.North)];
  begin -- Direction_Handler
    Log.Write ("Filter: " & The_Direction'img);
    Set (Az_Ranges(The_Direction));
    User_Signal_Update.all;
  exception
  when others =>
    Log.Error ("Direction_Handler");
  end Direction_Handler;


  procedure Create_Menu (Signal_Update : Update_Signal) is
    Sort_In_Altitude : constant String := "↑";
  begin
    User_Signal_Update := Signal_Update;
    Sort_Menu.Create (Sort_In_Altitude, Sort_Handler'access);
    Selection_Menu.Create (Lexicon.Image_Of (Lexicon.Selection), Selection_Handler'access);
    Set (The_Selection => All_Objects);
    Direction_Menu.Create (Lexicon.Image_Of (Lexicon.Direction), Direction_Handler'access);
  end Create_Menu;

end Targets.Filter;
