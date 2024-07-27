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
with Gui.Radio_Menu_Of;
with Lexicon;
with Text;
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

  package Selection_Menu is new Gui.Radio_Menu_Of (Selection_Group);

  procedure Selection_Handler (The_Group  : Selection_Group;
                               The_Filter : String) is
  begin
    Log.Write ("Group  : " & The_Group'image);
    Log.Write ("Filter : " & The_Filter);
    case The_Group is
    when Default =>
      declare
        Filter : constant Selection := Selection_Of (The_Filter);
      begin
        Log.Write ("Filter: " & Filter'image);
        Set (The_Selection => Filter);
        User_Signal_Update.all;
      end;
    when Moon =>
      declare
        Filter : constant Moon_Feature_Selection := Feature_Of (The_Filter);
      begin
        Log.Write ("Filter: " & Filter'image);
        Set (The_Feature => Filter);
        User_Signal_Update.all;
      end;
    when Neo =>
      null;
    end case;
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


  procedure Set (Group : Selection_Group) is
  begin
    Log.Write ("Set Group: " & Group'img);
    Selection_Menu.Set (Group);
    case Group is
    when Default =>
      Set (The_Selection => All_Objects);
    when Moon =>
      Set (The_Feature => All_Features);
    when Neo =>
      null;
    end case;
  end Set;


  function Selection_Images return Selection_Menu.Images is

    All_Objects : constant String := Lexicon.Image_Of(Lexicon.All_Objects);

    Default_Selection_Images : Text.List;
    Moon_Selection_Images    : Text.List;
    Neo_Selection_Images     : constant Text.Vector := [All_Objects];

  begin
    for Element in Selection loop
      Default_Selection_Images.Append (Image_Of (Element));
    end loop;
    for Element in Moon_Feature_Selection loop
      Moon_Selection_Images.Append (Image_Of (Element));
    end loop;
    return [Default  => Default_Selection_Images.Sorted.To_Vector,
            Moon     => Moon_Selection_Images.Sorted.To_Vector,
            Neo      => Neo_Selection_Images];
  end Selection_Images;


  procedure Create_Menu (Signal_Update : Update_Signal) is
    Sort_In_Altitude : constant String := "↑";
  begin
    User_Signal_Update := Signal_Update;
    Sort_Menu.Create (Sort_In_Altitude, Sort_Handler'access);
    Selection_Menu.Create (Lexicon.Image_Of (Lexicon.Selection), Selection_Images, Selection_Handler'access);
    Set (The_Selection => All_Objects);
    Direction_Menu.Create (Lexicon.Image_Of (Lexicon.Direction), Direction_Handler'access);
  end Create_Menu;

end Targets.Filter;
