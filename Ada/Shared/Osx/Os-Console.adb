-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
pragma Style_Astronomy;

with Ada.Text_IO;
with Log;

package body Os.Console is

  procedure Put (Item : String) renames Ada.Text_IO.Put;

  procedure Get (Item : out Character) renames Ada.Text_IO.Get_Immediate;

  procedure Get (Item : String) is
    The_Item : String(1..Item'length);
  begin
    for The_Character of The_Item loop
      Get (The_Character);
    end loop;
    if Item /= The_Item then
      Log.Write ("Os.Console.Get failed: received '" & The_Item & "' - expected '" & Item & "'");
    end if;
  end Get;


  function Get (Terminator : Character) return String is
    The_String    : String (1..3);
    The_Character : Character;
    The_Count     : Count := 0;
  begin
    loop
      Get (The_Character);
      exit when The_Character = Terminator;
      The_Count := The_Count + 1;
      The_String (The_Count) := The_Character;
    end loop;
    return The_String(1..The_Count);
  end Get;

  CSI : constant String := Ascii.Esc & '[';


  function Image_Of (Value : Count) return String is
    Image : constant String := Value'img;
  begin
    return Image(Image'first + 1 .. Image'last);
  end Image_Of;


  procedure Move_Cursor_To (The_Location : Location) is
  begin
    Put (CSI & Image_Of (The_Location.The_Row) & ';' & Image_Of (The_Location.The_Column) & 'H');
  end Move_Cursor_To;


  procedure Clear_Region (The_Region  : Region;
                          The_Display : Display) is
  begin
    if The_Region.With_Size = The_Display.Screen_Size then
      Put (CSI & "2J");
    else
      declare
        First_Row    : constant Count := The_Region.At_Location.The_Row;
        First_Column : constant Count := The_Region.At_Location.The_Column;
        Cleared_Line : constant String(1..The_Region.With_Size.Width) := (others => ' ');
      begin
        for The_Row in First_Row .. First_Row + The_Region.With_Size.Height - 1 loop
          Move_Cursor_To ((The_Row => The_Row, The_Column => First_Column));
          Put (Cleared_Line);
        end loop;
      end;
    end if;
  end Clear_Region;


  procedure Show_Cursor_At (The_Location     : Location;
                            The_Cursor_Style : Cursor_Style;
                            The_Display      : Display) is
    pragma Unreferenced (The_Display);
  begin
    Move_Cursor_To (The_Location);
    case The_Cursor_Style is
    when Insert_Cursor =>
      Put (CSI & "3 q");
    when Overstrike_Cursor =>
      Put (CSI & "0 q");
    when Out_Of_Bounds_Cursor =>
      Put (CSI & "4 q");
    end case;
  end Show_Cursor_At;


  procedure Hide_Cursor (The_Display : Display) is
    pragma Unreferenced (The_Display);
  begin
    Put (CSI & "?25h");
  end Hide_Cursor;


  function Actual_Cursor (The_Display : Display) return Location is
    pragma Unreferenced (The_Display);
  begin
    Put (CSI & "6n");
    Get (CSI);
    declare
      Actual_Column : constant String := Get (Terminator => ';');
      Actual_Row    : constant String := Get (Terminator => 'R');
    begin
      return (The_Column => Count'value(Actual_Column),
              The_Row    => Count'value(Actual_Row));
    end;
  exception
  when Item: others =>
    Log.Write (Item);
    return (The_Column => Column'first,
            The_Row    => Row'first);
  end Actual_Cursor;


  procedure Write (The_String    : String;
                   The_Location  : Location;
                   The_Attribute : Attribute;
                   The_Display   : Display) is
    pragma Unreferenced (The_Display);
  begin
    Move_Cursor_To (The_Location);
    case The_Attribute is
    when Normal =>
      Put (CSI & "0m");
    when Highlight =>
      Put (CSI & "7m");
    when Error  =>
      Put (CSI & "5m");
    end case;
    Put (The_String);
  end Write;


  function New_Display (The_Size    : Size;
                        Termination : Termination_Handler) return Display is
    pragma Unreferenced (Termination);
  begin
    return (Handle      => System.Null_Address,
            Screen_Size => The_Size);
  end New_Display;


  function Next_Character (The_Keyboard : Keyboard) return Character is
    pragma Unreferenced (The_Keyboard);
    The_Character : Character;
  begin
    loop
      Get (The_Character);
      case The_Character is
      when'<' =>
        return Escape;
      when Ascii.Del =>
        return Backspace;
      when Ascii.Lf =>
        return Enter;
      when Ascii.Esc =>
        Get (The_Character);
        case The_Character is
        when '[' =>
          Get (The_Character);
          case The_Character is
          when 'A' =>
            return Up_Arrow;
          when 'B' =>
            return Down_Arrow;
          when 'C' =>
            return Right_Arrow;
          when 'D' =>
            return Left_Arrow;
          when 'E' =>
            return Finish;
          when 'H' =>
            return Home;
          when 'I' =>
            return Insert;
          when '3' =>
            Get (The_Character);
            return Delete;
          when others =>
            Log.Write ("Next Character: '" & The_Character & "' - " & Character'pos(The_Character)'img);
          end case;
        when others =>
          Log.Write ("Next Character: '" & The_Character & "' - " & Character'pos(The_Character)'img);
        end case;
      when others =>
        return The_Character;
      end case;
    end loop;

  --Page_Up     : constant Character := Character'val(8#311#);
  --Page_Down   : constant Character := Character'val(8#321#);

  end Next_Character;


  function New_Keyboard return Keyboard is
  begin
    return (Handle => System.Null_Address);
  end New_Keyboard;

end Os.Console;
