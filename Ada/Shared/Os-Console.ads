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

with System;

package Os.Console is

  type Attribute is (Normal, Highlight, Error);

  type Cursor_Style is (Insert_Cursor, Overstrike_Cursor, Out_Of_Bounds_Cursor);

  subtype Count is Natural range 0 .. 255;

  subtype Column is Count range 1 .. Count'last; -- MIN(Column) = left most column on screen
  subtype Row    is Count range 1 .. Count'last; -- MIN(Row)    = top row on screen

  type Location is record
    The_Column : Column;
    The_Row    : Row;
  end record;

  type Size is record
    Width  : Count;
    Height : Count;
  end record;

  type Region is record
    At_Location : Location;
    With_Size   : Size;
  end record;

  type Display is private;

  procedure Clear_Region (The_Region  : Region;
                          The_Display : Display);

  procedure Show_Cursor_At (The_Location     : Location;
                            The_Cursor_Style : Cursor_Style;
                            The_Display      : Display);

  procedure Hide_Cursor (The_Display : Display);

  function Actual_Cursor (The_Display : Display) return Location;

  procedure Write (The_String    : String;
                   The_Location  : Location;
                   The_Attribute : Attribute;
                   The_Display   : Display);


  type Termination_Handler is access procedure;

  function New_Display (The_Size    : Size;
                        Termination : Termination_Handler) return Display;

  type Keyboard is private;

  Backspace   : constant Character := Ascii.Bs;
  Delete      : constant Character := Character'val(8#323#);
  Down_Arrow  : constant Character := Character'val(8#320#);
  Enter       : constant Character := Ascii.Cr;
  Escape      : constant Character := Ascii.Esc;
  Finish      : constant Character := Character'val(8#317#);
  Home        : constant Character := Character'val(8#307#);
  Insert      : constant Character := Character'val(8#322#);
  Left_Arrow  : constant Character := Character'val(8#313#);
  Page_Up     : constant Character := Character'val(8#311#);
  Page_Down   : constant Character := Character'val(8#321#);
  Right_Arrow : constant Character := Character'val(8#315#);
  Up_Arrow    : constant Character := Character'val(8#310#);

  function Next_Character (The_Keyboard : Keyboard) return Character;

  function New_Keyboard return Keyboard;

private

  type Display is record
    Handle      : System.Address;
    Screen_Size : Size;
  end record;

  type Keyboard is record
    Handle : System.Address;
  end record;

end Os.Console;
