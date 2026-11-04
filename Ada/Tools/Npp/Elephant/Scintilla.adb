-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Characters.Handling;
with Ada.Unchecked_Conversion;

package body Scintilla is

  SCI_GETDIRECTFUNCTION : constant := 2184;
  SCI_GETDIRECTPOINTER  : constant := 2185;

  function Convert is new Ada.Unchecked_Conversion (System.Address, Win.LPARAM);
  function Convert is new Ada.Unchecked_Conversion (Win.LRESULT, Fn_Type);
  function Convert is new Ada.Unchecked_Conversion (Win.LRESULT, Ptr_Type);


  procedure Create (The_Object  : in out Object;
                    From_Handle : Win.HWND) is
  begin
    The_Object.Fn := Convert(Win.Send_Message (From_Handle, SCI_GETDIRECTFUNCTION, 0, 0));
    The_Object.Ptr := Convert(Win.Send_Message (From_Handle, SCI_GETDIRECTPOINTER, 0, 0));
  end Create;


  function Execution_Of (Handle : Object;
                         Msg    : Win.UINT;
                         Wpar   : Win.WPARAM := 0;
                         Lpar   : Win.LPARAM := 0) return Win.LRESULT is
  begin
    return Handle.Fn (Handle.Ptr, Msg, Wpar, Lpar);
  end Execution_Of;


  procedure Execute (Handle : Object;
                     Msg    : Win.UINT;
                     Wpar   : Win.WPARAM := 0;
                     Lpar   : Win.LPARAM := 0) is
    Dummy : Win.LRESULT;
  begin
    Dummy := Handle.Fn (Handle.Ptr, Msg, Wpar, Lpar);
  end Execute;


  use type Win.LRESULT;

  Style_Default : constant Win.WPARAM := 36;

  SCI_FALSE : constant Win.LPARAM := 0;
  SCI_TRUE  : constant Win.LPARAM := 1;

  SCI_ENSUREVISIBLE             : constant := 2232;
  SCI_FINDCOLUMN                : constant := 2456;
  SCI_GETCOLUMN                 : constant := 2129;
  SCI_GETCURRENTPOS             : constant := 2008;
  SCI_GETLENGTH                 : constant := 2006;
  SCI_GETMODIFY                 : constant := 2159;
  SCI_GETRANGEPOINTER           : constant := 2643;
  SCI_GETTEXT                   : constant := 2182;
  SCI_GOTOPOS                   : constant := 2025;
  SCI_LINEFROMPOSITION          : constant := 2166;
  SCI_SETCARETLINEVISIBLE       : constant := 2096;
  SCI_SETCARETLINEBACK          : constant := 2098;
  SCI_SETCARETLINEVISIBLEALWAYS : constant := 2655;
  SCI_SETEDGECOLUMN             : constant := 2361;
  SCI_STYLECLEARALL             : constant := 2050;
  SCI_STYLESETBOLD              : constant := 2053;
  SCI_STYLESETFORE              : constant := 2051;
  SCI_STYLESETBACK              : constant := 2052;
  SCI_STYLESETFONT              : constant := 2056;
  SCI_STYLESETITALIC            : constant := 2054;
  SCI_STYLESETSIZE              : constant := 2055;
  SCI_STYLESETUNDERLINE         : constant := 2059;
  SCI_SETSTYLING                : constant := 2033;
  SCI_STARTSTYLING              : constant := 2032;

  function Buffer_Content (Handle : Object) return String is
    The_Content : aliased String(1 .. Natural(Execution_Of(Handle, SCI_GETLENGTH)) + 1);
  begin
    Execute(Handle, SCI_GETTEXT, The_Content'length, Convert(The_Content'address));
    return The_Content;
  end Buffer_Content;


  function Buffer_Is_Modified (Handle : Object) return Boolean is
  begin
    return Execution_Of (Handle, SCI_GETMODIFY) /= SCI_FALSE;
  end Buffer_Is_Modified;


  function Current_Line (Handle : Object) return Positive is
    The_Position : Win.LRESULT;
  begin
    The_Position := Execution_Of (Handle, SCI_GETCURRENTPOS);
    return Natural(Execution_Of (Handle, SCI_LINEFROMPOSITION, Win.WPARAM(The_Position))) + 1;
  end Current_Line;


  function Current_Column (Handle : Object) return Positive is
    The_Position : Win.LRESULT;
  begin
    The_Position := Execution_Of (Handle, SCI_GETCURRENTPOS);
    return Natural(Execution_Of (Handle, SCI_GETCOLUMN, Win.WPARAM(The_Position))) + 1;
  end Current_Column;


  procedure Set_Cursor (Handle : Object;
                        Line   : Positive;
                        Column : Positive := 1) is
    The_Position : Win.LRESULT;
  begin
    Execute (Handle, SCI_ENSUREVISIBLE, Win.WPARAM(Line - 1));
    The_Position := Execution_Of (Handle, SCI_FINDCOLUMN, Win.WPARAM(Line - 1), Win.LPARAM(Column - 1));
    Execute (Handle, SCI_GOTOPOS, Win.WPARAM(The_Position));
  end Set_Cursor;


  procedure Hide_Cursor_Line (Handle : Object) is
  begin
    Execute (Handle, SCI_SETCARETLINEVISIBLE, Win.WPARAM(SCI_FALSE));
  end Hide_Cursor_Line;


  procedure Show_Cursor_Line (Handle : Object) is
    Background_Color : constant Win.COLORREF := Win.RGB (Red => 230, Green => 230, Blue => 255);
  begin
    Execute (Handle, SCI_SETCARETLINEBACK, Win.WPARAM(Background_Color));
    Execute (Handle, SCI_SETCARETLINEVISIBLEALWAYS, Win.WPARAM(SCI_TRUE));
    Execute (Handle, SCI_SETCARETLINEVISIBLE, Win.WPARAM(SCI_TRUE));
  end Show_Cursor_Line;


  procedure Change_Case (Handle : Object;
                         Line   : Positive;
                         Column : Positive) is
    type Cell_Pointer is access all Character;
    The_Pointer  : Cell_Pointer;
    The_Position : Win.LRESULT;
    function Convert is new Ada.Unchecked_Conversion (Win.LRESULT, Cell_Pointer);
  begin
    Execute (Handle, SCI_ENSUREVISIBLE, Win.WPARAM(Line - 1));
    The_Position := Execution_Of (Handle, SCI_FINDCOLUMN, Win.WPARAM(Line - 1), Win.LPARAM(Column - 1));
    The_Pointer := Convert (Execution_Of (Handle, SCI_GETRANGEPOINTER, Win.WPARAM(The_Position), 1));
    if Ada.Characters.Handling.Is_Lower (The_Pointer.all) then
      The_Pointer.all := Ada.Characters.Handling.To_Upper (The_Pointer.all);
    else
      The_Pointer.all := Ada.Characters.Handling.To_Lower (The_Pointer.all);
    end if;
  end Change_Case;


  Default_Font  : constant String := "Fixedsys" & Ascii.Nul;

  procedure Define_Styles (Handle : Object) is

    procedure Set_Background_For (Item  : Style;
                                  Color : Win.COLORREF) is
    begin
      Execute (Handle, SCI_STYLESETBACK, Style'pos(Item), Win.LPARAM(Color));
    end Set_Background_For;

    procedure Set_Foreground_For (Item  : Style;
                                  Color : Win.COLORREF) is
    begin
      Execute (Handle, SCI_STYLESETFORE, Style'pos(Item), Win.LPARAM(Color));
    end Set_Foreground_For;

    procedure Set_Bold_For (Item   : Style;
                            Switch : Win.LPARAM := SCI_TRUE) is
    begin
      Execute (Handle, SCI_STYLESETBOLD, Style'pos(Item), Switch);
    end Set_Bold_For;

    procedure Set_Italic_For (Item   : Style;
                              Switch : Win.LPARAM := SCI_TRUE) is
    begin
      Execute (Handle, SCI_STYLESETITALIC, Style'pos(Item), Switch);
    end Set_Italic_For;

    procedure Set_Undeline_For (Item   : Style;
                                Switch : Win.LPARAM := SCI_TRUE) is
    begin
      Execute (Handle, SCI_STYLESETUNDERLINE, Style'pos(Item), Switch);
    end Set_Undeline_For;

  begin -- Define_Styles
    Execute (Handle, SCI_STYLESETFONT, Style_Default, Convert(Default_Font'address));
    Execute (Handle, SCI_STYLESETSIZE, Style_Default, Win.LPARAM(11));
    Set_Background_For (Default, Win.RGB (Red => 255, Green => 255, Blue => 255));
    Set_Foreground_For (Default, Win.Black);
    Set_Bold_For (Default, SCI_FALSE);
    Set_Italic_For (Default, SCI_FALSE);
    Set_Undeline_For (Default, SCI_FALSE);
    Execute (Handle, SCI_STYLECLEARALL);

    Set_Foreground_For (Attributes,
                        Win.RGB (Red => 180, Green => 180, Blue => 0));   -- Olive
    Set_Foreground_For (Comment,
                        Win.RGB (Red => 128, Green => 128, Blue => 128)); -- Gray
    Set_Foreground_For (Special_Comment,
                        Win.RGB (Red => 180, Green => 180, Blue => 0));   -- Olive
    Set_Foreground_For (Directive,
                        Win.RGB (Red => 180, Green => 180, Blue => 0));   -- Olive
    Set_Foreground_For (Reserved_Word,
                        Win.RGB (Red =>   0, Green =>   0, Blue => 255)); -- Light_Blue
    Set_Foreground_For (Character_Literal,
                        Win.RGB (Red => 255, Green => 128, Blue => 64));  -- Orange
    Set_Foreground_For (Numeric_Literal,
                        Win.RGB (Red => 255, Green =>   0, Blue =>   0)); -- Light_Red
    Set_Foreground_For (String_Literal,
                        Win.RGB (Red => 128, Green =>   0, Blue => 128)); -- Purple
    Set_Foreground_For (Types,
                        Win.RGB (Red =>   0, Green => 128, Blue =>   0)); -- Green
    Set_Foreground_For (Unused_Declaration,
                        Win.RGB (Red => 192, Green => 128, Blue =>  16)); -- Brown
    Set_Foreground_For (Unused_Type_Declaration,
                        Win.RGB (Red => 192, Green => 128, Blue =>  16)); -- Brown
    Set_Background_For (Style_Error,
                        Win.RGB (Red => 230, Green => 230, Blue => 255)); -- Light_Violett
    Set_Background_For (Semantic_Error,
                        Win.RGB (Red => 255, Green => 220, Blue => 220)); -- Light_Purple
    Set_Background_For (Syntax_Error,
                        Win.RGB (Red => 255, Green => 255, Blue =>   0)); -- Yellow
    Set_Background_For (Compilation_Error,
                        Win.RGB (Red =>   0, Green => 255, Blue => 255)); -- Light_Blue_Green

    Set_Undeline_For (Unused_Declaration);
    Set_Undeline_For (Unused_Type_Declaration);
    Set_Undeline_For (Unknown_Identifier);
  end Define_Styles;


  procedure Define (Handle      : Object;
                    Edge_Column : Natural) is
  begin
    Execute (Handle, SCI_SETEDGECOLUMN, Win.WPARAM(Edge_Column));
  end Define;


  procedure Set (Handle       : Object;
                 First_Line   : Positive := Positive'first;
                 Last_Line    : Positive := Last;
                 First_Column : Positive := Positive'first;
                 Last_Column  : Positive := Last;
                 To_Style     : Style) is

    Style_Mask      : constant Win.LPARAM := 2#11111#;

    First_Position  : Win.LRESULT;
    Last_Position   : Win.LRESULT;

  begin
    First_Position := Execution_Of (Handle, SCI_FINDCOLUMN, Win.WPARAM(First_Line - 1), Win.LPARAM(First_Column - 1));
    Last_Position := Execution_Of (Handle, SCI_FINDCOLUMN, Win.WPARAM(Last_Line - 1), Win.LPARAM(Last_Column - 1));
    Execute (Handle, SCI_STARTSTYLING, Win.WPARAM(First_Position), Style_Mask);
    Execute (Handle, SCI_SETSTYLING, Win.WPARAM(Last_Position - First_Position + 1), Style'pos(To_Style));
  end Set;

end Scintilla;