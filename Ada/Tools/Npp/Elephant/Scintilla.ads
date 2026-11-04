-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Win;
with System;

package Scintilla is

  type Object is tagged private;

  type Style is (Default,
                 Attributes,
                 Comment,
                 Special_Comment,
                 Directive,
                 Reserved_Word,
                 Character_Literal,
                 Numeric_Literal,
                 String_Literal,
                 Types,
                 Unused_Declaration,
                 Unused_Type_Declaration,
                 Unknown_Identifier,
                 Style_Error,
                 Semantic_Error,
                 Syntax_Error,
                 Compilation_Error);

  No_Edge_Column : constant := 0;

  function Buffer_Content (Handle : Object) return String with Inline;

  function Buffer_Is_Modified (Handle : Object) return Boolean;

  procedure Create (The_Object  : in out Object;
                    From_Handle : Win.HWND);

  function Current_Line (Handle : Object) return Positive;

  function Current_Column (Handle : Object) return Positive;

  procedure Define_Styles (Handle : Object);

  procedure Define (Handle      : Object;
                    Edge_Column : Natural);

  procedure Set_Cursor (Handle : Object;
                        Line   : Positive;
                        Column : Positive := 1);

  procedure Hide_Cursor_Line (Handle : Object);

  procedure Show_Cursor_Line (Handle : Object);

  procedure Change_Case (Handle : Object;
                         Line   : Positive;
                         Column : Positive);

  Last : constant := Positive'last;

  procedure Set (Handle       : Object;
                 First_Line   : Positive := Positive'first;
                 Last_Line    : Positive := Last;
                 First_Column : Positive := Positive'first;
                 Last_Column  : Positive := Last;
                 To_Style     : Style);


  SCN_UPDATEUI        : constant := 2007;
    SC_UPDATE_CONTENT : constant := 1;

  type Notify_Header is record
    -- Compatible with Windows NMHDR.
    -- hwndFrom is really an environment specific window handle or pointer
    -- but most clients of Scintilla.h do not have this type visible.
    Hwnd_From : Win.HWND;
    Id_From   : Win.LONG_PTR;
    Code      : Win.INT;
  end record with Convention => C;

  type Notification is record
    Nmhdr                  : Notify_Header;
    Position               : Win.INT;        -- SCN_STYLENEEDED, SCN_MODIFIED, SCN_DWELLSTART, SCN_DWELLEND
    Ch                     : Win.INT;        -- SCN_CHARADDED, SCN_KEY
    Modifiers              : Win.INT;        -- SCN_KEY
    Modification_Type      : Win.INT;        -- SCN_MODIFIED
    Char                   : Win.PTCHAR;     -- SCN_MODIFIED, SCN_USERLISTSELECTION, SCN_AUTOCSELECTION
    Length                 : Win.INT;        -- SCN_MODIFIED
    Lines_Added            : Win.INT;        -- SCN_MODIFIED
    Message                : Win.INT;        -- SCN_MACRORECORD
    W_Param                : Win.WPARAM;     -- SCN_MACRORECORD
    L_Param                : Win.LPARAM;     -- SCN_MACRORECORD
    Line                   : Win.INT;        -- SCN_MODIFIED
    Fold_Level_Now         : Win.INT;        -- SCN_MODIFIED
    Fold_Level_Prev        : Win.INT;        -- SCN_MODIFIED
    Margin                 : Win.INT;        -- SCN_MARGINCLICK
    List_Type              : Win.INT;        -- SCN_USERLISTSELECTION
    X                      : Win.INT;        -- SCN_DWELLSTART, SCN_DWELLEND
    Y                      : Win.INT;        -- SCN_DWELLSTART, SCN_DWELLEND
    Token                  : Win.INT;        -- SCN_MODIFIED with SC_MOD_CONTAINER
    Annotation_Lines_Added : Win.INT;        -- SCN_MODIFIED with SC_MOD_CHANGEANNOTATION
    Updated                : Win.UINT;       -- SCN_UPDATEUI bitset of changes
  end record with Convention => C;

private
  type Ptr_Type is new System.Address;

  type Fn_Type is access function (Handle : Ptr_Type;
                                   Msg    : Win.UINT;
                                   Wpar   : Win.WPARAM;
                                   Lpar   : Win.LPARAM) return Win.LRESULT with Convention => C;

  type Object is tagged record
    Fn  : Fn_Type;
    Ptr : Ptr_Type;
  end record;

end Scintilla;