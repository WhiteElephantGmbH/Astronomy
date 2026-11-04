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

with Interfaces.C;
with System;

package Win with Pure => True is

  type Signed is range -(2 ** (Standard'address_size - 1)) .. (2 ** (Standard'address_size - 1) - 1);
  type Unsigned is mod 2 ** Standard'address_size;

  type BYTE is new Interfaces.C.unsigned_char;

  subtype BOOL   is Interfaces.C.int;
  subtype CHAR   is Interfaces.C.char;
  subtype INT    is Interfaces.C.int;
  subtype LONG   is Interfaces.C.long;
  subtype WCHAR  is Interfaces.C.wchar_t;
  subtype UINT   is Interfaces.C.unsigned;
  subtype ULONG  is Interfaces.C.unsigned_long;
  subtype USHORT is Interfaces.C.unsigned_short;
  subtype WORD   is USHORT;
  subtype DWORD  is ULONG;

  subtype HANDLE    is System.Address;
  subtype HBITMAP   is HANDLE;
  subtype HICON     is HANDLE;
  subtype HINSTANCE is HANDLE;
  subtype HMENU     is HANDLE;
  subtype HWND      is HANDLE;
  subtype LPVOID    is HANDLE;
  subtype PTCHAR    is HANDLE;

  subtype LPSTR   is System.Address;
  subtype LPCSTR  is System.Address;
  subtype LPCWSTR is System.Address;

  type CHAR_Array  is array (Natural range <>) of aliased CHAR;
  type WCHAR_Array is array (Natural range <>) of aliased WCHAR;

  Wide_Nul : constant WCHAR := WCHAR'first;

  type COLORREF is new DWORD;

  type ULONG_PTR is new Unsigned;
  type INT_PTR   is new Signed;

  subtype LONG_PTR is Signed;
  subtype UINT_PTR is Unsigned;

  subtype WPARAM  is UINT_PTR;
  subtype LPARAM  is LONG_PTR;
  subtype LRESULT is LONG_PTR;

  FALSE : constant BOOL := 0;
  TRUE  : constant BOOL := 1;

  OK : constant LRESULT := 1;

  WM_NOTIFY  : constant := 78;
  WM_SETTEXT : constant := 12;
  WM_USER    : constant := 16#400#;
  WM_VSCROLL : constant := 16#115#;

  WM_USER_CALLBACK : constant := WM_USER + 1500; -- should not conflict with notepad++

  CFM_COLOR : constant := 16#40000000#;
  CFM_SIZE  : constant := 16#80000000#;

  EM_EXSETSEL      : constant := WM_USER + 55;
  EM_REPLACESEL    : constant := 16#c2#;
  EM_SETCHARFORMAT : constant := WM_USER + 68;

  ES_MULTILINE : constant := 16#4#;
  ES_READONLY  : constant := 16#800#;
  ES_SAVESEL   : constant := 16#00008000#;
  ES_SUNKEN    : constant := 16#00004000#;

  LF_FACESIZE : constant := 32;

  SB_BOTTOM : constant := 7;

  SCF_SELECTION : constant := 16#0001#;

  WS_CHILD   : constant := 16#40000000#;
  WS_BORDER  : constant := 16#800000#;
  WS_HSCROLL : constant := 16#100000#;
  WS_VSCROLL : constant := 16#200000#;

  TVE_EXPAND : constant := 16#0002#;

  TVGN_PARENT : constant := 16#0003#;
  TVGN_CHILD  : constant := 16#0004#;
  TVGN_CARET  : constant := 16#0009#;

  TVIF_TEXT     : constant := 16#0001#;
  TVIF_PARAM    : constant := 16#0004#;
  TVIF_CHILDREN : constant := 16#0040#;

  type HTREEITEM is new INT_PTR;

  TVI_ROOT : constant HTREEITEM := -16#10000#;
  TVI_LAST : constant HTREEITEM := -16#FFFE#;

  TV_FIRST            : constant := 16#1100#;
  TVM_INSERTITEM_ANSI : constant := TV_FIRST + 0;
  TVM_DELETEITEM      : constant := TV_FIRST + 1;
  TVM_EXPAND          : constant := TV_FIRST + 2;
  TVM_GETNEXTITEM     : constant := TV_FIRST + 10;
  TVM_GETITEM_ANSI    : constant := TV_FIRST + 12;
  TVM_SETITEM_ANSI    : constant := TV_FIRST + 13;

  TVS_HASBUTTONS  : constant := 16#01#;
  TVS_HASLINES    : constant := 16#02#;
  TVS_LINESATROOT : constant := 16#04#;

  NM_DBLCLK    : constant := -3;
  NM_SETFOCUS  : constant := -7;
  NM_KILLFOCUS : constant := -8;

  type TV_ITEM_ANSI is record
    Mask          : UINT;
    Hitem         : HTREEITEM;
    State         : UINT;
    Statemask     : UINT;
    Text          : LPSTR;
    Maxtextsize   : INT;
    Image         : INT;
    Selectedimage : INT;
    Children      : INT;
    Lparam        : Win.LPARAM;
  end record with Convention => C;

  type TV_INSERTSTRUCT_ANSI is record
    Parent      : HTREEITEM;
    Insertafter : HTREEITEM;
    Item        : TV_ITEM_ANSI;
  end record with Convention => C;

  use type Interfaces.C.unsigned;

  type CHARFORMAT is record
    Size           : UINT  := CHARFORMAT'size / System.Storage_Unit;
    Mask           : DWORD;
    Effects        : DWORD;
    Height         : LONG;
    Offset         : LONG;
    Textcolor      : COLORREF;
    Charset        : BYTE;
    Pitchandfamily : BYTE;
    Facename       : CHAR_Array (0..LF_FACESIZE -1);
    Padding        : WORD;
  end record with Convention => C;

  type CHARRANGE is record
    Min : LONG;
    Max : LONG;
  end record with Convention => C;

  type RECT is record
    Left   : LONG;
    Top    : LONG;
    Right  : LONG;
    Bottom : LONG;
  end record with Convention => C;

  Black : constant COLORREF := 0;

  function RGB (Red, Green, Blue : BYTE) return COLORREF;


  function Send_Message (To   : HWND;
                         Msg  : UINT;
                         Wpar : WPARAM;
                         Lpar : LPARAM) return LRESULT with Inline_Always;

  procedure Send_Message (To   : HWND;
                          Msg  : UINT;
                          Wpar : WPARAM;
                          Lpar : LPARAM) with Inline_Always;

  function Load_Bitmap (Instance    : HINSTANCE;
                        Bitmap_Name : String) return HBITMAP with Inline_Always;

  function Load_Library (Lib_File_Name : Wide_String) return HINSTANCE with Inline_Always;

  function Create_Window (Class_Name  : Wide_String;
                          Window_Name : Wide_String;
                          Style       : DWORD;
                          X           : INT;
                          Y           : INT;
                          Width       : INT;
                          Height      : INT;
                          Wnd_Parent  : HWND;
                          Menu        : HMENU;
                          Instance    : HINSTANCE;
                          Param       : LPVOID) return HWND with Inline_Always;
end Win;
