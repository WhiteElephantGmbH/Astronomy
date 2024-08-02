-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Log;
with Semaphore;
with Win32.Commctrl;
with Win32.Commdlg;
with Win32.Shellapi;
with Win32.Winbase;
with Win32.Winmain; pragma Elaborate_All (Win32.Winmain);
with Win32.Winnt;
with System.Address_To_Access_Conversions;

package body Gui is

  function Convert is new Ada.Unchecked_Conversion (Long_Long_Integer, Win32.LPCSTR);

  Icon_Resource : constant Win32.LPCSTR := Convert (1);

  package Utf renames Ada.Strings.UTF_Encoding.Wide_Strings;

  function To_Wide (Item : String) return Wide_String renames Utf.Decode;

  function To_Utf8 (Item       : Wide_String;
                    Output_BOM : Boolean := False) return String renames Utf.Encode;

  Font_Name : aliased constant String := "Consolas" & Ascii.Nul;

  use type Win32.DWORD;

  Character_Width : constant := 9;

  The_Font : constant Win32.Windef.HGDIOBJ
    := Win32.Wingdi.CreateFont(nHeight            => 20,
                               nWidth             => 0,
                               nEscapement        => 0,
                               nOrientation       => 0,
                               fnWeight           => Win32.Wingdi.FW_DONTCARE,
                               fdwItalic          => Win32.FALSE,
                               fdwUnderline       => Win32.FALSE,
                               fdwStrikeOut       => Win32.FALSE,
                               fdwCharSet         => Win32.Wingdi.DEFAULT_CHARSET,
                               fdwOutputPrecision => Win32.Wingdi.OUT_CHARACTER_PRECIS,
                               fdwClipPrecision   => Win32.Wingdi.CLIP_DEFAULT_PRECIS,
                               fdwQuality         => Win32.Wingdi.DEFAULT_QUALITY,
                               fdwPitchAndFamily  => Win32.Wingdi.DEFAULT_PITCH + Win32.Wingdi.FF_DONTCARE,
                               lpszFace           => Win32.Addr (Font_Name));


  Nul      : constant Character      := Character'first;
  Wide_Nul : constant Wide_Character := Wide_Character'first;

  Wide_Null_Name : aliased constant Wide_String := ""  & Wide_Nul;

  The_Wide_Text_Buffer : Wide_String (1..2000);


  function Wide_Text_Address_Of (Item : Wide_String) return Win32.PWCH is
  begin
    The_Wide_Text_Buffer (1..Item'length) := Item;
    The_Wide_Text_Buffer (Item'length + 1) := Wide_Nul;
    return Win32.Addr (The_Wide_Text_Buffer);
  end Wide_Text_Address_Of;


  function Wide_Text_Address_Of (Item : Wide_String) return Win32.PCWCH is
  begin
    The_Wide_Text_Buffer (1..Item'length) := Item;
    The_Wide_Text_Buffer (Item'length + 1) := Wide_Nul;
    return Win32.Addr (The_Wide_Text_Buffer);
  end Wide_Text_Address_Of;


  pragma Linker_Options ("-lcomctl32");

  type Click_Routine is access procedure (Item : Information);

  function Convert is new Ada.Unchecked_Conversion (Click_Routine, Click_Routine);

  task type Display is
    entry Create (Application_Name  : String;
                  Window_Width      : Win32.INT;
                  Window_Height     : Win32.INT;
                  Window_X_Position : Win32.INT;
                  Window_Y_Position : Win32.INT;
                  Always_Topmost    : Boolean);
    entry Ready;
  end Display;

  task type Action_Handler;

  task type Termination_Handler is
    entry Start;
    entry Finalize;
  end Termination_Handler;

   -- Private window messages
  Wm_Create_Child   : constant := Win32.Winuser.WM_USER + 1107;
  Wm_User_Callback  : constant := Win32.Winuser.WM_USER + 1108;
  Wm_User_Close     : constant := Win32.Winuser.WM_USER + 1109;

  The_Termination_Routine : Action_Routine := null;

  subtype Status_Line_Control is Win32.Windef.HWND;
  subtype Tab_Control         is Win32.Windef.HWND;

  type String_Ptr is access String;
  type Wide_String_Ptr is access Wide_String;

  type Display_Ptr is access Display;
  type Action_Handler_Ptr is access Action_Handler;
  type Termination_Handler_Ptr is access Termination_Handler;

  subtype Thread_Id is Win32.DWORD;

  The_Application_Name       : Wide_String_Ptr := new Wide_String'("" & Wide_Nul);
  The_Application_Class_Name : String_Ptr := new String'("" & Nul);

  Our_Instance  : constant Win32.Windef.HINSTANCE := Win32.Winmain.Get_hInstance;
  Rich_Edit_Dll : Win32.Windef.HINSTANCE := System.Null_Address;

  Main_Display            : Display_Ptr;
  Main_Thread             : Thread_Id := 0;
  The_Action_Handler      : Action_Handler_Ptr;
  The_Termination_Handler : Termination_Handler_Ptr;
  Private_Window          : Win32.Windef.HWND := System.Null_Address;
  Is_Active               : Boolean := False;
  Is_Destroying           : Boolean := False;

  The_Private_Window_Rectangle : aliased Win32.Windef.RECT;

  The_Status_Line : Status_Line_Control := System.Null_Address;
  The_Tabs        : Tab_Control := System.Null_Address;
  The_Menu_Bar    : constant Win32.Windef.HMENU := Win32.Winuser.CreateMenu;

  -- Child window Ids
  Id_Status_Bar     : constant Natural := 50;
  Id_Tabs           : constant Natural := 51;
  The_Next_Child_Id : Natural := 100;

  function Convert is new Ada.Unchecked_Conversion (Information, Win32.Winnt.HANDLE);

  type Child_Kind is (Button_Child, Progress_Bar_Child, Track_Bar_Child, Edit_Box_Child,
                      Check_Box_Child, Combo_Box_Child, Static_Child, List_View_Child,
                      Tree_View_Child, Text_View_Child);

  Display_Priority : constant array (Child_Kind) of Positive := [Button_Child       => 10,
                                                                 Progress_Bar_Child => 20,
                                                                 Track_Bar_Child    => 30,
                                                                 Edit_Box_Child     => 40,
                                                                 Check_Box_Child    => 50,
                                                                 Combo_Box_Child    => 40,
                                                                 Static_Child       => 60,
                                                                 List_View_Child    => 70,
                                                                 Tree_View_Child    => 70,
                                                                 Text_View_Child    => 70];

  type Child_Information is record
    The_Handle               : Win32.Windef.HWND := System.Null_Address;
    The_Kind                 : Child_Kind;
    Parent_Page              : Page;
    Initial_Title            : Win32.LPCWSTR := null;
    Initial_Title_Length     : Win32.INT := 0;
    The_Title_Size           : Win32.INT := 0;
    The_Window_Id            : Natural;
    Next_Sibling             : Child_Information_Ptr := null;
    Buddy_Child              : Child_Information_Ptr := null;
    Is_Hidden                : Boolean := False;
    Is_Buddy                 : Boolean := False;
    The_Action_Routine       : Action_Routine := null;
    Qualified_Action_Routine : Click_Routine := null;
    User_Information         : Information;
    The_Color_Routine        : Color_Handler := null;
    Number_Of_Rows           : Natural := 0;
    Number_Of_Columns        : Natural := 0;
    The_Sort_Routine         : Sort_Routine;
    The_Sort_Column          : Natural;
    The_Sort_Direction       : Sort_Direction := Forward;
    The_Click_Routine        : Click_Routine;
    The_Click_Kind           : Win32.INT := Win32.Comctl.Nm_Dblclk;
    For_Completion           : Semaphore.Binary;
    Is_Password              : Boolean := False;
    Use_Proportional_Font    : Boolean := False;
    Color_Background         : Boolean := False;
    Is_Modifiable            : Boolean := True;
    Is_Sorted                : Boolean := True;
  end record;

  type Population_Data is array (Child_Kind) of Win32.INT;

  type Page_Information is record
    Nr_Of_Children       : Win32.INT := 0;
    Nr_Of_Buddies        : Win32.INT := 0;
    Population           : Population_Data := [others => 0];
    First_Child          : Child_Information_Ptr := null;
    Total_Button_Width   : Win32.INT := 0;
    Last_Button_Width    : Win32.INT := 0;
    Minimum_Button_Width : Win32.INT := Default_Button_Width;
    Parent               : Page;
    The_Style            : Page_Style;
    Has_Cancel_Button    : Boolean := False;
    Termination_Routine  : Pop_Up_Termination;
    Keyboard_Focus       : Child_Information_Ptr := null;
    The_Action           : Action_Routine := null;  -- Called when page becomes the current page
  end record;

  type Menu_Information is record
    The_Callback : access procedure (Item : Information);
    The_Handle   : Win32.Windef.HMENU;
    The_Kind     : Menu_Kind;
    Radio_Item   : Menu_Item_Access;
  end record;

  type Menu_Item_Information is record
    The_Information : Information;
    The_Id          : Positive;
    The_Parent_Menu : Menu;
    Previous        : Menu_Item_Access;
    Next            : Menu_Item_Access;
  end record;


  protected Mutex is
    entry Wait;
    procedure Signal;
  private
    Count : Natural := 1;
  end Mutex;


  protected body Mutex is

    entry Wait when Count > 0 is
    begin
      Count := @ - 1;
    end Wait;

    procedure Signal is
    begin
      Count := @ + 1;
    end Signal;

  end Mutex;


  Last_Menu_Item         : Menu_Item_Access := null;
  First_Menu_Item_Id     : constant Positive := 1000;
  The_Next_Menu_Item_Id  : Positive := First_Menu_Item_Id;

  Nr_Of_Pages  : Natural := 0;
  Current_Page : Page;
  Active_Child : Child_Information_Ptr;

  The_Callback_Routine : Action_Routine := null;

  The_Keyboard_Hook         : Win32.Windef.HHOOK := System.Null_Address;
  The_Key_Handling_Rountine : Key_Handler := null;

  The_Mouse_Hook              : Win32.Windef.HHOOK := System.Null_Address;
  The_Mouse_Handling_Rountine : Mouse_Handler := null;


  function Current_Thread_Id return Thread_Id is
  begin
    return Win32.Winbase.GetCurrentThreadId;
  end Current_Thread_Id;


  protected Action is
    procedure Put (The_Action : Action_Routine);
    procedure Put (The_Click       : Click_Routine;
                   The_Information : Information);
    procedure Finish;
    entry Get (The_Action : out Action_Routine;
               The_Click  : out Click_Routine;
               The_Info   : out Information);
    procedure Enable;
    procedure Do_Terminate;
    entry Terminated;
  private
    Next_Action      : Action_Routine := null;
    Next_Click       : Click_Routine := null;
    Next_Information : Information;
    Is_Enabled       : Boolean := True;
    Is_Pending       : Boolean := False;
    Is_Finishing     : Boolean := False;
    Is_Terminated    : Boolean := False;
  end Action;


  protected body Action is

    procedure Put (The_Action : Action_Routine) is
    begin
      if Is_Enabled then
        Next_Action := The_Action;
        Next_Click := null;
        Is_Pending := True;
        Is_Enabled := False;
      end if;
    end Put;

    procedure Put (The_Click       : Click_Routine;
                   The_Information : Information) is
    begin
      if Is_Enabled  then
        Next_Action := null;
        Next_Click := The_Click;
        Next_Information := The_Information;
        Is_Pending := True;
        Is_Enabled := False;
      end if;
    end Put;

    procedure Finish is
    begin
      Next_Click := null;
      Next_Action := null;
      Is_Pending := True;
      Is_Enabled := False;
      Is_Finishing := True;
    end Finish;

    entry Get (The_Action : out Action_Routine;
               The_Click  : out Click_Routine;
               The_Info   : out Information) when Is_Pending is
    begin
      The_Action := Next_Action;
      The_Click  := Next_Click;
      The_Info := Next_Information;
      Is_Pending := False;
    end Get;

    procedure Enable is
    begin
      Is_Enabled := not Is_Finishing;
    end Enable;

    procedure Do_Terminate is
    begin
      Is_Terminated := True;
    end Do_Terminate;

    entry Terminated when Is_Terminated is
    begin
      null;
    end Terminated;

  end Action;


  procedure Execute (The_Action : Action_Routine) is
  begin
    Action.Put (The_Action);
  end Execute;


  procedure Execute (The_Action      : Click_Routine;
                     The_Information : Information) is
  begin
    Action.Put (The_Action, The_Information);
  end Execute;


  function Send_Message (Hwnd  : Win32.Windef.HWND;
                         Msg   : Win32.UINT;
                         Wparam: Win32.WPARAM;
                         Lparam: Win32.LPARAM) return Win32.LRESULT is
  begin
    if Is_Active then
      return Win32.Winuser.SendMessage (Hwnd, Msg, Wparam, Lparam);
    else
      return 0;
    end if;
  end Send_Message;


  procedure Send_Message (Hwnd  : Win32.Windef.HWND;
                          Msg   : Win32.UINT;
                          Wparam: Win32.WPARAM;
                          Lparam: Win32.LPARAM) is
    Unused : Win32.LRESULT;
  begin
    if Is_Active then
      Unused := Win32.Winuser.SendMessage (Hwnd, Msg, Wparam, Lparam);
    end if;
  end Send_Message;


  function Send_Message_Wide (Hwnd  : Win32.Windef.HWND;
                              Msg   : Win32.UINT;
                              Wparam: Win32.WPARAM;
                              Lparam: Win32.LPARAM) return Win32.LRESULT is
  begin
    if Is_Active then
      return Win32.Winuser.SendMessageW (Hwnd, Msg, Wparam, Lparam);
    else
      return 0;
    end if;
  end Send_Message_Wide;


  procedure Send_Message_Wide (Hwnd  : Win32.Windef.HWND;
                               Msg   : Win32.UINT;
                               Wparam: Win32.WPARAM;
                               Lparam: Win32.LPARAM) is
    Unused : Win32.LRESULT;
  begin
    if Is_Active then
      Unused := Win32.Winuser.SendMessageW (Hwnd, Msg, Wparam, Lparam);
    end if;
  end Send_Message_Wide;


  procedure Message_Box (The_Message    : String;
                         Is_Exclamation : Boolean := True) is
    Message   : aliased constant Wide_String := To_Wide (The_Message) & Wide_Nul;
    Unused    : Win32.INT;
    Box_Style : Win32.UINT := Win32.Winuser.MB_OK;
    use type Win32.UINT;
  begin
    if Is_Exclamation then
      Box_Style := Box_Style + Win32.Winuser.MB_ICONHAND;
    else
      Box_Style := Box_Style + Win32.Winuser.MB_ICONASTERISK;
    end if;
    Unused := Win32.Winuser.MessageBoxW (Private_Window,
                                         Win32.Addr(Message),
                                         Win32.Addr(The_Application_Name.all),
                                         Box_Style);
  end Message_Box;


  procedure Beep (With_Sound : Sound := Standard_Sound) is
    The_Sound : Win32.UINT;
    Unused    : Win32.BOOL;
  begin
    case With_Sound is
    when Standard_Sound => The_Sound := 16#FFFFFFFF#;
    when Asterisk       => The_Sound := Win32.Winuser.MB_ICONASTERISK;
    when Exclamation    => The_Sound := Win32.Winuser.MB_ICONEXCLAMATION;
    when Hand           => The_Sound := Win32.Winuser.MB_ICONHAND;
    when Question       => The_Sound := Win32.Winuser.MB_ICONQUESTION;
    when Default        => The_Sound := Win32.Winuser.MB_OK;
    end case;
    Unused := Win32.Winuser.MessageBeep (The_Sound);
  end Beep;


  procedure Shell_Open (The_File       : String;
                        The_Parameters : String := "") is
    Name   : aliased constant String := The_File & Nul;
    Unused : Win32.Windef.HINSTANCE;
  begin
    if The_Parameters = "" then
      Unused := Win32.Shellapi.ShellExecute (Private_Window, null,
                                             Win32.Addr (Name),
                                             null, null, 0);
    else
      declare
        Parameters : aliased constant String := The_Parameters & Nul;
      begin
        Unused := Win32.Shellapi.ShellExecute (Private_Window, null,
                                               Win32.Addr (Name),
                                               Win32.Addr (Parameters),
                                               null, 0);
      end;
    end if;
  end Shell_Open;


  procedure Change_Application_Name (Name : String) is
    Unused : Win32.BOOL;
    procedure Dispose is new Ada.Unchecked_Deallocation (Wide_String, Wide_String_Ptr);
    use type Win32.Windef.HWND;
  begin
    if Private_Window /= System.Null_Address then
      if The_Application_Name /= null then
        Dispose (The_Application_Name);
      end if;
      if Is_Active then
        The_Application_Name := new Wide_String'(To_Wide(Name) & Wide_Nul);
        Unused := Win32.Winuser.SetWindowTextW (Private_Window, Win32.Addr(The_Application_Name.all));
      end if;
    end if;
  end Change_Application_Name;


  function String_Of (The_String : String) return String is
  begin
    if The_String'length > 0 and then The_String (The_String'last) = Nul then
      return The_String (The_String'first .. The_String'last - 1);
    else
      return The_String;
    end if;
  end String_Of;


  function Name_Of_Application return String is
  begin
    if The_Application_Name = null then
      return "";
    else
      return String_Of (To_Utf8(The_Application_Name.all));
    end if;
  end Name_Of_Application;


  function Is_Confirmed (The_Question : String) return Boolean is
    With_Question : aliased constant Wide_String := To_Wide(The_Question) & Wide_Nul;
    Box_Style     : constant := Win32.Winuser.MB_ICONQUESTION + Win32.Winuser.MB_YESNO;
    use type Win32.INT;
  begin
    return Win32.Winuser.MessageBoxW (Private_Window,
                                      Win32.Addr(With_Question),
                                      Win32.Addr(The_Application_Name.all),
                                      Box_Style) = Win32.Winuser.IDYES;
  end Is_Confirmed;


  function Make_Long (The_Number : Natural) return Win32.LPARAM is
    type Dword_Rec is record
      Low  : Win32.WORD;
      High : Win32.WORD;
    end record;
    The_Longword : Win32.LPARAM;
    The_Record   : Dword_Rec;
    for The_Record'address use The_Longword'address;
    Temp : Win32.WORD;
  begin
    The_Longword := Win32.LPARAM (The_Number);
    Temp := The_Record.Low;
    The_Record.Low := The_Record.High;
    The_Record.High := Temp;
    return The_Longword;
  end Make_Long;


  procedure Set_Status_Line (The_Text : String) is
    function Convert is new Ada.Unchecked_Conversion(System.Address, Win32.LPARAM);
    Text : aliased constant Wide_String := To_Wide (The_Text) & Wide_Nul;
  begin
    Send_Message_Wide (Win32.Windef.HWND(The_Status_Line),
                       Win32.Commctrl.SB_SETTEXTW,
                       Win32.WPARAM(0),
                       Convert(Text(Text'first)'address));
  end Set_Status_Line;


  function Get_Window_Metrics return Window_Metrics is
    use type Win32.LONG;
  begin
    return (Height     => Natural(The_Private_Window_Rectangle.bottom - The_Private_Window_Rectangle.top),
            Width      => Natural (The_Private_Window_Rectangle.right - The_Private_Window_Rectangle.left),
            X_Position => Integer (The_Private_Window_Rectangle.left),
            Y_Position => Integer (The_Private_Window_Rectangle.top));
  end Get_Window_Metrics;


  procedure Set_Window_Metrics (X_Position    : Integer;
                                Y_Position    : Integer;
                                Window_Width  : Natural;
                                Window_Height : Natural) is
    Unused : Win32.BOOL;
  begin
    Unused := Win32.Winuser.SetWindowPos (Private_Window, System.Null_Address,
                                          Win32.INT(X_Position), Win32.INT(Y_Position),
                                          Win32.INT(Window_Width), Win32.INT(Window_Height),
                                          Win32.Winuser.SWP_NOZORDER);
  end Set_Window_Metrics;


  --
  -- Routines and definitions missing from Win32.Winuser
  --
  Monitor_Default_To_Null    : constant Win32.DWORD := 0;
--Monitor_Default_To_Primary : constant Win32.DWORD := 1;
--Monitor_Default_To_Nearest : constant Win32.DWORD := 2;

  function Monitor_From_Point(Point : Win32.Windef.POINT;
                              Flags : Win32.DWORD)
                             return Win32.Windef.HWND
  with
    Import,
    Convention    => Stdcall,
    External_Name => "MonitorFromPoint";


  function Monitor_From_Rect (Rect  : Win32.Windef.LPRECT;
                              Flags : Win32.DWORD)
                             return Win32.Windef.HWND
  with
    Import,
    Convention    => Stdcall,
    External_Name => "MonitorFromRect";


  function Is_Visible (X_Position : Win32.LONG;
                       Y_Position : Win32.LONG;
                       Width      : Win32.LONG := 0;
                       Height     : Win32.LONG := 0) return Boolean is
    The_Monitor : Win32.Windef.HWND := System.Null_Address;
    Flags       : constant Win32.DWORD := Monitor_Default_To_Null;
    use type Win32.Windef.HWND;
    use type Win32.LONG;
  begin
    if (X_Position = Default_Position) or
       (Y_Position = Default_Position) or
       (Width <= 0) or
       (Height <= 0)
    then
      declare
        Point : constant Win32.Windef.POINT := (x => X_Position,
                                                y => Y_Position);
      begin
        The_Monitor := Monitor_From_Point (Point, Win32.DWORD(0));
      end;
    else
      declare
        Rect : aliased Win32.Windef.RECT := (left   => X_Position,
                                             top    => Y_Position,
                                             right  => X_Position + Width,
                                             bottom => Y_Position + Height);

      begin
        The_Monitor := Monitor_From_Rect (Rect'unchecked_access, Flags);
      end;
    end if;
    return The_Monitor /= System.Null_Address;
  end Is_Visible;


  function Is_Visible (X_Position : Integer;
                       Y_Position : Integer;
                       Width      : Natural := 0;
                       Height     : Natural := 0) return Boolean is
  begin
    return Is_Visible (X_Position => Win32.LONG(X_Position),
                       Y_Position => Win32.LONG(Y_Position),
                       Width      => Win32.LONG(Width),
                       Height     => Win32.LONG(Height));
  end Is_Visible;


  function Application_Is_Minimized return Boolean is
    use type Win32.BOOL;
  begin
    return Win32.Winuser.IsIconic (Private_Window) /= Win32.FALSE;
  end Application_Is_Minimized;


  function Application_Is_Maximized return Boolean is
    use type Win32.BOOL;
  begin
    return Win32.Winuser.IsZoomed (Private_Window) /= Win32.FALSE;
  end Application_Is_Maximized;


  procedure Note_Private_Window_Metrics is
    Unused : Win32.BOOL;
  begin
    Unused := Win32.Winuser.GetWindowRect (Private_Window,
                                           The_Private_Window_Rectangle'unchecked_access);
  end Note_Private_Window_Metrics;


  procedure Redraw_Main_Window is

    use type Win32.INT;

    Unused_Bool           : Win32.BOOL;
    Unused_Hwnd           : Win32.Windef.HWND;
    Unused_Int            : Win32.INT;
    Unused_Hdwp           : Win32.Winuser.HDWP;
    Client_Rectangle      : aliased Win32.Windef.RECT;
    The_Rectangle         : aliased Win32.Windef.RECT;
    The_Device_Context    : Win32.Windef.HDC;
    Status_Line_Height    : Win32.INT;
    Deferred_Positioning  : Win32.Winuser.HDWP;
    The_Child             : Child_Information_Ptr;
    Object_Width          : Win32.INT;
    Buddy_Width           : Win32.INT := 0;
    Object_Height         : Win32.INT;
    Buddy_Height          : Win32.INT;
    Box_Height            : Win32.INT;
    Check_Box_Width       : constant := 20;
    Vertical_Raster       : constant Win32.INT := 30;
    Third_Raster          : constant Win32.INT := Vertical_Raster / 3;
    Button_Height         : Win32.INT := Vertical_Raster;
    Horizontal_Position   : Win32.INT;
    Large_Object_Position : Win32.INT := 0;
    Vertical_Position     : Win32.INT := 0;
    Vertical_Increment    : Win32.INT;
    Children_Drawn        : Population_Data := [others => 0];
    Nr_Of_Windows         : Win32.INT := 1;  -- The status bar
    The_Text_Size         : aliased Win32.Windef.SIZE;
    The_Min_Object_Width  : Win32.INT;

  function Convert is new Ada.Unchecked_Conversion(Win32.Windef.LPRECT, Win32.LPARAM);
  begin
    if not Is_Active then
      return;
    end if;
    Unused_Bool := Win32.Winuser.GetWindowRect (Win32.Windef.HWND(The_Status_Line),
                                                The_Rectangle'unchecked_access);
    Status_Line_Height := Win32.INT(The_Rectangle.bottom) - Win32.INT(The_Rectangle.top);
    Unused_Bool := Win32.Winuser.GetClientRect (Private_Window, Client_Rectangle'unchecked_access);
    Unused_Bool := Win32.Winuser.SetRect (Client_Rectangle'unchecked_access, 0, 0,
                                          Win32.INT(Client_Rectangle.right),
                                          Win32.INT(Client_Rectangle.bottom) - Status_Line_Height);
    if Current_Page /= null then
      Nr_Of_Windows := Nr_Of_Windows + Current_Page.Nr_Of_Children;
    end if;
    if Nr_Of_Pages > 1 then -- Has tab control
      Deferred_Positioning := Win32.Winuser.BeginDeferWindowPos (Nr_Of_Windows + 1);
      Send_Message (Win32.Windef.HWND(The_Tabs),
                    Win32.Comctl.Tcm_Adjustrect,
                    Win32.WPARAM(0),
                    Convert(Client_Rectangle'unchecked_access));
      Unused_Hdwp := Win32.Winuser.DeferWindowPos (Deferred_Positioning,
                                                   Win32.Windef.HWND(The_Tabs),
                                                   System.Null_Address, 0, 0,
                                                   Win32.INT(Client_Rectangle.right),
                                                   Win32.INT(Client_Rectangle.bottom),
                                                   Win32.UINT(Win32.Winuser.SWP_NOMOVE +
                                                              Win32.Winuser.SWP_NOZORDER +
                                                              Win32.Winuser.SWP_SHOWWINDOW));
    else
     Deferred_Positioning := Win32.Winuser.BeginDeferWindowPos (Nr_Of_Windows);
    end if;
    if Current_Page /= null then
      The_Device_Context := Win32.Winuser.GetDC(Private_Window);
      -- Determine the height of edit and combo boxes
      declare
        Any_Tall_Character : aliased constant String := "X";
      begin
        Unused_Bool := Win32.Wingdi.GetTextExtentPoint32 (The_Device_Context,
                                                          Win32.Addr(Any_Tall_Character), 1,
                                                          The_Text_Size'unchecked_access);
      end;
      Box_Height := Win32.INT(The_Text_Size.cy) + 8; -- Size of text plus margin
      -- Determine Button Height
      if Current_Page.Population(Button_Child) = 0 then
        Button_Height := 0;
      elsif Current_Page.The_Style(Buttons_Fill_Vertically) then -- Buttons fill page.
        Button_Height := Win32.INT(Client_Rectangle.bottom) -
                         Win32.INT(Client_Rectangle.top) -
                         Status_Line_Height; -- Available space;
        if Current_Page.Population (List_View_Child) +
           Current_Page.Population (Tree_View_Child) +
           Current_Page.Population (Text_View_Child) > 0
        then
          Button_Height := Button_Height / 2;  -- Reserve half for lists and trees
        end if;
        -- Calculate height of a single box
        if Box_Height < Vertical_Raster then -- Small boxes are made bigger
          Buddy_Height := Vertical_Raster; -- Can never be smaller than the raster
        else
          Buddy_Height := Box_Height;
        end if;
        -- Calculate height of non-button objects
        Object_Height := ((Current_Page.Population (Progress_Bar_Child) +
                           Current_Page.Population (Track_Bar_Child) +
                           Current_Page.Population (Check_Box_Child)) * Vertical_Raster) +
                         ((Current_Page.Population (Edit_Box_Child) +
                           Current_Page.Population (Combo_Box_Child)) * Buddy_Height);
        if (Object_Height + Vertical_Raster) > Button_Height then  -- Not enough room to display everything
          Button_Height := Vertical_Raster;  -- Minimum size for buttons
        else
          Button_Height := Button_Height - Object_Height;
        end if;
      elsif Box_Height > Vertical_Raster then
        Button_Height := Box_Height;
      else
        Button_Height := Vertical_Raster;
      end if;
      The_Child := Current_Page.First_Child;
      Vertical_Position := Win32.INT(Client_Rectangle.top);
      Horizontal_Position := Win32.INT(Client_Rectangle.left);
      loop  -- Draw, size and position all children
        exit when The_Child = null;
        if not (The_Child.Is_Hidden or The_Child.Is_Buddy) then  -- Draw child
          The_Min_Object_Width := Current_Page.Minimum_Button_Width;
          Vertical_Increment := Vertical_Raster;
          if (The_Child.The_Kind /= Button_Child) or else
             (Children_Drawn (Button_Child) = 0)
          then
            Horizontal_Position := Win32.INT(Client_Rectangle.left);
          end if;
          Object_Width := Win32.INT(Client_Rectangle.right) - Horizontal_Position;
          Object_Height := Vertical_Raster;
          case The_Child.The_Kind is
            when Button_Child =>
              Vertical_Position := Win32.INT(Client_Rectangle.top);
              Object_Height := Button_Height;
              if Current_Page.The_Style (Buttons_Fill_Horizontally) then
                Object_Width := (Win32.INT(Client_Rectangle.right) - Win32.INT(Client_Rectangle.left)) /
                                 Current_Page.Population (Button_Child);
              else
                Object_Width := The_Child.The_Title_Size;
              end if;
              if Children_Drawn(Button_Child) + 1 >= Current_Page.Population(Button_Child) then
                -- last is right aligned
                Horizontal_Position := Win32.INT(Client_Rectangle.right) - Object_Width;
                Vertical_Increment := Button_Height;
              elsif not Current_Page.The_Style (Buttons_Fill_Horizontally) then
                if (Horizontal_Position + Object_Width) >
                   (Win32.INT(Client_Rectangle.right) - Current_Page.Last_Button_Width)
                then
                  Object_Width := 0;  -- Cancel object if no room for it
                end if;
                Vertical_Increment := 0;
              end if;
            when Progress_Bar_Child =>
              if (not Current_Page.The_Style (Buttons_Fill_Horizontally)) and
                 (Children_Drawn (Progress_Bar_Child) = 0)
              then -- First shares button line
                if Button_Height /= 0 then -- buttons exist
                  Vertical_Position := Win32.INT(Client_Rectangle.top);
                  Vertical_Increment := Button_Height;
                  if Current_Page.Population(Button_Child) > 1 then -- between buttons
                    Horizontal_Position := Win32.INT(Client_Rectangle.left) +
                                           Current_Page.Total_Button_Width -
                                           Current_Page.Last_Button_Width;
                  end if;
                  Object_Width := Object_Width - Current_Page.Total_Button_Width;
                end if;
              end if;
              Object_Height := Third_Raster;
              Vertical_Position := Vertical_Position + Third_Raster;
              Vertical_Increment := Vertical_Increment - Third_Raster;
            when List_View_Child | Tree_View_Child | Text_View_Child =>
              if Large_Object_Position = 0 then -- Not yet known
                Large_Object_Position := Vertical_Position;  -- Start of large objects
              end if;
              Object_Height := (Win32.INT(Client_Rectangle.bottom) - Large_Object_Position) /
                               (Current_Page.Population (List_View_Child) +
                                Current_Page.Population (Tree_View_Child) +
                                Current_Page.Population (Text_View_Child));
              Vertical_Increment := Object_Height;
            when Edit_Box_Child | Combo_Box_Child =>
              if The_Child.The_Kind = Combo_Box_Child then
                Object_Width := Object_Width - 6; -- Allow extra room for box control
              end if;
              if (Children_Drawn(Edit_Box_Child) + Children_Drawn(Combo_Box_Child)= 0) and
                 (not Current_Page.The_Style(Buttons_Fill_Horizontally)) and
                 (Current_Page.Population(Button_Child) = 1) and
                 (Current_Page.Population(Progress_Bar_Child) = 0)
              then -- First shares button line;
                Vertical_Position := Win32.INT(Client_Rectangle.top);
                Object_Width := Object_Width - Current_Page.Last_Button_Width;
              end if;
              Object_Height := Box_Height;
              if Object_Height < Vertical_Raster then
                Vertical_Position := Vertical_Position + (Vertical_Raster - Object_Height) / 2;
                Vertical_Increment := Vertical_Increment - (Vertical_Raster - Object_Height) / 2;
              else
                Vertical_Increment := Object_Height;
              end if;
            when Check_Box_Child =>
              Object_Width := Object_Width - 2; -- Don't overwrite tab edge
            when others => null;
          end case;
          if The_Child.Buddy_Child /= null then -- Make room for buddy control
            Buddy_Width := The_Child.Buddy_Child.The_Title_Size;
            if Buddy_Width < 0 then -- Specifies number of characters in title
              Buddy_Width := 0 - Buddy_Width;  -- Make positive;
              Unused_Bool := Win32.Wingdi.GetTextExtentPoint32W (The_Device_Context,
                                                                 The_Child.Buddy_Child.Initial_Title,
                                                                 Buddy_Width,
                                                                 The_Text_Size'unchecked_access);
              Buddy_Width := Win32.INT(The_Text_Size.cx)+ 3; -- add a tiny separation
            elsif Buddy_Width = 0 then
              Buddy_Width := Object_Width / 2;
              Unused_Bool := Win32.Wingdi.GetTextExtentPoint32W (The_Device_Context,
                                                                 The_Child.Buddy_Child.Initial_Title,
                                                                 The_Child.Buddy_Child.Initial_Title_Length,
                                                                 The_Text_Size'unchecked_access);
              The_Min_Object_Width := Win32.INT(The_Text_Size.cx)+ 3;
              if The_Child.Buddy_Child.The_Kind = Check_Box_Child then
                The_Min_Object_Width := The_Min_Object_Width + Check_Box_Width;
                Buddy_Width := Buddy_Width - Check_Box_Width;
              end if;
            end if;
            if The_Child.Buddy_Child.The_Kind = Check_Box_Child then
              Buddy_Width := Buddy_Width + Check_Box_Width;  -- make room for check box
            end if;
            Horizontal_Position := Horizontal_Position + Buddy_Width;
            Object_Width := Object_Width - Buddy_Width;
          end if;
          if (The_Child.The_Kind /= Button_Child) and
             (The_Child.The_Title_Size /= 0) and
             (Object_Width > The_Child.The_Title_Size)
          then
            Object_Width := The_Child.The_Title_Size;
          elsif Object_Width < The_Min_Object_Width then -- Too small to display
            Object_Width := 0;
            Buddy_Width := 0;
          end if;
          Buddy_Height := Object_Height;
          if (Vertical_Position + Object_Height) > Win32.INT(Client_Rectangle.bottom) then
            Object_Height := 0; -- don't display it
            Buddy_Height := 0;
          elsif The_Child.The_Kind = Combo_Box_Child then -- make room for borders
            Object_Height := 200;
          end if;
          Unused_Hdwp := Win32.Winuser.DeferWindowPos (Deferred_Positioning,
                                                       The_Child.The_Handle,
                                                       Win32.Winuser.HWND_TOP,
                                                       Horizontal_Position,
                                                       Vertical_Position,
                                                       Object_Width,
                                                       Object_Height, 0);
          Children_Drawn(The_Child.The_Kind) := Children_Drawn(The_Child.The_Kind) + 1;
          if The_Child.Buddy_Child /= null then  -- draw buddy window
            Unused_Hdwp := Win32.Winuser.DeferWindowPos (Deferred_Positioning,
                                                         The_Child.Buddy_Child.The_Handle,
                                                         Win32.Winuser.HWND_TOP,
                                                         Horizontal_Position - Buddy_Width,
                                                         Vertical_Position,
                                                         Buddy_Width,
                                                         Buddy_Height, 0);
          end if;
          Vertical_Position := Vertical_Position + Vertical_Increment;
          Horizontal_Position := Horizontal_Position + Object_Width;
        end if;
        The_Child := The_Child.Next_Sibling;
      end loop;
      Unused_Int := Win32.Winuser.ReleaseDC (Private_Window, The_Device_Context);
      if Current_Page.Keyboard_Focus /= null then
        Unused_Hwnd := Win32.Winuser.SetFocus(Current_Page.Keyboard_Focus.The_Handle);
      end if;
    end if;
    Unused_Hdwp := Win32.Winuser.DeferWindowPos (Deferred_Positioning,
                                                 Win32.Windef.HWND(The_Status_Line),
                                                 Win32.Winuser.HWND_TOP,
                                                 0, 0, 0, 0, 0);
    Unused_Bool := Win32.Winuser.EndDeferWindowPos (Deferred_Positioning);
  end Redraw_Main_Window;


  procedure Make_Visible (The_Control : Tab_Control) is
    Unused_Bool : Win32.BOOL;
  begin
    Unused_Bool := Win32.Winuser.ShowWindow(Win32.Windef.HWND(The_Control), Win32.Winuser.SW_SHOW);
  end Make_Visible;


  procedure Make_Invisible (The_Control : Tab_Control) is
    Unused_Bool : Win32.BOOL;
  begin
    Unused_Bool := Win32.Winuser.ShowWindow(Win32.Windef.HWND(The_Control), Win32.Winuser.SW_HIDE);
  end Make_Invisible;


  procedure Make_Visible (The_Page : Page) is
    The_Child : Child_Information_Ptr;
    Unused : Win32.BOOL;
  begin
    The_Child := The_Page.First_Child;
    loop
      exit when The_Child = null;
      if not The_Child.Is_Hidden then
        Unused := Win32.Winuser.ShowWindow(The_Child.The_Handle, Win32.Winuser.SW_SHOW);
      end if;
      The_Child := The_Child.Next_Sibling;
    end loop;
  end Make_Visible;


  procedure Make_Invisible (The_Page : Page) is
    The_Child : Child_Information_Ptr;
    Unused    : Win32.BOOL;
  begin
    The_Child := The_Page.First_Child;
    loop
      exit when The_Child = null;
      Unused := Win32.Winuser.ShowWindow(The_Child.The_Handle, Win32.Winuser.SW_HIDE);
      The_Child := The_Child.Next_Sibling;
    end loop;
  end Make_Invisible;


  procedure Display_Page (The_Page : Page) is
  begin
    Make_Invisible (Current_Page);
    Current_Page := The_Page;
    Make_Visible (Current_Page);
    Redraw_Main_Window;
    if Current_Page.The_Action /= null then -- Signal page change
      Execute (Current_Page.The_Action);
    end if;
  end Display_Page;


  package Page_Conversion is new System.Address_To_Access_Conversions (Page_Information);

  function Index_Of (The_Page : Page) return Natural is
    Tab_Info  : aliased Win32.Comctl.Tc_Item;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPARAM);
    function Convert is new Ada.Unchecked_Conversion (Win32.LPARAM, System.Address);
  begin
    Tab_Info.Mask := Win32.Comctl.Tcif_Param;
    for Index in 0 .. Nr_Of_Pages - 1 loop
      Send_Message (Win32.Windef.HWND(The_Tabs),
                    Win32.Comctl.Tcm_Getitem,
                    Win32.WPARAM (Index),
                    Convert(Tab_Info'address));
      if The_Page = Page(Page_Conversion.To_Pointer(Convert(Tab_Info.Lparam))) then
        return Index;
      end if;
    end loop;
    return 0;  -- Return first if not found
  end Index_Of;


  procedure Select_Page (The_Page : Page) is
  begin
    if Is_Active and then Nr_Of_Pages > 0 then
      Send_Message (Win32.Windef.HWND(The_Tabs),
                    Win32.Comctl.Tcm_Setcursel,
                    Win32.WPARAM (Index_Of (The_Page)),0);
      Display_Page (The_Page);
    end if;
  end Select_Page;


  procedure Page_Change is
    Page_Number : Win32.LRESULT;
    Tab_Info    : aliased Win32.Comctl.Tc_Item;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPARAM);
    function Convert is new Ada.Unchecked_Conversion (Win32.LPARAM, System.Address);
    use type Win32.LRESULT;
  begin
    if Is_Active then
      Page_Number := Send_Message (Win32.Windef.HWND(The_Tabs),
                                   Win32.Comctl.Tcm_Getcursel, 0, 0);
      if Page_Number >= 0 then
        Tab_Info.Mask := Win32.Comctl.Tcif_Param;
        Send_Message (Win32.Windef.HWND(The_Tabs),
                      Win32.Comctl.Tcm_Getitem,
                      Win32.WPARAM(Page_Number),
                      Convert(Tab_Info'address));
        Display_Page (Page(Page_Conversion.To_Pointer(Convert (Tab_Info.Lparam))));
      end if;
    end if;
  end Page_Change;


  function Listview_Sort_Routine (First_Info    : Information;
                                  Second_Info   : Information;
                                  The_List_View : Child_Information_Ptr) return Integer
  with
    Convention => Stdcall;

  function Listview_Sort_Routine (First_Info    : Information;
                                  Second_Info   : Information;
                                  The_List_View : Child_Information_Ptr) return Integer is
    The_Return_Code : Relation;
  begin
    The_Return_Code := The_List_View.The_Sort_Routine.all (The_List_View.The_Sort_Column,
                                                           First_Info,
                                                           Second_Info);
    if The_List_View.The_Sort_Direction = Forward then
      case The_Return_Code is
        when Less_Than    => return -1;
        when Equal_To     => return 0;
        when Greater_Than => return 1;
      end case;
    else
      case The_Return_Code is
        when Less_Than    => return 1;
        when Equal_To     => return 0;
        when Greater_Than => return -1;
      end case;
    end if;
  end Listview_Sort_Routine;


  function Lparam_Of (The_Handle : Win32.Windef.HWND;
                      The_Item   : Win32.INT) return Win32.LPARAM is
    The_Info_Item : Win32.Comctl.Lv_Item_Wide;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPARAM);
  begin
    The_Info_Item.Mask := Win32.Comctl.Lvif_Param;
    The_Info_Item.Item := The_Item;
    The_Info_Item.Subitem := 0;
    Send_Message_Wide (The_Handle,
                       Win32.Comctl.Lvm_Getitem_Wide,
                       0, Convert(The_Info_Item'address));
    return The_Info_Item.Lparam;
  end Lparam_Of;


  procedure Process_Tree_View_Click (The_Handle  : Win32.Windef.HWND;
                                     The_Routine : access procedure (Item : Information)) is
    Selected_Item : aliased Win32.LRESULT;
    The_Item      : aliased Win32.Comctl.Tv_Item_Ansi;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPARAM);
    function Convert is new Ada.Unchecked_Conversion (Win32.LPARAM, Information);
  begin
    if Is_Active and (The_Routine /= null) then
      Selected_Item := Send_Message (The_Handle,
                                     Win32.Comctl.Tvm_Getnextitem,
                                     Win32.Comctl.Tvgn_Caret,
                                     Convert(Selected_Item'address));
      The_Item.Hitem := Tree_Item(Selected_Item);
      The_Item.Mask := Win32.Comctl.Tvif_Param;
      Send_Message (The_Handle,
                    Win32.Comctl.Tvm_Getitem_Ansi,
                    Win32.Comctl.Tvgn_Caret,
                    Convert(The_Item'address));
      Execute (The_Routine, Convert (The_Item.Lparam));
    end if;
  end Process_Tree_View_Click;


  function Ansi_Handler (The_Child        : Child_Information_Ptr;
                         For_Column       : Natural;
                         With_Information : Information) return Wide_String is
    The_Text_Handler : Text_Handler;
    function Convert is new Ada.Unchecked_Conversion (Action_Routine, Text_Handler);
  begin
    The_Text_Handler := Convert (The_Child.The_Action_Routine);
    return To_Wide (The_Text_Handler (For_Column, With_Information) & Nul);
  end Ansi_Handler;


  package Lv_Dispinfo_Wide_Conversion is new System.Address_To_Access_Conversions (Win32.Comctl.Lv_Dispinfo_Wide);

  package Nm_Listview_Conversion is new System.Address_To_Access_Conversions (Win32.Comctl.Nm_Listview);

  package Customdraw_Info_Conversion is new System.Address_To_Access_Conversions (Win32.Comctl.Nmlv_Customdraw);

  function Process_Notification_From_Child (Unused_Wparam : Win32.WPARAM;
                                                   Lparam : Win32.LPARAM) return Win32.LRESULT is

    The_Notification      : Win32.Comctl.Lv_Dispinfo_Wide_Ptr;
    The_Custom_Draw_Info  : Win32.Comctl.Customdraw_Info_Ptr;
    The_Listview_Info     : Win32.Comctl.Nm_Listview_Ptr;
    The_Child             : Child_Information_Ptr;
    function Convert is new Ada.Unchecked_Conversion (Win32.LPARAM, Information);
    function Convert is new Ada.Unchecked_Conversion (Win32.LPARAM, System.Address);
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPARAM);
    function Convert is new Ada.Unchecked_Conversion (Child_Information_Ptr, Win32.WPARAM);
    use type Win32.INT;
    use type Win32.Comctl.Lv_Dispinfo_Wide_Ptr;
    use type Win32.Comctl.Customdraw_Info_Ptr;
    use type Win32.Comctl.Nm_Listview_Ptr;
    use type Win32.INT_PTR;

  begin
    The_Notification := Win32.Comctl.Lv_Dispinfo_Wide_Ptr(Lv_Dispinfo_Wide_Conversion.To_Pointer (Convert (Lparam)));
    if The_Notification = null then
      return 0;
    elsif The_Notification.Hdr.Idfrom = Win32.INT_PTR(Id_Tabs) then
      if The_Notification.Hdr.Code = Win32.Comctl.Tcn_Selchange then
        Page_Change;
      end if;
    elsif Current_Page /= null then
      The_Child := Current_Page.First_Child;
      loop -- check if a list view in current page
        exit when The_Child = null;
        if The_Notification.Hdr.Idfrom = Win32.INT_PTR(The_Child.The_Window_Id) then
          -- Notification from a child in the current page
          Active_Child := The_Child;
          if The_Child.The_Kind = List_View_Child then -- we can process it further
            if The_Notification.Hdr.Code = Win32.Comctl.Lvn_Getdisplayinfo_Wide then
              The_Notification.Item.Text
                := Wide_Text_Address_Of (Ansi_Handler (The_Child,
                                                       Natural(The_Notification.Item.Subitem),
                                                       Convert(The_Notification.Item.Lparam)));
            elsif The_Notification.Hdr.Code = Win32.Comctl.Lvn_Columnclick then
              The_Listview_Info := Win32.Comctl.Nm_Listview_Ptr(Nm_Listview_Conversion.To_Pointer (Convert (Lparam)));
              if The_Listview_Info /= null then
                The_Child.The_Sort_Column := Natural(The_Listview_Info.Subitem);
                Send_Message (The_Child.The_Handle,
                              Win32.Comctl.Lvm_Sortitems,
                              Convert(The_Child),
                              Convert(Listview_Sort_Routine'address));
              end if;
              if The_Child.The_Sort_Direction = Forward then
                The_Child.The_Sort_Direction := Backward;
              else
                The_Child.The_Sort_Direction := Forward;
              end if;
            elsif The_Notification.Hdr.Code = The_Child.The_Click_Kind then
              if The_Child.The_Click_Routine /= null then
                The_Listview_Info := Win32.Comctl.Nm_Listview_Ptr(Nm_Listview_Conversion.To_Pointer (Convert (Lparam)));
                if (The_Listview_Info /= null) and then (The_Listview_Info.Item >= 0) then
                  -- Unfortunately Lparam isn't setup so must go and get it!
                  The_Listview_Info.Lparam := Lparam_Of (The_Child.The_Handle, The_Listview_Info.Item);
                  Execute (The_Child.The_Click_Routine, Convert (The_Listview_Info.Lparam));
                end if;
              end if;
            elsif The_Notification.Hdr.Code = Win32.Comctl.Nm_Customdraw then
              The_Custom_Draw_Info
                := Win32.Comctl.Customdraw_Info_Ptr(Customdraw_Info_Conversion.To_Pointer (Convert (Lparam)));
              if The_Custom_Draw_Info /= null then
                if The_Custom_Draw_Info.Nmcd.Drawstage = Win32.Comctl.Cdds_Prepaint and then
                   The_Child.The_Color_Routine /= null
                then  -- Ask to be notified when item is about to be drawn
                  return Win32.Comctl.Cdrf_Notifyitemdraw;
                elsif The_Custom_Draw_Info.Nmcd.Drawstage = Win32.Comctl.Cdds_Itemprepaint then
                  if The_Child.Color_Background then
                    The_Custom_Draw_Info.Bkgndcolor :=
                                         The_Child.The_Color_Routine (Convert(The_Notification.Item.Lparam));
                  else
                    The_Custom_Draw_Info.Textcolor :=
                                         The_Child.The_Color_Routine (Convert(The_Notification.Item.Lparam));
                  end if;
                end if;
              end if;
            end if;
          elsif The_Child.The_Kind = Tree_View_Child then
            if The_Notification.Hdr.Code = Win32.Comctl.Nm_Dblclk then
              Process_Tree_View_Click (The_Child.The_Handle, The_Child.The_Click_Routine);
            elsif The_Notification.Hdr.Code = Win32.Comctl.Nm_Customdraw then
              The_Custom_Draw_Info
                := Win32.Comctl.Customdraw_Info_Ptr (Customdraw_Info_Conversion.To_Pointer (Convert (Lparam)));
              if The_Custom_Draw_Info /= null then
                if (The_Custom_Draw_Info.Nmcd.Drawstage = Win32.Comctl.Cdds_Prepaint) and
                   (The_Child.The_Color_Routine /= null)
                then -- Ask to be notified when item is about to be drawn
                  return Win32.Comctl.Cdrf_Notifyitemdraw;
                elsif The_Custom_Draw_Info.Nmcd.Drawstage = Win32.Comctl.Cdds_Itemprepaint then
                  The_Custom_Draw_Info.Bkgndcolor := White; -- Disable blue background for selections
                  The_Custom_Draw_Info.Textcolor :=
                                       The_Child.The_Color_Routine (Convert(The_Notification.Item.Lparam));
                end if;
              end if;
            end if;
          end if;
          exit; -- finish processing
        end if;
        The_Child := The_Child.Next_Sibling;
      end loop;
    end if;
    return 0;
  end Process_Notification_From_Child;


  procedure Set_Font_For (The_Handle : Win32.Windef.HWND) is
    function Convert is new Ada.Unchecked_Conversion(Win32.Windef.HGDIOBJ, Win32.WPARAM);
  begin
    Send_Message (The_Handle,
                  Win32.Winuser.WM_SETFONT,
                  Convert(The_Font), Win32.FALSE);
  end Set_Font_For;


  procedure Set_Unicode_For (The_Handle : Win32.Windef.HWND) is
  begin
    Send_Message_Wide (The_Handle,
                       Win32.Comctl.Ccm_Setunicodeformat,
                       Win32.TRUE,
                       0);
  end Set_Unicode_For;


  package Child_Information_Conversion is new System.Address_To_Access_Conversions (Child_Information);

  procedure Create_Child (The_Object : Win32.LPARAM;
                          The_Page   : Win32.WPARAM) is

                         Window_Style : Win32.DWORD := Win32.Winuser.WS_CHILD;

    function Convert is new Ada.Unchecked_Conversion (Win32.LPARAM, System.Address);
    function Convert is new Ada.Unchecked_Conversion (Win32.WPARAM, System.Address);

    The_Child : constant Child_Information_Ptr
                           := Child_Information_Ptr(Child_Information_Conversion.To_Pointer (Convert (The_Object)));

    Parent_Page : constant Page := Page(Page_Conversion.To_Pointer (Convert (The_Page)));

    Class_Name     : Win32.LPCWSTR;
    Previous_Child : Child_Information_Ptr;

    use type Win32.INT;
    use type Win32.Windef.HWND;
  begin
    if (The_Child = null) or (Parent_Page = null) then
      return;
    end if;
    case The_Child.The_Kind is
      when Button_Child =>
        Class_Name := Wide_Text_Address_Of ("button");
        Window_Style := Window_Style + Win32.Winuser.BS_PUSHBUTTON;
      when Progress_Bar_Child =>
        Class_Name := Wide_Text_Address_Of ("msctls_progress32");
      when Tree_View_Child =>
        Class_Name := Wide_Text_Address_Of ("SysTreeView32");
        Window_Style := Window_Style + Win32.Winuser.WS_BORDER +
                                       Win32.Comctl.Tvs_Haslines +
                                       Win32.Comctl.Tvs_Hasbuttons +
                                       Win32.Comctl.Tvs_Linesatroot;
      when Text_View_Child =>
        Class_Name := Wide_Text_Address_Of ("RichEdit20W");
        Window_Style := Window_Style + Win32.Winuser.ES_MULTILINE +
                                       Win32.Winuser.ES_READONLY +
                                       Win32.Comctl.Es_Sunken +
                                       Win32.Comctl.Es_Savesel +
                                       Win32.Winuser.WS_HSCROLL +
                                       Win32.Winuser.WS_VSCROLL;
        if Rich_Edit_Dll = System.Null_Address then
          declare
            Dll_Name : aliased constant Wide_String := "RichEd32.DLL" & Wide_Nul;
          begin
            Rich_Edit_Dll := Win32.Winbase.LoadLibraryW (Win32.Addr(Dll_Name));
          end;
        end if;
      when List_View_Child =>
        Class_Name := Wide_Text_Address_Of ("SysListView32");
        Window_Style := Window_Style + Win32.Winuser.WS_BORDER +
                        Win32.Comctl.Lvs_Report + Win32.Comctl.Lvs_Showselalways;
        if The_Child.The_Sort_Routine = null then -- No sorting
          Window_Style := Window_Style + Win32.Comctl.Lvs_Nosortheader;
        end if;
      when Track_Bar_Child =>
        Class_Name := Wide_Text_Address_Of ("msctls_trackbar32");
        Window_Style := Window_Style + Win32.Comctl.Tbs_Autoticks +
                                       Win32.Comctl.Tbs_Tooltips;
      when Edit_Box_Child =>
        Class_Name := Wide_Text_Address_Of ("Edit");
        Window_Style := Window_Style + Win32.Winuser.WS_BORDER +
                                       Win32.Winuser.ES_LEFT +
                                       Win32.Winuser.ES_AUTOHSCROLL;
        if The_Child.Is_Password then
          Window_Style := Window_Style + Win32.Winuser.ES_PASSWORD;
        end if;
        if not The_Child.Is_Modifiable then
          Window_Style := Window_Style + Win32.Winuser.ES_READONLY;
        end if;
      when Check_Box_Child =>
        Class_Name := Wide_Text_Address_Of ("Button");
        Window_Style := Window_Style + Win32.Winuser.BS_AUTOCHECKBOX;
      when Combo_Box_Child =>
        Window_Style := Window_Style + Win32.Winuser.WS_VSCROLL + Win32.Winuser.CBS_AUTOHSCROLL;
        Class_Name := Wide_Text_Address_Of ("Combobox");
        if The_Child.Is_Modifiable then
          Window_Style := Window_Style + Win32.Winuser.CBS_DROPDOWN;
        else
          Window_Style := Window_Style + Win32.Winuser.CBS_DROPDOWNLIST;
        end if;
        if The_Child.Is_Sorted then
          Window_Style := Window_Style + Win32.Winuser.CBS_SORT;
        end if;
      when Static_Child =>
        Class_Name := Wide_Text_Address_Of ("Static");
    end case;
    if Parent_Page = Current_Page then
      Window_Style := Window_Style + Win32.Winuser.WS_VISIBLE;
    end if;
    if The_Child.The_Handle = System.Null_Address then
      The_Child.The_Handle := Win32.Winuser.CreateWindowExW (0, Class_Name,
                                                           The_Child.Initial_Title,
                                                           Window_Style,
                                                           0,0,0,0,
                                                           Private_Window,
                                                           Convert(Information(The_Child.The_Window_Id)),
                                                           Our_Instance,
                                                           System.Null_Address);
    end if;
    The_Child.Parent_Page := Parent_Page;
    Parent_Page.Nr_Of_Children := Parent_Page.Nr_Of_Children + 1;
    Set_Unicode_For (The_Child.The_Handle);
    Set_Font_For (The_Child.The_Handle);
    if Parent_Page.Nr_Of_Children = 1 then -- First and only child
      Parent_Page.First_Child := The_Child;
    elsif Display_Priority(Parent_Page.First_Child.The_Kind) > Display_Priority(The_Child.The_Kind) then
      -- Insert at head
      The_Child.Next_Sibling :=  Parent_Page.First_Child;
      Parent_Page.First_Child := The_Child;
    else -- Order children in order that they will be drawn.
      Previous_Child := Parent_Page.First_Child;
      loop
        exit when Previous_Child.Next_Sibling = null; -- this is the last
        exit when Display_Priority(The_Child.The_Kind) < Display_Priority(Previous_Child.Next_Sibling.The_Kind);
        Previous_Child := Previous_Child.Next_Sibling;
      end loop;
      The_Child.Next_Sibling := Previous_Child.Next_Sibling;
      Previous_Child.Next_Sibling := The_Child;
    end if;
    Parent_Page.Population(The_Child.The_Kind) := Parent_Page.Population(The_Child.The_Kind)+ 1;
    if Parent_Page = Current_Page then
      Redraw_Main_Window;
    end if;
    The_Child.For_Completion.Signal;
  end Create_Child;


  function Is_Defined (The_Child : Child'class) return Boolean is
  begin
    return The_Child.Ptr /= null;
  end Is_Defined;


  procedure Set (The_Item : Menu_Item_Access) is
    Unused : Win32.DWORD;
  begin
    Unused := Win32.Winuser.CheckMenuItem (The_Item.The_Parent_Menu.The_Handle,
                                           Win32.UINT(The_Item.The_Id),
                                           Win32.Winuser.MF_CHECKED);
  end Set;


  function Is_Set (The_Item : Menu_Item_Access) return Boolean is
    The_State : Win32.UINT;
    use type Win32.UINT;
  begin
    The_State := Win32.Winuser.GetMenuState (The_Item.The_Parent_Menu.The_Handle,
                                             Win32.UINT(The_Item.The_Id),
                                             Win32.Winuser.MF_BYCOMMAND);
    return (Win32.Winuser.MF_CHECKED and The_State) /= 0;
  end Is_Set;


  procedure Clear (The_Item : Menu_Item_Access) is
    Unused : Win32.DWORD;
  begin
    Unused := Win32.Winuser.CheckMenuItem (The_Item.The_Parent_Menu.The_Handle,
                                           Win32.UINT(The_Item.The_Id),
                                           Win32.Winuser.MF_UNCHECKED);
  end Clear;


  procedure Set_New (The_Radio_Item : Menu_Item_Access) is
  begin
    Clear (The_Radio_Item.The_Parent_Menu.Radio_Item);
    The_Radio_Item.The_Parent_Menu.Radio_Item := The_Radio_Item;
    Set (The_Radio_Item);
  end Set_New;


  procedure Process_Message_From_Child (       Wparam : Win32.WPARAM;
                                        Unused_Lparam : Win32.LPARAM) is
    type Dword_Rec is record
      Window_Id    : Win32.WORD;
      Notification : Win32.WORD;
    end record;
    The_Longword : Win32.WPARAM;
    The_Message  : Dword_Rec;
    for The_Message'address use The_Longword'address;
    The_Child         : Child_Information_Ptr;
    The_Menu_Item     : Menu_Item_Access;
    The_Window_Id     : Natural;
    Callback_Required : Boolean := False;
    use type Win32.WORD;
  begin
    The_Longword := Wparam;
    The_Window_Id := Natural (The_Message.Window_Id);
    if The_Window_Id >= First_Menu_Item_Id then -- look for menu item
      Mutex.Wait;
      The_Menu_Item := Last_Menu_Item;
      loop
        exit when The_Menu_Item = null;
        if The_Window_Id = The_Menu_Item.The_Id then
          case The_Menu_Item.The_Parent_Menu.The_Kind is
          when Plain =>
            null;
          when Checked =>
            if Is_Set (The_Menu_Item) then
              Clear (The_Menu_Item);
            else
              Set (The_Menu_Item);
            end if;
          when Radio =>
            Set_New (The_Menu_Item);
          end case;
          Execute (The_Menu_Item.The_Parent_Menu.The_Callback, The_Menu_Item.The_Information);
          exit;
        else
          The_Menu_Item := The_Menu_Item.Next;
        end if;
      end loop;
      Mutex.Signal;
    elsif Current_Page /= null then  -- look for child
      The_Child := Current_Page.First_Child;
      loop
        exit when The_Child = null;
        if The_Window_Id = The_Child.The_Window_Id then
          Active_Child := The_Child;
          if (The_Child.The_Action_Routine /= null) or
             (The_Child.Qualified_Action_Routine /= null)
          then -- Callback routine is supplied
            case The_Child.The_Kind is
              when Combo_Box_Child =>
                if The_Child.Is_Modifiable then
                   Callback_Required := (The_Message.Notification = Win32.Winuser.CBN_KILLFOCUS) or
                                        (The_Message.Notification = Win32.Winuser.CBN_SELCHANGE);
                else
                   Callback_Required := The_Message.Notification = Win32.Winuser.CBN_SELCHANGE;
                end if;
              when Edit_Box_Child =>
                   Callback_Required := The_Message.Notification = Win32.Winuser.EN_KILLFOCUS;
              when others =>
                   Callback_Required := True;
            end case;
            if Callback_Required then
              if The_Child.Qualified_Action_Routine /= null then
                Execute (The_Child.Qualified_Action_Routine, The_Child.User_Information);
              else
                Execute (The_Child.The_Action_Routine);
              end if;
            end if;
          end if;
          exit; -- Finish processing
        end if;
        The_Child := The_Child.Next_Sibling;
      end loop;
    end if;
  end Process_Message_From_Child;


  function Mainwindowproc(Hwnd    : Win32.Windef.HWND;
                          Message : Win32.UINT;
                          Wparam  : Win32.WPARAM;
                          Lparam  : Win32.LPARAM) return Win32.LRESULT
  with
    Convention => Stdcall;


  function Mainwindowproc(Hwnd    : Win32.Windef.HWND;
                          Message : Win32.UINT;
                          Wparam  : Win32.WPARAM;
                          Lparam  : Win32.LPARAM) return Win32.LRESULT is
    Unused     : Win32.BOOL;
    The_Wparam : Win32.WPARAM := Wparam;
    The_Lparam : Win32.LPARAM := Lparam;
  begin
    case Message is
      when Win32.Comctl.Wm_Notify =>
        return Process_Notification_From_Child (Wparam, Lparam);
--      when Win32.Comctl.Wm_Notifyformat =>  -- Allow control to decide the format
--        return either Nfr_Ansi or Nfr_Unicode
--      The above is not required as the format for a control is fixed.
      when Wm_Create_Child =>
        Create_Child (Lparam, Wparam);
      when Win32.Winuser.WM_MOVE =>
        Note_Private_Window_Metrics;
      when Win32.Winuser.WM_SIZE =>
        Note_Private_Window_Metrics;
        Redraw_Main_Window;
      when Win32.Winuser.WM_COMMAND =>
        Process_Message_From_Child (Wparam, Lparam);
        return Win32.Winuser.DefWindowProc (Hwnd, Message, Wparam, Lparam);
      when Win32.Winuser.WM_DESTROY =>
        if The_Key_Handling_Rountine /= null then
          Unused := Win32.Winuser.UnhookWindowsHookEx (The_Keyboard_Hook);
          The_Keyboard_Hook := System.Null_Address;
        end if;
        if The_Mouse_Handling_Rountine /= null then
          Unused := Win32.Winuser.UnhookWindowsHookEx (The_Mouse_Hook);
          The_Mouse_Hook := System.Null_Address;
        end if;
        Is_Destroying := True;
        Win32.Winuser.PostQuitMessage (0);
      when Wm_User_Callback =>
        Execute (The_Callback_Routine);
      when Win32.Winuser.WM_CLOSE =>
        The_Wparam := Wparam;
        The_Lparam := Lparam;
        select
          The_Termination_Handler.Start;
        else
          null;
        end select;
        return 0;
      when Wm_User_Close =>
        Is_Active := False;
        return Win32.Winuser.DefWindowProc (Hwnd, Win32.Winuser.WM_CLOSE, The_Wparam, The_Lparam);
      when others =>
        return Win32.Winuser.DefWindowProc (Hwnd, Message, Wparam, Lparam);
    end case;
    return 0;
  exception
  when Event: others =>
    Log.Write ("Gui.Mainwindowproc", Event);
    return 0;
  end Mainwindowproc;


  procedure Register_Class (Class_Name   : String;
                            The_Callback : Win32.Winuser.WNDPROC;
                            The_Icon     : Win32.Windef.HICON) is
    Unused     : Win32.Windef.ATOM;
    Class_Spec : constant Win32.Winuser.ac_WNDCLASSEXA_t := new Win32.Winuser.WNDCLASSEXA;
    use type Win32.UINT;
  begin
    Class_Spec.cbSize := Win32.Winuser.WNDCLASSEXA'size / 8;
    Class_Spec.style := 0;
    Class_Spec.lpfnWndProc   := The_Callback;
    Class_Spec.cbClsExtra    := 0;
    Class_Spec.cbWndExtra    := 0;
    Class_Spec.hInstance     := Our_Instance;
    Class_Spec.hIcon         := The_Icon;
    Class_Spec.hIconSm       := The_Icon;
    Class_Spec.hCursor       := Win32.Winuser.LoadCursor (System.Null_Address, Win32.LPCSTR(Win32.Winuser.IDC_ARROW));
    Class_Spec.hbrBackground := Convert(Win32.Winuser.COLOR_BACKGROUND);
    Class_Spec.lpszMenuName  := null;
    Class_Spec.lpszClassName := Win32.Addr(Class_Name);
    Unused := Win32.Winuser.RegisterClassExA (Class_Spec);
  end Register_Class;


  function Create_Window (The_Name          : Wide_String;
                          Window_Width      : Win32.INT;
                          Window_Height     : Win32.INT;
                          Window_X_Position : Win32.INT;
                          Window_Y_Position : Win32.INT;
                          Always_Topmost    : Boolean) return Win32.Windef.HWND is
    The_Extended_Style : Win32.DWORD := 0;
  begin
    if Always_Topmost then
      The_Extended_Style := Win32.Winuser.WS_EX_TOPMOST;
    end if;
    return Win32.Winuser.CreateWindowExW (The_Extended_Style,
                                          Win32.Addr(The_Name), -- Class
                                          Win32.Addr(The_Name), -- Window title
                                          Win32.Winuser.WS_OVERLAPPEDWINDOW,
                                          Window_X_Position,  Window_Y_Position,
                                          Window_Width, Window_Height,
                                          System.Null_Address, The_Menu_Bar, Our_Instance,
                                          System.Null_Address);
  end Create_Window;


  function Create_Status_Line (The_Parent : Win32.Windef.HWND) return Status_Line_Control is
    Window_Style  : constant := Win32.Winuser.WS_CHILD +
                                Win32.Winuser.WS_BORDER +
                                Win32.Winuser.WS_VISIBLE +
                                Win32.Comctl.Sbs_Sizegrip;
    Name : aliased constant Wide_String := "msctls_statusbar32" & Wide_Nul;
  begin
    return Status_Line_Control (
                  Win32.Winuser.CreateWindowW (
                                Win32.Addr(Name),
                                Win32.Addr(Wide_Null_Name),
                                Window_Style,
                                0,0,0,0,
                                The_Parent,
                                Convert(Information(Id_Status_Bar)),
                                Our_Instance,
                                System.Null_Address));
  end Create_Status_Line;


  function Create_Tabs (The_Parent : Win32.Windef.HWND) return Tab_Control is
    Window_Style : constant := Win32.Winuser.WS_CHILD +
                               Win32.Winuser.WS_CLIPSIBLINGS;
    Name : aliased constant Wide_String := "SysTabControl32" & Wide_Nul;
  begin
    return Tab_Control (Win32.Winuser.CreateWindowW (Win32.Addr(Name),
                                                     Win32.Addr(Wide_Null_Name),
                                                     Window_Style,
                                                     0,0,0,0,
                                                     The_Parent,
                                                     Convert(Information(Id_Tabs)),
                                                     Our_Instance,
                                                     System.Null_Address));
  end Create_Tabs;


  procedure Unregister_Class (Class_Name : String) is
    Unused : Win32.BOOL;
  begin
    Unused := Win32.Winuser.UnregisterClass (Win32.Addr(Class_Name), Our_Instance);
  end Unregister_Class;


  procedure Dispatch is
    The_Message : aliased Win32.Winuser.MSG;
    Unused_Bool : Win32.BOOL;
    Unused_Long : Win32.LRESULT;
    use type Win32.BOOL;
  begin
    while Win32.Winuser.GetMessage (The_Message'unchecked_access, System.Null_Address, 0, 0) = Win32.TRUE loop
      Unused_Bool := Win32.Winuser.TranslateMessage (The_Message'unchecked_access);
      Unused_Long := Win32.Winuser.DispatchMessage (The_Message'unchecked_access);
      exit when Is_Destroying;
    end loop;
  end Dispatch;


  task body Display is
    Unused   : Win32.BOOL;
    The_Icon : Win32.Windef.HICON;
    use type Win32.Windef.HWND;
  begin
    accept Create (Application_Name  : String;
                   Window_Width      : Win32.INT;
                   Window_Height     : Win32.INT;
                   Window_X_Position : Win32.INT;
                   Window_Y_Position : Win32.INT;
                   Always_Topmost    : Boolean)
    do
      The_Application_Name := new Wide_String'(To_Wide(Application_Name) & Wide_Nul);
      begin
        The_Icon := Win32.Winuser.LoadImageA (Our_Instance, Icon_Resource, 1, 0, 0, Win32.Winuser.LR_DEFAULT_SIZE);
        The_Application_Class_Name := new String'(Application_Name & Nul);
        Register_Class (The_Application_Class_Name.all, Mainwindowproc'access, The_Icon);
        if Is_Visible (X_Position => Win32.LONG(Window_X_Position),
                       Y_Position => Win32.LONG(Window_Y_Position),
                       Width      => Win32.LONG(Window_Width),
                       Height     => Win32.LONG(Window_Height))
        then
          Private_Window := Create_Window (The_Application_Name.all,
                                           Window_Width,
                                           Window_Height,
                                           Window_X_Position,
                                           Window_Y_Position,
                                           Always_Topmost);
        else
          Private_Window := Create_Window (The_Application_Name.all,
                                           Window_Width,
                                           Window_Height,
                                           Default_Position,
                                           Default_Position,
                                           Always_Topmost);
        end if;
      end;
    end Create;
    if Private_Window = System.Null_Address then
      raise Main_Window_Creation_Failure;
    end if;
    The_Status_Line := Create_Status_Line (Private_Window);
    if The_Status_Line = System.Null_Address then
      raise Status_Line_Creation_Failure;
    end if;
    The_Tabs := Create_Tabs (Private_Window);
    if The_Tabs = System.Null_Address then
      raise Tab_Creation_Failure;
    end if;
    Main_Thread := Current_Thread_Id;
    accept Ready;
    Unused := Win32.Winuser.ShowWindow (Private_Window, Win32.Winuser.SW_SHOW);
    Unused := Win32.Winuser.UpdateWindow (Private_Window);
    Dispatch;
    Unused := Win32.Winuser.DestroyWindow (Private_Window);
    Private_Window := System.Null_Address;
    Unregister_Class (The_Application_Class_Name.all);
    if Rich_Edit_Dll /= System.Null_Address then
      Unused := Win32.Winbase.FreeLibrary (Rich_Edit_Dll);
    end if;
  exception
  when Event: others =>
    Log.Write ("Gui.Display", Event);
    accept Ready;
    Unregister_Class (The_Application_Class_Name.all);
    Private_Window := System.Null_Address;
    select
      The_Termination_Handler.Start;
    else
      null;
    end select;
  end Display;


  procedure Disable_Close_Button is
    Unused : Win32.BOOL;
  begin
    Unused := Win32.Winuser.EnableMenuItem (Win32.Winuser.GetSystemMenu (Private_Window, Win32.FALSE),
                                            Win32.Winuser.SC_CLOSE,
                                            Win32.UINT(Win32.Winuser.MF_BYCOMMAND +
                                                       Win32.Winuser.MF_DISABLED +
                                                       Win32.Winuser.MF_GRAYED));
  end Disable_Close_Button;


  procedure Close is
  begin
    Send_Message (Private_Window, Win32.Winuser.WM_CLOSE, 0, 0);
  end Close;


  function Next_Child_Id return Natural is
    The_Id : constant Natural := The_Next_Child_Id;
  begin
    The_Next_Child_Id := The_Next_Child_Id + 1;
    return The_Id;
  end Next_Child_Id;


  procedure Create (The_Child  : Child_Information_Ptr;
                    The_Parent : Page) is
    function Convert is new Ada.Unchecked_Conversion (Page, Win32.WPARAM);
    function Convert is new Ada.Unchecked_Conversion (Child_Information_Ptr, Win32.LPARAM);
  begin
    Send_Message (Private_Window,
                  Wm_Create_Child,
                  Convert(The_Parent),
                  Convert(The_Child));
    The_Child.For_Completion.Wait;
  end Create;


  procedure Create (Application_Name    : String;
                    Termination_Routine : Action_Routine;
                    Window_Width        : Natural := 0;
                    Window_Height       : Natural := 0;
                    Window_X_Position   : Integer := Default_Position;
                    Window_Y_Position   : Integer := Default_Position;
                    Always_Topmost      : Boolean := False) is
    The_Height : Win32.INT := Win32.INT(Window_Height);
    The_Width  : Win32.INT := Win32.INT(Window_Width);
  begin
    if The_Action_Handler /= null then
      raise Sequence_Error;  -- Can only be used once
    end if;
    The_Termination_Routine := Termination_Routine;
    if Window_Width = 0 then -- default;
      The_Width := Win32.Winuser.CW_USEDEFAULT;
    end if;
    if Window_Height = 0 then -- default;
      The_Height := Win32.Winuser.CW_USEDEFAULT;
    end if;
    The_Action_Handler := new Action_Handler;
    The_Termination_Handler := new Termination_Handler;
    Main_Display := new Display;
    Is_Active := True;
    Main_Display.Create (Application_Name, The_Width, The_Height,
                         Win32.INT(Window_X_Position), Win32.INT(Window_Y_Position),
                         Always_Topmost);
    Main_Display.Ready;
  end Create;

  procedure Create (Application_Name    : String;
                    Termination_Routine : Action_Routine;
                    The_Metrics         : Window_Metrics;
                    Always_Topmost      : Boolean := False) is
  begin
    Create (Application_Name    => Application_Name,
            Termination_Routine => Termination_Routine,
            Window_Width        => The_Metrics.Width,
            Window_Height       => The_Metrics.Height,
            Window_X_Position   => The_Metrics.X_Position,
            Window_Y_Position   => The_Metrics.Y_Position,
            Always_Topmost      => Always_Topmost);
  end Create;


  procedure Execute (Application_Name    : String;
                     Startup_Routine     : not null access procedure;
                     Termination_Routine : access procedure := null;
                     Initial_Metrics     : Window_Metrics;
                     Always_Topmost      : Boolean := False) is
  begin
    Create (Application_Name    => Application_Name,
            Termination_Routine => Termination_Routine,
            Window_Width        => Initial_Metrics.Width,
            Window_Height       => Initial_Metrics.Height,
            Window_X_Position   => Initial_Metrics.X_Position,
            Window_Y_Position   => Initial_Metrics.Y_Position,
            Always_Topmost      => Always_Topmost);
    Startup_Routine.all;
    Action.Terminated;
  end Execute;


  function Keyboard_Handler (Code   : Win32.INT;
                             Wparam : Win32.WPARAM;
                             Lparam : Win32.LPARAM) return Win32.LRESULT
  with
    Convention => Stdcall;


  The_Key_Handling_Is_Enabled : Boolean := False;

  function Keyboard_Handler (Code   : Win32.INT;
                             Wparam : Win32.WPARAM;
                             Lparam : Win32.LPARAM) return Win32.LRESULT is
    Prev_Key_State   : constant Win32.UINT := 16#4000_0000#;
    Transition_State : constant Win32.UINT := 16#8000_0000#;
    function Convert is new Ada.Unchecked_Conversion (Win32.LPARAM, Information);
    use type Win32.UINT;
    use type Win32.INT;
  begin
    if The_Key_Handling_Is_Enabled then
      if Code >= 0 then
        if (The_Key_Handling_Rountine /= null) then
          if ((Win32.UINT(Convert(Lparam)) and Transition_State) /= 0) then
            The_Key_Handling_Rountine.all (Key_Released, Key_Code(Wparam));
          elsif ((Win32.UINT(Convert(Lparam)) and Prev_Key_State) = 0) then
            The_Key_Handling_Rountine.all (Key_Pressed, Key_Code(Wparam));
          end if;
        end if;
      end if;
      return Win32.TRUE;
    end if;
    return Win32.Winuser.CallNextHookEx (The_Keyboard_Hook, Code, Wparam, Lparam);
  end Keyboard_Handler;


  function Mouse_Handling (Code   : Win32.INT;
                           Wparam : Win32.WPARAM;
                           Lparam : Win32.LPARAM) return Win32.LRESULT
  with
    Convention => Stdcall;


  package MOUSEHOOKSTRUCT_Conversion is new System.Address_To_Access_Conversions (Win32.Winuser.MOUSEHOOKSTRUCT);

  function Mouse_Handling (Code   : Win32.INT;
                           Wparam : Win32.WPARAM;
                           Lparam : Win32.LPARAM) return Win32.LRESULT is

    function Convert is new Ada.Unchecked_Conversion (Win32.LPARAM, System.Address);

    use type Win32.INT;

  begin
    if Code >= 0 then
      if (The_Mouse_Handling_Rountine /= null) then
        The_Mouse_Handling_Rountine.all
          (Win32.Winuser.PMOUSEHOOKSTRUCT(MOUSEHOOKSTRUCT_Conversion.To_Pointer (Convert(Lparam))).pt, Wparam);
      end if;
    end if;
    return Win32.Winuser.CallNextHookEx (The_Mouse_Hook, Code, Wparam, Lparam);
  end Mouse_Handling;


  procedure Define_Callback (Callback_Routine : Action_Routine) is
  begin
    The_Callback_Routine := Callback_Routine;
  end Define_Callback;


  procedure Instigate_Callback is
  begin
    if The_Callback_Routine = null then
      raise Sequence_Error;
    else
      Send_Message (Private_Window, Wm_User_Callback, 0, 0);
    end if;
  end Instigate_Callback;


  procedure Install_Key_Handler (The_Key_Handler : Key_Handler) is
    use type Win32.Windef.HHOOK;
  begin
    if Main_Thread = 0 or else The_Keyboard_Hook /= System.Null_Address then
      raise Sequence_Error;
    else
      The_Key_Handling_Rountine := The_Key_Handler;
      The_Keyboard_Hook := Win32.Winuser.SetWindowsHookEx (Win32.Winuser.WH_KEYBOARD,
                                                           Keyboard_Handler'access,
                                                           Our_Instance,
                                                           Main_Thread);
    end if;
  end Install_Key_Handler;


  procedure Disable_Key_Handler is
  begin
    The_Key_Handling_Is_Enabled := False;
  end Disable_Key_Handler;


  procedure Enable_Key_Handler is
  begin
    The_Key_Handling_Is_Enabled := True;
  end Enable_Key_Handler;


  procedure Install_Mouse_Handler (The_Mouse_Handler : Mouse_Handler) is
    use type Win32.Windef.HHOOK;
  begin
    if Main_Thread = 0 or else The_Mouse_Hook /= System.Null_Address then
      raise Sequence_Error;
    else
      The_Mouse_Handling_Rountine := The_Mouse_Handler;
      The_Mouse_Hook := Win32.Winuser.SetWindowsHookEx (Win32.Winuser.WH_MOUSE,
                                                        Mouse_Handling'access,
                                                        Our_Instance,
                                                        Main_Thread);
    end if;
  end Install_Mouse_Handler;


  function Add_Menu (The_Text : String) return Menu is
    function Convert is new Ada.Unchecked_Conversion (Win32.Windef.HMENU, Win32.UINT_PTR);
    The_Menu : constant Menu := new Menu_Information'(The_Handle   => Win32.Winuser.CreateMenu,
                                                      The_Kind     => Plain,
                                                      The_Callback => null,
                                                      Radio_Item   => null);
    Unused   : Win32.BOOL;
    Text     : aliased constant Wide_String := To_Wide (The_Text) & Wide_Nul;
  begin
    Unused := Win32.Winuser.AppendMenuW (The_Menu_Bar, Win32.Winuser.MF_POPUP, Convert(The_Menu.The_Handle),
                                         Win32.Addr (Text));
    Unused := Win32.Winuser.DrawMenuBar (Private_Window);
    return The_Menu;
  end Add_Menu;


  function Add_Menu (The_Text : String;
                     To_Menu  : Menu) return Menu is
    function Convert is new Ada.Unchecked_Conversion (Win32.Windef.HMENU, Win32.UINT_PTR);
    The_Menu : constant Menu := new Menu_Information'(The_Handle   => Win32.Winuser.CreateMenu,
                                                      The_Kind     => Plain,
                                                      The_Callback => null,
                                                      Radio_Item   => null);
    Unused   : Win32.BOOL;
    Text     : aliased constant Wide_String := To_Wide (The_Text) & Wide_Nul;
  begin
    Unused := Win32.Winuser.AppendMenuW (To_Menu.The_Handle,
                                         Win32.Winuser.MF_POPUP, Convert(The_Menu.The_Handle),
                                         Win32.Addr (Text));
    return The_Menu;
  end Add_Menu;


  procedure Add_Menu_Separator (To_Menu : Menu) is
    Unused : Win32.BOOL;
  begin
    Unused := Win32.Winuser.AppendMenu (To_Menu.The_Handle,
                                        Win32.Winuser.MF_SEPARATOR,
                                        0, null);
  end Add_Menu_Separator;


  package Menu_Store is

    procedure Put (Id : Positive);

    function Has_Id return Boolean;

    function Id return Positive;

  end Menu_Store;


  function New_Menu_Item_Id  return Positive is
  begin
    if Menu_Store.Has_Id then
      return Menu_Store.Id;
    end if;
    return Id : constant Positive := The_Next_Menu_Item_Id do
      The_Next_Menu_Item_Id := Id + 1;
    end return;
  end New_Menu_Item_Id;


  function Menu_Item_Access_Of (The_Text         : String;
                                To_Menu          : Menu;
                                The_Menu_Handler : access procedure (Item : Information);
                                The_Information  : Information := No_Information) return Menu_Item_Access is

  begin
    Mutex.Wait;
    declare
      The_Item_Access : constant Menu_Item_Access := new Menu_Item_Information'(The_Information => The_Information,
                                                                                The_Id          => New_Menu_Item_Id,
                                                                                The_Parent_Menu => To_Menu,
                                                                                Previous        => null,
                                                                                Next            => Last_Menu_Item);
      Unused   : Win32.BOOL;
      Text     : aliased constant Wide_String := To_Wide (The_Text) & Wide_Nul;
    begin
      if Last_Menu_Item /= null then
        Last_Menu_Item.Previous := The_Item_Access;
      end if;
      To_Menu.The_Callback := Convert(The_Menu_Handler);
      Last_Menu_Item := The_Item_Access;
      Unused := Win32.Winuser.AppendMenuW (To_Menu.The_Handle,
                                           Win32.Winuser.MF_STRING,
                                           Win32.UINT_PTR(The_Item_Access.The_Id),
                                           Win32.Addr(Text));
      Mutex.Signal;
      return The_Item_Access;
    end;
  end Menu_Item_Access_Of;


  function Add_Menu_Item (The_Text         : String;
                          To_Menu          : Menu;
                          The_Menu_Handler : access procedure (Item : Information);
                          The_Information  : Information := No_Information) return Plain_Menu_Item is
  begin
    To_Menu.The_Kind := Plain;
    return Plain_Menu_Item'(The_Menu_Item => Menu_Item_Access_Of (The_Text         => The_Text,
                                                                  To_Menu          => To_Menu,
                                                                  The_Menu_Handler => The_Menu_Handler,
                                                                  The_Information  => The_Information));
  end Add_Menu_Item;


  function Add_Menu_Item (The_Text         : String;
                          To_Menu          : Menu;
                          The_Menu_Handler : access procedure (Item : Information);
                          The_Information  : Information := No_Information) return Checked_Menu_Item is
  begin
    To_Menu.The_Kind := Checked;
    return Checked_Menu_Item'(The_Menu_Item => Menu_Item_Access_Of (The_Text         => The_Text,
                                                                    To_Menu          => To_Menu,
                                                                    The_Menu_Handler => The_Menu_Handler,
                                                                    The_Information  => The_Information));
  end Add_Menu_Item;


  function Add_Menu_Item (The_Text : String;
                          To_Menu  : Menu) return Checked_Menu_Item is
  begin
    return Add_Menu_Item (The_Text, To_Menu, null);
  end Add_Menu_Item;


  function Add_Menu_Item (The_Text         : String;
                          To_Menu          : Menu;
                          The_Menu_Handler : access procedure (Item : Information);
                          The_Information  : Information := No_Information) return Radio_Menu_Item is

    The_Item_Access : constant Menu_Item_Access := Menu_Item_Access_Of (The_Text         => The_Text,
                                                                        To_Menu          => To_Menu,
                                                                        The_Menu_Handler => The_Menu_Handler,
                                                                        The_Information  => The_Information);

    The_Menu_Item : constant Radio_Menu_Item := (The_Menu_Item => The_Item_Access);

  begin
    To_Menu.The_Kind := Radio;
    if To_Menu.Radio_Item = null then
      To_Menu.Radio_Item := The_Item_Access; -- first item
      Set (The_Item_Access);
    end if;
    return The_Menu_Item;
  end Add_Menu_Item;


  function Add_Menu_Item (The_Text : String;
                          To_Menu  : Menu) return Radio_Menu_Item is
  begin
    return Add_Menu_Item (The_Text, To_Menu, null);
  end Add_Menu_Item;


  package body Menu_Store is

    The_Ids    : array (1..1000) of Positive;
    Last_Index : Natural := 0;

    procedure Put (Id : Positive) is
    begin
      Last_Index := @ + 1;
      The_Ids(Last_Index) := Id;
    end Put;

    function Has_Id return Boolean is (Last_Index > 0);

    function Id return Positive is
    begin
      Last_Index := @ - 1;
      return The_Ids(Last_Index + 1);
    end Id;

  end Menu_Store;


  procedure Delete_Menu_Item (Item : Radio_Menu_Item) is
    Menu_Item : Menu_Item_Access renames Item.The_Menu_Item;
    Id        : constant Positive := Menu_Item.The_Id;
    Parent    : Menu renames Menu_Item.The_Parent_Menu;
    use type Win32.BOOL;
  begin
    Menu_Store.Put (Id);
    if Win32.Winuser.DeleteMenu (Parent.The_Handle,
                                 Win32.UINT(Id),
                                 Win32.Winuser.MF_BYCOMMAND) /= Win32.TRUE
    then
      raise Program_Error;
    end if;
    if Menu_Item.Next = null then
      if Menu_Item.Previous = null then
        Last_Menu_Item := null; -- menu item list becomes empty
      else
        Last_Menu_Item := Menu_Item.Previous; -- previous becomes last
        Last_Menu_Item.Next := null;
      end if;
    else
      if Menu_Item.Previous /= null then
        Menu_Item.Previous.Next := Menu_Item.Next;
      end if;
      Menu_Item.Next.Previous := Menu_Item.Previous;
    end if;
  end Delete_Menu_Item;


  procedure Dispose_Menu_Item (Item : in out Radio_Menu_Item) is
    procedure Dispose is new Ada.Unchecked_Deallocation (Menu_Item_Information, Menu_Item_Access);
  begin
    Dispose (Item.The_Menu_Item);
    Item.The_Menu_Item := null;
  end Dispose_Menu_Item;


  procedure Set_Menubar (The_Flags : Win32.UINT) is
    The_Position : Win32.UINT := 0;
    Unused       : Win32.BOOL;
    use type Win32.UINT;
    use type Win32.BOOL;
  begin
    loop
      exit when Win32.Winuser.EnableMenuItem (The_Menu_Bar, The_Position,
                                              Win32.Winuser.MF_BYPOSITION + The_Flags) < 0;
      The_Position := The_Position + 1;
    end loop;
    Unused := Win32.Winuser.DrawMenuBar (Private_Window);
  end Set_Menubar;


  procedure Set_Menubar (The_Menu  : Menu;
                         The_Flags : Win32.UINT) is

    function Convert is new Ada.Unchecked_Conversion (Win32.Windef.HMENU, Information);

    Unused : Win32.BOOL;
    use type Win32.UINT;
  begin
    Unused := Win32.Winuser.EnableMenuItem (The_Menu_Bar,
                                            Win32.UINT(Convert (The_Menu.The_Handle)),
                                            Win32.UINT(Win32.Winuser.MF_BYCOMMAND + The_Flags));
    Unused := Win32.Winuser.DrawMenuBar (Private_Window);
  end Set_Menubar;


  procedure Enable_Menubar is
  begin
    Set_Menubar (Win32.Winuser.MF_ENABLED);
  end Enable_Menubar;


  procedure Disable_Menubar is
  begin
    Set_Menubar (Win32.Winuser.MF_GRAYED);
  end Disable_Menubar;


  procedure Enable (The_Menu : Menu) is
  begin
    Set_Menubar (The_Menu, Win32.Winuser.MF_ENABLED);
  end Enable;


  procedure Disable (The_Menu : Menu) is
  begin
    Set_Menubar (The_Menu, Win32.Winuser.MF_GRAYED);
  end Disable;


  procedure Enable (The_Item : Menu_Item'class) is
    Unused : Win32.BOOL;
  begin
    Unused := Win32.Winuser.EnableMenuItem (The_Item.The_Menu_Item.The_Parent_Menu.The_Handle,
                                            Win32.UINT(The_Item.The_Menu_Item.The_Id),
                                            Win32.Winuser.MF_ENABLED);
  end Enable;


  procedure Disable (The_Item : Menu_Item'class) is
    Unused : Win32.BOOL;
  begin
    Unused := Win32.Winuser.EnableMenuItem (The_Item.The_Menu_Item.The_Parent_Menu.The_Handle,
                                            Win32.UINT(The_Item.The_Menu_Item.The_Id),
                                            Win32.Winuser.MF_GRAYED);
  end Disable;


  procedure Set (The_Item : Selection_Menu_Item) is
  begin
    Set (The_Item.The_Menu_Item);
  end Set;


  function Is_Set (The_Item : Selection_Menu_Item) return Boolean is
  begin
    return Is_Set (The_Item.The_Menu_Item);
  end Is_Set;


  overriding procedure Set (The_Item : Checked_Menu_Item) is
  begin
    Set (The_Item.The_Menu_Item);
  end Set;


  procedure Clear (The_Item : Checked_Menu_Item) is
  begin
    Clear (The_Item.The_Menu_Item);
  end Clear;


  overriding procedure Set (The_Item : Radio_Menu_Item) is
  begin
    Set_New (The_Item.The_Menu_Item);
    The_Item.The_Menu_Item.The_Parent_Menu.The_Callback (The_Item.The_Menu_Item.The_Information);
  end Set;


  function Setting (The_Menu : Menu) return Natural is
  begin
    if The_Menu.The_Kind = Radio then
      return The_Menu.Radio_Item.The_Id;
    else
      return 0;
    end if;
  end Setting;


  procedure Add_Page (The_Page  : Page;
                      The_Title : String) is
    Title    : aliased constant String := The_Title & Nul;
    Tab_Info : Win32.Comctl.Tc_Item;
    function Convert is new Ada.Unchecked_Conversion(System.Address, Win32.LPARAM);
    function Convert is new Ada.Unchecked_Conversion(Page, Win32.LPARAM);
    use type Win32.UINT;
  begin
    Nr_Of_Pages := Nr_Of_Pages + 1;
    if Nr_Of_Pages = 1 then -- First page
      Current_Page := The_Page;
    elsif Nr_Of_Pages = 2 then -- Need tab control to switch between pages
      Redraw_Main_Window; -- So redraw window so that it gets shown
    end if;
    Tab_Info.Mask := Win32.Comctl.Tcif_Text + Win32.Comctl.Tcif_Param;
    Tab_Info.Text := Win32.Addr(Title);
    Tab_Info.Lparam := Convert(The_Page);
    Send_Message (Win32.Windef.HWND(The_Tabs),
                  Win32.Comctl.Tcm_Insertitem,
                  Win32.WPARAM(Nr_Of_Pages),
                  Convert(Tab_Info'address));
  end Add_Page;


  function Add_Page (The_Title            : String;
                     The_Style            : Page_Style     := Default_Page_Style;
                     The_Action           : Action_Routine := null;
                     Minimum_Button_Width : Natural        := Default_Button_Width) return Page is
    The_Page : constant Page := new Page_Information;
  begin
    The_Page.The_Style := The_Style;
    The_Page.The_Action := The_Action;
    The_Page.Minimum_Button_Width := Win32.INT(Minimum_Button_Width);
    Add_Page (The_Page, The_Title);
    return The_Page;
  end Add_Page;


  procedure Remove_Page (The_Page : Page) is
    Next_Page : Natural;
    Tab_Info  : aliased Win32.Comctl.Tc_Item;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPARAM);
    function Convert is new Ada.Unchecked_Conversion (Win32.LPARAM, System.Address);
  begin
    if Nr_Of_Pages = 0 then
      return;  -- Nothing to do
    else -- search for it
      for Page_Number in 0 .. Nr_Of_Pages - 1 loop -- Search for matching page
        Tab_Info.Mask := Win32.Comctl.Tcif_Param;
        Send_Message (Win32.Windef.HWND(The_Tabs),
                      Win32.Comctl.Tcm_Getitem,
                      Win32.WPARAM(Page_Number),
                      Convert(Tab_Info'address));
        if The_Page = Page(Page_Conversion.To_Pointer (Convert (Tab_Info.Lparam))) then -- found it
          Send_Message (Win32.Windef.HWND(The_Tabs),
                        Win32.Comctl.Tcm_Deleteitem,
                        Win32.WPARAM(Page_Number), 0);
          Nr_Of_Pages := Nr_Of_Pages - 1;
          Next_Page := Page_Number;
          if Next_Page >= Nr_Of_Pages then -- wrap around to first
            Next_Page := 0;
          end if;
          exit;
        end if;
      end loop;
    end if;
    if Current_Page = The_Page then
      Make_Invisible (Current_Page);
      if Nr_Of_Pages = 0 then
        Current_Page := null;
      else -- After removing current page goto next
        Tab_Info.Mask := Win32.Comctl.Tcif_Param;
        Send_Message (Win32.Windef.HWND(The_Tabs),
                      Win32.Comctl.Tcm_Getitem,
                      Win32.WPARAM (Next_Page),
                      Convert(Tab_Info'address));
        Current_Page := Page(Page_Conversion.To_Pointer (Convert (Tab_Info.Lparam)));
        Make_Visible (Current_Page);
        Send_Message (Win32.Windef.HWND(The_Tabs),
                      Win32.Comctl.Tcm_Setcursel,
                      Win32.WPARAM (Next_Page),0);
      end if;
    end if;
    if Nr_Of_Pages = 1 then -- Remove tabs
      Make_Invisible (The_Tabs);
    end if;
    Redraw_Main_Window;
  end Remove_Page;


  procedure Pop_Up_Button is
    The_Pop_Up_Return   : Pop_Up_Return;
    Termination_Routine : Pop_Up_Termination;
  begin
    if (Active_Child = Current_Page.First_Child) and Current_Page.Has_Cancel_Button then
      The_Pop_Up_Return := Cancel;
    else
      The_Pop_Up_Return := Ok;
    end if;
    Termination_Routine := Current_Page.Termination_Routine;
    Make_Invisible (Current_Page);
    Current_Page := Current_Page.Parent;
    if Current_Page /= null then
      if Nr_Of_Pages > 1 then
        Make_Visible (The_Tabs);
      end if;
      Make_Visible (Current_Page);
    end if;
    Termination_Routine (The_Pop_Up_Return);
  end Pop_Up_Button;


  function Create (The_Style   : Page_Style := Default_Page_Style;
                   The_Routine : Pop_Up_Termination;
                   Cancel_Text : String := "";
                   Ok_Text     : String := "") return Page is
    The_Page : constant Page := new Page_Information;
    Unused   : Button;
  begin
    The_Page.The_Style := The_Style;
    The_Page.Termination_Routine := The_Routine;
    if Cancel_Text /= "" then
      Unused := Create (The_Page, Cancel_Text, Pop_Up_Button'access);
      The_Page.Has_Cancel_Button := True;
    end if;
    if Ok_Text /= "" then
      Unused := Create (The_Page, Ok_Text, Pop_Up_Button'access);
    end if;
    return The_Page;
  end Create;


  function Create (The_Style  : Page_Style     := Default_Page_Style;
                   The_Action : Action_Routine := null ) return Page is
    The_Page : constant Page := new Page_Information;
  begin
    The_Page.The_Style := The_Style;
    The_Page.The_Action := The_Action;
    return The_Page;
  end Create;


  function Buddy_For (The_Text      : Wide_String;
                      The_Text_Size : Natural;
                      Is_Automatic  : Boolean;
                      The_Kind      : Child_Kind;
                      Parent_Page   : Page) return Child_Information_Ptr is
    The_Buddy : Child_Information_Ptr := null;
    The_Title : constant Wide_String_Ptr := new Wide_String'(The_Text & Wide_Nul);
    use type Win32.INT;
  begin
    if The_Text /= "" then
      The_Buddy := new Child_Information;
      The_Buddy.The_Window_Id := Next_Child_Id;
      The_Buddy.The_Kind := The_Kind;
      The_Buddy.Is_Buddy := True;
      The_Buddy.Initial_Title := Win32.Addr(The_Title.all);
      The_Buddy.Initial_Title_Length := The_Text'length;
      if Is_Automatic then
        The_Buddy.The_Title_Size := 0;
      elsif The_Text_Size = Automatic then
        The_Buddy.The_Title_Size := 0 - The_Text'length;
      else
        The_Buddy.The_Title_Size := Win32.INT(The_Text_Size);
      end if;
      Create (The_Buddy, Parent_Page);
      Parent_Page.Nr_Of_Buddies := Parent_Page.Nr_Of_Buddies + 1;
    end if;
    return The_Buddy;
  end Buddy_For;


  function Create (Parent_Page : Page;
                   The_Title   : String := "") return Track_Bar is
    The_Track_Bar : constant Track_Bar := Track_Bar'(Ptr => new Child_Information);
  begin
    The_Track_Bar.Ptr.The_Window_Id := Next_Child_Id;
    The_Track_Bar.Ptr.The_Kind := Track_Bar_Child;
    The_Track_Bar.Ptr.Buddy_Child := Buddy_For (The_Text      => To_Wide (The_Title),
                                                The_Text_Size => Automatic,
                                                Is_Automatic  => False,
                                                The_Kind      => Static_Child,
                                                Parent_Page   => Parent_Page);
    Create (The_Track_Bar.Ptr, Parent_Page);
    return The_Track_Bar;
  end Create;


  procedure Define_Range (The_Track_Bar : Track_Bar;
                          The_Extent    : Positive) is
  begin
    Send_Message (The_Track_Bar.Ptr.The_Handle,
                  Win32.Comctl.Tbm_Setrange,
                  Win32.TRUE,
                  Make_Long(The_Extent));
  end Define_Range;


  procedure Define_Tic_Frequency (The_Track_Bar : Track_Bar;
                                  The_Frequency : Natural) is
  begin
    Send_Message (The_Track_Bar.Ptr.The_Handle,
                  Win32.Comctl.Tbm_Setticfreq,
                  Win32.WPARAM (The_Frequency), 0);
  end Define_Tic_Frequency;


  procedure Set_Position (The_Track_Bar : Track_Bar;
                          The_Position  : Natural) is
  begin
    Send_Message (The_Track_Bar.Ptr.The_Handle,
                  Win32.Comctl.Tbm_Setpos,
                  Win32.TRUE, Win32.LPARAM (The_Position));
  end Set_Position;


  function Get_Position (The_Track_Bar : Track_Bar) return Natural is
    The_Position : Win32.LRESULT;
    use type Win32.LRESULT;
  begin
    The_Position := Send_Message (The_Track_Bar.Ptr.The_Handle,
                                  Win32.Comctl.Tbm_Getpos, 0, 0);
    if The_Position > 0 then
      return Natural(The_Position);
    else
      return 0;
    end if;
  end Get_Position;


  function Create (Parent_Page        : Page;
                   The_Text           : String;
                   The_Action_Routine : Action_Routine := null;
                   The_Button_Size    : Natural := Default_Button_Width) return Button is
    Text        : aliased constant Wide_String := To_Wide (The_Text) & Wide_Nul;
    The_Button  : constant Button := Button'(Ptr => new Child_Information);
    Button_Size : constant Win32.INT := Win32.INT (The_Button_Size);
    use type Win32.INT;
  begin
    The_Button.Ptr.The_Window_Id := Next_Child_Id;
    The_Button.Ptr.Initial_Title := Win32.Addr(Text);
    The_Button.Ptr.The_Kind := Button_Child;
    The_Button.Ptr.The_Action_Routine := The_Action_Routine;
    The_Button.Ptr.The_Title_Size := Button_Size;
    if Parent_Page.Minimum_Button_Width > Button_Size then
      Parent_Page.Minimum_Button_Width := Button_Size;
    end if;
    Parent_Page.Last_Button_Width := Button_Size;
    Parent_Page.Total_Button_Width := Parent_Page.Total_Button_Width + Button_Size;
    Create (The_Button.Ptr, Parent_Page);
    return The_Button;
  end Create;


  procedure Set_Text (The_Button : Button;
                      The_Text   : String) is
    Text   : aliased constant Wide_String := To_Wide (The_Text) & Wide_Nul;
    Unused : Win32.BOOL;
  begin
    Unused := Win32.Winuser.SetWindowTextW (The_Button.Ptr.The_Handle,
                                            Win32.Addr(Text));
  end Set_Text;


  function Create (Parent_Page : Page) return Progress_Bar is
    The_Progress_Bar : constant Progress_Bar := Progress_Bar'(Ptr => new Child_Information);
  begin
    The_Progress_Bar.Ptr.The_Window_Id := Next_Child_Id;
    The_Progress_Bar.Ptr.The_Kind := Progress_Bar_Child;
    Create (The_Progress_Bar.Ptr, Parent_Page);
    return The_Progress_Bar;
  end Create;


  procedure Define_Range (The_Progress_Bar : Progress_Bar;
                          The_Extent       : Positive;
                          The_Step         : Positive := 1) is
  begin
    Send_Message (The_Progress_Bar.Ptr.The_Handle,
                  Win32.Comctl.Pbm_Setrange,
                  Win32.WPARAM(0),
                  Make_Long(The_Extent));
    Send_Message (The_Progress_Bar.Ptr.The_Handle,
                  Win32.Comctl.Pbm_Setstep,
                  Win32.WPARAM(The_Step),
                  Win32.LPARAM(0));
  end Define_Range;


  procedure Report_Progress (The_Progress_Bar : Progress_Bar;
                             The_Extent       : Natural) is
  begin
    Send_Message (The_Progress_Bar.Ptr.The_Handle,
                  Win32.Comctl.Pbm_Setpos,
                  Win32.WPARAM(The_Extent),
                  Win32.LPARAM(0));
  end Report_Progress;


  procedure Increment_Progress (The_Progress_Bar : Progress_Bar) is
  begin
    Send_Message (The_Progress_Bar.Ptr.The_Handle,
                  Win32.Comctl.Pbm_Stepit, 0, 0);
  end Increment_Progress;


  procedure Set_Color (The_Progress_Bar : Progress_Bar;
                       The_Color        : Color) is
  begin
    Send_Message (The_Progress_Bar.Ptr.The_Handle,
                  Win32.Comctl.Pbm_Setbarcolor,
                  Win32.WPARAM(0),
                  Win32.LPARAM(The_Color));
  end Set_Color;


  procedure Enable (The_Child : Child_Information_Ptr) is
    Unused : Win32.BOOL;
  begin
    Unused := Win32.Winuser.EnableWindow (The_Child.The_Handle, Win32.TRUE);
    if The_Child.Buddy_Child /= null then
      Enable (The_Child.Buddy_Child);
    end if;
  end Enable;


  procedure Enable (The_Child : Child'class) is
  begin
    Enable (The_Child.Ptr);
  end Enable;


  procedure Disable (The_Child : Child_Information_Ptr) is
    Unused : Win32.BOOL;
  begin
    Unused := Win32.Winuser.EnableWindow(The_Child.The_Handle, Win32.FALSE);
    if The_Child.Buddy_Child /= null then
      Disable (The_Child.Buddy_Child);
    end if;
  end Disable;


  procedure Disable (The_Child : Child'class) is
  begin
    Disable (The_Child.Ptr);
  end Disable;


  function Is_Enabled (The_Child : Child'class) return Boolean is
    use type Win32.BOOL;
  begin
    return Win32.Winuser.IsWindowEnabled (The_Child.Ptr.The_Handle) = Win32.TRUE;
  exception
  when others =>
    return False;
  end Is_Enabled;


  procedure Increment (The_Count : in out Win32.INT) is
    use type Win32.INT;
  begin
    The_Count := The_Count + 1;
  end Increment;


  procedure Hide (The_Child : Child_Information_Ptr) is
    Unused : Win32.BOOL;
  begin
    if not The_Child.Is_Hidden then
      Increment (The_Child.Parent_Page.Population(The_Child.The_Kind));
      Unused := Win32.Winuser.ShowWindow(The_Child.The_Handle, Win32.Winuser.SW_HIDE);
      The_Child.Is_Hidden := True;
      if The_Child.Buddy_Child /= null then
        Hide (The_Child.Buddy_Child);
      end if;
    end if;
  end Hide;

  procedure Hide (The_Child : Child'class) is
  begin
    Hide (The_Child.Ptr);
    Redraw_Main_Window;
  end Hide;


  procedure Decrement (The_Count : in out Win32.INT) is
    use type Win32.INT;
  begin
    if The_Count > 0 then
      The_Count := The_Count - 1;
    end if;
  end Decrement;


  procedure Show (The_Child : Child_Information_Ptr) is
    Unused : Win32.BOOL;
  begin
    if The_Child.Is_Hidden then
      Decrement (The_Child.Parent_Page.Population(The_Child.The_Kind));
      if The_Child.Parent_Page = Current_Page then
        Unused := Win32.Winuser.ShowWindow(The_Child.The_Handle, Win32.Winuser.SW_SHOW);
      end if;
      The_Child.Is_Hidden := False;
      if The_Child.Buddy_Child /= null then
        Show (The_Child.Buddy_Child);
      end if;
    end if;
  end Show;

  procedure Show (The_Child : Child'class) is
  begin
    Show (The_Child.Ptr);
    Redraw_Main_Window;
  end Show;


  function Is_Hidden (The_Child : Child'class) return Boolean is
  begin
    return The_Child.Ptr.Is_Hidden;
  end Is_Hidden;


  procedure Set_Focus (The_Child : Child_Information_Ptr) is
    Unused   : Win32.Windef.HWND;
    The_Page : constant Page := The_Child.Parent_Page;
  begin
    The_Page.Keyboard_Focus := The_Child;
    if The_Page = Current_Page then
      Unused := Win32.Winuser.SetFocus(The_Child.The_Handle);
    end if;
  end Set_Focus;


  procedure Set_Focus (The_Child : Child'class) is
  begin
    Set_Focus (The_Child.Ptr);
  end Set_Focus;


  procedure Set_Action (The_Child          : Child'class;
                        The_Action_Routine : Action_Routine) is
  begin
    The_Child.Ptr.The_Action_Routine := The_Action_Routine;
  end Set_Action;


  procedure Set_Qualified_Action (The_Child          : Child'class;
                                  The_Action_Routine : access procedure (Item : Information);
                                  The_Information    : Information) is
  begin
    if The_Child.Ptr.The_Action_Routine /= null then
      raise Action_Routine_Is_Defined;
    end if;
    The_Child.Ptr.Qualified_Action_Routine := Convert(The_Action_Routine);
    The_Child.Ptr.User_Information := The_Information;
  end Set_Qualified_Action;


  function Parent_Page_Of (The_Child : Child'class) return Page is
  begin
    return The_Child.Ptr.Parent_Page;
  end Parent_Page_Of;


  function Click_Kind_Of (The_Click_Kind : Click_Kind) return Win32.INT is
  begin
    case The_Click_Kind is
    when Single_Click =>
      return Win32.Comctl.Nm_Click;
    when Double_Click =>
      return Win32.Comctl.Nm_Dblclk;
    end case;
  end Click_Kind_Of;


  function Create (Parent_Page       : Page;
                   The_Click_Routine : access procedure (Item : Information) := null;
                   The_Color_Handler     : Color_Handler := null) return Tree_View is

    The_Tree_View : constant Tree_View := Tree_View'(Ptr => new Child_Information);
  begin
    The_Tree_View.Ptr.The_Window_Id := Next_Child_Id;
    The_Tree_View.Ptr.The_Kind := Tree_View_Child;
    The_Tree_View.Ptr.The_Click_Routine := Convert(The_Click_Routine);
    The_Tree_View.Ptr.The_Color_Routine := The_Color_Handler;
    Create (The_Tree_View.Ptr, Parent_Page);
    return The_Tree_View;
  end Create;


  function Create (Parent_Page           : Page;
                   The_Text_Handler      : access function (For_Column       : Natural;
                                                            With_Information : Information) return String;
                   The_Color_Handler     : Color_Handler := null;
                   The_Sort_Routine      : Sort_Routine := null;
                   The_Click_Routine     : access procedure (Item : Information) := null;
                   The_Click_Kind        : Click_Kind := Double_Click;
                   Use_Proportional_Font : Boolean := True;
                   Color_Background      : Boolean := False) return List_View is
    The_List_View : constant List_View := List_View'(Ptr => new Child_Information);
    function Convert is new Ada.Unchecked_Conversion (Text_Handler, Action_Routine);
  begin
    The_List_View.Ptr.The_Window_Id := Next_Child_Id;
    The_List_View.Ptr.The_Kind := List_View_Child;
    The_List_View.Ptr.The_Action_Routine := Convert(The_Text_Handler);
    The_List_View.Ptr.The_Color_Routine := The_Color_Handler;
    The_List_View.Ptr.The_Sort_Routine := The_Sort_Routine;
    The_List_View.Ptr.The_Click_Routine := Convert(The_Click_Routine);
    The_List_View.Ptr.The_Click_Kind := Click_Kind_Of (The_Click_Kind);
    The_List_View.Ptr.Use_Proportional_Font := Use_Proportional_Font;
    The_List_View.Ptr.Color_Background := Color_Background;
    Create (The_List_View.Ptr, Parent_Page);
    return The_List_View;
  end Create;


  procedure Add_Gridlines (The_List_View : List_View) is
    Extended_Style : Win32.DWORD;
  begin
    if Is_Active then
      Extended_Style := Win32.DWORD(Send_Message (The_List_View.Ptr.The_Handle,
                                                  Win32.Comctl.Lvm_Getextendedlistviewstyle, 0, 0));
      Extended_Style := Extended_Style or Win32.Comctl.Lvs_Ex_Gridlines;
      Send_Message (The_List_View.Ptr.The_Handle,
                    Win32.Comctl.Lvm_Setextendedlistviewstyle,
                    Win32.WPARAM(0),  Win32.LPARAM(Extended_Style));
    end if;
  end Add_Gridlines;


  procedure Remove_Gridlines (The_List_View : List_View) is
    Extended_Style : Win32.DWORD;
  begin
    if Is_Active then
      Extended_Style := Win32.DWORD(Send_Message (The_List_View.Ptr.The_Handle,
                                                  Win32.Comctl.Lvm_Getextendedlistviewstyle, 0, 0));
      if (Extended_Style or Win32.Comctl.Lvs_Ex_Gridlines) /= 0 then -- Gridlines Are set
        Extended_Style := Extended_Style xor Win32.Comctl.Lvs_Ex_Gridlines;
        Send_Message (The_List_View.Ptr.The_Handle,
                      Win32.Comctl.Lvm_Setextendedlistviewstyle,
                      Win32.WPARAM(0),  Win32.LPARAM(Extended_Style));
      end if;
    end if;
  end Remove_Gridlines;


  function Add_Column (The_List_View     : List_View;
                       The_Title         : String;
                       The_Width         : Natural := Automatic;
                       The_Justification : Justification := Left) return Column is

    Title        : aliased constant Wide_String := To_Wide (The_Title) & Wide_Nul;
    Column_Index : constant Win32.WPARAM := Win32.WPARAM(The_List_View.Ptr.Number_Of_Columns);
    Column_Info  : aliased Win32.Comctl.Lv_Column_Wide;
    function Convert is new Ada.Unchecked_Conversion(System.Address, Win32.LPARAM);
    use type Win32.UINT;

  begin
    Column_Info.Mask := Win32.Comctl.Lvcf_Fmt +
                        Win32.Comctl.Lvcf_Width +
                        Win32.Comctl.Lvcf_Text;
    case The_Justification is
      when Left   => Column_Info.Fmt := Win32.Comctl.Lvcfmt_Left;
      when Center => Column_Info.Fmt := Win32.Comctl.Lvcfmt_Center;
      when Right  => Column_Info.Fmt := Win32.Comctl.Lvcfmt_Right;
    end case;
    Column_Info.Width := Win32.INT(The_Width);
    Column_Info.Text := Win32.Addr(Title);
    Send_Message_Wide (The_List_View.Ptr.The_Handle,
                       Win32.Comctl.Lvm_Insertcolumn_Wide,
                       Column_Index,
                       Convert(Column_Info'address));
    The_List_View.Ptr.Number_Of_Columns := The_List_View.Ptr.Number_Of_Columns + 1;
    return Column'(Position => The_List_View.Ptr.Number_Of_Columns,
                   List     => The_List_View);
  end Add_Column;


  procedure Set_Title (The_Column    : Column;
                       The_Title     : String) is
    Title       : aliased constant Wide_String := To_Wide (The_Title) & Wide_Nul;
    Column_Info : Win32.Comctl.Lv_Column_Wide;
    function Convert is new Ada.Unchecked_Conversion(System.Address, Win32.LPARAM);
  begin
    Column_Info.Mask := Win32.Comctl.Lvcf_Text;
    Column_Info.Text := Win32.Addr(Title);
    Send_Message (The_Column.List.Ptr.The_Handle,
                  Win32.Comctl.Lvm_Setcolumn_Wide,
                  Win32.WPARAM(The_Column.Position - Positive'first),
                  Convert(Column_Info'address));
  end Set_Title;


  procedure Set_Width (The_Column    : Column;
                       The_New_Width : Natural) is
  begin
    Send_Message (The_Column.List.Ptr.The_Handle,
                  Win32.Comctl.Lvm_Setcolumnwidth,
                  Win32.WPARAM(The_Column.Position - Positive'first),
                  Win32.LPARAM(The_New_Width));
  end Set_Width;


  function Width_Of (The_Column : Column) return Natural is
    The_Width : Win32.LRESULT;
    use type Win32.LRESULT;
  begin
    The_Width := Send_Message (The_Column.List.Ptr.The_Handle,
                               Win32.Comctl.Lvm_Getcolumnwidth,
                               Win32.WPARAM(The_Column.Position - Positive'first),
                               0);
    if The_Width > 0 then
      return Natural(The_Width);
    else
      return 0;
    end if;
  end Width_Of;


  procedure Remove_Last_Column (The_List_View : List_View) is
    use type Win32.LRESULT;
  begin
    if Is_Active and (The_List_View.Ptr.Number_Of_Columns > 0) then
      if Send_Message (The_List_View.Ptr.The_Handle,
                       Win32.Comctl.Lvm_Deletecolumn,
                       Win32.WPARAM(The_List_View.Ptr.Number_Of_Columns - 1), 0) = Win32.TRUE
      then
        The_List_View.Ptr.Number_Of_Columns := The_List_View.Ptr.Number_Of_Columns - 1;
      end if;
    end if;
  end Remove_Last_Column;


  function Number_Of_Columns_Of (The_List_View : List_View) return Natural is
  begin
    return The_List_View.Ptr.Number_Of_Columns;
  end Number_Of_Columns_Of;


  procedure Add_To (The_List_View   : List_View;
                    Item_Ordinal    : Positive := 1;
                    The_Information : Information) is
    function Convert is new Ada.Unchecked_Conversion (Information, Win32.LPWSTR);
    The_Info_Item : Win32.Comctl.Lv_Item_Wide;
    Text_Callback : constant Win32.LPWSTR := Convert(-1);
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPARAM);
    function Convert is new Ada.Unchecked_Conversion (Information, Win32.LPARAM);
    use type Win32.UINT;
  begin
    The_Info_Item.Mask := Win32.Comctl.Lvif_Text + Win32.Comctl.Lvif_Param;
    The_Info_Item.Item := Win32.INT(Item_Ordinal - 1);
    The_Info_Item.Subitem := 0;
    The_Info_Item.Text := Text_Callback;
    The_Info_Item.Lparam := Convert(The_Information);
    Send_Message_Wide (The_List_View.Ptr.The_Handle,
                       Win32.Comctl.Lvm_Insertitem_Wide,
                       0,
                       Convert(The_Info_Item'address));
    -- Note there is no need to Set the text items to callback as this is the default
    The_List_View.Ptr.Number_Of_Rows := The_List_View.Ptr.Number_Of_Rows + 1;
  end Add_To;


  procedure Add_Data (The_List_View   : List_View;
                      The_Information : Information) is
  begin
    Add_To (The_List_View, The_List_View.Ptr.Number_Of_Rows + 1, The_Information);
  end Add_Data;


  procedure Notify_Item_Update (The_List_View : List_View;
                                Item_Ordinal  : Positive) is
  begin
    Send_Message (The_List_View.Ptr.The_Handle,
                  Win32.Comctl.Lvm_Update,
                  Win32.WPARAM(Item_Ordinal - 1),
                  0);
  end Notify_Item_Update;


  procedure Remove_From (The_List_View : List_View;
                         Item_Ordinal  : Positive := 1) is
  begin
    if Item_Ordinal <= The_List_View.Ptr.Number_Of_Rows then
      Send_Message (The_List_View.Ptr.The_Handle,
                    Win32.Comctl.Lvm_Deleteitem,
                    Win32.WPARAM(Item_Ordinal - 1),
                    0);
      The_List_View.Ptr.Number_Of_Rows := The_List_View.Ptr.Number_Of_Rows - 1;
    end if;
  end Remove_From;


  procedure Remove_All_From (The_List_View : List_View) is
  begin
    Send_Message (The_List_View.Ptr.The_Handle,
                  Win32.Comctl.Lvm_Deleteallitems,
                  0, 0);
    The_List_View.Ptr.Number_Of_Rows := 0;
  end Remove_All_From;


  procedure Sort (The_Column    : Column;
                  The_Direction : Sort_Direction) is
    The_View  : constant Child_Information_Ptr := The_Column.List.Ptr;
    function Convert is new Ada.Unchecked_Conversion (Child_Information_Ptr, Win32.WPARAM);
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPARAM);
  begin
    if The_View.The_Sort_Routine /= null then
      The_View.The_Sort_Column := Natural(The_Column.Position - Positive'first);
      The_View.The_Sort_Direction := The_Direction;
      Send_Message (The_View.The_Handle,
                    Win32.Comctl.Lvm_Sortitems,
                    Convert(The_View),
                    Convert(Listview_Sort_Routine'address));
      if The_Direction = Forward then
        The_View.The_Sort_Direction := Backward;
      else
        The_View.The_Sort_Direction := Forward;
      end if;
    end if;
  end Sort;


  procedure Reserve_Space_For (The_List_View : List_View;
                               Amount        : Positive) is
  begin
   Send_Message (The_List_View.Ptr.The_Handle,
                 Win32.Comctl.Lvm_Setitemcount,
                 Win32.WPARAM (Amount), 0);
  end Reserve_Space_For;


  function Visible_Region_Of (The_List_View : List_View) return Natural is
    The_Region : Win32.LRESULT;
    use type Win32.LRESULT;
  begin
    The_Region :=  Send_Message (The_List_View.Ptr.The_Handle,
                                 Win32.Comctl.Lvm_Getcountperpage,
                                 0, 0);
    if The_Region > 0 then
      return Natural(The_Region);
    else
      return 0;
    end if;
  end Visible_Region_Of;


  procedure Make_Visible_In (The_List_View : List_View;
                             Item_Ordinal  : Positive := 1) is
  begin
    if The_List_View.Ptr.Number_Of_Rows >= Item_Ordinal then
      Send_Message (The_List_View.Ptr.The_Handle,
                    Win32.Comctl.Lvm_Ensurevisible,
                    Win32.WPARAM(Item_Ordinal - 1), 0);
    end if;
  end Make_Visible_In;


  procedure Make_Last_Visible_In (The_List_View : List_View) is
  begin
    if The_List_View.Ptr.Number_Of_Rows > 0 then
      Make_Visible_In (The_List_View, The_List_View.Ptr.Number_Of_Rows);
    end if;
  end Make_Last_Visible_In;


  function Get_Selection_Count (The_Handle : Win32.Windef.HWND) return Natural is
    Count : Win32.LRESULT;
  begin
    Count :=  Send_Message (The_Handle, Win32.Comctl.LVM_Getselectedcount, 0, 0);
    return Natural (Count);
  end Get_Selection_Count;


  function Selected_Information_From (The_List_View : List_View) return Information_Array is
    Count : constant Natural := Get_Selection_Count (The_List_View.Ptr.The_Handle);
    function Convert is new Ada.Unchecked_Conversion (Win32.LPARAM, Information);
    function Convert is new Ada.Unchecked_Conversion (Win32.LRESULT, Win32.WPARAM);
    use type Win32.LRESULT;
  begin
    declare
      No_Item         : constant Win32.LRESULT := Win32.LRESULT(-1);
      The_Information : Information_Array (1..Count);
      The_Item        : Win32.LRESULT := No_Item;
    begin
      for Index in The_Information'range loop
        The_Item := Send_Message (Hwnd   => The_List_View.Ptr.The_Handle,
                                  Msg    => Win32.Comctl.Lvm_Getnextitem,
                                  Wparam => Convert(The_Item),
                                  Lparam => Win32.Comctl.Lvni_Selected);
        if The_Item = No_Item then
          return Empty_Information;
        end if;
        The_Information (Index) := Convert(Lparam_Of (The_List_View.Ptr.The_Handle, Win32.INT(The_Item)));
      end loop;
      return The_Information;
    end;
  end Selected_Information_From;


  procedure Freeze (The_List_View : List_View) is
  begin
    null;
  end Freeze;


  procedure Thaw (The_List_View : List_View) is
  begin
    null;
  end Thaw;


  function Create (Parent_Page   : Page;
                   Word_Wrapping : Boolean := False) return Text_View is
    The_Text_View : constant Text_View := Text_View'(Ptr => new Child_Information);
  begin
    The_Text_View.Ptr.The_Window_Id := Next_Child_Id;
    The_Text_View.Ptr.The_Kind := Text_View_Child;
    Create (The_Text_View.Ptr, Parent_Page);
    Send_Message (The_Text_View.Ptr.The_Handle,
                  Win32.Comctl.Em_Exlimittext, -- Set the maximum amount of text
                  Win32.WPARAM(0),
                  Win32.LPARAM(10_000_000));   -- Default 32K
    if not Word_Wrapping then
      Send_Message (The_Text_View.Ptr.The_Handle,
                    Win32.Comctl.Em_Settargetdevice,
                    Win32.WPARAM(0),
                    Win32.LPARAM(1)); -- Set word wapping off
    end if;
    return The_Text_View;
  end Create;


  procedure Append_Wide_To (The_Rich_Edit  : Text_View;
                            The_Text       : Wide_String;
                            The_Color      : Color := Gui.Black;
                            Ensure_Visible : Boolean := True) is
    Text : aliased constant Wide_String := The_Text & Wide_Nul;
    function Convert is new Ada.Unchecked_Conversion(System.Address, Win32.LPARAM);
    Charformat : aliased Win32.Comctl.Charformat;
    use type Win32.LONG;
    Everything : aliased constant Win32.Comctl.Charrange := (Min => -1, Max => -1);
  begin
    Charformat.Mask := Win32.Comctl.Cfm_Color;
    Charformat.Effects := 0;
    Charformat.Textcolor := The_Color;
    Send_Message (The_Rich_Edit.Ptr.The_Handle,
                  Win32.Comctl.Em_Setcharformat,
                  Win32.WPARAM(Win32.Comctl.Scf_Selection),
                  Convert(Charformat'address));
    Send_Message_Wide (The_Rich_Edit.Ptr.The_Handle,
                       Win32.Comctl.Em_Exsetsel,
                       Win32.WPARAM(0),
                       Convert(Everything'address));
    Send_Message_Wide (The_Rich_Edit.Ptr.The_Handle,
                       Win32.Winuser.EM_REPLACESEL,
                       Win32.WPARAM(0),
                       Convert(Text(Text'first)'address));
    if Ensure_Visible then
      Send_Message (The_Rich_Edit.Ptr.The_Handle,
                    Win32.Winuser.WM_VSCROLL,
                    Win32.Winuser.SB_BOTTOM,
                    0);
    end if;
  end Append_Wide_To;


  procedure Append_To (The_Rich_Edit  : Text_View;
                       The_Text       : String;
                       The_Color      : Color := Gui.Black;
                       Ensure_Visible : Boolean := True) is
  begin
    Append_Wide_To (The_Rich_Edit, To_Wide (The_Text), The_Color, Ensure_Visible);
  end Append_To;


  procedure Append_Line_To (The_Rich_Edit  : Text_View;
                            The_Text       : String;
                            The_Color      : Color := Gui.Black;
                            Ensure_Visible : Boolean := True) is
  begin
    Append_Wide_To (The_Rich_Edit, To_Wide (The_Text) & Wide_Character'val(10), The_Color, Ensure_Visible);
  end Append_Line_To;


  procedure Clear (The_Rich_Edit : Text_View) is
  begin
    Send_Message (The_Rich_Edit.Ptr.The_Handle,
                  Win32.Winuser.WM_SETTEXT,
                  Win32.WPARAM(0),
                  Win32.LPARAM(0));
  end Clear;


  function Create (Parent_Page        : Page;
                   The_Title          : String;
                   Initial_Text       : String;
                   The_Action_Routine : Action_Routine := null;
                   Is_Password        : Boolean := False;
                   Is_Modifiable      : Boolean := True;
                   The_Size           : Natural := Automatic;
                   The_Title_Size     : Natural := Automatic) return Plain_Edit_Box is
    Text         : aliased constant Wide_String := To_Wide (Initial_Text) & Wide_Nul;
    The_Edit_Box : constant Plain_Edit_Box := Plain_Edit_Box'(Ptr => new Child_Information);
  begin
    The_Edit_Box.Ptr.Buddy_Child := Buddy_For (The_Text      => To_Wide (The_Title),
                                               The_Text_Size => The_Title_Size,
                                               Is_Automatic  => (The_Size = Automatic) and (The_Title_Size = Automatic),
                                               The_Kind      => Static_Child,
                                               Parent_Page   => Parent_Page);
    The_Edit_Box.Ptr.The_Window_Id := Next_Child_Id;
    The_Edit_Box.Ptr.The_Kind := Edit_Box_Child;
    The_Edit_Box.Ptr.Initial_Title := Win32.Addr(Text);
    The_Edit_Box.Ptr.The_Title_Size := Win32.INT(The_Size);
    The_Edit_Box.Ptr.The_Action_Routine := The_Action_Routine;
    The_Edit_Box.Ptr.Is_Password := Is_Password;
    The_Edit_Box.Ptr.Is_Modifiable := Is_Modifiable;
    Create (The_Edit_Box.Ptr, Parent_Page);
    return The_Edit_Box;
  end Create;


  function Text_Size_Of (The_Text : String) return Natural is
    Text : constant Wide_String := To_Wide (The_Text);
  begin
    return Text'length * Character_Width;
  end Text_Size_Of;


  procedure Set_Text (The_Edit_Box : Edit_Box'class;
                      The_Text     : String) is
    Text   : aliased constant Wide_String := To_Wide (The_Text) & Wide_Nul;
    Unused : Win32.BOOL;
  begin
    Unused := Win32.Winuser.SetWindowTextW (The_Edit_Box.Ptr.The_Handle, Win32.Addr(Text));
  end Set_Text;


  function Contents_Of (The_Edit_Box : Edit_Box'class) return String is
    Max_Text_Size : constant := 250;
    Text_Length   : Win32.INT;
    The_Text      : aliased Wide_String (1 .. Max_Text_Size);
    pragma Warnings (Off, The_Text);
  begin
    Text_Length := Win32.Winuser.GetWindowTextW (The_Edit_Box.Ptr.The_Handle,
                                                 Win32.Addr(The_Text),
                                                 Max_Text_Size);
    return To_Utf8 (The_Text (1..Integer(Text_Length)));
  end Contents_Of;


  function Is_Checked (The_Child : Child_Information_Ptr) return Boolean is
    use type Win32.LRESULT;
  begin
    if Is_Active then
      return Send_Message (The_Child.The_Handle,
                           Win32.Winuser.BM_GETCHECK, 0, 0) = Win32.TRUE;
    else
      return False;
    end if;
  end Is_Checked;


  procedure Set (The_Child : Child_Information_Ptr) is
  begin
    Send_Message (The_Child.The_Handle,
                  Win32.Winuser.BM_SETCHECK, 1, 0);
  end Set;


  procedure Clear (The_Child : Child_Information_Ptr) is
  begin
    Send_Message (The_Child.The_Handle,
                  Win32.Winuser.BM_SETCHECK, 0, 0);
  end Clear;


  function Create (Parent_Page        : Page;
                   The_Title          : String;
                   The_Action_Routine : Action_Routine := null) return Check_Box is
    Title         : aliased constant Wide_String := To_Wide (The_Title) & Wide_Nul;
    The_Check_Box : constant Check_Box := Check_Box'(Ptr => new Child_Information);
  begin
    The_Check_Box.Ptr.The_Window_Id := Next_Child_Id;
    The_Check_Box.Ptr.The_Kind := Check_Box_Child;
    The_Check_Box.Ptr.Initial_Title := Win32.Addr(Title);
    The_Check_Box.Ptr.The_Action_Routine := The_Action_Routine;
    Create (The_Check_Box.Ptr, Parent_Page);
    return The_Check_Box;
  end Create;


  function Is_Checked (The_Check_Box : Check_Box) return Boolean is
  begin
    return Is_Checked (The_Check_Box.Ptr);
  end Is_Checked;


  procedure Set (The_Check_Box : Check_Box) is
  begin
    Set (The_Check_Box.Ptr);
  end Set;


  procedure Clear (The_Check_Box : Check_Box) is
  begin
    Clear (The_Check_Box.Ptr);
  end Clear;


  function Create  (Parent_Page        : Page;
                    The_Title          : String;
                    Initial_Text       : String;
                    The_Action_Routine : Action_Routine := null;
                    Is_Password        : Boolean := False;
                    Is_Modifiable      : Boolean := True;
                    The_Size           : Natural := Automatic;
                    The_Title_Size     : Natural := Automatic) return Checked_Edit_Box is
    Text         : aliased constant Wide_String := To_Wide (Initial_Text) & Wide_Nul;
    The_Edit_Box : constant Checked_Edit_Box := Checked_Edit_Box'(Ptr => new Child_Information);
  begin
    The_Edit_Box.Ptr.Buddy_Child := Buddy_For (The_Text      => To_Wide (The_Title),
                                               The_Text_Size => The_Title_Size,
                                               Is_Automatic  => (The_Size = Automatic) and (The_Title_Size = Automatic),
                                               The_Kind      => Check_Box_Child,
                                               Parent_Page   => Parent_Page);
    if (The_Size = Automatic) and (The_Title_Size = Automatic) then
      The_Edit_Box.Ptr.Buddy_Child.The_Title_Size := 0;
    end if;
    The_Edit_Box.Ptr.Buddy_Child.The_Action_Routine := The_Action_Routine;
    The_Edit_Box.Ptr.The_Window_Id := Next_Child_Id;
    The_Edit_Box.Ptr.The_Kind := Edit_Box_Child;
    The_Edit_Box.Ptr.Initial_Title := Win32.Addr(Text);
    The_Edit_Box.Ptr.The_Title_Size := Win32.INT(The_Size);
    The_Edit_Box.Ptr.The_Action_Routine := The_Action_Routine;
    The_Edit_Box.Ptr.Is_Password := Is_Password;
    The_Edit_Box.Ptr.Is_Modifiable := Is_Modifiable;
    Create (The_Edit_Box.Ptr, Parent_Page);
    return The_Edit_Box;
  end Create;


  function Is_Checked (The_Checked_Edit_Box : Checked_Edit_Box) return Boolean is
  begin
    return Is_Checked (The_Checked_Edit_Box.Ptr.Buddy_Child);
  end Is_Checked;


  procedure Set (The_Checked_Edit_Box : Checked_Edit_Box) is
  begin
    Set (The_Checked_Edit_Box.Ptr.Buddy_Child);
  end Set;


  procedure Clear (The_Checked_Edit_Box : Checked_Edit_Box) is
  begin
    Clear (The_Checked_Edit_Box.Ptr.Buddy_Child);
  end Clear;


  function Create  (Parent_Page        : Page;
                    The_Title          : String;
                    The_Action_Routine : Action_Routine := null;
                    The_Style          : Combo_Style := Default_Combo_Style;
                    The_Size           : Natural := Automatic;
                    The_Title_Size     : Natural := Automatic) return Plain_Combo_Box is
    The_Combo_Box : constant Plain_Combo_Box := Plain_Combo_Box'(Ptr => new Child_Information);
  begin
    The_Combo_Box.Ptr.Buddy_Child
      := Buddy_For (The_Text      => To_Wide (The_Title),
                    The_Text_Size => The_Title_Size,
                    Is_Automatic  => (The_Size = Automatic) and (The_Title_Size = Automatic),
                    The_Kind      => Static_Child,
                    Parent_Page   => Parent_Page);
    The_Combo_Box.Ptr.Buddy_Child.The_Action_Routine := The_Action_Routine;
    The_Combo_Box.Ptr.The_Window_Id := Next_Child_Id;
    The_Combo_Box.Ptr.The_Kind := Combo_Box_Child;
    The_Combo_Box.Ptr.Is_Modifiable := The_Style (Modifiable);
    The_Combo_Box.Ptr.Is_Sorted := The_Style (Sorted);
    The_Combo_Box.Ptr.The_Title_Size := Win32.INT(The_Size);
    The_Combo_Box.Ptr.The_Action_Routine := The_Action_Routine;
    Create (The_Combo_Box.Ptr, Parent_Page);
    return The_Combo_Box;
  end Create;


  procedure Add_Text (The_Combo_Box : Combo_Box'class;
                      The_Text      : String) is
    Text : aliased constant String := The_Text & Nul;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPARAM);
  begin
    Send_Message (The_Combo_Box.Ptr.The_Handle,
                  Win32.Winuser.CB_ADDSTRING,
                  Win32.WPARAM(0),
                  Convert(Text(Text'first)'address));
  end Add_Text;


  procedure Insert_Text (The_Combo_Box : Combo_Box'class;
                         At_Index      : Natural;
                         The_Text      : String) is
    Text : aliased constant String := The_Text & Nul;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPARAM);
  begin
    Send_Message (The_Combo_Box.Ptr.The_Handle,
                  Win32.Winuser.CB_INSERTSTRING,
                  Win32.WPARAM(At_Index),
                  Convert(Text(Text'first)'address));
  end Insert_Text;


  procedure Remove_Text (The_Combo_Box : Combo_Box'class;
                         The_Text      : String) is
    Text        : aliased constant String := The_Text & Nul;
    Entire_List : constant Win32.WPARAM := 16#FFFFFFFF#;
    The_Index   : aliased Win32.LRESULT;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPARAM);
  begin
    The_Index := Send_Message (The_Combo_Box.Ptr.The_Handle,
                               Win32.Winuser.CB_SELECTSTRING,
                               Entire_List,
                               Convert(Text(Text'first)'address));
    Send_Message (The_Combo_Box.Ptr.The_Handle,
                  Win32.Winuser.CB_DELETESTRING,
                  Win32.WPARAM(The_Index),
                  0);
  end Remove_Text;


  procedure Clear_Contents_Of (The_Combo_Box : Combo_Box'class) is
  begin
    Send_Message (The_Combo_Box.Ptr.The_Handle,
                  Win32.Winuser.CB_RESETCONTENT,
                  Win32.WPARAM(0), Win32.LPARAM(0));
  end Clear_Contents_Of;


  procedure Select_Text (The_Combo_Box : Combo_Box'class;
                         The_Text      : String) is
    Text : aliased constant String := The_Text & Nul;
    Entire_List : constant Win32.WPARAM := 16#FFFFFFFF#;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPARAM);
  begin
    Send_Message (The_Combo_Box.Ptr.The_Handle,
                  Win32.Winuser.CB_SELECTSTRING,
                  Entire_List,
                  Convert(Text(Text'first)'address));
  end Select_Text;


  procedure Select_First (The_Combo_Box : Combo_Box'class) is
  begin
    Send_Message (The_Combo_Box.Ptr.The_Handle,
                  Win32.Winuser.CB_SETCURSEL, 0, 0);
  end Select_First;


  procedure Deselect (The_Combo_Box : Combo_Box'class) is
    Nothing : constant Win32.WPARAM := 16#FFFFFFFF#;
  begin
    Send_Message (The_Combo_Box.Ptr.The_Handle,
                  Win32.Winuser.CB_SETCURSEL, Nothing, 0);
  end Deselect;


  function Contents_Of (The_Combo_Box : Combo_Box'class) return String is
    The_Index : Win32.LRESULT;
    The_Size  : Win32.LRESULT;
    Unused    : Win32.INT;
    Max_Text_Size : constant := 255;
    The_Text      : aliased Wide_String (1..Max_Text_Size);
    pragma Warnings (Off, The_Text);
    function Convert is new Ada.Unchecked_Conversion (Win32.PWSTR, Win32.LPARAM);
    use type Win32.LRESULT;
  begin
    if Is_Active then
      The_Index := Send_Message_Wide (The_Combo_Box.Ptr.The_Handle,
                                      Win32.Winuser.CB_GETCURSEL, 0, 0);
      if The_Index < 0 then  -- nothing selected
        The_Size := Win32.LRESULT(Win32.Winuser.GetWindowTextLengthW (The_Combo_Box.Ptr.The_Handle));
        if The_Size <= 0 then -- Nothing typed
          return "";
        else
          Unused := Win32.Winuser.GetWindowTextW (The_Combo_Box.Ptr.The_Handle,
                                                  Win32.Addr(The_Text),
                                                  Max_Text_Size);
        end if;
      else
        The_Size := Send_Message_Wide (The_Combo_Box.Ptr.The_Handle,
                                       Win32.Winuser.CB_GETLBTEXTLEN,
                                       Win32.WPARAM(The_Index), 0);
        if (The_Size <= 0) or (The_Size > Max_Text_Size) then
          return "";
        else
          Send_Message_Wide (The_Combo_Box.Ptr.The_Handle,
                             Win32.Winuser.CB_GETLBTEXT,
                             Win32.WPARAM(The_Index),
                             Convert(Win32.Addr(The_Text)));
        end if;
      end if;
      return To_Utf8 (The_Text (1..Natural(The_Size)));
    else
      return "";
    end if;
  end Contents_Of;


  function Create  (Parent_Page        : Page;
                    The_Title          : String;
                    The_Action_Routine : Action_Routine := null;
                    The_Style          : Combo_Style := Default_Combo_Style;
                    The_Size           : Natural := Automatic;
                    The_Title_Size     : Natural := Automatic) return Checked_Combo_Box is
    The_Checked_Combo_Box : constant Checked_Combo_Box := Checked_Combo_Box'(Ptr => new Child_Information);
  begin
    The_Checked_Combo_Box.Ptr.Buddy_Child
      := Buddy_For (The_Text      => To_Wide (The_Title),
                    The_Text_Size => The_Title_Size,
                    Is_Automatic  => (The_Size = Automatic) and (The_Title_Size = Automatic),
                    The_Kind      => Check_Box_Child,
                    Parent_Page   => Parent_Page);
    The_Checked_Combo_Box.Ptr.Buddy_Child.The_Action_Routine := The_Action_Routine;
    The_Checked_Combo_Box.Ptr.The_Window_Id := Next_Child_Id;
    The_Checked_Combo_Box.Ptr.The_Kind := Combo_Box_Child;
    The_Checked_Combo_Box.Ptr.Is_Modifiable := The_Style (Modifiable);
    The_Checked_Combo_Box.Ptr.Is_Sorted := The_Style (Sorted);
    The_Checked_Combo_Box.Ptr.The_Title_Size := Win32.INT(The_Size);
    The_Checked_Combo_Box.Ptr.The_Action_Routine := The_Action_Routine;
    Create (The_Checked_Combo_Box.Ptr, Parent_Page);
    return The_Checked_Combo_Box;
  end Create;


  function Is_Checked (The_Checked_Combo_Box : Checked_Combo_Box) return Boolean is
  begin
    return Is_Checked (The_Checked_Combo_Box.Ptr.Buddy_Child);
  end Is_Checked;


  procedure Set (The_Checked_Combo_Box : Checked_Combo_Box) is
  begin
    Set (The_Checked_Combo_Box.Ptr.Buddy_Child);
  end Set;


  procedure Clear (The_Checked_Combo_Box : Checked_Combo_Box) is
  begin
    Clear (The_Checked_Combo_Box.Ptr.Buddy_Child);
  end Clear;


  function Is_Root (The_Tree_Item : Tree_Item) return Boolean is
    use type Tree_Item;
  begin
    return The_Tree_Item = Root_Item;
  end Is_Root;


  procedure Mark_Has_Children (The_Tree_View : Tree_View;
                               The_Tree_Item : Tree_Item) is
    The_Item : aliased Win32.Comctl.Tv_Item_Ansi;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPARAM);
  begin
    The_Item.Mask := Win32.Comctl.Tvif_Children;
    The_Item.Hitem := The_Tree_Item;
    The_Item.Children := 1;
    Send_Message (The_Tree_View.Ptr.The_Handle,
                  Win32.Comctl.Tvm_Setitem_Ansi,
                  0, Convert(The_Item'address));
  end Mark_Has_Children;


  procedure Mark_Has_No_Children (The_Tree_View : Tree_View;
                                       The_Tree_Item : Tree_Item) is
    The_Item : aliased Win32.Comctl.Tv_Item_Ansi;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPARAM);
  begin
    The_Item.Mask := Win32.Comctl.Tvif_Children;
    The_Item.Hitem := The_Tree_Item;
    The_Item.Children := 0;
    Send_Message (The_Tree_View.Ptr.The_Handle,
                  Win32.Comctl.Tvm_Setitem_Ansi,
                  0, Convert(The_Item'address));
  end Mark_Has_No_Children;


  function Add_Item (The_Tree_View   : Tree_View;
                     The_Title       : String;
                     The_Parent      : Tree_Item := Root_Item;
                     After_Item      : Tree_Item;
                     The_Information : Information := No_Information) return Tree_Item is
    Title : aliased constant String := The_Title & Nul;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPARAM);
    function Convert is new Ada.Unchecked_Conversion (Information, Win32.LPARAM);
    The_Item      : aliased Win32.Comctl.Tv_Item_Ansi;
    The_Insertion : aliased Win32.Comctl.Tv_Insertstruct_Ansi;
    New_Item      : Tree_Item;
    use type Win32.UINT;
  begin
    The_Item.Mask := Win32.Comctl.Tvif_Text + Win32.Comctl.Tvif_Param + Win32.Comctl.Tvif_Children;
    The_Item.Text := Win32.Addr(Title);
    The_Item.Lparam := Convert (The_Information);
    The_Item.Children := 0;
    The_Insertion.Parent := The_Parent;
    The_Insertion.Insertafter := After_Item;
    The_Insertion.Item := The_Item;
    New_Item := Tree_Item (Send_Message (The_Tree_View.Ptr.The_Handle,
                                         Win32.Comctl.Tvm_Insertitem_Ansi,
                                         0, Convert(The_Insertion'address)));
    if not Is_Root (The_Parent) then
      Mark_Has_Children (The_Tree_View, The_Parent);
    end if;
    return New_Item;
  end Add_Item;


  function Add_Wide_Item (The_Tree_View   : Tree_View;
                          The_Title       : Wide_String;
                          The_Parent      : Tree_Item := Root_Item;
                          After_Item      : Tree_Item;
                          The_Information : Information := No_Information) return Tree_Item is
    Title : aliased constant Wide_String := The_Title & Wide_Nul;
    function Convert is new Ada.Unchecked_Conversion (Information, Win32.LPARAM);
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPARAM);
    The_Item      : aliased Win32.Comctl.Tv_Item_Wide;
    The_Insertion : aliased Win32.Comctl.Tv_Insertstruct_Wide;
    New_Item      : Tree_Item;
    use type Win32.UINT;
  begin
    The_Item.Mask := Win32.Comctl.Tvif_Text + Win32.Comctl.Tvif_Param;
    The_Item.Text := Win32.Addr(Title);
    The_Item.Lparam := Convert (The_Information);
    The_Insertion.Parent := The_Parent;
    The_Insertion.Insertafter := After_Item;
    The_Insertion.Item := The_Item;
    New_Item := Tree_Item (Send_Message_Wide (The_Tree_View.Ptr.The_Handle,
                                              Win32.Comctl.Tvm_Insertitem_Wide,
                                              0, Convert(The_Insertion'address)));
    if not Is_Root (The_Parent) then
      Mark_Has_Children (The_Tree_View, The_Parent);
    end if;
    return New_Item;
  end Add_Wide_Item;


  function Add_Item (The_Tree_View   : Tree_View;
                     The_Title       : String;
                     The_Parent      : Tree_Item := Root_Item;
                     The_Position    : Item_Position := Sorted;
                     The_Information : Information := No_Information) return Tree_Item is
    The_Item : Tree_Item;
  begin
    case The_Position is
    when First  => The_Item := Tree_Item (Win32.Comctl.Tvi_First);
    when Last   => The_Item := Tree_Item (Win32.Comctl.Tvi_Last);
    when Sorted => The_Item := Tree_Item (Win32.Comctl.Tvi_Sort);
    end case;
    return Add_Wide_Item (The_Tree_View, To_Wide (The_Title), The_Parent, The_Item, The_Information);
  end Add_Item;


  procedure Rename_Item (The_Tree_View : Tree_View;
                         The_Item      : Tree_Item;
                         New_Title     : String) is
    Title : aliased constant Wide_String := To_Wide (New_Title) & Wide_Nul;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPARAM);
    Replacement : Win32.Comctl.Tv_Item_Wide;
  begin
    Replacement.Hitem := The_Item;
    Replacement.Text := Win32.Addr(Title);
    Replacement.Mask := Win32.Comctl.Tvif_Text;
    Send_Message_Wide (The_Tree_View.Ptr.The_Handle,
                       Win32.Comctl.Tvm_Setitem_Wide,
                       0, Convert (Replacement'address));
  end Rename_Item;


  function Get_Parent (The_Tree_View : Tree_View;
                       The_Item      : Tree_Item) return Tree_Item is
    function Convert is new Ada.Unchecked_Conversion (Tree_Item, Win32.LPARAM);
    Parent : Tree_Item;
    use type Tree_Item;
  begin
    if Is_Root (The_Item) then
      raise Tree_Item_Is_Root;
    end if;
    Parent := Tree_Item (Send_Message (The_Tree_View.Ptr.The_Handle,
                         Win32.Comctl.Tvm_Getnextitem,
                         Win32.Comctl.Tvgn_Parent,
                         Convert (The_Item)));
    if Parent = 0 then
      return Root_Item;
    else
      return Parent;
    end if;
  end Get_Parent;


  function Get_First_Child (The_Tree_View : Tree_View;
                            The_Item      : Tree_Item) return Tree_Item is
    function Convert is new Ada.Unchecked_Conversion (Tree_Item, Win32.LPARAM);
    The_Child : Tree_Item;
    use type Tree_Item;
  begin
    The_Child := Tree_Item (Send_Message (The_Tree_View.Ptr.The_Handle,
                                          Win32.Comctl.Tvm_Getnextitem,
                                          Win32.Comctl.Tvgn_Child,
                                          Convert (The_Item)));
    if The_Child = 0 then
      return Root_Item;
    else
      return The_Child;
    end if;
  end Get_First_Child;


  procedure Delete_Item (The_Tree_View : Tree_View;
                         The_Item      : Tree_Item) is
    function Convert is new Ada.Unchecked_Conversion (Tree_Item, Win32.LPARAM);
    Parent : Tree_Item := Root_Item;
  begin
    if not Is_Root (The_Item) then
      Parent := Get_Parent (The_Tree_View, The_Item);
    end if;
    Send_Message (The_Tree_View.Ptr.The_Handle,
                  Win32.Comctl.Tvm_Deleteitem,
                  0, Convert (The_Item));
    if not Is_Root (Parent) and then
       Is_Root (Get_First_Child (The_Tree_View, Parent))
    then
      Mark_Has_No_Children (The_Tree_View, Parent);
    end if;
  end Delete_Item;


  procedure Delete_All_Items (The_Tree_View : Tree_View) is
  begin
    Delete_Item (The_Tree_View, Root_Item);
  end Delete_All_Items;


  procedure Expand_Item (The_Tree_View : Tree_View;
                         The_Item      : Tree_Item) is
    function Convert is new Ada.Unchecked_Conversion (Tree_Item, Win32.LPARAM);
  begin
    Send_Message (The_Tree_View.Ptr.The_Handle,
                  Win32.Comctl.Tvm_Expand,
                  Win32.Comctl.Tve_Expand,
                  Convert(The_Item));
  end Expand_Item;


  procedure Ensure_Item_Visible (The_Tree_View : Tree_View;
                                 The_Item      : Tree_Item) is
    function Convert is new Ada.Unchecked_Conversion (Tree_Item, Win32.LPARAM);
  begin
    Send_Message (The_Tree_View.Ptr.The_Handle,
                  Win32.Comctl.Tvm_Ensurevisible,
                  0,
                  Convert(The_Item));
  end Ensure_Item_Visible;


  procedure Pop_Up (The_Page : Page) is
  begin
    The_Page.Parent := Current_Page;
    if Current_Page /= null then
      Make_Invisible (Current_Page);
      Make_Invisible (The_Tabs);
    end if;
    Current_Page := The_Page;
    Make_Visible (Current_Page);
    Redraw_Main_Window;
  end Pop_Up;


  function Nr_Of_Nuls_In (The_String : String) return Natural is
    Nr_Of_Nuls : Natural := 0;
  begin
    for The_Index in The_String'range loop
      if The_String (The_Index) = Nul then
        Nr_Of_Nuls := Nr_Of_Nuls + 1;
      end if;
    end loop;
    return Nr_Of_Nuls;
  end Nr_Of_Nuls_In;


  package OPENFILENAMEA_Conversion is new System.Address_To_Access_Conversions (Win32.Commdlg.OPENFILENAMEA);

  function Get_Input_Filename (Default_Extension     : String;
                               Extension_Description : String) return String is
    The_Information   : aliased Win32.Commdlg.OPENFILENAME;
    Extension         : aliased constant String := Default_Extension & Nul;
    Filters           : aliased constant String := Extension_Description & Nul & "*." & Extension & Nul;
    Max_Filename_Size : constant := 260;
    The_Filename      : aliased String (1..Max_Filename_Size);
    The_Index         : Natural;
    Flags             : constant := Win32.Commdlg.OFN_FILEMUSTEXIST +
                                    Win32.Commdlg.OFN_HIDEREADONLY;
    use type Win32.BOOL;
  begin
    The_Filename(1) := Nul;  -- Filename not initialized
    The_Information.lStructSize := Win32.Commdlg.OPENFILENAME'size / 8;
    The_Information.hwndOwner := Private_Window;
    The_Information.hInstance := Our_Instance;
    The_Information.lpstrFilter := Win32.Addr(Filters);
    The_Information.lpstrCustomFilter := null;
    The_Information.nFilterIndex := Win32.DWORD ((Nr_Of_Nuls_In (Extension_Description) / 2) + 1);
    The_Information.lpstrFile := Win32.Addr(The_Filename);
    The_Information.nMaxFile := Max_Filename_Size;
    The_Information.lpstrFileTitle := null;
    The_Information.lpstrInitialDir := null;
    The_Information.lpstrTitle := null;
    The_Information.Flags := Flags;
    The_Information.lpstrDefExt := Win32.Addr(Extension);
    if Win32.Commdlg.GetOpenFileName
         (Win32.Commdlg.LPOPENFILENAME(OPENFILENAMEA_Conversion.To_Pointer (The_Information'address))) = Win32.FALSE
    then
      return "";
    else
      The_Index := Natural(The_Information.nFileOffset); -- Skip directory.
      loop -- search for zero terminator
        exit when The_Filename(The_Index) = Nul;
        The_Index := The_Index + 1;
      end loop;
      return The_Filename (1..The_Index - 1);
    end if;
  end Get_Input_Filename;


  function Get_Output_Filename (Default_Extension     : String;
                                Extension_Description : String;
                                Initial_Filename      : String := "";
                                Default_Directory     : String := "") return String is

    Max_Filename_Size : constant := 260;
    The_Information   : aliased Win32.Commdlg.OPENFILENAME;
    The_Filename      : aliased String(1..Max_Filename_Size);
    Directory         : aliased constant String := Default_Directory & Nul;
    Extension         : aliased constant String := Default_Extension & Nul;
    Filters           : aliased constant String := Extension_Description & Nul & "*." & Extension & Nul;
    The_Index         : Natural;
    Flags             : constant := Win32.Commdlg.OFN_PATHMUSTEXIST +
                                    Win32.Commdlg.OFN_CREATEPROMPT +
                                    Win32.Commdlg.OFN_OVERWRITEPROMPT +
                                    Win32.Commdlg.OFN_HIDEREADONLY;
    use type Win32.BOOL;

  begin
    The_Filename(1..Initial_Filename'length + 1) := Initial_Filename & Nul;
    The_Information.lStructSize := Win32.Commdlg.OPENFILENAME'size / 8;
    The_Information.hwndOwner := Private_Window;
    The_Information.hInstance := Our_Instance;
    The_Information.lpstrFilter := Win32.Addr(Filters);
    The_Information.lpstrCustomFilter := null;
    The_Information.nFilterIndex := Win32.DWORD ((Nr_Of_Nuls_In (Extension_Description) / 2) + 1);
    The_Information.lpstrFile := Win32.Addr(The_Filename);
    The_Information.nMaxFile := Max_Filename_Size;
    The_Information.lpstrFileTitle := null;
    if Default_Directory = "" then
      The_Information.lpstrInitialDir := null;
    else
      The_Information.lpstrInitialDir := Win32.Addr(Directory);
    end if;
    The_Information.lpstrTitle := null;
    The_Information.Flags := Flags;
    The_Information.lpstrDefExt := Win32.Addr(Extension);
    if Win32.Commdlg.GetSaveFileName
         (Win32.Commdlg.LPOPENFILENAME(OPENFILENAMEA_Conversion.To_Pointer (The_Information'address))) = Win32.FALSE
    then
      return "";
    else
      The_Index := Natural(The_Information.nFileOffset); -- Skip directory.
      loop -- search for zero terminator
        exit when The_Filename(The_Index) = Nul;
        The_Index := The_Index + 1;
      end loop;
      return The_Filename (1..The_Index - 1);
    end if;
  end Get_Output_Filename;


  task body Termination_Handler is
  begin
    accept Start;
    Action.Finish;
    accept Finalize;
    if The_Termination_Routine /= null then
      The_Termination_Routine.all;
    end if;
    Send_Message (Private_Window, Wm_User_Close, 0, 0);
    Action.Do_Terminate;
  exception
  when Event: others =>
    Log.Write ("Gui.Termination_Handler", Event);
    Send_Message (Private_Window, Wm_User_Close, 0, 0);
    Action.Do_Terminate;
  end Termination_Handler;


  task body Action_Handler is
    The_Action : Action_Routine;
    The_Click  : Click_Routine;
    The_Info   : Information;
  begin
    loop
      Action.Get (The_Action, The_Click, The_Info);
      if The_Click /= null then
        The_Click.all (The_Info);
      else
        if The_Action /= null then
          The_Action.all;
        else
          exit;
        end if;
      end if;
      Action.Enable;
    end loop;
    The_Termination_Handler.Finalize;
  exception
  when Event: others =>
    Log.Write ("Gui.Action_Handler", Event);
  end Action_Handler;


begin
  Win32.Commctrl.InitCommonControls;
end Gui;
