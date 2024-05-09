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

with Win32.Comctl;
with Win32.Windef;
with Win32.Wingdi; pragma Elaborate (Win32.Wingdi);
with Win32.Winuser;

package Gui is

  Action_Routine_Is_Defined    : exception;
  Main_Window_Creation_Failure : exception;
  Sequence_Error               : exception;
  Status_Line_Creation_Failure : exception;
  Tab_Creation_Failure         : exception;
  Tree_Item_Is_Root            : exception;

  Automatic : constant Natural := 0;

  Default_Button_Width : constant := 100;

  subtype Color is Win32.Windef.COLORREF;

  -- Gdi colours
  Black   : constant Color := Win32.Wingdi.RGB (0,   0,   0);
  Gray    : constant Color := Win32.Wingdi.RGB (128, 128, 128);
  Brown   : constant Color := Win32.Wingdi.RGB (128, 64,  64);
  Red     : constant Color := Win32.Wingdi.RGB (192, 0,   0);
  Olive   : constant Color := Win32.Wingdi.RGB (128, 128, 0);
  Orange  : constant Color := Win32.Wingdi.RGB (255, 128, 0);
  Yellow  : constant Color := Win32.Wingdi.RGB (192, 192, 0);
  Green   : constant Color := Win32.Wingdi.RGB (0,   192, 0);
  Cyan    : constant Color := Win32.Wingdi.RGB (0,   192, 192);
  Blue    : constant Color := Win32.Wingdi.RGB (0,   0,   192);
  Magenta : constant Color := Win32.Wingdi.RGB (192, 0,   192);
  White   : constant Color := Win32.Wingdi.RGB (255, 255, 255);

  type Information is new Win32.INT_PTR; -- Large enough to hold an address

  No_Information : constant Information := 0;

  type Information_Array is array (Natural range <>) of Information;

  Empty_Information : constant Information_Array(1..0) := [];

  type Window_Metrics is record
    X_Position : Integer;
    Y_Position : Integer;
    Width      : Natural;
    Height     : Natural;
  end record;

  Default_Position : constant := Win32.Winuser.CW_USEDEFAULT;

  type Menu_Kind is (Plain, Checked, Radio);

  type Menu is private;
  type Menu_Item is abstract tagged private;

  type Plain_Menu_Item is new Menu_Item with private;

  type Selection_Menu_Item is abstract new Menu_Item with private;
  procedure Set (The_Item : Selection_Menu_Item);
  function Is_Set (The_Item : Selection_Menu_Item) return Boolean;

  type Checked_Menu_Item is new Selection_Menu_Item with private;
  type Radio_Menu_Item is new Selection_Menu_Item with private;

  type Page is private;
  type Child is abstract tagged private;
  type Button is new Child with private;
  type Progress_Bar is new Child with private;
  type List_View is new Child with private;
  type Tree_View is new Child with private;
  type Text_View is new Child with private;
  type Track_Bar is new Child with private;
  type Edit_Box is abstract new Child with private;
  type Check_Box is new Child with private;
  type Combo_Box is abstract new Child with private;


  type Checked_Edit_Box is new Edit_Box with private;
  type Plain_Edit_Box is new Edit_Box with private;

  type Checked_Combo_Box is new Combo_Box with private;
  type Plain_Combo_Box is new Combo_Box with private;

  type Action_Routine is access procedure;

  type Click_Kind is (Double_Click, Single_Click);

  type Mouse_Handler is access procedure (The_Point     : Win32.Windef.POINT;
                                          The_Mousecode : Win32.WPARAM);

  type Color_Handler is access function (For_Information : Information) return Color;

  type Text_Handler is access function (For_Column       : Natural;
                                        With_Information : Information) return String;

  type Relation is (Less_Than, Equal_To, Greater_Than);

  type Sort_Routine is access function (For_Column         : Natural;
                                        First_Information  : Information;
                                        Second_Information : Information) return Relation;

  type Sort_Direction is (Forward, Backward);

  type Justification is (Left, Center, Right);

  type Pop_Up_Return is (Cancel, Ok);

  type Pop_Up_Termination is access procedure (The_Return : Pop_Up_Return);

  type Page_Styles is (Buttons_Fill_Horizontally,
                       Buttons_Fill_Vertically);

  type Page_Style is array (Page_Styles'range) of Boolean;

  Default_Page_Style : constant Page_Style := [others => False];

  type Sound is (Standard_Sound, Asterisk, Exclamation, Hand, Question, Default);

  procedure Message_Box (The_Message    : String;
                         Is_Exclamation : Boolean := True);

  procedure Beep (With_Sound : Sound := Standard_Sound);

  function Is_Confirmed (The_Question : String) return Boolean;

  procedure Shell_Open (The_File       : String;
                        The_Parameters : String := "");

  procedure Create (Application_Name    : String;
                    Termination_Routine : Action_Routine;
                    Window_Width        : Natural := 0;
                    Window_Height       : Natural := 0;
                    Window_X_Position   : Integer := Default_Position;
                    Window_Y_Position   : Integer := Default_Position;
                    Always_Topmost      : Boolean := False);
  -- Creates the main window for the application.


  procedure Create (Application_Name    : String;
                    Termination_Routine : Action_Routine;
                    The_Metrics         : Window_Metrics;
                    Always_Topmost      : Boolean := False);
  -- Creates the main window for the application.

  procedure Execute (Application_Name    : String;
                     Startup_Routine     : not null access procedure;
                     Termination_Routine : access procedure := null;
                     Initial_Metrics     : Window_Metrics;
                     Always_Topmost      : Boolean := False);
  -- Creates the main window for the application.
  -- Note that this should be called from the main process under OSX and that it does not return until the
  -- Gui is closed either by the user or calling the procedure Close.
  -- Code to setup the GUI interface should be placed into the Startup_Routine
  -- Code to extract the contents of Gui objects immediately before the gui is closed may be placed in the
  -- optional Termination routine

  procedure Change_Application_Name (Name : String);
  -- Changes the application's name that is displayed as part of the main window's title


  function Name_Of_Application return String;
  -- Returns the application's name


  function Get_Window_Metrics return Window_Metrics;
  -- Returns information about the main window


  procedure Set_Window_Metrics (X_Position    : Integer;
                                Y_Position    : Integer;
                                Window_Width  : Natural;
                                Window_Height : Natural);

  function Is_Visible (X_Position : Integer;
                       Y_Position : Integer;
                       Width      : Natural := 0;
                       Height     : Natural := 0) return Boolean;
  -- Returns true if the rectangle intersects with a Desktop Monitor
  -- Ie At least part of it can be seen.
  -- If Width and Height are both zero, the function returns true if
  -- the point is contained within a monitor.

  function Application_Is_Minimized return Boolean;

  function Application_Is_Maximized return Boolean;

  procedure Disable_Close_Button;

  procedure Close;

  procedure Define_Callback (Callback_Routine : Action_Routine);

  procedure Instigate_Callback;

  procedure Install_Mouse_Handler (The_Mouse_Handler : Mouse_Handler);

  procedure Set_Status_Line (The_Text : String);

  function Create (The_Style  : Page_Style     := Default_Page_Style;
                   The_Action : Action_Routine := null ) return Page;
  -- Creates a page that can later be added to the window using the procedure Add_Page
  -- The page action routine will be called whenever it becomes the current page

  function Add_Page (The_Title            : String;
                     The_Style            : Page_Style     := Default_Page_Style;
                     The_Action           : Action_Routine := null;
                     Minimum_Button_Width : Natural        := Default_Button_Width) return Page;
  -- Adds a page to the window. This page is then accessed by the user via a tab control

  procedure Add_Page (The_Page  : Page;
                      The_Title : String);
  -- Adds an existing page to the window.
  -- The page was either created using Create or by Add_Page (but then subsequently removed from the window).
  -- This routine adds the page to the window and can then be accessed by the user via a tab control

  procedure Select_Page (The_Page : Page);
  --
  -- Causes the specified page to become the current (displayed) page
  -- (same effect as if the user had selected the page using the tab control
  --

  procedure Remove_Page (The_Page : Page);
  -- Removes the first occurrence of the page from the window.

  function Create (The_Style   : Page_Style := Default_Page_Style;
                   The_Routine : Pop_Up_Termination;
                   Cancel_Text : String := "";
                   Ok_Text     : String := "") return Page;
  -- Creates pop-up page that cannot be accessed directly by the user.
  -- The page must be explicitly caused to pop-up by the program calling the following routine.

  procedure Pop_Up (The_Page : Page);

  function Get_Input_Filename (Default_Extension     : String;
                               Extension_Description : String) return String;

  function Get_Output_Filename (Default_Extension     : String;
                                Extension_Description : String;
                                Initial_Filename      : String := "";
                                Default_Directory     : String := "") return String;

  ---------------------------------------
  --
  -- Keyboard handling
  --
  ---------------------------------------

  type Key_Event is (Key_Pressed, Key_Released);

  type Key_Code is new Win32.WPARAM;
  --  see Gui.Key_Codes for key type constants

  type Key_Handler is access procedure (The_Event   : Key_Event;
                                        The_Keycode : Key_Code);

  procedure Install_Key_Handler (The_Key_Handler : Key_Handler);

  procedure Disable_Key_Handler;

  procedure Enable_Key_Handler;


  ---------------------------------------
  --
  -- Functions available for all controls
  --
  ---------------------------------------

  function Is_Defined (The_Child : Child'class) return Boolean;
  --
  --  Function that returns True if the specified child has been created
  --

  procedure Hide (The_Child : Child'class);

  procedure Show (The_Child : Child'class);

  function Is_Hidden (The_Child : Child'class) return Boolean;

  procedure Enable (The_Child : Child'class);

  procedure Disable (The_Child : Child'class);

  function Is_Enabled (The_Child : Child'class) return Boolean;

  procedure Set_Action (The_Child          : Child'class;
                        The_Action_Routine : Action_Routine);

  procedure Set_Qualified_Action (The_Child          : Child'class;
                                  The_Action_Routine : access procedure (Item : Information);
                                  The_Information    : Information);

  procedure Set_Focus (The_Child : Child'class);

  function Parent_Page_Of (The_Child : Child'class) return Page;

  ---------------------------------------
  --
  -- Menu handling
  --
  ---------------------------------------

  function Add_Menu (The_Text : String) return Menu;

  function Add_Menu (The_Text : String;
                     To_Menu  : Menu) return Menu;

  procedure Add_Menu_Separator (To_Menu : Menu);

  function Add_Menu_Item (The_Text         : String;
                          To_Menu          : Menu;
                          The_Menu_Handler : access procedure (Item : Information);
                          The_Information  : Information := No_Information) return Plain_Menu_Item;

  function Add_Menu_Item (The_Text         : String;
                          To_Menu          : Menu;
                          The_Menu_Handler : access procedure (Item : Information);
                          The_Information  : Information := No_Information) return Checked_Menu_Item;

  function Add_Menu_Item (The_Text : String;
                          To_Menu  : Menu) return Checked_Menu_Item;

  function Add_Menu_Item (The_Text         : String;
                          To_Menu          : Menu;
                          The_Menu_Handler : access procedure (Item : Information);
                          The_Information  : Information := No_Information) return Radio_Menu_Item;
  -- Adds a radio item to the menu. By default the first item is set.
  -- This can be explicitly set using the procedure Set

  function Add_Menu_Item (The_Text : String;
                          To_Menu  : Menu) return Radio_Menu_Item;

  procedure Enable_Menubar;

  procedure Disable_Menubar;

  procedure Enable (The_Menu : Menu);

  procedure Disable (The_Menu : Menu);

  procedure Enable (The_Item : Menu_Item'class);

  procedure Disable (The_Item : Menu_Item'class);

  overriding procedure Set (The_Item : Checked_Menu_Item);

  procedure Clear (The_Item : Checked_Menu_Item);

  overriding procedure Set (The_Item : Radio_Menu_Item);

  function Setting (The_Menu : Menu) return Natural;
  -- Returns the ordinal of the radio button currently selected
  -- Returns zero if there are no radio buttons
  --
  -- If this procedure is not used, the first radio item of the menu is set by default

  -------------------
  --
  -- Button functions
  --
  -------------------
  function Create (Parent_Page        : Page;
                   The_Text           : String;
                   The_Action_Routine : Action_Routine := null;
                   The_Button_Size    : Natural := Default_Button_Width) return Button;

  procedure Set_Text (The_Button : Button;
                      The_Text   : String);

  -------------------------
  --
  -- Progress Bar functions
  --
  -------------------------
  function Create (Parent_Page : Page) return Progress_Bar;
  -- Creates a progress bar with a range 0..100 incrementing in steps of 10

  procedure Define_Range (The_Progress_Bar : Progress_Bar;
                          The_Extent       : Positive;
                          The_Step         : Positive := 1);

  procedure Report_Progress (The_Progress_Bar : Progress_Bar;
                             The_Extent       : Natural);

  procedure Increment_Progress (The_Progress_Bar : Progress_Bar);
  -- Increment the progress bar by a step

  procedure Set_Color (The_Progress_Bar : Progress_Bar;
                       The_Color        : Color);

  ----------------------
  --
  -- List View functions
  --
  ----------------------

  type Column is record
    Position : Positive;
    List     : List_View;
  end record;

  function Create (Parent_Page           : Page;
                   The_Text_Handler      : access function (For_Column       : Natural;
                                                            With_Information : Information) return String;
                   The_Color_Handler     : Color_Handler := null;
                   The_Sort_Routine      : Sort_Routine := null;
                   The_Click_Routine     : access procedure (Item : Information) := null;
                   The_Click_Kind        : Click_Kind := Double_Click;
                   Use_Proportional_Font : Boolean := True;
                   Color_Background      : Boolean := False) return List_View;

  procedure Add_Gridlines (The_List_View : List_View);

  procedure Remove_Gridlines (The_List_View : List_View);

  function Add_Column (The_List_View     : List_View;
                       The_Title         : String;
                       The_Width         : Natural := Automatic;
                       The_Justification : Justification := Left) return Column;

  procedure Set_Title (The_Column : Column;
                       The_Title  : String);

  procedure Set_Width (The_Column    : Column;
                       The_New_Width : Natural);

  function Width_Of (The_Column : Column) return Natural;

  procedure Sort (The_Column    : Column;
                  The_Direction : Sort_Direction);

  procedure Remove_Last_Column (The_List_View : List_View);

  function Number_Of_Columns_Of (The_List_View : List_View) return Natural;

  procedure Add_To (The_List_View   : List_View;
                    Item_Ordinal    : Positive := 1;
                    The_Information : Information);

  procedure Add_Data (The_List_View   : List_View;
                      The_Information : Information) with Inline => True;

  procedure Notify_Item_Update (The_List_View : List_View;
                                Item_Ordinal  : Positive);

  procedure Remove_From (The_List_View : List_View;
                         Item_Ordinal  : Positive := 1);

  procedure Remove_All_From (The_List_View : List_View);

  procedure Reserve_Space_For (The_List_View : List_View;
                               Amount        : Positive);

  function Visible_Region_Of (The_List_View : List_View) return Natural;
  -- returns how many items can be seen in the list view.

  procedure Make_Visible_In (The_List_View : List_View;
                             Item_Ordinal  : Positive := 1);

  procedure Make_Last_Visible_In (The_List_View : List_View) with Inline => True;

  function Selected_Information_From (The_List_View : List_View) return Information_Array;

  procedure Freeze (The_List_View : List_View);

  procedure Thaw (The_List_View : List_View);

  ----------------------
  --
  -- Tree View functions
  --
  ----------------------
  subtype Tree_Item is Win32.Comctl.Htreeitem;

  Root_Item : constant Tree_Item := Win32.Comctl.Tvi_First;

  type Item_Position is (First, Last, Sorted);

  function Create (Parent_Page       : Page;
                   The_Click_Routine : access procedure (Item : Information) := null;
                   The_Color_Handler : Color_Handler := null) return Tree_View;
--
-- Note: If a colour handler is supplied then it is called to define the colour
--       of each tree item. The background colour is fixed to be always white
--       Ie Selected items no longer have a blue background but retain the enclosing frame
--       (so it is still possible to identify which item is selected)
--

  function Add_Item (The_Tree_View   : Tree_View;
                     The_Title       : String;
                     The_Parent      : Tree_Item := Root_Item;
                     The_Position    : Item_Position := Sorted;
                     The_Information : Information := No_Information) return Tree_Item;

  function Add_Item (The_Tree_View   : Tree_View;
                     The_Title       : String;
                     The_Parent      : Tree_Item := Root_Item;
                     After_Item      : Tree_Item;
                     The_Information : Information := No_Information) return Tree_Item;

  function Get_Parent (The_Tree_View : Tree_View;
                       The_Item      : Tree_Item) return Tree_Item;
  --
  -- Exception Tree_Item_Is_Root raised if The_Item is Root_Item (as this has no parent)
  --

  procedure Rename_Item (The_Tree_View : Tree_View;
                         The_Item      : Tree_Item;
                         New_Title     : String);

  procedure Delete_Item (The_Tree_View : Tree_View;
                         The_Item      : Tree_Item);

  procedure Delete_All_Items (The_Tree_View : Tree_View);

  procedure Expand_Item (The_Tree_View : Tree_View;
                         The_Item      : Tree_Item);

  procedure Ensure_Item_Visible (The_Tree_View : Tree_View;
                                 The_Item      : Tree_Item);
  --
  -- Expands all ancestor nodes and scrolls Tree_view so that the item is visible
  --

  ----------------------
  --
  -- Text View functions
  --
  ----------------------
  function Create (Parent_Page   : Page;
                   Word_Wrapping : Boolean := False) return Text_View;

  procedure Append_To (The_Rich_Edit  : Text_View;
                       The_Text       : String;
                       The_Color      : Color := Gui.Black;
                       Ensure_Visible : Boolean := True);

  procedure Append_Line_To (The_Rich_Edit  : Text_View;
                            The_Text       : String;
                            The_Color      : Color := Gui.Black;
                            Ensure_Visible : Boolean := True);

  procedure Clear (The_Rich_Edit : Text_View);

  ----------------------
  --
  -- Track Bar functions
  --
  ----------------------
  function Create (Parent_Page : Page;
                   The_Title   : String := "") return Track_Bar;

  procedure Define_Range (The_Track_Bar : Track_Bar;
                          The_Extent    : Positive);

  procedure Define_Tic_Frequency (The_Track_Bar : Track_Bar;
                                  The_Frequency : Natural);

  procedure Set_Position (The_Track_Bar : Track_Bar;
                          The_Position  : Natural);

  function Get_Position (The_Track_Bar : Track_Bar) return Natural;

  ---------------------
  --
  -- Edit Box functions
  --
  ---------------------
  function Create (Parent_Page        : Page;
                   The_Title          : String;
                   Initial_Text       : String;
                   The_Action_Routine : Action_Routine := null;
                   Is_Password        : Boolean := False;
                   Is_Modifiable      : Boolean := True;
                   The_Size           : Natural := Automatic;
                   The_Title_Size     : Natural := Automatic) return Plain_Edit_Box;

  procedure Set_Text (The_Edit_Box : Edit_Box'class;
                      The_Text     : String);

  function Text_Size_Of (The_Text : String) return Natural;

  function Contents_Of (The_Edit_Box : Edit_Box'class) return String;

  function Create  (Parent_Page        : Page;
                    The_Title          : String;
                    Initial_Text       : String;
                    The_Action_Routine : Action_Routine := null;
                    Is_Password        : Boolean := False;
                    Is_Modifiable      : Boolean := True;
                    The_Size           : Natural := Automatic;
                    The_Title_Size     : Natural := Automatic) return Checked_Edit_Box;

  function Is_Checked (The_Checked_Edit_Box : Checked_Edit_Box) return Boolean;

  procedure Set (The_Checked_Edit_Box : Checked_Edit_Box);

  procedure Clear (The_Checked_Edit_Box : Checked_Edit_Box);

  ----------------------
  --
  -- Check Box functions
  --
  ----------------------
  function Create (Parent_Page        : Page;
                   The_Title          : String;
                   The_Action_Routine : Action_Routine := null) return Check_Box;

  function Is_Checked (The_Check_Box : Check_Box) return Boolean;

  procedure Set (The_Check_Box : Check_Box);

  procedure Clear (The_Check_Box : Check_Box);

  ----------------------
  --
  -- Combo Box functions
  --
  ----------------------
  type Combo_Styles is (Sorted,
                        Modifiable);

  type Combo_Style is array (Combo_Styles'range) of Boolean;

  Default_Combo_Style : constant Combo_Style := [Sorted => True, others => False];

  function Create  (Parent_Page        : Page;
                    The_Title          : String;
                    The_Action_Routine : Action_Routine := null;
                    The_Style          : Combo_Style := Default_Combo_Style;
                    The_Size           : Natural := Automatic;
                    The_Title_Size     : Natural := Automatic) return Plain_Combo_Box;

  procedure Add_Text (The_Combo_Box : Combo_Box'class;
                      The_Text      : String);

  procedure Insert_Text (The_Combo_Box : Combo_Box'class;
                         At_Index      : Natural;
                         The_Text      : String);

  procedure Remove_Text (The_Combo_Box : Combo_Box'class;
                         The_Text      : String);

  function Contents_Of (The_Combo_Box : Combo_Box'class) return String;

  procedure Clear_Contents_Of (The_Combo_Box : Combo_Box'class);

  procedure Select_First (The_Combo_Box : Combo_Box'class);

  procedure Select_Text (The_Combo_Box : Combo_Box'class;
                         The_Text      : String);

  procedure Deselect (The_Combo_Box : Combo_Box'class);

  function Create (Parent_Page        : Page;
                   The_Title          : String;
                   The_Action_Routine : Action_Routine := null;
                   The_Style          : Combo_Style := Default_Combo_Style;
                   The_Size           : Natural := Automatic;
                   The_Title_Size     : Natural := Automatic) return Checked_Combo_Box;

  function Is_Checked (The_Checked_Combo_Box : Checked_Combo_Box) return Boolean;

  procedure Set (The_Checked_Combo_Box : Checked_Combo_Box);

  procedure Clear (The_Checked_Combo_Box : Checked_Combo_Box);

private

  type Menu_Information;
  type Menu is access all Menu_Information;

  type Menu_Item_Information;
  type Menu_Item_Access is access Menu_Item_Information;

  type Menu_Item is abstract tagged record
    The_Menu_Item : Menu_Item_Access;
  end record;

  type Plain_Menu_Item is new Menu_Item with null record;

  type Selection_Menu_Item is new Menu_Item with null record;

  type Checked_Menu_Item is new Selection_Menu_Item with null record;

  type Radio_Menu_Item is new Selection_Menu_Item with null record;

  type Page_Information;
  type Page is access all Page_Information;

  type Child_Information;
  type Child_Information_Ptr is access all Child_Information;

  type Child is abstract tagged record
    Ptr : Child_Information_Ptr;
  end record;

  type Button is new Child with null record;
  type Progress_Bar is new Child with null record;
  type List_View is new Child with null record;
  type Tree_View is new Child with null record;
  type Text_View is new Child with null record;
  type Track_Bar is new Child with null record;
  type Edit_Box is abstract new Child with null record;
  type Check_Box is new Child with null record;
  type Combo_Box is abstract new Child with null record;

  type Checked_Edit_Box is new Edit_Box with null record;
  type Plain_Edit_Box is new Edit_Box with null record;

  type Checked_Combo_Box is new Combo_Box with null record;
  type Plain_Combo_Box is new Combo_Box with null record;

end Gui;
