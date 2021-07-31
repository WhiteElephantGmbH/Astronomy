-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Glib;
with Gdk.Color;
with Gdk.Types;
with Gtk.Button;
with Gtk.Check_Button;
with Gtk.Check_Menu_Item;
with Gtk.Combo_Box_Text;
with Gtk.GEntry;
with Gtk.List_Store;
with Gtk.Menu;
with Gtk.Menu_Item;
with Gtk.Progress_Bar;
with Gtk.Radio_Menu_Item;
with Gtk.Scale;
with Gtk.Separator;
with Gtk.Text_View;
with Gtk.Text_Buffer;
with Gtk.Tree_Model;
with Gtk.Tree_Store;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;
with Gtk.Widget;
with Gtk.Window;

package Gui is

  Sequence_Error : exception;

  Is_Gtk_Implementation : constant Boolean := True;

  type Color is (Black,  -- Note this list of colours can be extended with any X11 colour.
                 Blue,
                 Brown,
                 Cyan,
                 Gray,
                 Green,
                 Gold,
                 Goldenrod,
                 Honeydew,
                 Magenta,
                 Olive,
                 Orange,
                 Pink,
                 Red,
                 White,
                 Yellow);

  subtype RGB_Value is Natural range 0..255;

  type RGB is record
    Red   : RGB_Value;
    Green : RGB_Value;
    Blue  : RGB_Value;
  end record;

  function Convert (The_Color : Color) return RGB;

  Automatic : constant Natural := 0;

  Default_Position : constant := Integer'first;

  Default_Button_Width : constant := 100;

  type Information is new Glib.Gsize; -- Large enough to hold an address

  No_Information : constant Information := 0;

  type Information_Array is array (Natural range <>) of Information;

  type Window_Metrics is record
    X_Position : Integer := 0;
    Y_Position : Integer := 0;
    Width      : Natural := 0;
    Height     : Natural := 0;
  end record;

  type Action_Routine is access procedure;

  type Color_Handler is access function (For_Information : Information) return Color;

  type Text_Handler is access function (For_Column       : Natural;
                                        With_Information : Information) return String;

  type Click_Kind is (Double_Click, Single_Click);

  type Relation is (Less_Than, Equal_To, Greater_Than);

  type Sort_Routine is access function (For_Column         : Natural;
                                        First_Information  : Information;
                                        Second_Information : Information) return Relation;

  type Sort_Order is (Ascending, Descending);

  type Click_Routine is access procedure (Item : Information);

  type Justification is (Left, Center, Right);

  type Selection is (None, Single, Multiple);

  type Page_Styles is (Buttons_Fill_Horizontally,
                       Buttons_Fill_Vertically);

  type Page_Style is array (Page_Styles'range) of Boolean;

  Default_Page_Style : constant Page_Style := (others => False);

  type Amount is (First_Occurrence, All_Occurrences);

  type Page is private;
  type Child is abstract tagged private;
  type Button is new Child with private;
  type Progress_Bar is new Child with private;
  type List_View is new Child with private;
  type Tree_View is new Child with private;
  subtype Tree_Item is Gtk.Tree_Model.Gtk_Tree_Iter;

  type Text_View is new Child with private;
  type Track_Bar is new Child with private;
  type Edit_Box is abstract new Child with private;
  type Check_Box is new Child with private;
  type Combo_Box is abstract new Child with private;

  type Checked_Edit_Box is new Edit_Box with private;
  type Plain_Edit_Box is new Edit_Box with private;

  type Checked_Combo_Box is new Combo_Box with private;
  type Plain_Combo_Box is new Combo_Box with private;

  type Scale is new Child with private;

  type Menu is private;
  type Menu_Item is abstract tagged private;

  type Plain_Menu_Item is new Menu_Item with private;

  type Selection_Menu_Item is abstract new Menu_Item with private;
  procedure Set (The_Item : Selection_Menu_Item);
  function Is_Set (The_Item : Selection_Menu_Item) return Boolean;

  type Checked_Menu_Item is new Selection_Menu_Item with private;
  type Radio_Menu_Item is new Selection_Menu_Item with private;

  type Menu_Kind is (Plain, Checked, Radio);

  type Column is private;

  type Separator is new Child with private;

  procedure Message_Box (The_Message    : String;
                         Is_Exclamation : Boolean := True);
  --
  -- This procedure may be called before or instead of Execute
  -- (This is useful when the application cannot be started)
  --

  procedure Beep;
  -- Emit a beep.

  procedure Execute (Application_Name    : String;
                     Startup_Routine     : access procedure;
                     Termination_Routine : access procedure := null;
                     Initial_Metrics     : Window_Metrics;
                     Always_Topmost      : Boolean := False);
  -- Creates the main window for the application.
  -- Note that this should be called from the main process under OSX and that it does not return until the
  -- Gui is closed either by the user or calling the procedure Close.
  -- Code to setup the GUI interface should be placed into the Startup_Routine
  -- Code to extract the contents of Gui objects immediately before the gui is closed may be placed in the
  -- optional Termination routine

  function Get_Window_Metrics return Window_Metrics;

  function Application_Is_Minimized return Boolean;

  function Application_Is_Maximized return Boolean;

  function Text_Size_Of (The_Text : String) return Natural;
  --  Returns the width of the specified text in pixels.

  procedure Change_Application_Name (Name : String);
  -- Changes the application's name that is displayed as part of the main window's title

  function Name_Of_Application return String;

  procedure Clear_Focus;
  -- Unsets the focus for the window.

  procedure Show;
  -- Causes created GUI objects to become operative.
  -- Typically called after one or more objects have been created
  -- and the users wants them to become part of the GUI.
  -- This procedure made be called any number of times.

  procedure Close;

  function Add_Page (The_Title            : String;
                     The_Style            : Page_Style     := Default_Page_Style;
                     The_Action           : Action_Routine := null;
                     Minimum_Button_Width : Natural        := Default_Button_Width) return Page;
  -- Adds a page to the window. This page is then accessed by the user via a tab control


  procedure Select_Page (The_Page : Page);
  --
  -- Causes the specified page to become the current (displayed) page
  -- (same effect as if the user had selected the page using the tab control
  --

  function Add_Always_Displayed_Page (The_Style            : Page_Style := Default_Page_Style;
                                      Minimum_Button_Width : Natural    := Default_Button_Width) return Page;

  procedure Set_Status_Line (The_Text : String);

  procedure Set_Main_Window_Background_Color (The_Color : Color);

  function Is_Confirmed (The_Question : String) return Boolean;
  --
  -- Raises a pop-up asking the specified question.
  -- Two buttons are provided, Yes and No.
  -- The function reutns True if the Yes button is pressed
  --

  ---------------------------------------
  --
  -- Keyboard handling
  --
  ---------------------------------------

  type Key_Event is (Key_Pressed, Key_Released);

  type Key_Code is new Gdk.Types.Gdk_Key_Type;
  --  see Gui.Key_Codes for key type constants

  type Key_Handler is access procedure (The_Event   : Key_Event;
                                        The_Keycode : Key_Code);

  procedure Install_Key_Handler (The_Key_Handler : Key_Handler);
  -- Postcondition: Key handler is disabled.

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

  procedure Disable (The_Child : Child'class);

  procedure Enable (The_Child : Child'class);

  function Is_Enabled (The_Child : Child'class) return Boolean;

  procedure Hide (The_Child : Child'class);

  procedure Show (The_Child : Child'class);

  function Is_Hidden (The_Child : Child'class) return Boolean;

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

  function Add_Menu_Item (The_Text         :        String;
                          To_Menu          : in out Menu;
                          The_Menu_Handler : access procedure (Item : Information);
                          The_Information  :        Information := No_Information) return Radio_Menu_Item;
  -- Adds a radio item to the menu. By default the first item is set.
  -- This can be explicitly set using the procedure Set
  function Add_Menu_Item (The_Text :        String;
                          To_Menu  : in out Menu) return Radio_Menu_Item;

  procedure Enable_Menubar;

  procedure Disable_Menubar;

  procedure Enable (The_Menu : Menu);

  procedure Disable (The_Menu : Menu);

  procedure Enable (The_Menu_Item : Menu_Item'class);

  procedure Disable (The_Menu_Item : Menu_Item'class);

  overriding function Is_Set (The_Item : Checked_Menu_Item) return Boolean;

  overriding procedure Set (The_Item : Checked_Menu_Item);

  procedure Clear (The_Item : Checked_Menu_Item);

  overriding procedure Set (The_Item : Radio_Menu_Item);

  function Is_Set (The_Item : Radio_Menu_Item) return Boolean;

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

  function Create (Parent_Page        : Page;
                   Place_With_Buttons : Boolean := True;
                   Progress_Color     : Color := Blue) return Progress_Bar;
  -- Creates a progress bar with a range 0..100 incrementing in steps of 10
  -- Progress_Color defines the color used to show progress

  procedure Change_Color (The_Progress_Bar : Progress_Bar;
                          Progress_Color   : Color);
  -- Change the color used to show progress

  procedure Define_Range (The_Progress_Bar : in out Progress_Bar;
                          The_Extent       : Positive;
                          The_Step         : Positive := 1);

  procedure Report_Progress (The_Progress_Bar : in out Progress_Bar;
                             The_Progress     : Natural);

  procedure Increment_Progress (The_Progress_Bar : in out Progress_Bar);
  -- Increment the progress bar by a step


  ----------------------
  --
  -- List View functions
  --
  ----------------------

  function Create (Parent_Page           : Page;
                   The_Text_Handler      : not null access function (For_Column       : Natural;
                                                                     With_Information : Information) return String;
                   The_Color_Handler     : Color_Handler := null;
                   The_Sort_Routine      : Sort_Routine := null;
                   The_Click_Routine     : Click_Routine := null;
                   The_Click_Kind        : Click_Kind := Double_Click;
                   Use_Proportional_Font : Boolean := True;
                   Selection_Criteria    : Selection := None;
                   Color_Background      : Boolean := False) return List_View;
  --
  -- Note: The parameter Selection_Criteria is ignored if a Click_Routine is provided.
  --       The click routine can only provide a single selection so in this case the selection criteira must be Single
  -- Note: The_Color_Handler is only called when Data is added, NOT when the row is clicked.
  --

  procedure Freeze (The_List_View : List_View);
  --
  -- Freezes the LV. Added or removed data does not update the displayed LV until it is later thawed.
  --

  procedure Thaw (The_List_View : List_View);
  -- Undoes a previous Freeze;
  -- The LV is updated with any data changes made whilst it was frozen.

  procedure Add_Gridlines (The_List_View : List_View);

  procedure Remove_Gridlines (The_List_View : List_View);

  function Add_Column (The_List_View     : List_View;
                       The_Title         : String;
                       The_Width         : Natural := Automatic;
                       The_Justification : Justification := Left;
                       Is_Sortable       : Boolean := False) return Column;
  -- Sequence_Error will be raised if an attempt is made to add a column after
  -- any data has been added. At this point the number of columns is fixed.
  -- Columns made be add or removed by changing their visibility using Make_Column_Visible

  function Width_Of (The_Column : Column) return Natural;

  procedure Sort (The_Column    : Column;
                  The_Direction : Sort_Order);
  -- Note: Has no effect if called before any data is added to the associated list view or when it is frozen.

  procedure Set_Title (The_Column : Column;
                       The_Title  : String);

  procedure Add_Data (The_List_View   : in out List_View;
                      The_Information : Information);
  -- Appends data to the list_view

  procedure Add_To (The_List_View   : in out List_View;
                    The_Position    : Positive := Positive'first;
                    The_Information : Information);
  -- Adds data to the list view at the designated position.

  procedure Remove_From (The_List_View : List_View;
                         The_Position  : Positive := Positive'first;
                         The_Amount    : Positive := 1);

  procedure Remove_From (The_List_View   : List_View;
                         The_Information : Information;
                         The_Amount      : Amount := First_Occurrence);
  --
  -- Remove from the list view all rows that have the associated information
  -- The_Amount specifies how many rows should be removed - either the first occurrence or all occurrences
  --

  procedure Remove_All_From (The_List_View : List_View);

  procedure Notify_Item_Update (The_List_View   : List_View;
                                The_Information : Information);

  procedure Make_Column_Visible (The_Column : Column;
                                 Is_Visible : Boolean);

  function Visible_Region_Of (The_List_View : List_View) return Natural;
  -- returns the number of rows that could be shown.
  -- Ie the size of the visible region in rows.
  -- Zero is returned if the height of a row could not be determined (Empty List_View)

  procedure Make_Last_Row_Visible_In (The_List_View : List_View);

  function Selected_Information_From (The_List_View : List_View) return Information_Array;

  procedure Clear_Selection (The_List_View : List_View);

  ---------------------
  --
  -- Text View functions
  --
  ---------------------

  function Create (Parent_Page   : Page;
                   Word_Wrapping : Boolean := False) return Text_View;

  procedure Append_Line_To (The_Text_View  : Text_View;
                            The_Text       : String;
                            The_Color      : Color := Gui.Black;
                            Ensure_Visible : Boolean := True);

  procedure Clear (The_Text_View : Text_View);

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


  ---------------------
  --
  -- Edit Box functions
  --
  ---------------------
  function Create (Parent_Page        : Page;
                   The_Title          : String;
                   Initial_Text       : String := "";
                   The_Action_Routine : Action_Routine := null;
                   Is_Password        : Boolean := False;
                   Is_Modifiable      : Boolean := True;
                   The_Size           : Natural := Automatic;
                   The_Title_Size     : Natural := Automatic) return Plain_Edit_Box;

  function Contents_Of (The_Edit_Box : Edit_Box'class) return String;


  procedure Set_Text (The_Edit_Box : Edit_Box'class;
                      The_Text     : String);


  function Create  (Parent_Page        : Page;
                    The_Title          : String;
                    Initial_Text       : String := "";
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
  -- Combo Box functions
  --
  ----------------------

  function Create  (Parent_Page        : Page;
                    The_Title          : String;
                    The_Action_Routine : Action_Routine := null;
                    Is_Modifiable      : Boolean := False;
                    The_Size           : Natural := Automatic;
                    The_Title_Size     : Natural := Automatic) return Plain_Combo_Box;

  procedure Add_Text (The_Combo_Box : Combo_Box'class;
                      The_Text      : String);

  function Contents_Of (The_Combo_Box : Combo_Box'class) return String;

  procedure Clear_Contents_Of (The_Combo_Box : Combo_Box'class);

  procedure Select_First (The_Combo_Box : Combo_Box'class);

  procedure Select_Text (The_Combo_Box : Combo_Box'class;
                         The_Text      : String);
  -- If The_Text is null or not found in the combobox then the first item is selected.

  procedure Deselect (The_Combo_Box : Combo_Box'class);

  function Create (Parent_Page        : Page;
                   The_Title          : String;
                   The_Action_Routine : Action_Routine := null;
                   Is_Modifiable      : Boolean := False;
                   The_Size           : Natural := Automatic;
                   The_Title_Size     : Natural := Automatic) return Checked_Combo_Box;

  function Is_Checked (The_Checked_Combo_Box : Checked_Combo_Box) return Boolean;

  procedure Set (The_Checked_Combo_Box : Checked_Combo_Box);

  procedure Clear (The_Checked_Combo_Box : Checked_Combo_Box);

  ----------------------
  --
  -- Tree View functions
  --
  ----------------------
  Root_Item : constant Tree_Item := Gtk.Tree_Model.Null_Iter;

  type Tree_View_Style is record
    The_Click_Kind  : Click_Kind := Double_Click;
    Expand_By_Click : Boolean    := False;  -- When true, clicking the tree item will toggle the item expansion
  end record;

  function Create (Parent_Page       : Page;
                   The_Click_Routine : Click_Routine   := null;
                   The_Color_Handler : Color_Handler   := null;
                   The_Style         : Tree_View_Style := (others => <>)) return Tree_View;
  --
  -- Note: The_Color_Handler is only called when an item is added, NOT when the item is clicked.
  --

  function Add_Item (The_Tree_View     : Tree_View;
                     The_Title         : String;
                     The_Parent        : Tree_Item := Root_Item;
                     Position_At_Front : Boolean := False;  -- otherwise position at end
                     The_Information   : Information := No_Information) return Tree_Item;

  procedure Expand (The_Tree_View : Tree_View;
                    The_Tree_Item : Tree_Item;
                    Expand_All    : Boolean := False);

  procedure Delete_All_Items (The_Tree_View : Tree_View);


  -------------------
  --
  -- Scale functions
  --
  -------------------

  function Create (Parent_Page        : Page;
                   The_Title          : String;
                   The_Initial_Value  : Natural;
                   The_Minimum_Value  : Natural;
                   The_Maximum_Value  : Natural;
                   The_Action_Routine : Action_Routine := null;
                   The_Size           : Natural := Automatic;
                   The_Title_Size     : Natural := Automatic;
                   Show_Marks         : Boolean := False) return Scale;

  procedure Set (The_Scale : Scale;
                 The_Value : Natural);

  function Get (The_Scale : Scale) return Natural;


  ----------------------
  --
  -- Separator function
  --
  ----------------------

  function Create (Parent_Page : Page;
                   Padding     : Natural := Automatic) return Separator;


private

  function Main_Window return Gtk.Window.Gtk_Window;

  procedure Add_Paned_Widget (The_Page   : Page;
                              The_Widget : Gtk.Widget.Gtk_Widget);

  function Convert (The_Color : Color) return Gdk.Color.Gdk_Color;

  type Page_Information;
  type Page is access Page_Information;

  type Child is abstract tagged record
    Widget : Gtk.Widget.Gtk_Widget := null;
  end record;

  type Menu is record
    Widget    : Gtk.Widget.Gtk_Widget;
    The_Item  : Gtk.Menu_Item.Gtk_Menu_Item;
    The_Menu  : Gtk.Menu.Gtk_Menu;
    The_Group : Gtk.Widget.Widget_SList.GSlist;
  end record;

  type Menu_Item is abstract tagged record
    Widget : Gtk.Widget.Gtk_Widget;
  end record;

  type Plain_Menu_Item is new Menu_Item with record
    The_Item : Gtk.Menu_Item.Gtk_Menu_Item;
  end record;

  type Selection_Menu_Item is new Menu_Item with null record;

  type Checked_Menu_Item is new Selection_Menu_Item with record
    The_Item : Gtk.Check_Menu_Item.Gtk_Check_Menu_Item;
  end record;

  type Radio_Menu_Item is new Selection_Menu_Item with record
    The_Item : Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item;
  end record;

  type Button is new Child with record
    The_Button : Gtk.Button.Gtk_Button;
  end record;

  type Progress_Bar is new Child with record
    The_Bar      : Gtk.Progress_Bar.Gtk_Progress_Bar;
    The_Extent   : Glib.Gdouble;
    The_Step     : Glib.Gdouble;
    The_Position : Glib.Gdouble;
  end record;

  type List_View is new Child with record
    The_View          : Gtk.Tree_View.Gtk_Tree_View;
    The_Store         : Gtk.List_Store.Gtk_List_Store;
    The_Text_Handler  : Text_Handler;
    The_Color_Handler : Color_Handler;
    The_Sort_Routine  : Sort_Routine;
    Color_Background  : Boolean;
  end record;

  type Tree_View is new Child with record
    The_View          : Gtk.Tree_View.Gtk_Tree_View;
    The_Store         : Gtk.Tree_Store.Gtk_Tree_Store;
    The_Color_Handler : Color_Handler;
  end record;

  type Text_View is new Child with record
    The_View   : Gtk.Text_View.Gtk_Text_View;
    The_Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
  end record;

  type Track_Bar is new Child with null record;

  type Edit_Box is abstract new Child with record
    The_Box : Gtk.GEntry.Gtk_Entry;
  end record;

  type Scale is new Child with record
    The_Scale : Gtk.Scale.Gtk_Scale;
  end record;

  type Check_Box is new Child with record
    The_Box : Gtk.Check_Button.Gtk_Check_Button;
  end record;

  type Combo_Box is abstract new Child with record
    The_Box : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
  end record;

  type Plain_Edit_Box is new Edit_Box with null record;
  type Checked_Edit_Box is new Edit_Box with record
    The_Check_Box : Gtk.Check_Button.Gtk_Check_Button;
  end record;

  type Plain_Combo_Box is new Combo_Box with null record;
  type Checked_Combo_Box is new Combo_Box with record
    The_Check_Box : Gtk.Check_Button.Gtk_Check_Button;
  end record;

  type Column is record
    The_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
    Column_Nr  : Glib.Gint;
    The_View   : Gtk.Tree_View.Gtk_Tree_View;
  end record;

  type Separator is new Child with record
    The_Separator : Gtk.Separator.Gtk_Separator;
  end record;

end Gui;
