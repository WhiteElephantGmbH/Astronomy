-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Definite_Doubly_Linked_Lists;
with Gdk.RGBA;
with Gdk.Event;
with Gdk.Rectangle;
with Gdk.Screen;
with Gdk.Main;
with Glib.Values;
with Gtk.Arguments;
with Gtk.Bin;
with Gtk.Box;
with Gtk.Cell_Renderer_Text;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Main;
with Gtk.Menu_Bar;
with Gtk.Misc;
with Gtk.Notebook;
with Gtk.Paned;
with Gtk.Scrolled_Window;
with Gtk.Separator_Menu_Item;
with Gtk.Status_Bar;
with Gtk.Text_Iter;
with Gtk.Text_Tag;
with Gtk.Text_Tag_Table;
with Gtk.Tree_Sortable;
with Gtk.Tree_Selection;
with Gtkada.Dialogs;
with Gui.Router;
with Os;
with Pango.Font;
with Pango.Layout;
with Text;

package body Gui is

  pragma Warnings ("J");  -- Gtk.Misc.Set_Alignment is deprecated in v3.14
                          -- However its functional equivalent Set_Xalign is not availabel until 3.15!
                          -- Note: Set_Halign sets the alignment of the widget *NOT* the text within it!

  Unknown_Color : exception;

  package Information_List is new Definite_Doubly_Linked_Lists (Information);

  type Page_Information is record
    Minimum_Button_Width : Natural := Default_Button_Width;
    The_Style            : Page_Style;
    The_Action           : Action_Routine := null;  -- Called when page becomes the current page
    The_Button_Box       : Gtk.Box.Gtk_Box;
    Nr_Of_Buttons        : Natural := 0;
    Paned_Window         : Gtk.Paned.Gtk_Vpaned;
    Nr_Of_Panes          : Natural := 0;
    Paned_Handler_Id     : Gtk.Handlers.Handler_Id;
    Page_Box             : Gtk.Box.Gtk_Box;
  end record;

  package Page_List is new Definite_Doubly_Linked_Lists (Page);

  The_Main_Window : Gtk.Window.Gtk_Window := null;
  The_Notebook    : Gtk.Notebook.Gtk_Notebook;
  Main_Box        : Gtk.Box.Gtk_Box;
  Status_Box      : Gtk.Box.Gtk_Box;
  Status_Line     : Gtk.Status_Bar.Gtk_Status_Bar;
  The_Menu_Bar    : Gtk.Menu_Bar.Gtk_Menu_Bar := null;

  The_Pages : Page_List.Item;

  Window_Is_Minimized : Boolean := False;
  Window_Is_Maximized : Boolean := False;

  Colors : array (Color) of Gdk.RGBA.Gdk_RGBA;

  Color_Text_Tags : array (Color) of Gtk.Text_Tag.Gtk_Text_Tag;

  Default_RGB_For : constant array (Color) of String (1..7)
                  := (Black   => "#000000",
                      Blue    => "#0000FF",
                      Brown   => "#A52A2A",
                      Cyan    => "#00FFFF",
                      Gray    => "#808080",
                      Green   => "#008000",
                      Gold    => "#FFD700",
                      Magenta => "#FF00FF",
                      Olive   => "#808000",
                      Orange  => "#FFA500",
                      Red     => "#FF0000",
                      White   => "#FFFFFF",
                      Yellow  => "#FFFF00");


  procedure Define_Color_Tags is
    Success  : Boolean;
  begin
    for The_Color in Color'range loop
      Gdk.RGBA.Parse (Colors(The_Color), The_Color'img, Success);
      if not Success then -- Colour unknown by name - use default defin-tion
        Gdk.RGBA.Parse (Colors(The_Color), Default_RGB_For (The_Color), Success);
      end if;
      if Success then
        Gtk.Text_Tag.Gtk_New (Color_Text_Tags (The_Color));
        Gdk.RGBA.Set_Property (Color_Text_Tags (The_Color),
                               Gtk.Text_Tag.Foreground_Rgba_Property,
                               Colors(The_Color));
      else
        raise Unknown_Color; -- Default definition must have wrong syntax
      end if;
    end loop;
  end Define_Color_Tags;


  function Convert (The_Color : Color) return RGB is
    use type Glib.Gdouble;
  begin
    return (Red   => RGB_Value(Colors (The_Color).Red   * 255.0),
            Green => RGB_Value(Colors (The_Color).Green * 255.0),
            Blue  => RGB_Value(Colors (The_Color).Blue  * 255.0));
  end Convert;


  function Convert (The_Color : Glib.Gdouble) return Glib.Guint16 is
    use type Glib.Gdouble;
  begin
    if The_Color <= 0.0 then
      return 0;
    elsif The_Color >= 1.0 then
      return Glib.Guint16'last;
    else
      return Glib.Guint16 (The_Color * Glib.Gdouble (Glib.Guint16'last));
    end if;
  end Convert;


  function Convert (The_Color : Color) return Gdk.Color.Gdk_Color is
    The_Result : Gdk.Color.Gdk_Color;
  begin
    Gdk.Color.Set_Rgb (Color => The_Result,
                       Red   => Convert (Colors (The_Color).Red),
                       Green => Convert (Colors (The_Color).Green),
                       Blue  => Convert (Colors (The_Color).Blue));
    return The_Result;
  end Convert;


  function Main_Window return Gtk.Window.Gtk_Window is
  begin
    return The_Main_Window;
  end Main_Window;


  type Message_Box_Data (Message_Size : Natural) is new Gui.Router.Request_Data with record
    Severity    : Gtkada.Dialogs.Message_Dialog_Type;
    Buttons     : Gtkada.Dialogs.Message_Dialog_Buttons;
    The_Message : String (1..Message_Size);
  end record;

  procedure Synchronous_Service (Data : in out Message_Box_Data) is
  begin
    Data.Buttons := Gtkada.Dialogs.Message_Dialog (Msg         => Data.The_Message,
                                                   Dialog_Type => Data.Severity,
                                                   Buttons     => Data.Buttons,
                                                   Parent      => The_Main_Window);
  end Synchronous_Service;

  procedure Message_Box (The_Message    : String;
                         Is_Exclamation : Boolean := True) is
    Data : Message_Box_Data
         := (Gui.Router.Request_Data with Message_Size => The_Message'length,
                                          The_Message  => The_Message,
                                          Buttons      => Gtkada.Dialogs.Button_OK,
                                          Severity     => Gtkada.Dialogs.Warning);
    use type Gtk.Window.Gtk_Window;
  begin
    if Is_Exclamation then
      Data.Severity := Gtkada.Dialogs.Error;
    end if;
    if The_Main_Window = null then -- Main window hasn't been created yet.
      Synchronous_Service (Data);
    else
      Gui.Router.Request (Data);
    end if;
  end Message_Box;


  type Beep_Data is new Gui.Router.Message_Data with null record;

  overriding
  procedure Asynchronous_Service (Unused_Data : Beep_Data) is
  begin
    Gdk.Main.Beep;
  end Asynchronous_Service;


  procedure Beep is
    Data : constant Beep_Data := (null record);
    use type Gtk.Window.Gtk_Window;
  begin
    if The_Main_Window = null then -- Main window hasn't been created yet.
      Asynchronous_Service (Data);
    else
      Gui.Router.Send (Data);
    end if;
  end Beep;


  function Is_Confirmed (The_Question : String) return Boolean is
    use type Gtkada.Dialogs.Message_Dialog_Buttons;
    Data : Message_Box_Data
         := (Gui.Router.Request_Data with Message_Size => The_Question'length,
                                          The_Message  => The_Question,
                                          Buttons      => (Gtkada.Dialogs.Button_Yes or Gtkada.Dialogs.Button_No),
                                          Severity     => Gtkada.Dialogs.Confirmation);
  begin
    Gui.Router.Request (Data);
    return Data.Buttons = Gtkada.Dialogs.Button_Yes;
  end Is_Confirmed;

  package Window_Callback is new Gtk.Handlers.Return_Callback (Widget_Type => Gtk.Window.Gtk_Window_Record,
                                                               Return_Type => Boolean);

  function Window_State_Event (Unused : access Gtk.Window.Gtk_Window_Record'class;
                               Event  : Gdk.Event.Gdk_Event) return Boolean is
    use type Gdk.Event.Gdk_Event_Type;
    use type Gdk.Event.Gdk_Window_State;
  begin
    if Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Window_State then
      Window_Is_Minimized := (Event.Window_State.New_Window_State and Gdk.Event.Window_State_Iconified) /= 0;
      Window_Is_Maximized := (Event.Window_State.New_Window_State and Gdk.Event.Window_State_Maximized) /= 0;
    end if;
    return False;
  end Window_State_Event;


  type Execute_Data is new Gui.Router.Request_Data with record
    Keep_Above    : Boolean;
    X_Position    : Glib.Gint;
    Y_Position    : Glib.Gint;
    Window_Width  : Glib.Gint;
    Window_Height : Glib.Gint;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Execute_Data) is
    Default : constant Glib.Gint := Glib.Gint(Default_Position);
    The_Screen : Gdk.Screen.Gdk_Screen;
    use type Glib.Gint;
  begin
    Gtk.Box.Gtk_New_Vbox (Main_Box);
    The_Main_Window.Add (Main_Box);

    Gtk.Box.Gtk_New_Hbox (Status_Box);
    Gtk.Status_Bar.Gtk_New (Status_Line);
    Status_Box.Pack_Start (Status_Line, True, True, 0);
    Main_Box.Pack_End (Status_Box, False, True, 0);
    Main_Box.Show_All;
    if Data.Keep_Above then
      The_Main_Window.Set_Keep_Above (True);
    end if;

    if Data.Window_Width = 0 then
      Data.Window_Width := -1;
    end if;
    if Data.Window_Height = 0 then
      Data.Window_Height := -1;
    end if;
    The_Main_Window.Set_Default_Size (Width  => Data.Window_Width,
                                      Height => Data.Window_Height);

    The_Screen := Gdk.Screen.Get_Default;
    if (Data.X_Position /= Default) and (Data.Y_Position /= Default) then
      declare
        Minimum_Visibility : constant Glib.Gint := 100;  -- Minimum amount that must be seen on screen
        Screen_Width  : constant Glib.Gint := The_Screen.Get_Width;
        Screen_Height : constant Glib.Gint := The_Screen.Get_Width;
      begin
        if Data.X_Position + Minimum_Visibility > Screen_Width then
          Data.X_Position := Screen_Width - Minimum_Visibility;
        end if;
        if Data.Y_Position + Minimum_Visibility > Screen_Height then
          Data.Y_Position := Screen_Height - Minimum_Visibility;
        end if;
        The_Main_Window.Move (Data.X_Position, Data.Y_Position);
      end;
    end if;

    Window_Callback.Connect (The_Main_Window,
                             "window-state-event",
                             Window_Callback.To_Marshaller (Window_Callback.Event_Marshaller.Handler'(
                                                            Window_State_Event'access)));

    The_Main_Window.Show;
  end Synchronous_Service;


  procedure Execute (Application_Name    : String;
                     Startup_Routine     : access procedure;
                     Termination_Routine : access procedure := null;
                     Initial_Metrics     : Window_Metrics;
                     Always_Topmost      : Boolean := False) is

    procedure Execute (Window : Gtk.Window.Gtk_Window) is
      Data : Execute_Data := (Gui.Router.Request_Data with Keep_Above    => Always_Topmost,
                                                           X_Position    => Glib.Gint(Initial_Metrics.X_Position),
                                                           Y_Position    => Glib.Gint(Initial_Metrics.Y_Position),
                                                           Window_Width  => Glib.Gint(Initial_Metrics.Width),
                                                           Window_Height => Glib.Gint(Initial_Metrics.Height));
    begin
      The_Main_Window := Window;
      Gui.Router.Request (Data);
      if Startup_Routine /= null then
        Startup_Routine.all;
      end if;
    end Execute;

  begin
    Gui.Router.Execute (Application_Name, Execute'access, Termination_Routine, Period => 0.05);
  end Execute;


  function Get_Window_Metrics return Window_Metrics is
  begin
    return Gui.Router.Get_Window_Metrics (The_Main_Window);
  end Get_Window_Metrics;


  function Application_Is_Minimized return Boolean is
  begin
    return Window_Is_Minimized;
  end Application_Is_Minimized;

  function Application_Is_Maximized return Boolean is
  begin
    return Window_Is_Maximized;
  end Application_Is_Maximized;


  type Text_Size_Data (Text_Size : Natural) is new Gui.Router.Request_Data with record
    The_Text  : String (1..Text_Size);
    The_Width : Glib.Gint;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Text_Size_Data) is
    Unused_Height : Glib.Gint;
    The_Layout    : Pango.Layout.Pango_Layout;
  begin
    Pango.Layout.Gdk_New (The_Layout, Gtk.Widget.Get_Pango_Context (Gtk.Widget.Gtk_Widget(The_Main_Window)));
    The_Layout.Set_Text (Data.The_Text);
    The_Layout.Get_Pixel_Size (Data.The_Width, Unused_Height);
  end Synchronous_Service;

  function Text_Size_Of (The_Text : String) return Natural is
    Data : Text_Size_Data := (Gui.Router.Request_Data with Text_Size => The_Text'length,
                                                           The_Text  => The_Text,
                                                           The_Width => 0);
  begin
    Gui.Router.Request (Data);
    return Natural (Data.The_Width);
  end Text_Size_Of;


  type Change_Title_Data (Title_Size : Natural) is new Gui.Router.Message_Data with record
    Title : String (1..Title_Size);
  end record;

  overriding
  procedure Asynchronous_Service (Data : Change_Title_Data) is
  begin
    Gtk.Window.Set_Title (The_Main_Window, Data.Title);
  end Asynchronous_Service;

  procedure Change_Application_Name (Name : String) is
    Data : constant Change_Title_Data := (Title_Size => Name'length,
                                          Title      => Name);
  begin
    Gui.Router.Send (Data);
  end Change_Application_Name;


  type Get_Title_Data is new Gui.Router.Request_Data with record
    Title : Text.String;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Get_Title_Data) is
  begin
    Data.Title := Text.String_Of (The_Main_Window.Get_Title);
  end Synchronous_Service;

  function Name_Of_Application return String is
    Data : Get_Title_Data;
  begin
    Gui.Router.Request (Data);
    return Text.String_Of (Data.Title);
  end Name_Of_Application;

  type Clear_Focus_Data is new Gui.Router.Message_Data with null record;

  overriding
  procedure Asynchronous_Service (Unused : Clear_Focus_Data) is
  begin
    The_Main_Window.Set_Focus (null);
  end Asynchronous_Service;

  procedure Clear_Focus is
    Data : constant Clear_Focus_Data := (null record);
  begin
    Gui.Router.Send (Data);
  end Clear_Focus;


  type Show_Data is new Gui.Router.Request_Data with null record;

  overriding
  procedure Synchronous_Service (Unused_Data : in out Show_Data) is
  begin
    Main_Box.Show_All;
  end Synchronous_Service;

  procedure Show is
    Data : Show_Data;
  begin
    Gui.Router.Request (Data);
  end Show;


  procedure Close is
  begin
    Gui.Router.Close (The_Main_Window);
  end Close;


  type Status_Line_Data (Text_Size : Natural) is new Gui.Router.Message_Data with record
    The_Text : String (1..Text_Size);
  end record;

  overriding
  procedure Asynchronous_Service (Data : Status_Line_Data) is
    Unused_Id : Gtk.Status_Bar.Message_Id;
  begin
    Unused_Id := Status_Line.Push (1, Data.The_Text);
  end Asynchronous_Service;

  procedure Set_Status_Line (The_Text : String) is
    Data : constant Status_Line_Data := (Text_Size => The_Text'length,
                                         The_Text  => The_Text);
  begin
    Gui.Router.Send (Data);
  end Set_Status_Line;


  ---------------------------------------
  --
  -- Page handling
  --
  ---------------------------------------

  package Notebook_Cb is new Gtk.Handlers.Callback (Gtk.Notebook.Gtk_Notebook_Record);

  procedure Page_Switch (Unused : access Gtk.Notebook.Gtk_Notebook_Record'class;
                         Params : Gtk.Arguments.Gtk_Args) is
    New_Page_Nr : constant Glib.Guint := (Gtk.Arguments.To_Guint (Params, 2));
    Page_Nr     : Glib.Guint := 0;
    use type Glib.Guint;
  begin
    for The_Page of The_Pages loop  -- Search for page
      if Page_Nr = New_Page_Nr then
        if The_Page.The_Action /= null then -- It has an action routine
          Gui.Router.Execute (The_Page.The_Action); -- So call it
        end if;
        return;
      end if;
      Page_Nr := Page_Nr + 1;
    end loop;
  end Page_Switch;

  type Page_Data (Title_Size : Natural) is new Gui.Router.Request_Data with record
    The_Page  : Page;
    The_Title : String (1..Title_Size);
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Page_Data) is
    The_Label : Gtk.Label.Gtk_Label;
  begin
    Gtk.Box.Gtk_New_Vbox (Data.The_Page.Page_Box);
    Gtk.Label.Gtk_New (The_Label, Data.The_Title);
    if The_Pages.Is_Empty then -- This is the first page
      Gtk.Notebook.Gtk_New (The_Notebook);  -- So create the note book
      The_Notebook.Append_Page (Data.The_Page.Page_Box, The_Label);
      The_Notebook.Set_Show_Tabs (False);
      Main_Box.Pack_Start (The_Notebook, True, True, 0);
      Notebook_Cb.Connect (The_Notebook, "switch_page", Page_Switch'access);
    else -- Add it to the notebook and display the tabs
      The_Notebook.Append_Page (Data.The_Page.Page_Box, The_Label);
      The_Notebook.Set_Show_Tabs (True);
    end if;
  end Synchronous_Service;

  function Add_Page (The_Title            : String;
                     The_Style            : Page_Style     := Default_Page_Style;
                     The_Action           : Action_Routine := null;
                     Minimum_Button_Width : Natural        := Default_Button_Width) return Page is
    The_Page : constant Page := new Page_Information;
    Data     : Page_Data := (Gui.Router.Request_Data with Title_Size => The_Title'length,
                                                          The_Page   => The_Page,
                                                          The_Title  => The_Title);
  begin
    Gui.Router.Request (Data);
    The_Page.The_Style := The_Style;
    The_Page.The_Action := The_Action;
    The_Page.Minimum_Button_Width := Minimum_Button_Width;
    The_Pages.Append (The_Page);
    return The_Page;
  end Add_Page;


  type Select_Page_Data is new Gui.Router.Message_Data with record
    Page_Box : Gtk.Box.Gtk_Box;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Select_Page_Data) is
  begin
    The_Notebook.Set_Current_Page (Gtk.Notebook.Page_Num (The_Notebook, Data.Page_Box));
  end Asynchronous_Service;


  procedure Select_Page (The_Page : Page) is
    Data : constant Select_Page_Data := (Page_Box => The_Page.Page_Box);
  begin
    Gui.Router.Send (Data);
  end Select_Page;



  ---------------------------------------
  --
  -- Keyboard handling
  --
  ---------------------------------------

  type Key_Action is record
    Handler         : Key_Handler;
    Key_Is_Pressed  : Boolean;
    The_Key_Pressed : Key_Code;
  end record;

  type Key_Action_Access is access Key_Action;

  Key_Handling_Is_Enabled : Boolean := False;


  function Key_Press_Event (Unused     : access Gtk.Window.Gtk_Window_Record'class;
                            The_Event  : Gdk.Event.Gdk_Event;
                            The_Action : Key_Action_Access) return Boolean is
    Event_Type : constant Gdk.Event.Gdk_Event_Type := Gdk.Event.Get_Event_Type (The_Event);
    The_Key    : constant Key_Code := Key_Code(Gdk.Event.Get_Key_Val (The_Event));
  begin
    if Key_Handling_Is_Enabled then
      case Event_Type is
      when Gdk.Event.Key_Press =>
        if (not The_Action.Key_Is_Pressed) or (The_Key /= The_Action.The_Key_Pressed) then
          The_Action.Key_Is_Pressed := True;
          The_Action.The_Key_Pressed := The_Key;
          Gui.Router.Execute (The_Action.Handler, Key_Pressed, The_Key);
        end if;
        return True;
      when Gdk.Event.Key_Release =>
        The_Action.Key_Is_Pressed := False;
        Gui.Router.Execute (The_Action.Handler, Key_Released, The_Key);
        return True;
      when others =>
        null;
      end case;
    end if;
    return False;
  end Key_Press_Event;


  package Key_Event_Callback is new Gtk.Handlers.User_Return_Callback (Widget_Type => Gtk.Window.Gtk_Window_Record,
                                                                       Return_Type => Boolean,
                                                                       User_Type   => Key_Action_Access);

  type Key_Handler_Data is new Gui.Router.Message_Data with record
    Handler : Key_Handler;
  end record;


  overriding
  procedure Asynchronous_Service (Data : Key_Handler_Data) is

    The_Action : constant Key_Action_Access := new Key_Action'(Handler         => Data.Handler,
                                                               Key_Is_Pressed  => False,
                                                               The_Key_Pressed => 0);
  begin
    Key_Event_Callback.Connect (The_Main_Window,
                                "key_press_event",
                                Key_Event_Callback.To_Marshaller (Key_Event_Callback.Event_Marshaller.Handler'(
                                                                  Key_Press_Event'access)),
                                User_Data => The_Action);

    Key_Event_Callback.Connect (The_Main_Window,
                                "key_release_event",
                                Key_Event_Callback.To_Marshaller (Key_Event_Callback.Event_Marshaller.Handler'(
                                                                  Key_Press_Event'access)),
                                User_Data => The_Action);
  end Asynchronous_Service;


  procedure Install_Key_Handler (The_Key_Handler : Key_Handler) is
    Data : constant Key_Handler_Data := (Handler => The_Key_Handler);
  begin
    Gui.Router.Send (Data);
  end Install_Key_Handler;


  procedure Disable_Key_Handler is
  begin
    Key_Handling_Is_Enabled := False;
  end Disable_Key_Handler;


  procedure Enable_Key_Handler is
  begin
    Key_Handling_Is_Enabled := True;
  end Enable_Key_Handler;



  ---------------------------------------
  --
  -- Functions available for all controls
  --
  ---------------------------------------

  package Action_Callback is new Gtk.Handlers.User_Callback (Widget_Type => Gtk.Widget.Gtk_Widget_Record,
                                                             User_Type   => Action_Routine);

  procedure Action_Handler (Unused             : access Gtk.Widget.Gtk_Widget_Record'class;
                            The_Action_Routine : Action_Routine) is
  begin
    Gui.Router.Execute (The_Action_Routine);
  end Action_Handler;


  package Event_Callback is new Gtk.Handlers.User_Return_Callback (Widget_Type => Gtk.Widget.Gtk_Widget_Record,
                                                                   User_Type   => Action_Routine,
                                                                   Return_Type => Boolean);

  function Event_Handler (Unused             : access Gtk.Widget.Gtk_Widget_Record'class;
                          Unused_Event       : Gdk.Event.Gdk_Event;
                          The_Action_Routine : Action_Routine) return Boolean is
  begin
    Gui.Router.Execute (The_Action_Routine);
    return False;
  end Event_Handler;




  function Is_Defined (The_Child : Child'class) return Boolean is
    use type Gtk.Widget.Gtk_Widget;
  begin
    return The_Child.Widget /= null;
  end Is_Defined;


  type Sensitivity_Data is new Gui.Router.Message_Data with record
    The_Widget   : Gtk.Widget.Gtk_Widget;
    Is_Sensitive : Boolean;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Sensitivity_Data) is
  begin
    Data.The_Widget.Set_Sensitive (Sensitive => Data.Is_Sensitive);
  end Asynchronous_Service;

  procedure Disable (The_Child : Child'class) is
    Data : constant Sensitivity_Data := (The_Widget   => The_Child.Widget,
                                         Is_Sensitive => False);
  begin
    Gui.Router.Send (Data);
  end Disable;

  procedure Enable (The_Child : Child'class) is
    Data : constant Sensitivity_Data := (The_Widget   => The_Child.Widget,
                                         Is_Sensitive => True);
  begin
    Gui.Router.Send (Data);
  end Enable;


  type Set_Visibility_Data is new Gui.Router.Message_Data with record
    The_Widget : Gtk.Widget.Gtk_Widget;
    Is_Visible : Boolean;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Set_Visibility_Data) is
  begin
    Data.The_Widget.Set_Visible (Visible => Data.Is_Visible);
  end Asynchronous_Service;

  procedure Hide (The_Child : Child'class) is
    Data : constant Set_Visibility_Data := (The_Widget => The_Child.Widget,
                                            Is_Visible => False);
  begin
    Gui.Router.Send (Data);
  end Hide;

  procedure Show (The_Child : Child'class) is
    Data : constant Set_Visibility_Data := (The_Widget => The_Child.Widget,
                                            Is_Visible => True);
  begin
    Gui.Router.Send (Data);
  end Show;


  type Get_Widget_Data is new Gui.Router.Request_Data with record
    The_Widget   : Gtk.Widget.Gtk_Widget;
    Is_Visible   : Boolean;
    Is_Sensitive : Boolean;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Get_Widget_Data) is
  begin
    Data.Is_Visible := Data.The_Widget.Get_Visible;
    Data.Is_Sensitive := Data.The_Widget.Get_Sensitive;
  end Synchronous_Service;

  function Is_Enabled (The_Child : Child'class) return Boolean is
    Data : Get_Widget_Data
         := (Gui.Router.Request_Data with The_Widget   => The_Child.Widget,
                                          Is_Visible   => False,
                                          Is_Sensitive => False);
  begin
    Gui.Router.Request (Data);
    return Data.Is_Sensitive;
  end Is_Enabled;

  function Is_Hidden (The_Child : Child'class) return Boolean is
    Data : Get_Widget_Data
         := (Gui.Router.Request_Data with The_Widget   => The_Child.Widget,
                                          Is_Visible   => False,
                                          Is_Sensitive => False);
  begin
    Gui.Router.Request (Data);
    return Data.Is_Visible;
  end Is_Hidden;


  ---------------------------------------
  --
  -- Menu handling
  --
  ---------------------------------------

  type Add_Menu_Data (Label_Size : Natural) is new Gui.Router.Request_Data with record
    Parent : Gtk.Menu.Gtk_Menu;
    Menu   : Gtk.Menu.Gtk_Menu;
    Item   : Gtk.Menu_Item.Gtk_Menu_Item;
    Label  : String (1..Label_Size);
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Add_Menu_Data) is
    use type Gtk.Menu_Bar.Gtk_Menu_Bar;
    use type Gtk.Menu.Gtk_Menu;
  begin
    if (Data.Parent = null) and (The_Menu_Bar = null) then
      Gtk.Menu_Bar.Gtk_New (The_Menu_Bar);
      Main_Box.Pack_Start (The_Menu_Bar, False, True, 0);
      Main_Box.Reorder_Child (The_Menu_Bar, 0); -- Position at top
    end if;
    Gtk.Menu.Gtk_New (Data.Menu);
    Gtk.Menu_Item.Gtk_New_With_Label (Data.Item, Data.Label);
    Data.Item.Set_Submenu (Data.Menu);
    if Data.Parent = null then
      The_Menu_Bar.Append (Data.Item);
    else
      Data.Parent.Append (Data.Item);
    end if;
  end Synchronous_Service;

  function Add_Menu (The_Text : String) return Menu is
    Data : Add_Menu_Data
         := (Gui.Router.Request_Data with Label_Size => The_Text'length,
                                          Parent     => null,
                                          Menu       => null,  -- returned by call
                                          Item       => null,  -- returned by call
                                          Label      => The_Text);
  begin
    Gui.Router.Request (Data);
    return (Widget    => Gtk.Widget.Gtk_Widget (Data.Item),
            The_Item  => Data.Item,
            The_Menu  => Data.Menu,
            The_Group => Gtk.Widget.Widget_SList.Null_List);
  end Add_Menu;


  function Add_Menu (The_Text : String;
                     To_Menu  : Menu) return Menu is
    Data : Add_Menu_Data
         := (Gui.Router.Request_Data with Label_Size => The_Text'length,
                                          Parent     => To_Menu.The_Menu,
                                          Menu       => null,  -- returned by call
                                          Item       => null,  -- returned by call
                                          Label      => The_Text);
  begin
    Gui.Router.Request (Data);
    return (Widget    => Gtk.Widget.Gtk_Widget (Data.Item),
            The_Item  => Data.Item,
            The_Menu  => Data.Menu,
            The_Group => Gtk.Widget.Widget_SList.Null_List);
  end Add_Menu;


  type Add_Separator_Data is new Gui.Router.Message_Data with record
    Menu : Gtk.Menu.Gtk_Menu;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Add_Separator_Data) is
    Separator : Gtk.Separator_Menu_Item.Gtk_Separator_Menu_Item;
  begin
    Gtk.Separator_Menu_Item.Gtk_New (Separator);
    Data.Menu.Append (Separator);
  end Asynchronous_Service;

  procedure Add_Menu_Separator (To_Menu : Menu) is
    Data : constant Add_Separator_Data := (Menu => To_Menu.The_Menu);
  begin
    Gui.Router.Send (Data);
  end Add_Menu_Separator;


  type Menu_Item_Data is record
    Routine : Click_Routine;
    Info    : Information;
  end record;

  type Menu_Item_Data_Access is access Menu_Item_Data;

  package Menu_Item_Callback is new Gtk.Handlers.User_Callback (Gtk.Widget.Gtk_Widget_Record, Menu_Item_Data_Access);

  procedure Menu_Item_Click_Handler (Unused_Widget : access Gtk.Widget.Gtk_Widget_Record'class;
                                     Data          : Menu_Item_Data_Access) is
  begin
    Gui.Router.Execute (Data.Routine, Data.Info);
  end Menu_Item_Click_Handler;

  procedure Radio_Menu_Item_Click_Handler (The_Widget : access Gtk.Widget.Gtk_Widget_Record'class;
                                           Data       : Menu_Item_Data_Access) is
  begin
    if Gtk.Radio_Menu_Item.Get_Active (Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item (The_Widget)) then
      Gui.Router.Execute (Data.Routine, Data.Info);
    end if;
  end Radio_Menu_Item_Click_Handler;


  type Add_Checked_Menu_Item_Data (Label_Size : Natural) is new Gui.Router.Request_Data with record
    Menu      : Gtk.Menu.Gtk_Menu;
    Item      : Gtk.Check_Menu_Item.Gtk_Check_Menu_Item;
    Label     : String (1..Label_Size);
    Menu_Data : Menu_Item_Data;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Add_Checked_Menu_Item_Data) is
  begin
    Gtk.Check_Menu_Item.Gtk_New (Data.Item, Data.Label);
    Gtk.Menu.Append (Data.Menu, Data.Item);
    if Data.Menu_Data.Routine /= null then
      declare
        The_Menu_Item_Data : constant Menu_Item_Data_Access := new Menu_Item_Data'(Data.Menu_Data);
      begin
        Menu_Item_Callback.Connect (Data.Item,
                                    "activate",
                                    Menu_Item_Callback.To_Marshaller(Menu_Item_Click_Handler'access),
                                    The_Menu_Item_Data);
      end;
    end if;
  end Synchronous_Service;


  function Add_Menu_Item (The_Text         : String;
                          To_Menu          : Menu;
                          The_Menu_Handler : access procedure (Item : Information);
                          The_Information  : Information := No_Information) return Checked_Menu_Item is
    Data : Add_Checked_Menu_Item_Data := (Gui.Router.Request_Data with Label_Size => The_Text'length,
                                                                       Item       => null,  -- returned by call
                                                                       Label      => The_Text,
                                                                       Menu       => To_Menu.The_Menu,
                                                                       Menu_Data  => (Routine => The_Menu_Handler,
                                                                                      Info    => The_Information));
  begin
    Gui.Router.Request (Data);
    return (The_Item => Data.Item,
            Widget   => Gtk.Widget.Gtk_Widget(Data.Item));
  end Add_Menu_Item;


  function Add_Menu_Item (The_Text : String;
                          To_Menu  : Menu) return Checked_Menu_Item is
  begin
    return Add_Menu_Item (The_Text, To_Menu, null);
  end Add_Menu_Item;


  type Add_Radio_Menu_Item_Data (Label_Size : Natural) is new Gui.Router.Request_Data with record
    Menu      : Gtk.Menu.Gtk_Menu;
    Group     : Gtk.Widget.Widget_SList.GSlist;
    Item      : Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item;
    Label     : String (1..Label_Size);
    Menu_Data : Menu_Item_Data;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Add_Radio_Menu_Item_Data) is
  begin
    Gtk.Radio_Menu_Item.Gtk_New (Data.Item, Data.Group, Data.Label);
    Data.Group := Data.Item.Get_Group;
    Data.Menu.Append (Data.Item);
    if Data.Menu_Data.Routine /= null then
      declare
        The_Menu_Item_Data : constant Menu_Item_Data_Access := new Menu_Item_Data'(Data.Menu_Data);
      begin
        Menu_Item_Callback.Connect (Data.Item,
                                    "toggled",
                                    Menu_Item_Callback.To_Marshaller(Radio_Menu_Item_Click_Handler'access),
                                    The_Menu_Item_Data);
      end;
    end if;
  end Synchronous_Service;

  function Add_Menu_Item (The_Text         : String;
                          To_Menu          : in out Menu;
                          The_Menu_Handler : access procedure (Item : Information);
                          The_Information  : Information := No_Information) return Radio_Menu_Item is
    Data : Add_Radio_Menu_Item_Data := (Gui.Router.Request_Data with Label_Size => The_Text'length,
                                                                     Item       => null,  -- returned by call
                                                                     Label      => The_Text,
                                                                     Menu       => To_Menu.The_Menu,
                                                                     Group      => To_Menu.The_Group,
                                                                     Menu_Data  => (Routine => The_Menu_Handler,
                                                                                    Info    => The_Information));
  begin
    Gui.Router.Request (Data);
    declare
      The_Item : constant Radio_Menu_Item := (The_Item => Data.Item,
                                              Widget   => Gtk.Widget.Gtk_Widget(Data.Item));
    begin
      To_Menu.The_Group := Data.Group;
      return The_Item;
    end;
  end Add_Menu_Item;


  function Add_Menu_Item (The_Text :        String;
                          To_Menu  : in out Menu) return Radio_Menu_Item is
  begin
    return Add_Menu_Item (The_Text, To_Menu, null);
  end Add_Menu_Item;


  type Add_Plain_Menu_Item_Data (Label_Size : Natural) is new Gui.Router.Request_Data with record
    Menu      : Gtk.Menu.Gtk_Menu;
    Item      : Gtk.Menu_Item.Gtk_Menu_Item;
    Label     : String (1..Label_Size);
    Menu_Data : Menu_Item_Data;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Add_Plain_Menu_Item_Data) is
  begin
    Gtk.Menu_Item.Gtk_New_With_Label (Data.Item, Data.Label);
    Data.Menu.Append (Data.Item);
    if Data.Menu_Data.Routine /= null then
      declare
        The_Menu_Item_Data : constant Menu_Item_Data_Access := new Menu_Item_Data'(Data.Menu_Data);
      begin
        Menu_Item_Callback.Connect (Data.Item,
                                    "activate",
                                    Menu_Item_Callback.To_Marshaller(Menu_Item_Click_Handler'access),
                                    The_Menu_Item_Data);
      end;
    end if;
  end Synchronous_Service;


  function Add_Menu_Item (The_Text         : String;
                          To_Menu          : Menu;
                          The_Menu_Handler : access procedure (Item : Information);
                          The_Information  : Information := No_Information) return Plain_Menu_Item is
    Data : Add_Plain_Menu_Item_Data := (Gui.Router.Request_Data with Label_Size => The_Text'length,
                                                                     Item       => null, -- returned by call
                                                                     Label      => The_Text,
                                                                     Menu       => To_Menu.The_Menu,
                                                                     Menu_Data  => (Routine => The_Menu_Handler,
                                                                                    Info    => The_Information));
  begin
    Gui.Router.Request (Data);
    return (The_Item => Data.Item,
            Widget   => Gtk.Widget.Gtk_Widget(Data.Item));
  end Add_Menu_Item;


  procedure Enable_Menubar is
    Data : constant Sensitivity_Data := (The_Widget   => Gtk.Widget.Gtk_Widget(The_Menu_Bar),
                                         Is_Sensitive => True);
  begin
    Gui.Router.Send (Data);
  end Enable_Menubar;


  procedure Disable_Menubar is
    Data : constant Sensitivity_Data := (The_Widget   => Gtk.Widget.Gtk_Widget(The_Menu_Bar),
                                         Is_Sensitive => False);
  begin
    Gui.Router.Send (Data);
  end Disable_Menubar;


  procedure Enable (The_Menu : Menu) is
    Data : constant Sensitivity_Data := (The_Widget   => The_Menu.Widget,
                                         Is_Sensitive => True);
  begin
    Gui.Router.Send (Data);
  end Enable;


  procedure Disable (The_Menu : Menu) is
    Data : constant Sensitivity_Data := (The_Widget   => The_Menu.Widget,
                                         Is_Sensitive => False);
  begin
    Gui.Router.Send (Data);
  end Disable;


  procedure Enable (The_Menu_Item : Menu_Item'class) is
    Data : constant Sensitivity_Data := (The_Widget   => The_Menu_Item.Widget,
                                         Is_Sensitive => True);
  begin
    Gui.Router.Send (Data);
  end Enable;


  procedure Disable (The_Menu_Item : Menu_Item'class) is
    Data : constant Sensitivity_Data := (The_Widget   => The_Menu_Item.Widget,
                                         Is_Sensitive => False);
  begin
    Gui.Router.Send (Data);
  end Disable;

  procedure Set (The_Item : Selection_Menu_Item) is
  begin
    raise Program_Error;
  end Set;

  function Is_Set (The_Item : Selection_Menu_Item) return Boolean is
  begin
    raise Program_Error;
    return False;
  end Is_Set;

  type Checked_Menu_Item_Enquiry_Data is new Gui.Router.Request_Data with record
    Item       : Gtk.Check_Menu_Item.Gtk_Check_Menu_Item;
    Is_Checked : Boolean;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Checked_Menu_Item_Enquiry_Data) is
  begin
    Data.Is_Checked := Data.Item.Get_Active;
  end Synchronous_Service;


  function Is_Set (The_Item : Checked_Menu_Item) return Boolean is
    Data : Checked_Menu_Item_Enquiry_Data := (Gui.Router.Request_Data with Item => The_Item.The_Item,
                                                                           Is_Checked => False);
  begin
    Gui.Router.Request (Data);
    return Data.Is_Checked;
  end Is_Set;


  type Set_Check_Menu_Item_Data is new Gui.Router.Message_Data with record
    Item   : Gtk.Check_Menu_Item.Gtk_Check_Menu_Item;
    Is_Set : Boolean;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Set_Check_Menu_Item_Data) is
  begin
    Data.Item.Set_Active (Data.Is_Set);
  end Asynchronous_Service;


  procedure Set (The_Item : Checked_Menu_Item) is
    Data : constant Set_Check_Menu_Item_Data := (Item   => The_Item.The_Item,
                                                 Is_Set => True);
  begin
    Gui.Router.Send (Data);
  end Set;


  procedure Clear (The_Item : Checked_Menu_Item) is
    Data : constant Set_Check_Menu_Item_Data := (Item   => The_Item.The_Item,
                                                 Is_Set => False);
  begin
    Gui.Router.Send (Data);
  end Clear;


  type Set_Radio_Menu_Item_Data is new Gui.Router.Message_Data with record
    Item   : Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item;
    Is_Set : Boolean;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Set_Radio_Menu_Item_Data) is
  begin
    Data.Item.Set_Active (Data.Is_Set);
  end Asynchronous_Service;


  procedure Set (The_Item : Radio_Menu_Item) is
    Data : constant Set_Radio_Menu_Item_Data := (Item   => The_Item.The_Item,
                                                 Is_Set => True);
  begin
    Gui.Router.Send (Data);
  end Set;


  type Radio_Menu_Item_Enquiry_Data is new Gui.Router.Request_Data with record
    Item       : Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item;
    Is_Checked : Boolean;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Radio_Menu_Item_Enquiry_Data) is
  begin
    Data.Is_Checked := Data.Item.Get_Active;
  end Synchronous_Service;

  function Is_Set (The_Item : Radio_Menu_Item) return Boolean is
    Data : Radio_Menu_Item_Enquiry_Data := (Gui.Router.Request_Data with Item       => The_Item.The_Item,
                                                                         Is_Checked => False);
  begin
    Gui.Router.Request (Data);
    return Data.Is_Checked;
  end Is_Set;


  type Menu_Selection_Enquiry_Data is new Gui.Router.Request_Data with record
    Group     : Gtk.Widget.Widget_SList.GSlist;
    Selection : Natural;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Menu_Selection_Enquiry_Data) is
    The_List : Gtk.Widget.Widget_SList.GSlist := Data.Group;
    Counter  : Natural := Natural (Gtk.Widget.Widget_SList.Length (The_List));
    use type Gtk.Widget.Widget_SList.GSlist;
  begin
    while The_List /= Gtk.Widget.Widget_SList.Null_List loop
      exit when Gtk.Radio_Menu_Item.Get_Active (
                    Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item (Gtk.Widget.Widget_SList.Get_Data (The_List)));
      Counter := Counter - 1;
      The_List := Gtk.Widget.Widget_SList.Next (The_List);
    end loop;
    Data.Selection := Counter;
  end Synchronous_Service;

  function Setting (The_Menu : Menu) return Natural is
    Data : Menu_Selection_Enquiry_Data := (Gui.Router.Request_Data with Group     => The_Menu.The_Group,
                                                                        Selection => 0);
  begin
    Gui.Router.Request (Data);
    return Data.Selection;
  end Setting;


  ---------------------------------------
  --
  -- Paned widgets
  --
  ---------------------------------------


  package Pane_Item_Callback is new Gtk.Handlers.User_Callback (Widget_Type => Gtk.Paned.Gtk_Paned_Record,
                                                                User_Type   => Page);

  procedure Size_Allocate (The_Pane : access Gtk.Paned.Gtk_Paned_Record'class;
                           Unused   : Glib.Values.GValues;
                           The_Page : Page) is
    use type Glib.Gint;
  begin
    -- Called when a paned window with 2 panes is allocaed its size
    -- Automatically set the separator position to the mid-point of the paned window
    The_Pane.Set_Position (The_Pane.Get_Allocated_Height / 2);
    --
    -- Only do this once - when the size is first allocated
    --
    Gtk.Handlers.Disconnect (The_Pane, The_Page.Paned_Handler_Id);
  end Size_Allocate;


  procedure Add_Paned_Widget (The_Page   : Page;
                              The_Widget : Gtk.Widget.Gtk_Widget) is
    Scrolled : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
  begin
    -- Place object into scrolled window
    Gtk.Scrolled_Window.Gtk_New (Scrolled);
    Scrolled.Set_Policy (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
    Scrolled.Add (The_Widget);
    The_Page.Nr_Of_Panes := The_Page.Nr_Of_Panes + 1;
    case The_Page.Nr_Of_Panes is
    when 1 => -- Create paned window
      Gtk.Paned.Gtk_New_Vpaned (The_Page.Paned_Window);
      The_Page.Paned_Window.Add1 (Scrolled);
      The_Page.Page_Box.Pack_End (The_Page.Paned_Window);
    when 2 => -- Add second pane
      The_Page.Paned_Window.Add2 (Scrolled);
      -- Add callback for when paned window is allocated its size (so that we can set the separator)
      The_Page.Paned_Handler_Id := Pane_Item_Callback.Connect (The_Page.Paned_Window,
                                                               "size_allocate",
                                                               Size_Allocate'access,
                                                               The_Page);
    when others =>  -- can only pane two items
      The_Page.Page_Box.Pack_Start (Scrolled);
    end case;
  end Add_Paned_Widget;


  -------------------
  --
  -- Button functions
  --
  -------------------

  type Button_Data (Text_Size : Natural) is new Gui.Router.Request_Data with record
    Parent_Page        : Page;
    The_Button         : Button;
    The_Text           : String (1..Text_Size);
    The_Action_Routine : Action_Routine;
    The_Button_Size    : Natural;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Button_Data) is
  begin
    Gtk.Button.Gtk_New (Data.The_Button.The_Button, Data.The_Text);
    Data.The_Button.Widget := Gtk.Widget.Gtk_Widget(Data.The_Button.The_Button);
    if Data.The_Action_Routine /= null then
      Action_Callback.Connect (Data.The_Button.The_Button,
                               "clicked",
                               Action_Callback.To_Marshaller(Action_Handler'access),
                               Data.The_Action_Routine);
    end if;
    declare
      The_Button_Width  : Glib.Gint;
      use type Glib.Gint;
     begin  -- Set start-up size of window
       if Data.The_Button_Size > Data.Parent_Page.Minimum_Button_Width then
         The_Button_Width := Glib.Gint(Data.The_Button_Size);
       else
         The_Button_Width := Glib.Gint(Data.The_Button_Size);
       end if;
       if The_Button_Width = 0 then
         The_Button_Width := -1;
       end if;
       Data.The_Button.Widget.Set_Size_Request (Width  => The_Button_Width);
    end;
    Data.Parent_Page.Nr_Of_Buttons := Data.Parent_Page.Nr_Of_Buttons + 1;
    if Data.Parent_Page.Nr_Of_Buttons = 1 then -- first button creates the box
      Gtk.Box.Gtk_New_Hbox (Data.Parent_Page.The_Button_Box, False, 0);
      if Data.Parent_Page.The_Style(Buttons_Fill_Vertically) then
        Data.Parent_Page.Page_Box.Pack_Start (Data.Parent_Page.The_Button_Box, True, True);
      else
        Data.Parent_Page.Page_Box.Pack_Start (Data.Parent_Page.The_Button_Box, False);
      end if;
    end if;
    if Data.Parent_Page.The_Style (Buttons_Fill_Horizontally) then
      Data.Parent_Page.The_Button_Box.Pack_Start (Data.The_Button.The_Button, True, True);
    elsif Data.Parent_Page.Nr_Of_Buttons = 2 then
      Data.Parent_Page.The_Button_Box.Pack_End (Data.The_Button.The_Button, False);
    else
      Data.Parent_Page.The_Button_Box.Pack_Start (Data.The_Button.The_Button, False);
    end if;
  end Synchronous_Service;


  function Create (Parent_Page        : Page;
                   The_Text           : String;
                   The_Action_Routine : Action_Routine := null;
                   The_Button_Size    : Natural := Default_Button_Width) return Button is
    Data : Button_Data := (Gui.Router.Request_Data with Text_Size          => The_Text'length,
                                                        Parent_Page        => Parent_Page,
                                                        The_Button         => (others => <>), -- Returned by call
                                                        The_Text           => The_Text,
                                                        The_Action_Routine => The_Action_Routine,
                                                        The_Button_Size    => The_Button_Size);
  begin
    Gui.Router.Request (Data);
    return Data.The_Button;
  end Create;


  type Set_Button_Text_Data (Text_Size : Natural) is new Gui.Router.Message_Data with record
    The_Button : Gtk.Button.Gtk_Button;
    The_Text   : String (1..Text_Size);
  end record;

  overriding
  procedure Asynchronous_Service (Data : Set_Button_Text_Data) is
  begin
    Data.The_Button.Set_Label (Data.The_Text);
  end Asynchronous_Service;

  procedure Set_Text (The_Button : Button;
                      The_Text   : String) is
    Data : constant Set_Button_Text_Data := (Text_Size  => The_Text'length,
                                             The_Button => The_Button.The_Button,
                                             The_Text   => The_Text);
  begin
    Gui.Router.Send (Data);
  end Set_Text;


  -------------------------
  --
  -- Progress Bar functions
  --
  -------------------------

  type Progress_Bar_Data is new Gui.Router.Request_Data with record
    Parent_Page        : Page;
    Place_With_Buttons : Boolean;
    The_Bar            : Gtk.Progress_Bar.Gtk_Progress_Bar;
    The_Widget         : Gtk.Widget.Gtk_Widget;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Progress_Bar_Data) is
  begin
    Gtk.Progress_Bar.Gtk_New (Data.The_Bar);
    Data.The_Widget := Gtk.Widget.Gtk_Widget(Data.The_Bar);
    if Data.Place_With_Buttons and (Data.Parent_Page.Nr_Of_Buttons > 0) then
      Data.Parent_Page.The_Button_Box.Pack_Start (Data.The_Bar, True, True);
    else
      Data.The_Widget.Set_Size_Request (Height => 30);
      Data.Parent_Page.Page_Box.Pack_Start (Data.The_Bar, False);
    end if;
  end Synchronous_Service;


  function Create (Parent_Page        : Page;
                   Place_With_Buttons : Boolean := True) return Progress_Bar is
    Data : Progress_Bar_Data:= (Gui.Router.Request_Data with Parent_Page        => Parent_Page,
                                                             Place_With_Buttons => Place_With_Buttons,
                                                             The_Bar            => null,
                                                             The_Widget         => null);
  begin
    if Parent_Page.The_Style(Buttons_Fill_Horizontally) then
      Data.Place_With_Buttons := False;
    end if;
    Gui.Router.Request (Data);
    return (Widget       => Data.The_Widget,
            The_Bar      => Data.The_Bar,
            The_Extent   => 100.0,
            The_Step     => 10.0,
            The_Position => 0.0);
  end Create;


  procedure Define_Range (The_Progress_Bar : in out Progress_Bar;
                          The_Extent       : Positive;
                          The_Step         : Positive := 1) is
    The_Position : Glib.Gdouble;
    use type Glib.Gdouble;
  begin
    The_Position := The_Progress_Bar.The_Position / The_Progress_Bar.The_Extent;
    The_Progress_Bar.The_Extent := Glib.Gdouble(The_Extent);
    The_Progress_Bar.The_Step := Glib.Gdouble(The_Step);
    The_Progress_Bar.The_Position := The_Progress_Bar.The_Extent * The_Position;
  end Define_Range;


  type Set_Progress_Bar_Data is new Gui.Router.Message_Data with record
    The_Bar      : Gtk.Progress_Bar.Gtk_Progress_Bar;
    The_Progress : Glib.Gdouble;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Set_Progress_Bar_Data) is
  begin
    Data.The_Bar.Set_Fraction (Data.The_Progress);
  end Asynchronous_Service;


  procedure Report_Progress (The_Progress_Bar : in out Progress_Bar;
                             The_Progress     : Natural) is
    use type Glib.Gdouble;
  begin
    The_Progress_Bar.The_Position := Glib.Gdouble(The_Progress);
    if The_Progress_Bar.The_Position > The_Progress_Bar.The_Extent then
      The_Progress_Bar.The_Position := The_Progress_Bar.The_Extent;
    end if;
    declare
      Data : constant Set_Progress_Bar_Data
           := (The_Bar      => The_Progress_Bar.The_Bar,
                               The_Progress => The_Progress_Bar.The_Position / The_Progress_Bar.The_Extent);
    begin
      Gui.Router.Send (Data);
    end;
  end Report_Progress;


  procedure Increment_Progress (The_Progress_Bar : in out Progress_Bar) is
    use type Glib.Gdouble;
  begin
    The_Progress_Bar.The_Position := The_Progress_Bar.The_Position + The_Progress_Bar.The_Step;
    if The_Progress_Bar.The_Position > The_Progress_Bar.The_Extent then
      The_Progress_Bar.The_Position := The_Progress_Bar.The_Extent;
    end if;
    declare
      Data : constant Set_Progress_Bar_Data
           := (The_Bar      => The_Progress_Bar.The_Bar,
                               The_Progress => The_Progress_Bar.The_Position / The_Progress_Bar.The_Extent);
    begin
      Gui.Router.Send (Data);
    end;
  end Increment_Progress;


  ----------------------
  --
  -- List View functions
  --
  ----------------------

  procedure List_Click_Handler (Widget            : access Gtk.Widget.Gtk_Widget_Record'class;
                                The_Click_Routine : Click_Routine) is
    Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
    Model : Gtk.Tree_Model.Gtk_Tree_Model;
    Value : Glib.Values.GValue;
  begin
    Gtk.Tree_Selection.Get_Selected (Gtk.Tree_View.Get_Selection (Gtk.Tree_View.Gtk_Tree_View(Widget)),
                                     Model, Iter);
    Gtk.Tree_Model.Get_Value (Model, Iter, 0, Value);
    Gui.Router.Execute (The_Click_Routine, Information(Glib.Values.Get_Ulong(Value)));
  end List_Click_Handler;

  package Click_Callback is new Gtk.Handlers.User_Callback (Gtk.Widget.Gtk_Widget_Record, Click_Routine);

  type List_View_Data is new Gui.Router.Request_Data with record
    Parent_Page  : Page;
    The_View     : Gtk.Tree_View.Gtk_Tree_View;
    The_Widget   : Gtk.Widget.Gtk_Widget;
    The_Routine  : Click_Routine;
    Selection    : Gui.Selection;
    Monospaced   : Boolean;
    Single_Click : Boolean;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out List_View_Data) is
  begin
    Gtk.Tree_View.Gtk_New (Data.The_View);
    Data.The_Widget := Gtk.Widget.Gtk_Widget (Data.The_View);

    Data.The_View.Set_Fixed_Height_Mode (True);

    if Data.Monospaced then
      declare
        Font_Description : Pango.Font.Pango_Font_Description;
      begin
        Pango.Font.Gdk_New (Font_Description);
        Pango.Font.Set_Family (Font_Description, (case Os.Family is when Os.Windows => "Fixedsys",
                                                                    when others     => "Monospace"));
        Data.The_Widget.Override_Font (Font_Description);
      end;
    end if;

    if Data.The_Routine = null then
      declare
        The_Selection : Gtk.Enums.Gtk_Selection_Mode;
      begin
        case Data.Selection is
        when None =>
          The_Selection := Gtk.Enums.Selection_None;
        when Single =>
          The_Selection := Gtk.Enums.Selection_Single;
        when Multiple =>
          The_Selection := Gtk.Enums.Selection_Multiple;
          Data.The_View.Set_Rubber_Banding (True);
        end case;
        Gtk.Tree_Selection.Set_Mode (Gtk.Tree_View.Get_Selection (Data.The_View), The_Selection);
      end;
    else
      Click_Callback.Connect (Data.The_View,
                              "row-activated",
                              Click_Callback.To_Marshaller(List_Click_Handler'access),
                              Data.The_Routine);
      Gtk.Tree_Selection.Set_Mode (Gtk.Tree_View.Get_Selection (Data.The_View), Gtk.Enums.Selection_Single);
      Data.The_View.Set_Activate_On_Single_Click (Data.Single_Click);
    end if;

    Add_Paned_Widget (Data.Parent_Page, Data.The_Widget);
  end Synchronous_Service;


  function Create (Parent_Page           : Page;
                   The_Text_Handler      : not null access function (For_Column       : Natural;
                                                                     With_Information : Information) return String;
                   The_Color_Handler     : Color_Handler := null;
                   The_Sort_Routine      : Sort_Routine := null;
                   The_Click_Routine     : Click_Routine := null;
                   The_Click_Kind        : Click_Kind := Double_Click;
                   Use_Proportional_Font : Boolean := True;
                   Selection_Criteria    : Selection := None;
                   Color_Background      : Boolean := False) return List_View is
    Data : List_View_Data := (Gui.Router.Request_Data with Parent_Page  => Parent_Page,
                                                           The_View     => null, -- return by call
                                                           The_Widget   => null, -- return by call
                                                           The_Routine  => The_Click_Routine,
                                                           Selection    => Selection_Criteria,
                                                           Monospaced   => not Use_Proportional_Font,
                                                           Single_Click => The_Click_Kind = Single_Click);

  begin
    Gui.Router.Request (Data);
    return (The_View          => Data.The_View,
            The_Store         => null,   -- Created later
            Widget            => Gtk.Widget.Gtk_Widget (Data.The_View),
            The_Text_Handler  => The_Text_Handler,
            The_Color_Handler => The_Color_Handler,
            The_Sort_Routine  => The_Sort_Routine,
            Color_Background  => Color_Background);
  end Create;


  type Set_Model_Data is new Gui.Router.Message_Data with record
    The_Widget : Gtk.Widget.Gtk_Widget;
    The_View   : Gtk.Tree_View.Gtk_Tree_View;
    The_Store  : Gtk.List_Store.Gtk_List_Store;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Set_Model_Data) is
    use Gtk.List_Store;
  begin
    if Data.The_Store = null then
      Data.The_Widget.Freeze_Child_Notify;
      Data.The_View.Set_Model (Gtk.Tree_Model.Null_Gtk_Tree_Model);
    else
      Data.The_View.Set_Model (+Data.The_Store);
      Data.The_Widget.Thaw_Child_Notify;
    end if;
  end Asynchronous_Service;

  procedure Freeze (The_List_View : List_View) is
    Data : constant Set_Model_Data := (The_Widget => The_List_View.Widget,
                                       The_View   => The_List_View.The_View,
                                       The_Store  => null);
  begin
    Gui.Router.Send (Data);
  end Freeze;

  procedure Thaw (The_List_View : List_View) is
    Data : constant Set_Model_Data := (The_Widget => The_List_View.Widget,
                                       The_View   => The_List_View.The_View,
                                       The_Store  => The_List_View.The_Store);
  begin
    Gui.Router.Send (Data);
  end Thaw;


  type Gridline_Data is new Gui.Router.Message_Data with record
    The_View    : Gtk.Tree_View.Gtk_Tree_View;
    Are_Visible : Boolean;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Gridline_Data) is
  begin
    if Data.Are_Visible then
      Data.The_View.Set_Grid_Lines (Gtk.Enums.Grid_Lines_Both);
    else
      Data.The_View.Set_Grid_Lines (Gtk.Enums.Grid_Lines_None);
    end if;
  end Asynchronous_Service;

  procedure Add_Gridlines (The_List_View : List_View) is
    Data : constant Gridline_Data := (The_View    => The_List_View.The_View,
                                      Are_Visible => True);
  begin
    Gui.Router.Send (Data);
  end Add_Gridlines;

  procedure Remove_Gridlines (The_List_View : List_View) is
    Data : constant Gridline_Data := (The_View    => The_List_View.The_View,
                                      Are_Visible => False);
  begin
    Gui.Router.Send (Data);
  end Remove_Gridlines;


  function Alignment_Of (The_Justification : Justification) return Glib.Gfloat is
  begin
    case The_Justification is
      when Left   => return 0.0;
      when Center => return 0.5;
      when Right  => return 1.0;
    end case;
  end Alignment_Of;

  type Add_Column_Data (Title_Size : Natural) is new Gui.Router.Request_Data with record
    The_View          : Gtk.Tree_View.Gtk_Tree_View;
    The_Title         : String (1..Title_Size);
    The_Width         : Natural;
    The_Justification : Glib.Gfloat;
    Color_Background  : Boolean;
    Is_Sortable       : Boolean;
    The_Column        : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
    Column_Nr         : Glib.Gint;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Add_Column_Data) is
    Unused        : Glib.Gint;
    Cell_Renderer : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
    use type Glib.Gint;
  begin
    Data.Column_Nr := Glib.Gint(Gtk.Tree_View.Get_N_Columns (Data.The_View));
    Gtk.Tree_View_Column.Gtk_New (Data.The_Column);
    Data.The_Column.Set_Title (Data.The_Title);
    Data.The_Column.Set_Resizable (True);
    Data.The_Column.Set_Sizing (Gtk.Tree_View_Column.Tree_View_Column_Fixed);
    if Data.The_Width = Automatic then
      Data.The_Column.Set_Expand (True);
    else
      Data.The_Column.Set_Fixed_Width (Glib.Gint(Data.The_Width));
    end if;
    Data.The_Column.Set_Alignment (Data.The_Justification);
    Data.The_Column.Set_Clickable (Data.Is_Sortable);
    Gtk.Cell_Renderer_Text.Gtk_New (Cell_Renderer);
    Cell_Renderer.Set_Alignment (Data.The_Justification, 0.0);
    Cell_Renderer.Set_Padding (0, 0);
    Data.The_Column.Pack_Start (Cell_Renderer, True);
    Unused := Data.The_View.Append_Column (Data.The_Column);
    if Data.Color_Background then
      Data.The_Column.Add_Attribute (Cell_Renderer, "background", 1);
    else
      Data.The_Column.Add_Attribute (Cell_Renderer, "foreground", 1);
    end if;
    Data.The_Column.Add_Attribute (Cell_Renderer, "text", Data.Column_Nr + 2);
  end Synchronous_Service;


  function Add_Column (The_List_View     : List_View;
                       The_Title         : String;
                       The_Width         : Natural := Automatic;
                       The_Justification : Justification := Left;
                       Is_Sortable       : Boolean := False) return Column is
    use type Gtk.List_Store.Gtk_List_Store;
  begin
    if The_List_View.The_Store = null then
      declare
        Data : Add_Column_Data := (Gui.Router.Request_Data with Title_Size        => The_Title'length,
                                                                The_View          => The_List_View.The_View,
                                                                The_Title         => The_Title,
                                                                The_Width         => The_Width,
                                                                The_Justification => Alignment_Of (The_Justification),
                                                                Color_Background  => The_List_View.Color_Background,
                                                                Is_Sortable       => Is_Sortable,
                                                                The_Column        => null,
                                                                Column_Nr         => 0);
      begin
        Gui.Router.Request (Data);
        return Column'(The_View   => The_List_View.The_View,
                       Column_Nr  => Data.Column_Nr,
                       The_Column => Data.The_Column);
      end;
    else
      raise Sequence_Error;
    end if;
  end Add_Column;


  type Get_Column_Width_Data is new Gui.Router.Request_Data with record
    The_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
    The_Width  : Natural;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Get_Column_Width_Data) is
  begin
    Data.The_Width := Natural (Data.The_Column.Get_Width);
  end Synchronous_Service;

  function Width_Of (The_Column : Column) return Natural is
    Data : Get_Column_Width_Data := (Gui.Router.Request_Data with The_Column => The_Column.The_Column,
                                                                  The_Width  => 0);
  begin
    Gui.Router.Request (Data);
    return Data.The_Width;
  end Width_Of;


  type Sort_Direction_Data is new Gui.Router.Message_Data with record
    The_View   : Gtk.Tree_View.Gtk_Tree_View;
    The_Column : Glib.Gint;
    The_Order  : Gtk.Enums.Gtk_Sort_Type;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Sort_Direction_Data) is
    The_Model : Gtk.Tree_Model.Gtk_Tree_Model;
    use Gtk.List_Store;
    use type Gtk.Tree_Model.Gtk_Tree_Model;
  begin
    The_Model := Gtk.Tree_View.Get_Model (Data.The_View);
    if The_Model /= Gtk.Tree_Model.Null_Gtk_Tree_Model then
      Gtk.List_Store.Set_Sort_Column_Id (Sortable       => "-"(The_Model),
                                         Sort_Column_Id => Data.The_Column,
                                         Order          => Data.The_Order);
    end if;
  end Asynchronous_Service;

  function Sort_Order_Of (The_Order : Sort_Order) return Gtk.Enums.Gtk_Sort_Type is
  begin
    case The_Order is
      when Ascending  => return Gtk.Enums.Sort_Ascending;
      when Descending => return Gtk.Enums.Sort_Descending;
    end case;
  end Sort_Order_Of;

  procedure Sort (The_Column    : Column;
                  The_Direction : Sort_Order) is
    Data : constant Sort_Direction_Data := (The_View   => The_Column.The_View,
                                            The_Column => The_Column.Column_Nr,
                                            The_Order  => Sort_Order_Of(The_Direction));
  begin
    Gui.Router.Send (Data);
  end Sort;


  type Set_Column_Title_Data (Title_Size : Natural) is new Gui.Router.Message_Data with record
    The_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
    The_Title  : String (1..Title_Size);
  end record;

  overriding
  procedure Asynchronous_Service (Data : Set_Column_Title_Data) is
  begin
    Data.The_Column.Set_Title (Data.The_Title);
  end Asynchronous_Service;


  procedure Set_Title (The_Column : Column;
                       The_Title  : String) is
    Data : constant Set_Column_Title_Data := (Title_Size => The_Title'length,
                                              The_Column => The_Column.The_Column,
                                              The_Title  => The_Title);
  begin
    Gui.Router.Send (Data);
  end Set_Title;


  type Sort_Data is record
    The_Sort_Routine : Sort_Routine;
    The_Column       : Natural;
  end record;

  function Compare_Function (Model         : Gtk.Tree_Model.Gtk_Tree_Model;
                             A             : Gtk.Tree_Model.Gtk_Tree_Iter;
                             B             : Gtk.Tree_Model.Gtk_Tree_Iter;
                             The_Sort_Data : Sort_Data) return Glib.Gint is
    Value  : Glib.Values.GValue;
    Info_A : Information;
    Info_B : Information;
  begin
    Gtk.Tree_Model.Get_Value (Model, A, 0, Value);
    Info_A := Information(Glib.Values.Get_Ulong(Value));
    Gtk.Tree_Model.Get_Value (Model, B, 0, Value);
    Info_B := Information(Glib.Values.Get_Ulong(Value));
    if The_Sort_Data.The_Sort_Routine = null then
      if Info_A > Info_B then
        return 1;
      elsif Info_A < Info_B then
        return Glib.Gint (-1);
      else
        return 0;
      end if;
    else
      case The_Sort_Data.The_Sort_Routine (The_Sort_Data.The_Column, Info_A, Info_B) is
        when Greater_Than => return 1;
        when Less_Than    => return Glib.Gint(-1);
        when Equal_To     => return 0;
      end case;
    end if;
  end Compare_Function;

  package Sortable_List_View is new Gtk.Tree_Sortable.Set_Sort_Func_User_Data (Sort_Data);

  type Store_Data is new Gui.Router.Request_Data with record
    The_View         : Gtk.Tree_View.Gtk_Tree_View;
    The_Store        : Gtk.List_Store.Gtk_List_Store;
    The_Sort_Routine : Sort_Routine;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Store_Data) is
    use Gtk.List_Store;
    use type Glib.Guint;
    use type Glib.Gint;
    Nr_Of_Columns : constant Glib.Guint := Data.The_View.Get_N_Columns;
    -- Note: The first two columns of the store are used to store the row information and colour respectively
    The_Columns : constant Glib.GType_Array (0..Nr_Of_Columns + 1) := (0      => Glib.GType_Ulong,
                                                                       others => Glib.GType_String);
  begin
    Gtk.List_Store.Gtk_New (Data.The_Store, The_Columns);
    Data.The_View.Set_Model (+Data.The_Store);
    for The_Column in 0 .. Glib.Gint(Nr_Of_Columns) - 1 loop
      declare
        View_Column   : constant Gtk.Tree_View_Column.Gtk_Tree_View_Column := Data.The_View.Get_Column (The_Column);
        The_Sort_Data : constant Sort_Data := (The_Sort_Routine => Data.The_Sort_Routine,
                                               The_Column       => Natural(The_Column));
      begin
        Sortable_List_View.Set_Sort_Func ("+"(Data.The_Store), The_Column, Compare_Function'access, The_Sort_Data);
        if View_Column.Get_Clickable then
          View_Column.Set_Sort_Column_Id (The_Column);
        end if;
      end;
    end loop;
  end Synchronous_Service;


  type Add_Row_Data is new Gui.Router.Message_Data with record
    The_View         : Gtk.Tree_View.Gtk_Tree_View;
    The_Store        : Gtk.List_Store.Gtk_List_Store;
    The_Position     : Glib.Gint;
    The_Information  : Information;
    The_Text_Handler : Text_Handler;
    The_Color        : Color;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Add_Row_Data) is
    Nr_Of_Columns : constant Glib.Guint := Gtk.Tree_View.Get_N_Columns (Data.The_View);
    Iter          : Gtk.Tree_Model.Gtk_Tree_Iter;
    use type Glib.Guint;
  begin
    Gtk.List_Store.Insert (Data.The_Store, Iter, Data.The_Position);
    declare
      The_Value : Glib.Values.GValue;
    begin
      Glib.Values.Init (The_Value, Glib.GType_Ulong);
      Glib.Values.Set_Ulong (The_Value, Glib.Gulong(Data.The_Information));
      Data.The_Store.Set_Value (Iter, 0, The_Value);
      Glib.Values.Unset (The_Value);
    end;
    Data.The_Store.Set (Iter, 1, Data.The_Color'img);
    if Data.The_Text_Handler /= null then
      for The_Column in 2 .. (Nr_Of_Columns + 1) loop
        Data.The_Store.Set (Iter, Glib.Gint(The_Column),
                            Data.The_Text_Handler (Natural (The_Column) - 2, Data.The_Information));
      end loop;
    end if;
  end Asynchronous_Service;

  procedure Add_Data_To (The_List_View   : in out List_View;
                         The_Position    : Integer;
                         The_Information : Information) is
    use type Gtk.List_Store.Gtk_List_Store;
  begin
    if The_List_View.The_Store = null then
      declare
        Database : Store_Data := (Gui.Router.Request_Data with The_View         => The_List_View.The_View,
                                                               The_Sort_Routine => The_List_View.The_Sort_Routine,
                                                               The_Store        => null);
      begin
        Gui.Router.Request (Database);
        The_List_View.The_Store := Database.The_Store;
      end;
    end if;
    declare
      Data : Add_Row_Data := (The_View         => The_List_View.The_View,
                              The_Store        => The_List_View.The_Store,
                              The_Position     => Glib.Gint (The_Position),
                              The_Information  => The_Information,
                              The_Text_Handler => The_List_View.The_Text_Handler,
                              The_Color        => Black);
    begin
      if The_List_View.The_Color_Handler /= null then
        Data.The_Color := The_List_View.The_Color_Handler (The_Information);
      elsif The_List_View.Color_Background then
        Data.The_Color := White;
      end if;
      Gui.Router.Send (Data);
    end;
  end Add_Data_To;


  procedure Add_Data (The_List_View   : in out List_View;
                      The_Information : Information) is
    At_End : constant Integer := -1;
  begin
    Add_Data_To (The_List_View, At_End, The_Information);
  end Add_Data;


  procedure Add_To (The_List_View   : in out List_View;
                    The_Position    : Positive := Positive'first;
                    The_Information : Information) is
  begin
    Add_Data_To (The_List_View, The_Position - 1, The_Information);
  end Add_To;


  type Remove_List_Data is new Gui.Router.Message_Data with record
    The_Store    : Gtk.List_Store.Gtk_List_Store;
    The_Position : Glib.Gint;
    The_Amount   : Positive;
  end record;


  overriding
  procedure Asynchronous_Service (Data : Remove_List_Data) is
    Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
    use Gtk.List_Store;
    use type Gtk.Tree_Model.Gtk_Tree_Iter;
  begin
    for Unused_Index in 1 .. Data.The_Amount loop
      Iter := Gtk.Tree_Model.Nth_Child (+Data.The_Store, Gtk.Tree_Model.Null_Iter, Data.The_Position);
      exit when Iter = Gtk.Tree_Model.Null_Iter;
      Data.The_Store.Remove (Iter);
    end loop;
  end Asynchronous_Service;

  procedure Remove_From (The_List_View : List_View;
                         The_Position  : Positive := Positive'first;
                         The_Amount    : Positive := 1) is
    use type Gtk.List_Store.Gtk_List_Store;
  begin
    if The_List_View.The_Store /= null then
      declare
        Data : constant Remove_List_Data := (The_Store    => The_List_View.The_Store,
                                             The_Position => Glib.Gint (The_Position - 1),
                                             The_Amount   => The_Amount);
      begin
        Gui.Router.Send (Data);
      end;
    end if;
  end Remove_From;


  type Remove_Information_Data is new Gui.Router.Message_Data with record
    The_Store       : Gtk.List_Store.Gtk_List_Store;
    The_Information : Information;
    Remove_All      : Boolean;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Remove_Information_Data) is
    Iter  : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.List_Store.Get_Iter_First (Data.The_Store);
    Value : Glib.Values.GValue;
    use type Gtk.Tree_Model.Gtk_Tree_Iter;
    use type Glib.Gulong;
  begin
    while Iter /= Gtk.Tree_Model.Null_Iter loop
      Gtk.List_Store.Get_Value (Data.The_Store, Iter, 0, Value);
      if Glib.Values.Get_Ulong(Value) = Glib.Gulong (Data.The_Information) then
        Data.The_Store.Remove (Iter);
        exit when not Data.Remove_All;
      else
        Data.The_Store.Next (Iter);
      end if;
    end loop;
  end Asynchronous_Service;

  procedure Remove_From (The_List_View   : List_View;
                         The_Information : Information;
                         The_Amount      : Amount := First_Occurrence) is
    use type Gtk.List_Store.Gtk_List_Store;
  begin
    if The_List_View.The_Store /= null then
      declare
        Data : constant Remove_Information_Data := (The_Store       => The_List_View.The_Store,
                                                    The_Information => The_Information,
                                                    Remove_All      => The_Amount = All_Occurrences);
      begin
        Gui.Router.Send (Data);
      end;
    end if;
  end Remove_From;


  type Remove_All_List_Data is new Gui.Router.Message_Data with record
    The_Store : Gtk.List_Store.Gtk_List_Store;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Remove_All_List_Data) is
  begin
    Data.The_Store.Clear;
  end Asynchronous_Service;

  procedure Remove_All_From (The_List_View : List_View) is
    use type Gtk.List_Store.Gtk_List_Store;
  begin
    if The_List_View.The_Store /= null then -- Store has been created
      declare
        Data : constant Remove_All_List_Data := (The_Store => The_List_View.The_Store);
      begin
        Gui.Router.Send (Data);
      end;
    end if;
  end Remove_All_From;


  type Match_Data is record
    The_List_View   : List_View;
    The_Information : Information;
    Nr_Of_Columns   : Glib.Gint;
  end record;

  function Match_Information (Model       : Gtk.Tree_Model.Gtk_Tree_Model;
                              Unused_Path : Gtk.Tree_Model.Gtk_Tree_Path;
                              Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
                              Data        : Match_Data) return Boolean is
    Value : Glib.Values.GValue;
    use type Glib.Gint;
    use type Glib.Gulong;
  begin
    Gtk.Tree_Model.Get_Value (Model, Iter, 0, Value);
    if Glib.Values.Get_Ulong(Value) = Glib.Gulong (Data.The_Information) then
      if Data.The_List_View.The_Color_Handler /= null then
        Data.The_List_View.The_Store.Set (Iter, 1, Data.The_List_View.The_Color_Handler (Data.The_Information)'img);
      end if;
      if Data.The_List_View.The_Text_Handler /= null then
        for The_Column in 2 .. (Data.Nr_Of_Columns - 1) loop
          Data.The_List_View.The_Store.Set (Iter, Glib.Gint(The_Column),
                             Data.The_List_View.The_Text_Handler (Natural(The_Column - 2), Data.The_Information));
        end loop;
      end if;
      return True; -- Stop iterating over the store
    end if;
    return False;  -- Continue looking;
  end Match_Information;

  type Notify_Data is new Gui.Router.Message_Data with record
    The_List_View   : List_View;
    The_Information : Information;
  end record;

  package Store_User_Data is new Gtk.List_Store.Foreach_User_Data (Match_Data);

  overriding
  procedure Asynchronous_Service (Data : Notify_Data) is
    The_Store : constant Gtk.List_Store.Gtk_List_Store := Data.The_List_View.The_Store;
    The_Data  : constant Match_Data := (The_List_View   => Data.The_List_View,
                                        Nr_Of_Columns   => Gtk.List_Store.Get_N_Columns (The_Store),
                                        The_Information => Data.The_Information);
  begin
    Store_User_Data.Foreach (The_Store, Match_Information'access, The_Data);
  end Asynchronous_Service;

  procedure Notify_Item_Update (The_List_View   : List_View;
                                The_Information : Information) is
    Data : constant Notify_Data := (The_List_View   => The_List_View,
                                    The_Information => The_Information);

  begin
    Gui.Router.Send (Data);
  end Notify_Item_Update;


  type Column_Visibility_Data is new Gui.Router.Message_Data with record
    The_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
    Is_Visible : Boolean;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Column_Visibility_Data) is
  begin
    Data.The_Column.Set_Visible (Data.Is_Visible);
  end Asynchronous_Service;


  procedure Make_Column_Visible (The_Column : Column;
                                 Is_Visible : Boolean) is
    Data : constant Column_Visibility_Data := (The_Column => The_Column.The_Column,
                                               Is_Visible => Is_Visible);
  begin
    Gui.Router.Send (Data);
  end Make_Column_Visible;


  type Row_Visibility_Data is new Gui.Router.Message_Data with record
    The_View  : Gtk.Tree_View.Gtk_Tree_View;
    The_Store : Gtk.List_Store.Gtk_List_Store;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Row_Visibility_Data) is
    The_Model      : Gtk.Tree_Model.Gtk_Tree_Model;
    Iter           : Gtk.Tree_Model.Gtk_Tree_Iter;
    Path           : Gtk.Tree_Model.Gtk_Tree_Path;
    Nr_Of_Children : Glib.Gint;
    use Gtk.List_Store;
    use type Glib.Gint;
    use type Gtk.Tree_Model.Gtk_Tree_Model;
  begin
    The_Model := Gtk.Tree_View.Get_Model (Data.The_View);
    if The_Model = Gtk.Tree_Model.Null_Gtk_Tree_Model then
      return;
    end if;
    Nr_Of_Children := Gtk.Tree_Model.N_Children (+Data.The_Store);
    if Nr_Of_Children > 0 then
      Iter := Gtk.Tree_Model.Nth_Child (+Data.The_Store,
                                        Gtk.Tree_Model.Null_Iter,
                                        Nr_Of_Children - 1);
      Path := Data.The_Store.Get_Path (Iter);
      Data.The_View.Scroll_To_Cell (Path, null, False, 0.0, 0.0);
      Path.Path_Free;
    end if;
  end Asynchronous_Service;

  procedure Make_Last_Row_Visible_In (The_List_View : List_View) is
    use type Gtk.List_Store.Gtk_List_Store;
  begin
    if The_List_View.The_Store /= null then
      declare
        Data : constant Row_Visibility_Data := (The_View  => The_List_View.The_View,
                                                The_Store => The_List_View.The_Store);
      begin
        Gui.Router.Send (Data);
      end;
    end if;
  end Make_Last_Row_Visible_In;


  type Lv_Data is new Gui.Router.Request_Data with record
    The_View       : Gtk.Tree_View.Gtk_Tree_View;
    Visible_Height : Glib.Gint;
    Cell_Height    : Glib.Gint;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Lv_Data) is
    The_Rectangle : Gdk.Rectangle.Gdk_Rectangle;
    Start_Path    : Gtk.Tree_Model.Gtk_Tree_Path;
    End_Path      : Gtk.Tree_Model.Gtk_Tree_Path;
    Success       : Boolean;
  begin
    Data.The_View.Get_Visible_Rect (The_Rectangle);
    Data.Visible_Height := The_Rectangle.Height;
    Data.The_View.Get_Visible_Range (Start_Path, End_Path, Success);
    if Success then
      Data.The_View.Get_Background_Area (Start_Path, null, The_Rectangle);
      Data.Cell_Height := The_Rectangle.Height;
      Start_Path.Path_Free;
      End_Path.Path_Free;
    else
      Data.Cell_Height := 0; -- We don't know the height of a cell if none are there.
    end if;
  end Synchronous_Service;


  function Visible_Region_Of (The_List_View : List_View) return Natural is
    Data : Lv_Data := (Gui.Router.Request_Data with The_View       => The_List_View.The_View,
                                                    Visible_Height => 0,
                                                    Cell_Height    => 0);
    use type Glib.Gint;
  begin
    Gui.Router.Request (Data);
    if Data.Cell_Height = 0 then -- we don't know
      return 0;
    else
      return Natural(Data.Visible_Height / Data.Cell_Height);
    end if;
  end Visible_Region_Of;


  type Selection_Data is new Gui.Router.Request_Data with record
    The_View  : Gtk.Tree_View.Gtk_Tree_View;
    Selection : Information_List.Item;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Selection_Data) is

    procedure Add_Selection (Model       : Gtk.Tree_Model.Gtk_Tree_Model;
                             Unused_Path : Gtk.Tree_Model.Gtk_Tree_Path;
                             Iter        : Gtk.Tree_Model.Gtk_Tree_Iter) is
      Value : Glib.Values.GValue;
    begin
      Gtk.Tree_Model.Get_Value (Model, Iter, 0, Value);
      Data.Selection.Append (Information(Glib.Values.Get_Ulong(Value)));
    end Add_Selection;

  begin
    Gtk.Tree_View.Get_Selection (Data.The_View).Selected_Foreach (Add_Selection'unrestricted_access);
  end Synchronous_Service;

  function Selected_Information_From (The_List_View : List_View) return Information_Array is
    Data : Selection_Data := (Gui.Router.Request_Data with The_View  => The_List_View.The_View,
                                                           Selection => Information_List.Empty);
  begin
    Gui.Router.Request (Data);
    return Information_Array(Data.Selection.Elements);
  end Selected_Information_From;


  type Clear_Selection_Data is new Gui.Router.Request_Data with record
    The_View : Gtk.Tree_View.Gtk_Tree_View;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Clear_Selection_Data) is
  begin
    Gtk.Tree_View.Get_Selection (Data.The_View).Unselect_All;
  end Synchronous_Service;

  procedure Clear_Selection (The_List_View : List_View) is
    Data : Clear_Selection_Data := (Gui.Router.Request_Data with The_View => The_List_View.The_View);
  begin
    Gui.Router.Request (Data);
  end Clear_Selection;


  ---------------------
  --
  -- Text_View functions
  --
  ---------------------

  type Text_View_Data is new Gui.Router.Request_Data with record
    Parent_Page   : Page;
    Word_Wrapping : Boolean;
    The_View      : Text_View;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Text_View_Data) is
    The_Tags : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table;
  begin
    Gtk.Text_Tag_Table.Gtk_New (The_Tags);
    for The_Color in Color loop
      The_Tags.Add (Color_Text_Tags(The_Color));
    end loop;
    Gtk.Text_Buffer.Gtk_New (Data.The_View.The_Buffer, The_Tags);
    Data.The_View.The_Buffer.Set_Text ("" & Ascii.Nul);
    Gtk.Text_View.Gtk_New (Data.The_View.The_View, Data.The_View.The_Buffer);
    Data.The_View.The_View.Set_Editable (False);
    Data.The_View.The_View.Set_Cursor_Visible (False);
    if Data.Word_Wrapping then
      Data.The_View.The_View.Set_Wrap_Mode (Gtk.Enums.Wrap_Word);
    else
      Data.The_View.The_View.Set_Wrap_Mode (Gtk.Enums.Wrap_None);
    end if;
    Data.The_View.Widget := Gtk.Widget.Gtk_Widget(Data.The_View.The_View);
    Add_Paned_Widget (Data.Parent_Page, Data.The_View.Widget);
  end Synchronous_Service;

  function Create (Parent_Page   : Page;
                   Word_Wrapping : Boolean := False) return Text_View is
    Data : Text_View_Data := (Gui.Router.Request_Data with Parent_Page   => Parent_Page,
                                                           Word_Wrapping => Word_Wrapping,
                                                           The_View      => (others => <>));
  begin
    Gui.Router.Request (Data);
    return Data.The_View;
  end Create;

  type Text_Append_Data (Text_Size : Natural) is new Gui.Router.Message_Data with record
    The_View       : Text_View;
    The_Text       : String (1..Text_Size);
    The_Color      : Color;
    Ensure_Visible : Boolean;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Text_Append_Data) is
    Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
    Unused : Boolean;
  begin
    Data.The_View.The_Buffer.Get_End_Iter (Iter);
    Data.The_View.The_Buffer.Insert_With_Tags (Iter,
                                               Data.The_Text & Ascii.Lf,
                                               Color_Text_Tags (Data.The_Color));
    if Data.Ensure_Visible then
      Unused := Data.The_View.The_View.Scroll_To_Iter (Iter, 0.0, False, 0.0, 0.0);
    end if;
  end Asynchronous_Service;

  procedure Append_Line_To (The_Text_View  : Text_View;
                            The_Text       : String;
                            The_Color      : Color := Gui.Black;
                            Ensure_Visible : Boolean := True) is
    Data : constant Text_Append_Data := (Text_Size      => The_Text'length,
                                         The_View       => The_Text_View,
                                         The_Text       => The_Text,
                                         The_Color      => The_Color,
                                         Ensure_Visible => Ensure_Visible);
  begin
    Gui.Router.Send (Data);
  end Append_Line_To;


  type Text_Clear_Data is new Gui.Router.Message_Data with record
    The_Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Text_Clear_Data) is
  begin
    Data.The_Buffer.Set_Text ("" & Ascii.Nul);
  end Asynchronous_Service;

  procedure Clear (The_Text_View : Text_View) is
    Data : constant Text_Clear_Data := (The_Buffer => The_Text_View.The_Buffer);
  begin
    Gui.Router.Send (Data);
  end Clear;


  ----------------------
  --
  -- Check Box functions
  --
  ----------------------

  type Create_Check_Box_Data (Title_Size : Natural) is new Gui.Router.Request_Data with record
    Checked_Box : Gtk.Check_Button.Gtk_Check_Button;
    Parent_Page : Page;
    Title       : String (1..Title_Size);
    Box_Width   : Natural;
    Action      : Action_Routine;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Create_Check_Box_Data) is
  begin
    Gtk.Check_Button.Gtk_New (Data.Checked_Box, Data.Title);
    if Data.Parent_Page /= null then
      Data.Parent_Page.Page_Box.Pack_Start (Data.Checked_Box, False);
    end if;
    if Data.Box_Width /= Automatic then
      Gtk.Widget.Set_Size_Request (Widget => Gtk.Bin.Get_Child (Gtk.Bin.Gtk_Bin(Data.Checked_Box)),
                                   Width  => Glib.Gint(Data.Box_Width));
    end if;
    if Data.Action /= null then
      Action_Callback.Connect (Data.Checked_Box,
                               "toggled",
                               Action_Callback.To_Marshaller(Action_Handler'access),
                               Data.Action);
    end if;
  end Synchronous_Service;

  function Create (Parent_Page        : Page;
                   The_Title          : String;
                   The_Action_Routine : Action_Routine := null) return Check_Box is
    Data : Create_Check_Box_Data := (Gui.Router.Request_Data with Title_Size  => The_Title'length,
                                                                  Checked_Box => null, -- returned by call
                                                                  Parent_Page => Parent_Page,
                                                                  Title       => The_Title,
                                                                  Box_Width   => Automatic,
                                                                  Action      => The_Action_Routine);
  begin
    Gui.Router.Request (Data);
    return (The_Box => Data.Checked_Box,
            Widget  => Gtk.Widget.Gtk_Widget(Data.Checked_Box));
  end Create;


  type Check_Enquiry_Data is new Gui.Router.Request_Data with record
    Check_Box  : Gtk.Check_Button.Gtk_Check_Button;
    Is_Checked : Boolean;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Check_Enquiry_Data) is
  begin
    Data.Is_Checked := Data.Check_Box.Get_Active;
  end Synchronous_Service;

  type Set_Check_Data is new Gui.Router.Message_Data with record
    Check_Box  : Gtk.Check_Button.Gtk_Check_Button;
    Is_Set     : Boolean;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Set_Check_Data) is
  begin
    Data.Check_Box.Set_Active (Data.Is_Set);
  end Asynchronous_Service;


  function Is_Checked (The_Check_Box : Check_Box) return Boolean is
    Data : Check_Enquiry_Data := (Gui.Router.Request_Data with Check_Box  => The_Check_Box.The_Box,
                                                               Is_Checked => False);

  begin
    Gui.Router.Request (Data);
    return Data.Is_Checked;
  end Is_Checked;


  procedure Set (The_Check_Box : Check_Box) is
    Data : constant Set_Check_Data := (Check_Box => The_Check_Box.The_Box,
                                       Is_Set    => True);
  begin
    Gui.Router.Send (Data);
  end Set;


  procedure Clear (The_Check_Box : Check_Box) is
    Data : constant Set_Check_Data := (Check_Box => The_Check_Box.The_Box,
                                       Is_Set    => False);
  begin
    Gui.Router.Send (Data);
  end Clear;



  ---------------------
  --
  -- Edit Box functions
  --
  ---------------------

  type Edit_Box_Data (Title_Size : Natural;
                      Text_Size  : Natural) is new Gui.Router.Request_Data with record
    Parent_Page   : Page;
    Edit_Box      : Gtk.GEntry.Gtk_Entry;
    The_Size      : Natural;
    The_Title     : String (1..Title_Size);
    Box_Width     : Natural;
    Initial_Text  : String (1..Text_Size);
    Is_Modifiable : Boolean;
    Is_Visable    : Boolean;
    Action        : Action_Routine;
    Checked_Box   : Gtk.Check_Button.Gtk_Check_Button;
    Widget        : Gtk.Widget.Gtk_Widget;
  end record;


  overriding
  procedure Synchronous_Service (Data : in out Edit_Box_Data) is
    The_Box   : Gtk.Box.Gtk_Box;
    The_Label : Gtk.Label.Gtk_Label;
    use type Gtk.Check_Button.Gtk_Check_Button;
  begin
    Gtk.Box.Gtk_New_Hbox (The_Box);
    if Data.Checked_Box = null then
      Gtk.Label.Gtk_New (The_Label, Data.The_Title);
      Gtk.Misc.Set_Alignment (Gtk.Misc.Gtk_Misc (The_Label), 0.0, 0.5);
      if Data.Box_Width /= Automatic then
        Gtk.Widget.Set_Size_Request (Gtk.Widget.Gtk_Widget(The_Label), Width => Glib.Gint(Data.Box_Width));
      end if;
      The_Box.Pack_Start (The_Label, Expand => False);
    else
      The_Box.Pack_Start (Data.Checked_Box, Expand => False);
    end if;

    Gtk.GEntry.Gtk_New (Data.Edit_Box);
    if Data.Action /= null then
      Event_Callback.Connect (Data.Edit_Box,
                              "focus-out-event",
                              Event_Callback.To_Marshaller (Event_Callback.Event_Marshaller.Handler'(
                                                            Event_Handler'access)),
                              Data.Action);
      Action_Callback.Connect (Data.Edit_Box,
                               "activate",
                               Action_Callback.To_Marshaller(Action_Handler'access),
                               Data.Action);
    end if;
    if Data.Text_Size > 0 then
      Data.Edit_Box.Set_Text (Data.Initial_Text);
    end if;
    Data.Edit_Box.Set_Editable (Data.Is_Modifiable);
    Data.Edit_Box.Set_Visibility (Data.Is_Visable);

    if Data.The_Size = Automatic then
      The_Box.Pack_Start (Data.Edit_Box, True, True);
    else
      Data.Edit_Box.Set_Width_Chars (0); -- Set the minimum size to zero
      -- Then request the size we actually want.
      Gtk.Widget.Set_Size_Request (Widget => Gtk.Widget.Gtk_Widget(Data.Edit_Box),
                                   Width  => Glib.Gint(Data.The_Size));
      The_Box.Pack_Start (Data.Edit_Box, Expand => False);
    end if;

    Data.Parent_Page.Page_Box.Pack_Start (The_Box, False, False);
    Data.Widget := Gtk.Widget.Gtk_Widget(The_Box);
  end Synchronous_Service;


  function Create (Parent_Page        : Page;
                   The_Title          : String;
                   Initial_Text       : String := "";
                   The_Action_Routine : Action_Routine := null;
                   Is_Password        : Boolean := False;
                   Is_Modifiable      : Boolean := True;
                   The_Size           : Natural := Automatic;
                   The_Title_Size     : Natural := Automatic) return Plain_Edit_Box is
    Data  : Edit_Box_Data
          := (Gui.Router.Request_Data with Title_Size    => The_Title'length,
                                           Text_Size     => Initial_Text'length,
                                           Parent_Page   => Parent_Page,
                                           Edit_Box      => null,  -- Returned by call
                                           The_Size      => The_Size,
                                           The_Title     => The_Title,
                                           Box_Width     => The_Title_Size,
                                           Initial_Text  => Initial_Text,
                                           Is_Modifiable => Is_Modifiable,
                                           Is_Visable    => not Is_Password,
                                           Action        => The_Action_Routine,
                                           Checked_Box   => null,
                                           Widget        => null);  -- Returned by call
  begin
    Gui.Router.Request (Data);
    return (The_Box => Data.Edit_Box,
            Widget  => Data.Widget);
  end Create;


  type Get_Edit_Box_Data is new Gui.Router.Request_Data with record
    Edit_Box : Gtk.GEntry.Gtk_Entry;
    The_Text : Text.String;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Get_Edit_Box_Data) is
  begin
    Data.The_Text := Text.String_Of (Data.Edit_Box.Get_Text);
  end Synchronous_Service;

  function Contents_Of (The_Edit_Box : Edit_Box'class) return String is
    Data : Get_Edit_Box_Data := (Gui.Router.Request_Data with Edit_Box => The_Edit_Box.The_Box,
                                                              The_Text => Text.Null_String);
  begin
    Gui.Router.Request (Data);
    return Text.String_Of (Data.The_Text);
  end Contents_Of;


  type Set_Edit_Box_Data (Text_Size : Natural) is new Gui.Router.Message_Data with record
    Edit_Box : Gtk.GEntry.Gtk_Entry;
    The_Text : String (1..Text_Size);
  end record;

  overriding
  procedure Asynchronous_Service (Data : Set_Edit_Box_Data) is
  begin
    Data.Edit_Box.Set_Text (Data.The_Text);
  end Asynchronous_Service;

  procedure Set_Text (The_Edit_Box : Edit_Box'class;
                      The_Text     : String) is
    Data : constant Set_Edit_Box_Data := (Edit_Box  => The_Edit_Box.The_Box,
                                          Text_Size => The_Text'length,
                                          The_Text  => The_Text);
  begin
    Gui.Router.Send (Data);
  end Set_Text;


  function Create  (Parent_Page        : Page;
                    The_Title          : String;
                    Initial_Text       : String := "";
                    The_Action_Routine : Action_Routine := null;
                    Is_Password        : Boolean := False;
                    Is_Modifiable      : Boolean := True;
                    The_Size           : Natural := Automatic;
                    The_Title_Size     : Natural := Automatic) return Checked_Edit_Box is
    The_Checked_Edit_Box : Checked_Edit_Box;
    Check_Box_Data       : Create_Check_Box_Data
                         := (Gui.Router.Request_Data with Title_Size  => The_Title'length,
                                                          Checked_Box => null, -- returned by service routine
                                                          Parent_Page => null, -- part of composite object
                                                          Title       => The_Title,
                                                          Box_Width   => The_Title_Size,
                                                          Action      => The_Action_Routine);
  begin
    Gui.Router.Request (Check_Box_Data);
    The_Checked_Edit_Box.The_Check_Box := Check_Box_Data.Checked_Box;
    declare
      Data : Edit_Box_Data := (Gui.Router.Request_Data with Edit_Box      => null, -- returned by service routine
                                                            Widget        => null, -- returned by service routine
                                                            The_Size      => The_Size,
                                                            Title_Size    => 0,
                                                            The_Title     => "", -- Unused
                                                            Box_Width     => Automatic,
                                                            Checked_Box   => Check_Box_Data.Checked_Box,
                                                            Parent_Page   => Parent_Page,
                                                            Text_Size     => Initial_Text'length,
                                                            Initial_Text  => Initial_Text,
                                                            Is_Modifiable => Is_Modifiable,
                                                            Is_Visable    => not Is_Password,
                                                            Action        => The_Action_Routine);
    begin
      Gui.Router.Request (Data);
      The_Checked_Edit_Box.The_Box := Data.Edit_Box;
      The_Checked_Edit_Box.Widget := Data.Widget;
    end;
    return The_Checked_Edit_Box;
  end Create;


  function Is_Checked (The_Checked_Edit_Box : Checked_Edit_Box) return Boolean is
    Data : Check_Enquiry_Data := (Gui.Router.Request_Data with Check_Box  => The_Checked_Edit_Box.The_Check_Box,
                                                               Is_Checked => False);
  begin
    Gui.Router.Request (Data);
    return Data.Is_Checked;
  end Is_Checked;


  procedure Set (The_Checked_Edit_Box : Checked_Edit_Box) is
    Data : constant Set_Check_Data := (Check_Box => The_Checked_Edit_Box.The_Check_Box,
                                       Is_Set    => True);
  begin
    Gui.Router.Send (Data);
  end Set;


  procedure Clear (The_Checked_Edit_Box : Checked_Edit_Box) is
    Data : constant Set_Check_Data := (Check_Box => The_Checked_Edit_Box.The_Check_Box,
                                       Is_Set    => False);
  begin
    Gui.Router.Send (Data);
  end Clear;

  ----------------------
  --
  -- Combo Box functions
  --
  ----------------------

  type Combo_Box_Data (Title_Size : Natural) is new Gui.Router.Request_Data with record
    Parent_Page   : Page;
    Combo_Box     : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
    The_Size      : Natural;
    Is_Modifiable : Boolean;
    The_Title     : String (1..Title_Size);
    Box_Size      : Natural;
    Action        : Action_Routine;
    Checked_Box   : Gtk.Check_Button.Gtk_Check_Button;
    Widget        : Gtk.Widget.Gtk_Widget;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Combo_Box_Data) is
    The_Box   : Gtk.Box.Gtk_Box;
    The_Label : Gtk.Label.Gtk_Label;
    use type Gtk.Check_Button.Gtk_Check_Button;
  begin
    Gtk.Box.Gtk_New_Hbox (The_Box);
    if Data.Checked_Box = null then
      Gtk.Label.Gtk_New (The_Label, Data.The_Title);
      Gtk.Misc.Set_Alignment (Gtk.Misc.Gtk_Misc (The_Label), 0.0, 0.5);
      if Data.Box_Size /= Automatic then
        The_Label.Set_Size_Request (Width => Glib.Gint(Data.Box_Size));
      end if;
      The_Box.Pack_Start (The_Label, Expand => False);
    else
      The_Box.Pack_Start (Data.Checked_Box, Expand => False);
    end if;
    if Data.Is_Modifiable then
      Gtk.Combo_Box_Text.Gtk_New_With_Entry (Data.Combo_Box);
    else
      Gtk.Combo_Box_Text.Gtk_New (Data.Combo_Box);
    end if;
    if Data.Action /= null then
      Action_Callback.Connect (Data.Combo_Box,
                               "changed",
                               Action_Callback.To_Marshaller(Action_Handler'access),
                               Data.Action);
    end if;
    if Data.The_Size = Automatic then
      The_Box.Pack_Start (Data.Combo_Box, True, True);
    else
      Data.Combo_Box.Set_Size_Request (Width  => Glib.Gint(Data.The_Size));
      The_Box.Pack_Start (Data.Combo_Box, Expand => False);
    end if;

    Data.Parent_Page.Page_Box.Pack_Start (The_Box, False, False);
    Data.Widget := Gtk.Widget.Gtk_Widget(The_Box);

  end Synchronous_Service;


  function Create  (Parent_Page        : Page;
                    The_Title          : String;
                    The_Action_Routine : Action_Routine := null;
                    Is_Modifiable      : Boolean := False;
                    The_Size           : Natural := Automatic;
                    The_Title_Size     : Natural := Automatic) return Plain_Combo_Box is
    Data : Combo_Box_Data := (Gui.Router.Request_Data with Combo_Box     => null, -- returned by service routine
                                                           Widget        => null, -- returned by service routine
                                                           Parent_Page   => Parent_Page,
                                                           The_Size      => The_Size,
                                                           Is_Modifiable => Is_Modifiable,
                                                           Title_Size    => The_Title'length,
                                                           The_Title     => The_Title,
                                                           Box_Size      => The_Title_Size,
                                                           Action        => The_Action_Routine,
                                                           Checked_Box   => null);
  begin
    Gui.Router.Request (Data);
    return (The_Box => Data.Combo_Box,
            Widget  => Data.Widget);
  end Create;


  type Add_Combo_Box_Data (Text_Size : Natural) is new Gui.Router.Message_Data with record
    Combo_Box : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
    The_Text  : String (1..Text_Size);
  end record;

  overriding
  procedure Asynchronous_Service (Data : Add_Combo_Box_Data) is
  begin
    Data.Combo_Box.Append (Data.The_Text, Data.The_Text);
  end Asynchronous_Service;

  procedure Add_Text (The_Combo_Box : Combo_Box'class;
                      The_Text      : String) is
    Data : constant Add_Combo_Box_Data := (Combo_Box => The_Combo_Box.The_Box,
                                           Text_Size => The_Text'length,
                                           The_Text  => The_Text);
  begin
    Gui.Router.Send (Data);
  end Add_Text;


  type Clear_Combo_Box_Data is new Gui.Router.Message_Data with record
    Combo_Box : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Clear_Combo_Box_Data) is
    The_Entry : Gtk.Widget.Gtk_Widget;
  begin
    The_Entry := Gtk.Bin.Get_Child (Gtk.Bin.Gtk_Bin (Data.Combo_Box));
    Gtk.GEntry.Set_Text (Gtk.GEntry.Gtk_Entry(The_Entry), "");
    Data.Combo_Box.Remove_All;
  end Asynchronous_Service;

  procedure Clear_Contents_Of (The_Combo_Box : Combo_Box'class) is
    Data : constant Clear_Combo_Box_Data := (Combo_Box => The_Combo_Box.The_Box);
  begin
    Gui.Router.Send (Data);
  end Clear_Contents_Of;


  type Select_Combo_Box_Data (Text_Size : Natural) is new Gui.Router.Message_Data with record
    Combo_Box : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
    The_Text  : String (1..Text_Size);
  end record;

  overriding
  procedure Asynchronous_Service (Data : Select_Combo_Box_Data) is
  begin
    if Data.Text_Size > 0 then
      if Data.Combo_Box.Set_Active_Id (Data.The_Text) then
        return;
      end if;
    end if;
    Data.Combo_Box.Set_Active (0); -- select first
  end Asynchronous_Service;

  procedure Select_Text (The_Combo_Box : Combo_Box'class;
                         The_Text      : String) is
    Data : constant Select_Combo_Box_Data := (Combo_Box => The_Combo_Box.The_Box,
                                              Text_Size => The_Text'length,
                                              The_Text  => The_Text);
  begin
    Gui.Router.Send (Data);
  end Select_Text;


  procedure Select_First (The_Combo_Box : Combo_Box'class) is
    Data : constant Select_Combo_Box_Data := (Combo_Box => The_Combo_Box.The_Box,
                                              Text_Size => 0,
                                              The_Text  => "");
  begin
    Gui.Router.Send (Data);
  end Select_First;


  type Deselect_Combo_Box_Data is new Gui.Router.Message_Data with record
    Combo_Box : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Deselect_Combo_Box_Data) is
  begin
    Data.Combo_Box.Set_Active (0); -- select first
  end Asynchronous_Service;

  procedure Deselect (The_Combo_Box : Combo_Box'class) is
    Data : constant Deselect_Combo_Box_Data := (Combo_Box => The_Combo_Box.The_Box);
  begin
    Gui.Router.Send (Data);
  end Deselect;


  type Get_Combo_Box_Data is new Gui.Router.Request_Data with record
    Combo_Box : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
    The_Text  : Text.String;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Get_Combo_Box_Data) is
  begin
    Data.The_Text := Text.String_Of (Data.Combo_Box.Get_Active_Text);
  end Synchronous_Service;

  function Contents_Of (The_Combo_Box : Combo_Box'class) return String is
    Data : Get_Combo_Box_Data := (Gui.Router.Request_Data with Combo_Box => The_Combo_Box.The_Box,
                                                               The_Text  => Text.Null_String);
  begin
    Gui.Router.Request (Data);
    return Text.String_Of (Data.The_Text);
  end Contents_Of;


  function Create (Parent_Page        : Page;
                   The_Title          : String;
                   The_Action_Routine : Action_Routine := null;
                   Is_Modifiable      : Boolean := False;
                   The_Size           : Natural := Automatic;
                   The_Title_Size     : Natural := Automatic) return Checked_Combo_Box is
    The_Checked_Combo_Box : Checked_Combo_Box;
    Check_Box_Data        : Create_Check_Box_Data
                          := (Gui.Router.Request_Data with Title_Size  => The_Title'length,
                                                           Checked_Box => null, -- returned by service routine
                                                           Parent_Page => null, -- part of composite object
                                                           Title       => The_Title,
                                                           Box_Width   => The_Title_Size,
                                                           Action      => The_Action_Routine);
  begin
    Gui.Router.Request (Check_Box_Data);
    The_Checked_Combo_Box.The_Check_Box := Check_Box_Data.Checked_Box;
    declare
      Creation_Data : Combo_Box_Data
                    := (Gui.Router.Request_Data with Combo_Box     => null,  -- returned by service routine
                                                     Widget        => null,  -- returned by service routine
                                                     Is_Modifiable => Is_Modifiable,
                                                     Title_Size    => 0,
                                                     The_Title     => "", -- Unused
                                                     Box_Size      => Automatic,
                                                     The_Size      => The_Size,
                                                     Checked_Box   => Check_Box_Data.Checked_Box,
                                                     Parent_Page   => Parent_Page,
                                                     Action        => The_Action_Routine);
    begin
      Gui.Router.Request (Creation_Data);
      The_Checked_Combo_Box.The_Box := Creation_Data.Combo_Box;
      The_Checked_Combo_Box.Widget := Creation_Data.Widget;
    end;
    return The_Checked_Combo_Box;
  end Create;


  function Is_Checked (The_Checked_Combo_Box : Checked_Combo_Box) return Boolean is
    Data : Check_Enquiry_Data := (Gui.Router.Request_Data with Check_Box  => The_Checked_Combo_Box.The_Check_Box,
                                                               Is_Checked => False);
  begin
    Gui.Router.Request (Data);
    return Data.Is_Checked;
  end Is_Checked;


  procedure Set (The_Checked_Combo_Box : Checked_Combo_Box) is
    Data : constant Set_Check_Data := (Check_Box => The_Checked_Combo_Box.The_Check_Box,
                                       Is_Set    => True);
  begin
    Gui.Router.Send (Data);
  end Set;


  procedure Clear (The_Checked_Combo_Box : Checked_Combo_Box) is
    Data : constant Set_Check_Data := (Check_Box => The_Checked_Combo_Box.The_Check_Box,
                                       Is_Set    => False);
  begin
    Gui.Router.Send (Data);
  end Clear;

  ----------------------
  --
  -- Tree View functions
  --
  ----------------------

  procedure Tree_Click_Expander (Widget            : access Gtk.Widget.Gtk_Widget_Record'class;
                                 The_Click_Routine : Click_Routine) is
    Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
    Model  : Gtk.Tree_Model.Gtk_Tree_Model;
    Path   : Gtk.Tree_Model.Gtk_Tree_Path;
    Value  : Glib.Values.GValue;
    Tree   : constant Gtk.Tree_View.Gtk_Tree_View := Gtk.Tree_View.Gtk_Tree_View (Widget);
    Unused : Boolean;
  begin
    Gtk.Tree_Selection.Get_Selected (Gtk.Tree_View.Get_Selection (Tree), Model, Iter);
    Path := Gtk.Tree_Model.Get_Path (Model, Iter);
    if Gtk.Tree_View.Row_Expanded (Tree, Path) then
      if The_Click_Routine = null then
        Unused := Gtk.Tree_View.Collapse_Row (Tree, Path);
      else
        Gtk.Tree_Model.Get_Value (Model, Iter, 0, Value);
        declare
          The_Information : constant Information := Information(Glib.Values.Get_Ulong(Value));
        begin
          if The_Information = No_Information then
            Unused := Gtk.Tree_View.Collapse_Row (Tree, Path);
          else
            Gui.Router.Execute (The_Click_Routine, The_Information);
          end if;
        end;
      end if;
    else
      Gtk.Tree_View.Expand_To_Path (Tree, Path);
      if not Gtk.Tree_View.Row_Expanded (Tree, Path) then
        if The_Click_Routine /= null then
          Gtk.Tree_Model.Get_Value (Model, Iter, 0, Value);
          declare
            The_Information : constant Information := Information(Glib.Values.Get_Ulong(Value));
          begin
            if The_Information /= No_Information then
              Gui.Router.Execute (The_Click_Routine, The_Information);
            end if;
          end;
        end if;
      end if;
    end if;
  end Tree_Click_Expander;


  procedure Tree_Click_Handler (Widget            : access Gtk.Widget.Gtk_Widget_Record'class;
                                The_Click_Routine : Click_Routine) is
    Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
    Model  : Gtk.Tree_Model.Gtk_Tree_Model;
    Value  : Glib.Values.GValue;
    Tree   : constant Gtk.Tree_View.Gtk_Tree_View := Gtk.Tree_View.Gtk_Tree_View (Widget);
    Unused : Boolean;
  begin
    Gtk.Tree_Selection.Get_Selected (Gtk.Tree_View.Get_Selection (Tree),
                                     Model, Iter);
    Gtk.Tree_Model.Get_Value (Model, Iter, 0, Value);
    declare
      The_Information : constant Information := Information(Glib.Values.Get_Ulong(Value));
    begin
      if The_Information /= No_Information then
        Gui.Router.Execute (The_Click_Routine, The_Information);
      end if;
    end;
  end Tree_Click_Handler;


  type Tree_View_Data is new Gui.Router.Request_Data with record
    Parent_Page     : Page;
    The_View        : Tree_View;
    Store           : Gtk.Tree_Store.Gtk_Tree_Store;
    The_Routine     : Click_Routine;
    Single_Click    : Boolean;
    Expand_By_Click : Boolean;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Tree_View_Data) is
    use Gtk.Tree_Store;
    Unused        : Glib.Gint;
    The_Types     : constant Glib.GType_Array (0..2) := (0      => Glib.GType_Ulong,
                                                         others => Glib.GType_String);
    Cell_Renderer : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
    The_Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
  begin
    Gtk.Tree_Store.Gtk_New (Data.Store, The_Types);
    Gtk.Tree_View.Gtk_New (Data.The_View.The_View, +Data.Store);
    Data.The_View.Widget := Gtk.Widget.Gtk_Widget(Data.The_View.The_View);
    Data.The_View.The_View.Set_Headers_Visible (False);
    Data.The_View.The_View.Set_Enable_Tree_Lines (True);
    Gtk.Cell_Renderer_Text.Gtk_New (Cell_Renderer);
    Cell_Renderer.Set_Padding (0, 0);
    Gtk.Tree_View_Column.Gtk_New (The_Column);
    The_Column.Pack_Start (Cell_Renderer, True);
    Unused := Gtk.Tree_View.Append_Column (Data.The_View.The_View, The_Column);
    The_Column.Add_Attribute (Cell_Renderer, "foreground", 1);
    The_Column.Add_Attribute (Cell_Renderer, "text", 2);
    if Data.Expand_By_Click then
      Click_Callback.Connect (Data.The_View.The_View,
                              "row-activated",
                              Click_Callback.To_Marshaller(Tree_Click_Expander'access),
                              Data.The_Routine);
    elsif Data.The_Routine /= null then
      Click_Callback.Connect (Data.The_View.The_View,
                              "row-activated",
                              Click_Callback.To_Marshaller(Tree_Click_Handler'access),
                              Data.The_Routine);
    end if;
    Data.The_View.The_View.Set_Activate_On_Single_Click (Data.Single_Click);

    Add_Paned_Widget (Data.Parent_Page, Data.The_View.Widget);
  end Synchronous_Service;

  function Create (Parent_Page       : Page;
                   The_Click_Routine : Click_Routine   := null;
                   The_Color_Handler : Color_Handler   := null;
                   Style             : Tree_View_Style := (others => <>)) return Tree_View is
    Data : Tree_View_Data := (Gui.Router.Request_Data with Parent_Page     => Parent_Page,
                                                           The_View         => (others => <>),
                                                           Store           => null, -- Returned by call
                                                           Single_Click    => Style.The_Click_Kind = Single_Click,
                                                           Expand_By_Click => Style.Expand_By_Click,
                                                           The_Routine     => The_Click_Routine);
  begin
    Gui.Router.Request (Data);
    Data.The_View.The_Store := Data.Store;
    Data.The_View.The_Color_Handler := The_Color_Handler;
    return Data.The_View;
  end Create;


  type Add_Item_Data (Text_Size : Natural) is new Gui.Router.Request_Data with record
    The_Store         : Gtk.Tree_Store.Gtk_Tree_Store;
    The_Parent        : Tree_Item;
    Position_At_Front : Boolean;
    The_Information   : Information;
    The_Text          : String (1..Text_Size);
    The_Handler       : Color_Handler;
    The_Item          : Tree_Item;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Add_Item_Data) is
  begin
    if Data.Position_At_Front then
      Data.The_Store.Insert (Data.The_Item, Data.The_Parent, 0);
    else
      Data.The_Store.Append (Data.The_Item, Data.The_Parent);
    end if;
    declare
      The_Value : Glib.Values.GValue;
    begin
      Glib.Values.Init (The_Value, Glib.GType_Ulong);
      Glib.Values.Set_Ulong (The_Value, Glib.Gulong (Data.The_Information));
      Data.The_Store.Set_Value (Data.The_Item, 0, The_Value);
      Glib.Values.Unset (The_Value);
    end;
    if Data.The_Handler = null then
      Data.The_Store.Set (Data.The_Item, 1, "Black");
    else
      Data.The_Store.Set (Data.The_Item, 1, Data.The_Handler (Data.The_Information)'img);
    end if;
    Data.The_Store.Set (Data.The_Item, 2, Data.The_Text);
  end Synchronous_Service;


  function Add_Item (The_Tree_View     : Tree_View;
                     The_Title         : String;
                     The_Parent        : Tree_Item := Root_Item;
                     Position_At_Front : Boolean := False;  -- otherwise position at front
                     The_Information   : Information := No_Information) return Tree_Item is
    Data : Add_Item_Data := (Gui.Router.Request_Data with Text_Size         => The_Title'length,
                                                          The_Store         => The_Tree_View.The_Store,
                                                          The_Parent        => The_Parent,
                                                          Position_At_Front => Position_At_Front,
                                                          The_Information   => The_Information,
                                                          The_Text          => The_Title,
                                                          The_Handler       => The_Tree_View.The_Color_Handler,
                                                          The_Item          => Gtk.Tree_Model.Null_Iter);
  begin
    Gui.Router.Request (Data);
    return Data.The_Item;
  end Add_Item;


  type Expand_Item_Data is new Gui.Router.Message_Data with record
    The_View   : Gtk.Tree_View.Gtk_Tree_View;
    The_Store  : Gtk.Tree_Store.Gtk_Tree_Store;
    The_Item   : Tree_Item;
    Expand_All : Boolean;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Expand_Item_Data) is
    Path   : Gtk.Tree_Model.Gtk_Tree_Path;
    Unused : Boolean;
  begin
    Path := Data.The_Store.Get_Path (Data.The_Item);
    Unused := Data.The_View.Expand_Row (Path, Data.Expand_All);
  end Asynchronous_Service;

  procedure Expand (The_Tree_View : Tree_View;
                    The_Tree_Item : Tree_Item;
                    Expand_All    : Boolean := False) is
    Data : constant Expand_Item_Data := (The_View   => The_Tree_View.The_View,
                                         The_Store  => The_Tree_View.The_Store,
                                         The_Item   => The_Tree_Item,
                                         Expand_All => Expand_All);
  begin
    Gui.Router.Send (Data);
  end Expand;


  type Delete_All_Data is new Gui.Router.Message_Data with record
    The_Store : Gtk.Tree_Store.Gtk_Tree_Store;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Delete_All_Data) is
  begin
    Data.The_Store.Clear;
  end Asynchronous_Service;


  procedure Delete_All_Items (The_Tree_View : Tree_View) is
    use type Gtk.Tree_Store.Gtk_Tree_Store;
  begin
    if The_Tree_View.The_Store /= null then
      declare
        Data : constant Delete_All_Data := (The_Store => The_Tree_View.The_Store);
      begin
        Gui.Router.Send (Data);
      end;
    end if;
  end Delete_All_Items;

begin
  Gtk.Main.Init;
  Define_Color_Tags;
end Gui;
