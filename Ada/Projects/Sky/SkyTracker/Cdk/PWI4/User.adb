-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Real_Time;
with Ada.Unchecked_Conversion;
with Application;
with Cwe;
with Gui.Enumeration_Menu_Of;
with Gui.Registered;
with Lexicon;
with Name.Catalog;
with Persistent;
with Remote;
with Sky.Catalog;
with Targets.Filter;
with Traces;

package body User is

  package Log is new Traces ("User");

  Page         : Gui.Page;
  Left_Button  : Gui.Button;
  Right_Button : Gui.Button;
  Progress_Bar : Gui.Progress_Bar;
  Target       : Gui.Plain_Edit_Box;
  Description  : Gui.Plain_Edit_Box;
  Display      : Gui.List_View;

  type Target_Selection is (No_Target, Target_Object);

  The_Target_Selection  : Target_Selection := No_Target;
  Last_Target_Selection : Target_Selection := Target_Object;

  The_Status : Telescope.State := Telescope.Unknown;

  Action_Routine   : Action_Handler;
  The_Last_Action  : Action := Action'pred (Button_Action'first);
  Last_Action_Time : Ada.Real_Time.Time := Ada.Real_Time.Time_First;


  procedure Signal_Action (The_Action : Action) is
    Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
    use type Ada.Real_Time.Time;
  begin
    if Action_Routine /= null then
      if The_Action in Button_Action and The_Last_Action = The_Action then
        if Now < Last_Action_Time + Ada.Real_Time.Milliseconds (500) then
          Log.Write ("Double Click on " & The_Action'img);
          return;
        end if;
      end if;
      Log.Write ("Signal: " & The_Action'img);
      Action_Routine.all (The_Action);
      Last_Action_Time := Now;
      The_Last_Action := The_Action;
    end if;
  end Signal_Action;


  procedure Set_Target_Name (Item : String) is
  begin
    if Item = "" then
      The_Target_Selection := No_Target;
    else
      Log.Write ("Target: " & Item);
      The_Target_Selection := Target_Object;
    end if;
    Gui.Set_Text (Target, Item);
    Gui.Set_Text (Description, "");
  end Set_Target_Name;


  function Image_Of (The_Mode : Cwe.Mode) return String is
  begin
    case The_Mode is
    when Cwe.Off =>
      return Lexicon.Image_Of (Lexicon.Off);
    when Cwe.On =>
      return Lexicon.Image_Of (Lexicon.On);
    end case;
  end Image_Of;

  package Cwe_Menu is new Gui.Enumeration_Menu_Of (Cwe.Mode, Gui.Radio, Image_Of);

  procedure Cwe_Handler (The_Mode : Cwe.Mode) is
  begin
    Log.Write ("CWE: " & The_Mode'img);
    Cwe.Set (To => The_Mode);
  exception
  when others =>
    Log.Error ("Cwe_Handler");
  end Cwe_Handler;


  function Image_Of (The_Command : Remote.Command) return String is
  begin
    case The_Command is
    when Remote.Start_Session =>
      return Lexicon.Image_Of (Lexicon.Start);
    when Remote.Generate_Qr_Code =>
      return Lexicon.Image_Of (Lexicon.Qr_Code);
    when Remote.End_Session =>
      return Lexicon.Image_Of (Lexicon.Finish);
    end case;
  end Image_Of;

  package Demo_21_Menu is new Gui.Enumeration_Menu_Of (Remote.Command, Gui.Plain, Image_Of);

  procedure Demo_21_Handler (The_Command : Remote.Command) is
  begin
    Remote.Execute (The_Command);
  exception
  when others =>
    Log.Error ("Demo_21_Handler");
  end Demo_21_Handler;


  procedure Show_Error (The_Text : String := Error.Message) is
  begin
    Gui.Beep;
    Gui.Message_Box (The_Text);
    Log.Warning ("Show Error: " & The_Text);
  end Show_Error;


  procedure Show (The_Progress : Percent) is
  begin
    Gui.Report_Progress (Progress_Bar, Natural(The_Progress));
  end Show;


  procedure Show (Visible_In : Duration) is
  begin
    Show_Description (Targets.Text_Of (Visible_In));
  end Show;


  Perform_Left_Handler : access procedure := null;

  procedure Perform_Left is
  begin
    Perform_Left_Handler.all;
  end Perform_Left;


  Perform_Right_Handler : access procedure := null;

  procedure Perform_Right is
  begin
    Perform_Right_Handler.all;
  end Perform_Right;


  procedure Perform_Startup is
  begin
    Signal_Action (Startup);
  end Perform_Startup;


  procedure Perform_Shutdown is
  begin
    Signal_Action (Shutdown);
  end Perform_Shutdown;


  procedure Set_Startup_Text is
  begin
    Gui.Set_Text (Left_Button, "Startup");
  end Set_Startup_Text;


  procedure Set_Goto_Text is
  begin
    Gui.Set_Text (Left_Button, "Goto");
  end Set_Goto_Text;


  procedure Set_Stop_Text is
  begin
    Gui.Set_Text (Right_Button, "Stop");
  end Set_Stop_Text;


  procedure Set_Shutdown_Text is
  begin
    if Telescope.Park_Position_Defined then
      Gui.Set_Text (Right_Button, "Park");
    else
      Gui.Set_Text (Right_Button, "Shutdown");
    end if;
  end Set_Shutdown_Text;


  procedure Disable_Startup_Button is
  begin
    Perform_Left_Handler := null;
    Set_Startup_Text;
    Gui.Disable (Left_Button);
  end Disable_Startup_Button;


  procedure Enable_Startup_Button is
  begin
    Perform_Left_Handler := Perform_Startup'access;
    Set_Startup_Text;
    Gui.Enable (Left_Button);
  end Enable_Startup_Button;


  procedure Disable_Stop_Button is
  begin
    Perform_Right_Handler := null;
    Set_Stop_Text;
    Gui.Disable (Right_Button);
  end Disable_Stop_Button;


  procedure Enable_Stop_Button is
  begin
    Perform_Right_Handler := Perform_Stop'access;
    Set_Stop_Text;
    Gui.Enable (Right_Button);
  end Enable_Stop_Button;


  procedure Disable_Goto_Button is
  begin
    Perform_Left_Handler := null;
    Set_Goto_Text;
    Gui.Disable (Left_Button);
  end Disable_Goto_Button;


  procedure Enable_Goto_Button is
  begin
    Set_Goto_Text;
    if The_Target_Selection = Target_Object then
      Perform_Left_Handler := Perform_Goto'access;
      Gui.Enable (Left_Button);
    else
      Perform_Left_Handler := null;
      Disable_Goto_Button;
    end if;
  end Enable_Goto_Button;


  procedure Enable_Shutdown_Button is
  begin
    Perform_Right_Handler := Perform_Shutdown'access;
    Set_Shutdown_Text;
    Gui.Enable (Right_Button);
  end Enable_Shutdown_Button;


  procedure Disable_Shutdown_Button is
  begin
    Perform_Right_Handler := null;
    Set_Shutdown_Text;
    Gui.Disable (Right_Button);
  end Disable_Shutdown_Button;


  procedure Show (Information : Telescope.Data) is
    use type Telescope.State;
  begin
    if (The_Status /= Information.Status) or (Last_Target_Selection /= The_Target_Selection) then
      The_Status := Information.Status;
      Last_Target_Selection := The_Target_Selection;
      case The_Status is
      when Telescope.Unknown | Telescope.Restarting | Telescope.Mount_Error =>
        Disable_Startup_Button;
        Disable_Stop_Button;
      when Telescope.Disconnected =>
        Enable_Startup_Button;
        Disable_Shutdown_Button;
      when Telescope.Connected | Telescope.Enabled =>
        Enable_Startup_Button;
        Enable_Shutdown_Button;
      when Telescope.Disconnecting | Telescope.Disabling | Telescope.Parking =>
        Disable_Startup_Button;
        Enable_Stop_Button;
      when Telescope.Connecting | Telescope.Enabling | Telescope.Homing =>
        Disable_Startup_Button;
        Enable_Stop_Button;
      when Telescope.Stopped =>
        Enable_Goto_Button;
        Enable_Shutdown_Button;
      when Telescope.Positioned | Telescope.Waiting =>
        Enable_Stop_Button;
        Enable_Goto_Button;
      when Telescope.Positioning | Telescope.Preparing | Telescope.Approaching | Telescope.Is_Tracking =>
        Enable_Goto_Button;
        Enable_Stop_Button;
      when Telescope.Solving =>
        Disable_Goto_Button;
        Enable_Stop_Button;
      when Telescope.Stopping =>
        Disable_Goto_Button;
        Disable_Stop_Button;
      end case;
    end if;
    Gui.Set_Status_Line (Information.Status'img);
  end Show;


  procedure Set (The_Target : Name.Id) is
  begin
    Set_Target_Name (Name.Image_Of (The_Target));
    Signal_Action (Define_Target);
    Perform_Goto;
  end Set;


  procedure Clear_Target is
  begin
    Set_Target_Name ("");
  end Clear_Target;


  procedure Perform_Goto is
  begin
    Cwe.New_Offset;
    Signal_Action (Go_To);
  end Perform_Goto;


  procedure Perform_Stop is
  begin
    Signal_Action (Stop);
  end Perform_Stop;


  function Target_Name return String is
  begin
    return Gui.Contents_Of (Target);
  end Target_Name;


  procedure Show_Description (Image : String) is
  begin
    Gui.Set_Text (Description, Image);
  end Show_Description;


  function Window_Minimized return Boolean is (Gui.Application_Is_Minimized);


  The_Targets : Name.Id_List_Access;


  function Convertion is new Ada.Unchecked_Conversion (Gui.Information, Name.Id_Access);

  function Display_Text_Handler (For_Column       : Natural;
                                 With_Information : Gui.Information) return String is
    Name_Id : constant Name.Id_Access := Convertion (With_Information);
    use type Name.Id_Access;
  begin
    if For_Column = 0 then
      if Name_Id = null then
        return "";
      else
        return Name.Image_Of (Name_Id.all);
      end if;
    else
      raise Program_Error;
    end if;
  exception
  when others =>
    Log.Error ("Display_Text_Handler failed");
    return "";
  end Display_Text_Handler;


  procedure Select_Target (Item : Gui.Information) is
    Name_Id : constant Name.Id_Access := Convertion (Item);
    use type Name.Id_Access;
  begin
    if Name_Id /= null then
      Set_Target_Name (Name.Image_Of (Name_Id.all));
    end if;
    Signal_Action (Define_Target);
  exception
  when others =>
    Log.Error ("Select_Target failed");
  end Select_Target;


  type Display_Data is record
    Width : Natural;
  end record;

  package Persistent_Display is new Persistent (Display_Data, "Display");

  The_Persistent_Display_Data : Persistent_Display.Data;

  The_Display_Data : Display_Data renames The_Persistent_Display_Data.Storage;

  The_Targets_Column : Gui.Column;


  procedure Define_Signal is
  begin
    Signal_Action (Define_Catalog);
  end Define_Signal;


  procedure Update_Signal is
  begin
    Signal_Action (Update);
  end Update_Signal;


  procedure Execute (The_Startup_Handler     : not null access procedure;
                     The_Action_Handler      : Action_Handler;
                     The_Termination_Handler : not null access procedure) is

    Windows_Width  : constant := 310;
    Windows_Height : constant := 800;
    Minimum_Width  : constant := 200;
    Separation     : constant := 10;

    procedure Create_Interface is

      procedure Define_Page is
        Target_Image : constant String  := Lexicon.Image_Of (Lexicon.Target);
        Target_Size  : constant Natural := Gui.Text_Size_Of (Target_Image) + Separation;
      begin
        Page := Gui.Add_Page (The_Title  => Lexicon.Image_Of (Lexicon.Ocular),
                              The_Style  => [Gui.Buttons_Fill_Horizontally => True,
                                             Gui.Buttons_Fill_Vertically   => False]);
        Left_Button := Gui.Create (Page, "", Perform_Left'access);
        Gui.Disable (Left_Button);
        Right_Button := Gui.Create (Page, "", Perform_Right'access);
        Gui.Disable (Right_Button);

        Progress_Bar := Gui.Create (Page);
        Gui.Define_Range (Progress_Bar, Natural(Percent'last));

        Target := Gui.Create (Page, Target_Image, "", The_Title_Size => Target_Size, Is_Modifiable  => False);
        Description := Gui.Create (Page, "", "", Is_Modifiable  => False);

        Display := Gui.Create (Parent_Page           => Page,
                               The_Text_Handler      => Display_Text_Handler'access,
                               The_Click_Routine     => Select_Target'access,
                               The_Click_Kind        => Gui.Single_Click,
                               Use_Proportional_Font => True);
        if Persistent_Display.Storage_Is_Empty then
          The_Display_Data := (Width => Windows_Width - 35);
        end if;
        if The_Display_Data.Width < Minimum_Width then
          The_Display_Data.Width := Minimum_Width;
        end if;
        The_Targets_Column := Gui.Add_Column (The_List_View     => Display,
                                              The_Title         => "",
                                              The_Width         => The_Display_Data.Width,
                                              The_Justification => Gui.Left);
      end Define_Page;

    begin -- Create_Interface
      Name.Catalog.Create_Menu (Define_Signal'access);
      Targets.Filter.Create_Menu (Update_Signal'access);
      Cwe_Menu.Create ("CWE", Cwe_Handler'access);
      if Remote.Configured then
        Demo_21_Menu.Create ("Demo 21", Demo_21_Handler'access);
      end if;
      Define_Page;
      The_Startup_Handler.all;
    exception
    when Item: others =>
      Log.Termination (Item);
    end Create_Interface;


    procedure Termination is
    begin
      The_Display_Data.Width := Gui.Width_Of (The_Targets_Column);
      Signal_Action (Close);
      Log.Write ("Teminating");
      The_Termination_Handler.all;
    exception
    when others =>
      Log.Error ("Termination failed");
    end Termination;

  begin -- Execute
    Action_Routine := The_Action_Handler;
    Gui.Registered.Execute (The_Application_Name    => Application.Title,
                            The_Startup_Routine     => Create_Interface'access,
                            The_Termination_Routine => Termination'access,
                            Initial_Metrics         => (Width  => Windows_Width,
                                                        Height => Windows_Height,
                                                        others => <>));
    Log.Write ("Teminated");
  end Execute;


  procedure Clear_Targets is
  begin
    Gui.Hide (Display);
    Gui.Remove_All_From (Display);
  end Clear_Targets;


  procedure Define (New_Targets : Name.Id_List_Access) is
  begin
    The_Targets := New_Targets;
    Gui.Set_Title (The_Targets_Column, Sky.Catalog.Image_Of (The_Targets.Kind));
    Update_Targets;
    Gui.Show (Display);
  end Define;


  procedure Update_Targets is

    procedure Remove_Target (Index : Natural) is
    begin
      Gui.Remove_From (Display, Index);
    end Remove_Target;

    procedure Insert_Target (Item  : Name.Id_Access;
                             Index : Natural) is
      function Convert is new Ada.Unchecked_Conversion (Name.Id_Access, Gui.Information);
    begin
      Gui.Add_To (Display, Index, Convert(Item));
    end Insert_Target;

  begin -- Update
    Name.Update (The_Targets, Remove_Target'access, Insert_Target'access);
  end Update_Targets;

end User;
