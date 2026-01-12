-- *********************************************************************************************************************
-- *                       (c) 2025 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with C.Helper;
with Camera.Canon.C_Interface;
with Camera.Raw;
with System;
with Traces;

package body Camera.Canon is

  package Log is new Traces ("Canon");

  package CI renames Camera.Canon.C_Interface;
  package RT renames Ada.Real_Time;

  task type Control with  Priority => System.Max_Priority is

    entry Get (Is_Ready : out Boolean);

    entry Capture_Picture (Filename : String;
                           Time     : Exposure.Item;
                           Iso      : Sensitivity.Item);

    entry Capture_Grid (Size : Square_Size;
                        Time : Exposure.Item;
                        Iso  : Sensitivity.Item);

    entry Await_Stop;

    entry Shutdown;

  end Control;

  The_Control : access Control;


  procedure Start_Control is
  begin
    The_Control := new Control;
  end Start_Control;


  ----------------------------------------------------------
  -- Shared state to pass info from callback to main task --
  ----------------------------------------------------------
  protected Event_State is

    procedure Reset;

    procedure Set (Item : CI.Directory_Item);

    entry Wait_For (Item : out CI.Directory_Item);

  private
    Got_Item : Boolean := False;
    Dir_Item : CI.Directory_Item := CI.No_Directory;
  end Event_State;


  protected body Event_State is

    procedure Reset is
    begin
      Got_Item := False;
      Dir_Item := CI.No_Directory;
    end Reset;

    procedure Set (Item : CI.Directory_Item) is
    begin
      Got_Item := True;
      Dir_Item := Item;
    end Set;

    entry Wait_For (Item : out CI.Directory_Item) when Got_Item is
    begin
      Item     := Dir_Item;
      Got_Item := False;
      Dir_Item := CI.No_Directory;
    end Wait_For;

  end Event_State;


  function Is_Available return Boolean is
    Is_Ready : Boolean;
  begin
    The_Control.Get (Is_Ready);
    return Is_Ready;
  end Is_Available;


  procedure Capture_Picture (Filename  : String;
                             Time      : Exposure.Item;
                             Parameter : Sensitivity.Item) is
  begin
    The_Control.Capture_Picture (Filename, Time, Parameter);
  end Capture_Picture;


  procedure Capture_Grid (Size      : Square_Size;
                          Time      : Exposure.Item;
                          Parameter : Sensitivity.Item) is
  begin
    The_Control.Capture_Grid (Size, Time, Parameter);
  end Capture_Grid;


  procedure Stop_Capture is
  begin
    The_Control.Await_Stop;
  end Stop_Capture;


  procedure End_Control is
  begin
    The_Control.Shutdown;
  end End_Control;


  ---------------------------
  -- Object event callback --
  ---------------------------
  -- Canon EDSDK lifetime rule:
  --   Directory items received in this callback are only guaranteed to be
  --   valid *during* the execution of the callback itself.  If we want to
  --   use the directory item later (e.g. in another task to download the
  --   image), we MUST call EdsRetain here to increment the reference
  --   count.  The control task will eventually call EdsRelease to balance
  --   this retain.  Without this retain the SDK may destroy the object
  --   immediately after the callback returns, leading to crashes or
  --   download failures.
  function On_Object_Event (Event   : CI.Object_Event;
                            Object  : CI.Directory_Item;
                            Context : System.Address) return CI.Result
    with Convention => StdCall;

  function On_Object_Event (Event   : CI.Object_Event;
                            Object  : CI.Directory_Item;
                            Context : System.Address) return CI.Result
  is
    pragma Unreferenced (Context);
    Count : CI.Ref_Count;
  begin
    case Event is
    when CI.Object_Event_Dir_Item_Created => --| CI.Object_Event_Dir_Item_Request_Transfer =>
      Count := CI.Retain (Object);
      Log.Write ("Received file - Event:" & Event'image & " - Count:" & Count'image);
      Event_State.Set (Object);
    when others =>
      null;
    end case;
    return CI.OK;
  end On_Object_Event;

  Handler : constant CI.Object_Event_Handler := On_Object_Event'access;


  -------------
  -- Control --
  -------------

  task body Control is

    Default_Filename : constant String := "Raw_Picture.CR2";

    The_Camera : Canon_Model;

    ------------------------
    -- Check return codes --
    ------------------------
    procedure Check (Where   : String;
                     Result  : CI.Result;
                     Logging : Boolean := True) is
      use type CI.Result;
    begin
      if Logging then
        Log.Write (Where);
      end if;
      if Result /= CI.OK then
        Raise_Error (Where & " failed, error =" & CI.Result'image (Result));
      end if;
    end Check;

    -------------------------------------------------
    -- Local mapping for ISO to  Canon enum values --
    -------------------------------------------------
    function To_K_Iso (Item : Sensitivity.Item) return CI.ISO_Speed is
      Iso : constant Sensitivity.Iso := Item.Value;
    begin
      case Iso is
        when 100    => return CI.K_ISO_100;
        when 125    => return CI.K_ISO_125;
        when 160    => return CI.K_ISO_160;
        when 200    => return CI.K_ISO_200;
        when 250    => return CI.K_ISO_250;
        when 320    => return CI.K_ISO_320;
        when 400    => return CI.K_ISO_400;
        when 500    => return CI.K_ISO_500;
        when 640    => return CI.K_ISO_640;
        when 800    => return CI.K_ISO_800;
        when 1000   => return CI.K_ISO_1000;
        when 1250   => return CI.K_ISO_1250;
        when 1600   => return CI.K_ISO_1600;
        when 2000   => return CI.K_ISO_2000;
        when 2500   => return CI.K_ISO_2500;
        when 3200   => return CI.K_ISO_3200;
        when 4000   => return CI.K_ISO_4000;
        when 5000   => return CI.K_ISO_5000;
        when 6400   => return CI.K_ISO_6400;
        when others => null;
        end case;
      case The_Camera is
      when Canon_Eos_6D =>
        case Iso is
          when 12800  => return CI.K_ISO_12800;
          when 25600  => return CI.K_ISO_25600;
          when others => null;
        end case;
      when Canon_Eos_60D =>
        case Iso is
          when others => null;
        end case;
      end case;
      Raise_Error ("Iso value " & Iso'image & " not valid for " & The_Camera'image);
    end To_K_Iso;

    -----------------------------------------------
    -- Local mapping for Tv to Canon enum values --
    -----------------------------------------------
    function To_K_Tv (Item : Exposure.Item) return CI.Tv_Value is
      Tv : constant Exposure.Tv := Item.Time_Value;
    begin
      case Tv is
        when Exposure.Tv_0_8_S    => return CI.K_TV_0_8;
        when Exposure.Tv_0_6_S    => return CI.K_TV_0_6;
        when Exposure.Tv_0_5_S    => return CI.K_TV_0_5;
        when Exposure.Tv_0_4_S    => return CI.K_TV_0_4;
        when Exposure.Tv_0_3_S    => return CI.K_TV_0_3_S;
        when Exposure.Tv_D_4_S    => return CI.K_TV_D_4;
        when Exposure.Tv_D_5_S    => return CI.K_TV_D_5;
        when Exposure.Tv_D_6_S    => return CI.K_TV_D_6_S;
        when Exposure.Tv_D_8_S    => return CI.K_TV_D_8;
        when Exposure.Tv_D_10_S   => return CI.K_TV_D_10_S;
        when Exposure.Tv_D_13_S   => return CI.K_TV_D_13;
        when Exposure.Tv_D_15_S   => return CI.K_TV_D_15;
        when Exposure.Tv_D_20_S   => return CI.K_TV_D_20_S;
        when Exposure.Tv_D_25_S   => return CI.K_TV_D_25;
        when Exposure.Tv_D_30_S   => return CI.K_TV_D_30;
        when Exposure.Tv_D_40_S   => return CI.K_TV_D_40;
        when Exposure.Tv_D_50_S   => return CI.K_TV_D_50;
        when Exposure.Tv_D_60_S   => return CI.K_TV_D_60;
        when Exposure.Tv_D_80_S   => return CI.K_TV_D_80;
        when Exposure.Tv_D_100_S  => return CI.K_TV_D_100;
        when Exposure.Tv_D_125_S  => return CI.K_TV_D_125;
        when Exposure.Tv_D_160_S  => return CI.K_TV_D_160;
        when Exposure.Tv_D_200_S  => return CI.K_TV_D_200;
        when Exposure.Tv_D_250_S  => return CI.K_TV_D_250;
        when Exposure.Tv_D_320_S  => return CI.K_TV_D_320;
        when Exposure.Tv_D_400_S  => return CI.K_TV_D_400;
        when Exposure.Tv_D_500_S  => return CI.K_TV_D_500;
        when Exposure.Tv_D_640_S  => return CI.K_TV_D_640;
        when Exposure.Tv_D_800_S  => return CI.K_TV_D_800;
        when Exposure.Tv_D_1000_S => return CI.K_TV_D_1000;
        when Exposure.Tv_D_1250_S => return CI.K_TV_D_1250;
        when Exposure.Tv_D_1600_S => return CI.K_TV_D_1600;
        when Exposure.Tv_D_2000_S => return CI.K_TV_D_2000;
        when Exposure.Tv_D_2500_S => return CI.K_TV_D_2500;
        when Exposure.Tv_D_3200_S => return CI.K_TV_D_3200;
        when Exposure.Tv_D_4000_S => return CI.K_TV_D_4000;
        when others => null;
      end case;
      case The_Camera is
      when Canon_Eos_6D =>
        null;
      when Canon_Eos_60D =>
        case Tv is
        when Exposure.Tv_D_5000_S => return CI.K_TV_D_5000;
        when Exposure.Tv_D_6400_S => return CI.K_TV_D_6400;
        when Exposure.Tv_D_8000_S => return CI.K_TV_D_8000;
        when others => null;
        end case;
      end case;
      Raise_Error ("Tv value " & Item'image & " not valid for " & The_Camera'image);
      return CI.Tv_Value'last; --!!!WE!!! GNAT Bug
    end To_K_Tv;

    The_Filename : Text.String := [Default_Filename];
    The_Exposure : Exposure.Item;
    The_Iso      : Sensitivity.Item;

    Max_Tv_Time       : constant Duration := 30.0;
    Max_Download_Time : constant Duration := 15.0;
    AE_Mode_Set_Time  : constant Duration := 0.1;

    Delta_Time : constant RT.Time_Span := RT.To_Time_Span (1.0 / 5);

    use type RT.Time;

    type Session is record
      Device_List : aliased CI.Device_List;
      Device      : aliased CI.Device;
      Device_Info : aliased CI.Device_Info;
    end record;

    The_Session : Session;
    The_Item    : CI.Directory_Item := CI.No_Directory;

    Is_Cropping   : Boolean;
    The_Grid_Size : Square_Size;


    procedure Set (Where_Label : String;
                   Property    : CI.Property_Id;
                   Value       : CI.Uint32;
                   Param       : CI.Int32 := 0)
    is
      Val : aliased constant CI.Uint32 := Value;
      use type CI.Uint32;
    begin
      Check (Where_Label,
             CI.Set_Property_Data (Item     => The_Session.Device,
                                   Property => Property,
                                   Param    => Param,
                                   Size     => CI.Uint32'size / System.Storage_Unit,
                                   Data     => Val'address));
    end Set;


    procedure Get (Property    :     CI.Property_Id;
                   Value       : out CI.Uint32;
                   Param       :     CI.Int32 := 0)
    is
      Val : aliased CI.Uint32;
      use type CI.Uint32;
      What : constant String := Text.Legible_Of (Property'image);
    begin
      Check (What,
             CI.Get_Property_Data (Item     => The_Session.Device,
                                   Property => Property,
                                   Param    => Param,
                                   Size     => CI.Uint32'size / System.Storage_Unit,
                                   Data     => Val'address));
      Value := Val;
      Log.Write (What & ":" & Value'image);
    end Get;


    procedure Command (What  : String;
                       Item  : CI.Camera_Command;
                       Param : CI.Int32 := 0) is
    begin
      Check (What, CI.Send_Command (The_Session.Device, Item, Param));
    end Command;


    procedure Disable_Electronic_View_Finder is
      use type CI.Uint32;
    begin
      Set (Property    => CI.Prop_Id_Evf_Output_Device,
           Value       => CI.Evf_Output_None'enum_rep,
           Where_Label => "Disable EVF output");
      loop
        declare
          Value : CI.Uint32;
        begin
          Get (CI.Prop_Id_Evf_Output_Device, Value);
          exit when Value = CI.Evf_Output_None'enum_rep;
          delay 0.1;
        end;
      end loop;
    end Disable_Electronic_View_Finder;


    procedure Release_Device_List_And_Terminate_SDK is
      Dummy_Result : CI.Result;
      Dummy_Count  : CI.Ref_Count;
    begin
      begin
        Dummy_Count := CI.Release (The_Session.Device_List);
      exception
      when others =>
        null;
      end;
      begin
        Dummy_Result := CI.Terminate_SDK;
      exception
      when others =>
        null;
      end;
    end Release_Device_List_And_Terminate_SDK;


    procedure Disconnect (Next_State : Status := Idle) is
      Dummy_Result : CI.Result;
      Dummy_Count  : CI.Ref_Count;
      use type CI.Directory_Item;
    begin
      Log.Write ("Disconnect");
      if The_Item /= CI.No_Directory then
        begin
          Check ("Delete file on camera",  CI.Delete_Directory_Item (The_Item));
          declare
            Count : constant CI.Ref_Count := CI.Release (The_Item);
          begin
            Log.Write ("Release directory item, Count:" & Count'image);
          end;
          The_Item := CI.No_Directory;
        exception
        when others =>
          null;
        end;
      end if;
      begin
        Dummy_Result := CI.Send_Status_Command (The_Session.Device, CI.Camera_Status_UI_Unlock, 0);
      exception
      when others =>
        null;
      end;
      begin
        Dummy_Result := CI.Close_Session (The_Session.Device);
      exception
      when others =>
        null;
      end;
      begin
        Dummy_Count := CI.Release (The_Session.Device);
      exception
      when others =>
        null;
      end;
      Release_Device_List_And_Terminate_SDK;
      Camera_Data.Set (Next_State);
    end Disconnect;


    procedure Internal_Error (Item : Exceptions.Occurrence) is
    begin
      Disconnect;
      Camera_Data.Set_Fatal (Item);
    end Internal_Error;

    ---------------------------------------------
    -- Is_One_Device_Ready                    --
    --   Initializes EDSDK                    --
    --   Gets the number of connected cameras --
    ---------------------------------------------
    function One_Device_Is_Ready return Boolean is
      Dummy_Result : CI.Result;
      Count : aliased CI.Uint32;
      use type CI.Uint32;
    begin
      Camera_Data.Check (Idle);
      Dummy_Result := CI.Initialize_SDK;
      Dummy_Result := CI.Get_Camera_List (The_Session.Device_List'access);
      Dummy_Result := CI.Get_Child_Count (The_Session.Device_List, Count'access);
      Log.Write ("Number of detected cameras:" & CI.Uint32'image (Count));
      Release_Device_List_And_Terminate_SDK;
      return Count = 1;
    exception
    when others =>
      Release_Device_List_And_Terminate_SDK;
      return False;
    end One_Device_Is_Ready;

    --------------------------------------------
    -- Open                                  --
    --   Initializes EDSDK                   --
    --   Finds the first connected Canon EOS --
    --   Opens a session                     --
    --   Sets image quality to RAW only      --
    --------------------------------------------
    procedure Open is
      Count : aliased CI.Uint32;
      use type CI.Uint32;
    begin
      Check ("Initialize SDK", CI.Initialize_SDK);
      Check ("Get camera list", CI.Get_Camera_List (The_Session.Device_List'access));
      Check ("Get child count", CI.Get_Child_Count (The_Session.Device_List, Count'access));
      Log.Write ("Number of detected cameras:" & CI.Uint32'image (Count));
      if Count = 0 then
        Raise_Error ("No Canon cameras found");
      elsif Count > 1 then
        Raise_Error ("Multiple Canon cameras found");
      end if;
      Check ("Get camera at index 0",
             CI.Get_Camera_At_Index (List       => The_Session.Device_List,
                                      Index      => 0,
                                      The_Camera => The_Session.Device'access));
      Check ("Get device information",
             CI.Get_Device_Info (From     => The_Session.Device,
                                  The_Info => The_Session.Device_Info'access));
      declare
        Device_Description : String := C.Helper.String_Of (The_Session.Device_Info.Sz_Device_Description);
      begin
        Log.Write ("Camera description: " & Device_Description);
        for The_Character of Device_Description loop
          if The_Character = ' ' then
            The_Character := '_';
          end if;
        end loop;
        The_Camera := Model'value (Device_Description);
        Camera_Data.Set (The_Camera);
      exception
      when others =>
        Raise_Error ("Unknown Camera");
      end;

      Log.Write ("Device subtype:" & The_Session.Device_Info.Device_Sub_Type'image);

      Check ("Open session",
             CI.Open_Session (The_Session.Device));

      Disable_Electronic_View_Finder;

      Check ("UI lock",
             CI.Send_Status_Command (The_Session.Device,
                                     CI.Camera_Status_UI_Lock,
                                     0));

      Set (Property    => CI.Prop_Id_Image_Quality,
           Value       => CI.Image_Quality_LR,
           Where_Label => "Set image quality to RAW");

      Camera_Data.Set (Connected);
    end Open;


    -------------------------------------------------------------
    -- Capture Picture                                         --
    --   Triggers one exposure of a picture                    --
    --   Waits for the resulting RAW file directory-item event --
    --   Downloads the RAW file from the camera to Filename    --
    --   Deletes the RAW file in the camera                    --
    -------------------------------------------------------------
    The_Start_Time  : RT.Time;
    The_Wakeup_Time : RT.Time;
    The_Timeout     : Duration;

    Shutter_Is_On            : Boolean;
    Shutter_Release          : Boolean;
    The_Shutter_Release_Time : RT.Time;

    procedure Start_Capture is
      The_Mode : CI.Uint32;
      use type CI.AE_Mode;
    begin
      Camera_Data.Check (Idle);
      Camera_Data.Set (Connecting);
      if not The_Iso.Is_Iso_Or_Default then
        Raise_Error ("Parameter must be ISO or default value");
      end if;
      Shutter_Is_On := False;
      Shutter_Release := False;
      Open;
      if not The_Iso.Is_Default then
        Set (Property    => CI.Prop_Id_ISO,
             Value       => To_K_Iso (The_Iso)'enum_rep,
             Where_Label => "Set ISO " & The_Iso'image);
      end if;
      Get (Property => CI.Prop_Id_AE_Mode_Select,
           Value    => The_Mode);
      case The_Exposure.Mode is
      when Exposure.Tv_Mode =>
        case The_Camera is
        when Canon_Eos_60D =>
          if CI.AE_Mode'enum_val(The_Mode) /= CI.K_AE_Mode_Manual then
            Raise_Error ("Mode should be Manual");
          end if;
        when Canon_Eos_6D =>
          Set (Property    => CI.Prop_Id_AE_Mode_Select,
               Value       => CI.K_AE_Mode_Manual'enum_rep,
               Where_Label => "Set AE Mode Select to Manual");
          delay AE_Mode_Set_Time;
        end case;
        Set (Property    => CI.Prop_Id_Tv,
             Value       => To_K_Tv (The_Exposure)'enum_rep,
             Where_Label => "Set exposure (Tv) " & The_Exposure'image);
      when Exposure.Timer_Mode =>
        case The_Camera is
        when Canon_Eos_60D =>
          if CI.AE_Mode'enum_val(The_Mode) /= CI.K_AE_Mode_Bulb then
            Raise_Error ("Mode should be Buld");
          end if;
        when Canon_Eos_6D =>
          Set (Property    => CI.Prop_Id_AE_Mode_Select,
               Value       => CI.K_AE_Mode_Bulb'enum_rep,
               Where_Label => "Set AE mode select to Bulb");
        end case;
      when Exposure.From_Camera =>
        case The_Camera is
        when Canon_Eos_60D =>
          if CI.AE_Mode'enum_val(The_Mode) /= CI.K_AE_Mode_Manual then
            Raise_Error ("Mode should be Manual");
          end if;
        when Canon_Eos_6D =>
          Set (Property    => CI.Prop_Id_AE_Mode_Select,
               Value       => CI.K_AE_Mode_Manual'enum_rep,
               Where_Label => "Set AE Mode Select to Manual");
          delay AE_Mode_Set_Time;
        end case;
      end case;

      Event_State.Reset;
      Check ("Register object event handler",
             CI.Set_Object_Event_Handler (The_Session.Device,
                                          CI.Object_Event_All,
                                          Handler,
                                          System.Null_Address));
      The_Timeout := Duration(The_Exposure.Time) + Max_Download_Time;
      case The_Exposure.Mode is
      when Exposure.From_Camera =>
        The_Timeout := Max_Tv_Time + Max_Download_Time;
        Command ("Trigger single shot (exposure from camera)", CI.Camera_Command_Take_Picture);
      when Exposure.Tv_Mode =>
        Command ("Trigger single shot", CI.Camera_Command_Take_Picture);
      when Exposure.Timer_Mode =>
        Command ("Press shutter button completely non AF",
                 CI.Camera_Command_Press_Shutter_Button,
                 CI.Camera_Command_Shutter_Button_Completely_Non_AF'enum_rep);
        Shutter_Is_On := True;
        The_Shutter_Release_Time := RT.Clock + RT.To_Time_Span (Duration(The_Exposure.Time));
        The_Wakeup_Time := RT.Clock;
      end case;
      The_Start_Time := RT.Clock;
      Camera_Data.Set (Capturing);
    exception
    when Camera_Error =>
      Disconnect;
    when Occurrence: others =>
      Internal_Error (Occurrence);
    end Start_Capture;


    procedure Continue_Capture is
      use type CI.Int32;
    begin
      if Shutter_Is_On then
        if Shutter_Release then
          Command ("Release shutter button",
          CI.Camera_Command_Press_Shutter_Button,
          CI.Camera_Command_Shutter_Button_Off'enum_rep);
          Shutter_Release := False;
          Shutter_Is_On := False;
        elsif The_Wakeup_Time > The_Shutter_Release_Time then
          The_Wakeup_Time := The_Shutter_Release_Time;
          Shutter_Release := True;
        end if;
      elsif RT.To_Duration (RT.Clock - The_Start_Time) > The_Timeout then
        Raise_Error ("Timeout: no directory item event received");
      end if;

      Check ("Get event", CI.Get_Event, Logging => False);
      select
        Event_State.Wait_For (The_Item);
        declare
          The_Info : aliased CI.Directory_Item_Info;
        begin
          Check ("Get directory item info",
                 CI.Get_Directory_Item_Info (Item     => The_Item,
                                              The_Info => The_Info'access),
                 Logging => False);

          if The_Info.Is_Folder /= 0 then
            declare
              Count : constant CI.Ref_Count := CI.Release (The_Item);
            begin
              Log.Write ("Release Folder - Count:" & Count'image);
            end;
          else
            Log.Write ("Received file " & C.Helper.String_Of (The_Info.Sz_File_Name));
            Camera_Data.Set (Captured);
            return;
          end if;
        end;
      else
        null;
      end select;
    exception
    when Camera_Error =>
      Disconnect;
    when Occurrence: others =>
      Internal_Error (Occurrence);
    end Continue_Capture;


    procedure Download is
      File_Name         : aliased constant String := The_Filename.S & Ascii.Nul;
      The_Info          : aliased CI.Directory_Item_Info;
      File_Stream_Write : aliased CI.Stream;
    begin
      Camera_Data.Set (Downloading);
      Check ("Get directory item info",
             CI.Get_Directory_Item_Info (Item     => The_Item,
                                         The_Info => The_Info'access));

      Log.Write ("RAW size from camera (bytes): " & CI.Uint64'image (The_Info.Size));
      Log.Write ("Download " & C.Helper.String_Of (The_Info.Sz_File_Name) & " to " & The_Filename.S);

      Check ("Create file stream (write)",
             CI.Create_File_Stream (File_Name   => File_Name'address,
                                    Disposition => CI.File_Create_Always,
                                    Eds_Access  => CI.Access_Read_Write,
                                    Out_Stream  => File_Stream_Write'access));

      Check ("Download",
             CI.Download (The_Item, The_Info.Size, File_Stream_Write));

      Check ("Download complete",
             CI.Download_Complete (The_Item));
      declare
        Dummy : CI.Ref_Count := CI.Release (File_Stream_Write);
      begin
        null;
      end;
      Disconnect (Next_State => (if Is_Cropping then Cropping else Idle));
    exception
    when Camera_Error =>
      Disconnect;
    when Occurrence: others =>
      Internal_Error (Occurrence);
    end Download;

  begin -- Control
    Camera_Data.Set (Idle);
    The_Wakeup_Time := RT.Clock + Delta_Time;
    loop
      select
        accept Get (Is_Ready : out Boolean) do
          Is_Ready := One_Device_Is_Ready;
        end Get;
      or
        accept Capture_Picture (Filename : String;
                                Time     : Exposure.Item;
                                Iso      : Sensitivity.Item)
        do
          The_Filename := [Filename];
          The_Exposure := Time;
          The_Iso := Iso;
          Is_Cropping := False;
        end;
        Start_Capture;
      or
        accept Capture_Grid (Size : Square_Size;
                             Time : Exposure.Item;
                             Iso  : Sensitivity.Item)
        do
          The_Grid_Size := Size;
          The_Exposure := Time;
          The_Iso := Iso;
          Is_Cropping := True;
          The_Filename := [Default_Filename];
        end;
        Start_Capture;
      or
        accept Await_Stop do
          Log.Write ("Stopping");
          loop
            case Camera_Data.Actual.State is
            when Capturing =>
              The_Shutter_Release_Time := RT.Clock;
              Continue_Capture;
            when Cropping | Cropped =>
              exit;
            when others =>
              exit;
            end case;
          end loop;
          Disconnect;
        end Await_Stop;
      or
        accept Shutdown do
          if Camera_Data.Actual.Camera in Canon_Model then
            Disconnect;
          end if;
        end Shutdown;
        exit;
      or
        when Camera_Data.Actual.Camera in Canon_Model => delay until The_Wakeup_Time;
        The_Wakeup_Time := RT.Clock + Delta_Time;
        case Camera_Data.Actual.State is
        when Capturing =>
          Continue_Capture;
        when Captured =>
          Download;
        when Cropping =>
          Raw.Prepare_Grid (The_Filename.S, The_Grid_Size);
        when Connecting | Connected | Downloading | Cropped | Stopping | Idle | Failed =>
          null;
        end case;
      end select;
    end loop;
  exception
  when Occurrance: others =>
    Log.Termination (Occurrance);
    Disconnect;
  end Control;

end Camera.Canon;
