-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
with Camera.Canon.Eos;
with Camera.Raw;
with System;
with Text;
with Traces;

package body Camera.Canon is

  package Log is new Traces ("Canon");

  package RT renames Ada.Real_Time;

  task type Control with  Priority => System.Max_Priority is

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
  -- shared state to pass info from callback to main task --
  ----------------------------------------------------------
  protected Event_State is

    procedure Reset;

    procedure Set (Item : Eos.Directory_Item);

    entry Wait_For (Item : out Eos.Directory_Item);

  private
    Got_Item : Boolean := False;
    Dir_Item : Eos.Directory_Item := Eos.No_Directory;
  end Event_State;


  protected body Event_State is

    procedure Reset is
    begin
      Got_Item := False;
      Dir_Item := Eos.No_Directory;
    end Reset;

    procedure Set (Item : Eos.Directory_Item) is
    begin
      Got_Item := True;
      Dir_Item := Item;
    end Set;

    entry Wait_For (Item : out Eos.Directory_Item) when Got_Item is
    begin
      Item     := Dir_Item;
      Got_Item := False;
      Dir_Item := Eos.No_Directory;
    end Wait_For;

  end Event_State;


  protected Data is

    procedure Set (State : Status);

    procedure Set (Item : Model);

    function Actual return Information;

    function Grid return Green_Grid;

    procedure Set_Error (Message : String);

    function Last_Error return String;

    procedure Reset_Error;

  private
    The_Information : Information;
    The_Last_Error  : Text.String;
  end Data;



  protected body Data is

    procedure Set (State : Status) is
    begin
      if The_Information.State /= Error then
        The_Information.State := State;
      end if;
    end Set;


    procedure Set (Item : Model) is
    begin
      The_Information.Camera := Item;
    end Set;


    function Actual return Information is
    begin
      return The_Information;
    end Actual;


    function Grid return Green_Grid is
    begin
      if The_Information.State /= Cropped then
        raise Program_Error;
      end if;
      return Camera.Raw.Grid;
    end Grid;


    procedure Set_Error (Message : String) is
    begin
      The_Last_Error := [Message];
      The_Information.State := Error;
    end Set_Error;


    function Last_Error return String is
    begin
      return The_Last_Error.To_String;
    end Last_Error;


    procedure Reset_Error is
    begin
      The_Information.State := Idle;
    end Reset_Error;

  end Data;


  function Actual_Info return Information is
  begin
    return Data.Actual;
  end Actual_Info;


  procedure Capture_Picture (Filename : String;
                             Time     : Exposure.Item;
                             Iso      : Sensitivity.Item) is
  begin
    Data.Set (Connecting);
    The_Control.Capture_Picture (Filename, Time, Iso);
  end Capture_Picture;


  procedure Capture_Grid (Size : Square_Size;
                          Time : Exposure.Item;
                          Iso  : Sensitivity.Item) is
  begin
    Data.Set (Connecting);
    The_Control.Capture_Grid (Size, Time, Iso);
  end Capture_Grid;


  function Captured_Grid return Green_Grid is
    Grid : constant Green_Grid := Data.Grid;
  begin
    Data.Set (Idle);
    return Grid;
  end Captured_Grid;


  procedure Stop_Capture is
  begin
    The_Control.Await_Stop;
  end Stop_Capture;


  function Last_Error_Message return String is
  begin
    Data.Reset_Error;
    return Data.Last_Error;
  end Last_Error_Message;


  procedure End_Control is
  begin
    The_Control.Shutdown;
  end End_Control;


  ---------------------------
  -- object event callback --
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
  function On_Object_Event (Event   : Eos.Object_Event;
                            Object  : Eos.Directory_Item;
                            Context : System.Address) return Eos.Result
    with Convention => StdCall;

  function On_Object_Event (Event   : Eos.Object_Event;
                            Object  : Eos.Directory_Item;
                            Context : System.Address) return Eos.Result
  is
    pragma Unreferenced (Context);
    Count : Eos.Ref_Count;
  begin
    case Event is
    when Eos.Object_Event_Dir_Item_Created => --| Eos.Object_Event_Dir_Item_Request_Transfer =>
      Count := Eos.Retain (Object);
      Log.Write ("Received file - Event:" & Event'image & " - Count:" & Count'image);
      Event_State.Set (Object);
    when others =>
      null;
    end case;
    return Eos.OK;
  end On_Object_Event;

  Handler : constant Eos.Object_Event_Handler := On_Object_Event'access;


  task body Control is

    Default_Filename : constant String := "Raw_Picture.CR2";

    The_Camera : Model;

    -----------
    -- Error --
    -----------
    Canon_Error : exception;

    procedure Raise_Error (Message : String) with No_Return is
    begin
      Log.Error (Message);
      Data.Set_Error (Message);
      raise Canon_Error;
    end Raise_Error;

    ------------------------
    -- Check return codes --
    ------------------------
    procedure Check (Where   : String;
                     Result  : Eos.Result;
                     Logging : Boolean := True) is
      use type Eos.Result;
    begin
      if Logging then
        Log.Write (Where);
      end if;
      if Result /= Eos.OK then
        Raise_Error (Where & " failed, error =" & Eos.Result'image (Result));
      end if;
    end Check;

    -------------------------------------------------
    -- Local mapping for ISO to  Canon enum values --
    -------------------------------------------------
    function To_K_Iso (Item : Sensitivity.Item) return Eos.ISO_Speed is
      Iso : constant Sensitivity.Iso := Item.Value;
    begin
      case Iso is
        when 100    => return Eos.K_ISO_100;
        when 125    => return Eos.K_ISO_125;
        when 160    => return Eos.K_ISO_160;
        when 200    => return Eos.K_ISO_200;
        when 250    => return Eos.K_ISO_250;
        when 320    => return Eos.K_ISO_320;
        when 400    => return Eos.K_ISO_400;
        when 500    => return Eos.K_ISO_500;
        when 640    => return Eos.K_ISO_640;
        when 800    => return Eos.K_ISO_800;
        when 1000   => return Eos.K_ISO_1000;
        when 1250   => return Eos.K_ISO_1250;
        when 1600   => return Eos.K_ISO_1600;
        when 2000   => return Eos.K_ISO_2000;
        when 2500   => return Eos.K_ISO_2500;
        when 3200   => return Eos.K_ISO_3200;
        when 4000   => return Eos.K_ISO_4000;
        when 5000   => return Eos.K_ISO_5000;
        when 6400   => return Eos.K_ISO_6400;
        when others => null;
        end case;
      case The_Camera is
      when Canon_Eos_6D =>
        case Iso is
          when 12800  => return Eos.K_ISO_12800;
          when 25600  => return Eos.K_ISO_25600;
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
    function To_K_Tv (Item : Exposure.Item) return Eos.Tv_Value is
      Tv : constant Exposure.Tv := Item.Time_Value;
    begin
      case Tv is
        when Exposure.Tv_0_8_S    => return Eos.K_TV_0_8;
        when Exposure.Tv_0_6_S    => return Eos.K_TV_0_6;
        when Exposure.Tv_0_5_S    => return Eos.K_TV_0_5;
        when Exposure.Tv_0_4_S    => return Eos.K_TV_0_4;
        when Exposure.Tv_0_3_S    => return Eos.K_TV_0_3_S;
        when Exposure.Tv_D_4_S    => return Eos.K_TV_D_4;
        when Exposure.Tv_D_5_S    => return Eos.K_TV_D_5;
        when Exposure.Tv_D_6_S    => return Eos.K_TV_D_6_S;
        when Exposure.Tv_D_8_S    => return Eos.K_TV_D_8;
        when Exposure.Tv_D_10_S   => return Eos.K_TV_D_10_S;
        when Exposure.Tv_D_13_S   => return Eos.K_TV_D_13;
        when Exposure.Tv_D_15_S   => return Eos.K_TV_D_15;
        when Exposure.Tv_D_20_S   => return Eos.K_TV_D_20_S;
        when Exposure.Tv_D_25_S   => return Eos.K_TV_D_25;
        when Exposure.Tv_D_30_S   => return Eos.K_TV_D_30;
        when Exposure.Tv_D_40_S   => return Eos.K_TV_D_40;
        when Exposure.Tv_D_50_S   => return Eos.K_TV_D_50;
        when Exposure.Tv_D_60_S   => return Eos.K_TV_D_60;
        when Exposure.Tv_D_80_S   => return Eos.K_TV_D_80;
        when Exposure.Tv_D_100_S  => return Eos.K_TV_D_100;
        when Exposure.Tv_D_125_S  => return Eos.K_TV_D_125;
        when Exposure.Tv_D_160_S  => return Eos.K_TV_D_160;
        when Exposure.Tv_D_200_S  => return Eos.K_TV_D_200;
        when Exposure.Tv_D_250_S  => return Eos.K_TV_D_250;
        when Exposure.Tv_D_320_S  => return Eos.K_TV_D_320;
        when Exposure.Tv_D_400_S  => return Eos.K_TV_D_400;
        when Exposure.Tv_D_500_S  => return Eos.K_TV_D_500;
        when Exposure.Tv_D_640_S  => return Eos.K_TV_D_640;
        when Exposure.Tv_D_800_S  => return Eos.K_TV_D_800;
        when Exposure.Tv_D_1000_S => return Eos.K_TV_D_1000;
        when Exposure.Tv_D_1250_S => return Eos.K_TV_D_1250;
        when Exposure.Tv_D_1600_S => return Eos.K_TV_D_1600;
        when Exposure.Tv_D_2000_S => return Eos.K_TV_D_2000;
        when Exposure.Tv_D_2500_S => return Eos.K_TV_D_2500;
        when Exposure.Tv_D_3200_S => return Eos.K_TV_D_3200;
        when Exposure.Tv_D_4000_S => return Eos.K_TV_D_4000;
        when others => null;
      end case;
      case The_Camera is
      when Canon_Eos_6D =>
        null;
      when Canon_Eos_60D =>
        case Tv is
        when Exposure.Tv_D_5000_S => return Eos.K_TV_D_5000;
        when Exposure.Tv_D_6400_S => return Eos.K_TV_D_6400;
        when Exposure.Tv_D_8000_S => return Eos.K_TV_D_8000;
        when others => null;
        end case;
      end case;
      Raise_Error ("Tv value " & Item'image & " not valid for " & The_Camera'image);
      return Eos.Tv_Value'last; --!!!WE!!! GNAT Bug
    end To_K_Tv;


    The_Filename : Text.String := [Default_Filename];
    The_Exposure : Exposure.Item;
    The_Iso      : Sensitivity.Item;

    Delta_Time : constant RT.Time_Span := RT.To_Time_Span (1.0 / 5);

    use type RT.Time;


    type Session is record
      Device_List : aliased Eos.Device_List;
      Device      : aliased Eos.Device;
      Device_Info : aliased Eos.Device_Info;
    end record;

    The_Session : Session;
    The_Item    : Eos.Directory_Item := Eos.No_Directory;

    Is_Cropping   : Boolean;
    The_Grid_Size : Square_Size;


    procedure Set (Where_Label : String;
                   Property    : Eos.Property_Id;
                   Value       : Eos.Uint32;
                   Param       : Eos.Int32 := 0)
    is
      Val : aliased constant Eos.Uint32 := Value;
      use type Eos.Uint32;
    begin
      Check (Where_Label,
             Eos.Set_Property_Data (Item     => The_Session.Device,
                                    Property => Property,
                                    Param    => Param,
                                    Size     => Eos.Uint32'size / System.Storage_Unit,
                                    Data     => Val'address));
    end Set;


    procedure Get (Property    :     Eos.Property_Id;
                   Value       : out Eos.Uint32;
                   Param       :     Eos.Int32 := 0)
    is
      Val : aliased Eos.Uint32;
      use type Eos.Uint32;
      What : constant String := Text.Legible_Of (Property'image);
    begin
      Check (What,
             Eos.Get_Property_Data (Item     => The_Session.Device,
                                    Property => Property,
                                    Param    => Param,
                                    Size     => Eos.Uint32'size / System.Storage_Unit,
                                    Data     => Val'address));
      Value := Val;
      Log.Write (What & ":" & Value'image);
    end Get;


    procedure Command (What  : String;
                       Item  : Eos.Camera_Command;
                       Param : Eos.Int32 := 0) is
    begin
      Check (What, Eos.Send_Command (The_Session.Device, Item, Param));
    end Command;


    procedure Disable_Electronic_View_Finder is
      use type Eos.Uint32;
    begin
      Set (Property    => Eos.Prop_Id_Evf_Output_Device,
           Value       => Eos.Evf_Output_None'enum_rep,
           Where_Label => "Disable EVF output");
      loop
        declare
          Value : Eos.Uint32;
        begin
          Get (Eos.Prop_Id_Evf_Output_Device, Value);
          exit when Value = Eos.Evf_Output_None'enum_rep;
          delay 0.1;
        end;
      end loop;
    end Disable_Electronic_View_Finder;


    procedure Disconnect (Next_State : Status := Idle) is
      Dummy_Result : Eos.Result;
      Dummy_Count  : Eos.Ref_Count;
      use type Eos.Directory_Item;
    begin
      if The_Item /= Eos.No_Directory then
        begin
          Check ("Delete file on camera",  Eos.Delete_Directory_Item (The_Item));
          declare
            Count : constant Eos.Ref_Count := Eos.Release (The_Item);
          begin
            Log.Write ("Release directory item, Count:" & Count'image);
          end;
          The_Item := Eos.No_Directory;
        exception
        when others =>
          null;
        end;
      end if;
      begin
        Dummy_Result := Eos.Send_Status_Command (The_Session.Device, Eos.Camera_Status_UI_Unlock, 0);
      exception
      when others =>
        null;
      end;
      begin
        Dummy_Result := Eos.Close_Session (The_Session.Device);
      exception
      when others =>
        null;
      end;
      begin
        Dummy_Count := Eos.Release (The_Session.Device);
      exception
      when others =>
        null;
      end;
      begin
        Dummy_Count := Eos.Release (The_Session.Device_List);
      exception
      when others =>
        null;
      end;
      begin
        Dummy_Result := Eos.Terminate_SDK;
      exception
      when others =>
        null;
      end;
      Data.Set (Next_State);
    end Disconnect;

    --------------------------------------------
    --  Open                                  --
    --    Initializes EDSDK                   --
    --    Finds the first connected Canon EOS --
    --    Opens a session                     --
    --    Sets image quality to RAW only      --
    --------------------------------------------
    procedure Open is
      Count : aliased Eos.Uint32;
      use type Eos.Uint32;
    begin
      Check ("Initialize SDK", Eos.Initialize_SDK);
      Check ("Get camera list", Eos.Get_Camera_List (The_Session.Device_List'access));
      Check ("Get child count", Eos.Get_Child_Count (The_Session.Device_List, Count'access));
      Log.Write ("Number of detected cameras:" & Eos.Uint32'image (Count));
      if Count = 0 then
        Raise_Error ("No Canon cameras found");
      elsif Count > 1 then
        Raise_Error ("Multiple Canon cameras found");
      end if;
      Check ("Get camera at index 0",
             Eos.Get_Camera_At_Index (List       => The_Session.Device_List,
                                      Index      => 0,
                                      The_Camera => The_Session.Device'access));
      Check ("Get device information",
             Eos.Get_Device_Info (From     => The_Session.Device,
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
        Data.Set (The_Camera);
      exception
      when others =>
        Raise_Error ("Unknown Camera");
      end;

      Log.Write ("Device subtype:" & The_Session.Device_Info.Device_Sub_Type'image);

      Check ("Open session",
             Eos.Open_Session (The_Session.Device));

      Disable_Electronic_View_Finder;

      Check ("UI lock",
             Eos.Send_Status_Command (The_Session.Device,
                                      Eos.Camera_Status_UI_Lock,
                                      0));

      Set (Property    => Eos.Prop_Id_Image_Quality,
           Value       => Eos.Image_Quality_LR,
           Where_Label => "Set image quality to RAW");

      Data.Set (Connected);
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
      The_Mode : Eos.Uint32;
      use type Eos.AE_Mode;
    begin
      Shutter_Is_On := False;
      Shutter_Release := False;
      Open;
      if not The_Iso.Is_From_Camera then
        Set (Property    => Eos.Prop_Id_ISO,
             Value       => To_K_Iso (The_Iso)'enum_rep,
             Where_Label => "Set ISO " & The_Iso'image);
      end if;
      Get (Property => Eos.Prop_Id_AE_Mode_Select,
           Value    => The_Mode);
      case The_Exposure.Mode is
      when Exposure.Tv_Mode =>
        case The_Camera is
        when Canon_Eos_60D =>
          if Eos.AE_Mode'enum_val(The_Mode) /= Eos.K_AE_Mode_Manual then
            Raise_Error ("Mode should be Manual");
          end if;
        when Canon_Eos_6D =>
          Set (Property    => Eos.Prop_Id_AE_Mode_Select,
               Value       => Eos.K_AE_Mode_Manual'enum_rep,
               Where_Label => "Set AE Mode Select to Manual");
          delay 0.1; -- wait for set
        end case;
        Set (Property    => Eos.Prop_Id_Tv,
             Value       => To_K_Tv (The_Exposure)'enum_rep,
             Where_Label => "Set exposure (Tv) " & The_Exposure'image);
      when Exposure.Timer_Mode =>
        case The_Camera is
        when Canon_Eos_60D =>
          if Eos.AE_Mode'enum_val(The_Mode) /= Eos.K_AE_Mode_Bulb then
            Raise_Error ("Mode should be Buld");
          end if;
        when Canon_Eos_6D =>
          Set (Property    => Eos.Prop_Id_AE_Mode_Select,
               Value       => Eos.K_AE_Mode_Bulb'enum_rep,
               Where_Label => "Set AE mode select to Bulb");
        end case;
      when Exposure.From_Camera =>
        null;
      end case;

      Event_State.Reset;
      Check ("Register object event handler",
             Eos.Set_Object_Event_Handler (The_Session.Device,
                                           Eos.Object_Event_All,
                                           Handler,
                                           System.Null_Address));
      case The_Exposure.Mode is
      when Exposure.Tv_Mode =>
        Command ("Trigger single shot", Eos.Camera_Command_Take_Picture);
      when Exposure.Timer_Mode =>
        Command ("Press shutter button completely non AF",
                 Eos.Camera_Command_Press_Shutter_Button,
                 Eos.Camera_Command_Shutter_Button_Completely_Non_AF'enum_rep);
        Shutter_Is_On := True;
        The_Shutter_Release_Time := RT.Clock + RT.To_Time_Span (Duration(The_Exposure.Time));
        The_Wakeup_Time := RT.Clock;
      when Exposure.From_Camera =>
        null;
      end case;
      The_Start_Time := RT.Clock;
      The_Timeout := Duration(The_Exposure.Time) + 15.0;

      Data.Set (Capturing);
    exception
    when others =>
      Disconnect;
    end Start_Capture;


    procedure Continue_Capture is
      use type Eos.Int32;
    begin
      if Shutter_Is_On then
        if Shutter_Release then
          Command ("Release shutter button",
          Eos.Camera_Command_Press_Shutter_Button,
          Eos.Camera_Command_Shutter_Button_Off'enum_rep);
          Shutter_Release := False;
          Shutter_Is_On := False;
        elsif The_Wakeup_Time > The_Shutter_Release_Time then
          The_Wakeup_Time := The_Shutter_Release_Time;
          Shutter_Release := True;
        end if;
      elsif RT.To_Duration (RT.Clock - The_Start_Time) > The_Timeout then
        Raise_Error ("Timeout: no directory item event received");
      end if;

      Check ("Get event", Eos.Get_Event, Logging => False);
      select
        Event_State.Wait_For (The_Item);
        declare
          The_Info : aliased Eos.Directory_Item_Info;
        begin
          Check ("Get directory item info",
                 Eos.Get_Directory_Item_Info (Item     => The_Item,
                                              The_Info => The_Info'access),
                 Logging => False);

          if The_Info.Is_Folder /= 0 then
            declare
              Count : constant Eos.Ref_Count := Eos.Release (The_Item);
            begin
              Log.Write ("Release Folder - Count:" & Count'image);
            end;
          else
            Log.Write ("Received file " & C.Helper.String_Of (The_Info.Sz_File_Name));
            Data.Set (Captured);
            return;
          end if;
        end;
      else
        null;
      end select;
    exception
    when others =>
      Disconnect;
    end Continue_Capture;


    procedure Download is
      File_Name         : aliased constant String := The_Filename.To_String & Ascii.Nul;
      The_Info          : aliased Eos.Directory_Item_Info;
      File_Stream_Write : aliased Eos.Stream;
    begin
      Data.Set (Downloading);
      Check ("Get directory item info",
             Eos.Get_Directory_Item_Info (Item     => The_Item,
                                          The_Info => The_Info'access));

      Log.Write ("RAW size from camera (bytes): " & Eos.Uint64'image (The_Info.Size));
      Log.Write ("Download " & C.Helper.String_Of (The_Info.Sz_File_Name) & " to " & The_Filename.To_String);

      Check ("Create file stream (write)",
             Eos.Create_File_Stream (File_Name   => File_Name'address,
                                     Disposition => Eos.File_Create_Always,
                                     Eds_Access  => Eos.Access_Read_Write,
                                     Out_Stream  => File_Stream_Write'access));

      Check ("Download",
             Eos.Download (The_Item, The_Info.Size, File_Stream_Write));

      Check ("Download complete",
             Eos.Download_Complete (The_Item));
      declare
        Dummy : Eos.Ref_Count := Eos.Release (File_Stream_Write);
      begin
        null;
      end;
      Disconnect (Next_State => (if Is_Cropping then Cropping else Idle));
    exception
    when others =>
      Disconnect;
    end Download;

  begin -- Control
    Data.Set (Idle);
    The_Wakeup_Time := RT.Clock + Delta_Time;
    loop
      select
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
            case Data.Actual.State is
            when Capturing =>
              The_Shutter_Release_Time := RT.Clock;
              Continue_Capture;
            when Cropping =>
              Data.Set (Cropped);
              exit;
            when Cropped =>
              Raw.Stop_Preparing;
              exit;
            when others =>
              exit;
            end case;
          end loop;
          Disconnect;
        end Await_Stop;
      or
        accept Shutdown do
          Disconnect;
        end Shutdown;
        exit;
      or
        delay until The_Wakeup_Time;
        The_Wakeup_Time := RT.Clock + Delta_Time;
        case Data.Actual.State is
        when Capturing =>
          Continue_Capture;
        when Captured =>
          Download;
        when Cropping =>
          Raw.Prepare_Grid (The_Filename.To_String, The_Grid_Size);
          Data.Set (Cropped);
        when Connecting | Connected | Downloading | Cropped | Stopping | Idle | Error=>
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
