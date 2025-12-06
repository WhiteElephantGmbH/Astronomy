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
with System;
with Text;
with Traces;

package body Camera.Canon is

  package Log is new Traces ("Canon");

  package RT renames Ada.Real_Time;

  task type Control is

    entry Capture_Picture (Filename : String;
                           Time     : Exposure.Item;
                           Iso      : Sensitivity.Item);
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

    procedure Stop_Capturing;

    function Actual return Information;

  private
    The_Information : Information;
  end Data;


  protected body Data is

    procedure Set (State : Status) is
    begin
      if State = Idle or else The_Information.State /= Stopping then
        The_Information.State := State;
      end if;
    end Set;


    procedure Set (Item : Model) is
    begin
      The_Information.Camera := Item;
    end Set;


    procedure Stop_Capturing is
    begin
      if The_Information.State in Capturing then
        The_Information.State := Stopping;
      end if;
    end Stop_Capturing;


    function Actual return Information is
    begin
      return The_Information;
    end Actual;

  end Data;


  function Actual_Info return Information is
  begin
    return Data.Actual;
  end Actual_Info;


  procedure Capture_Picture (Filename : String;
                             Time     : Exposure.Item;
                             Iso      : Sensitivity.Item) is
  begin
    The_Control.Capture_Picture (Filename, Time, Iso);
  end Capture_Picture;


  procedure Stop_Capture is
  begin
    Data.Stop_Capturing;
  end Stop_Capture;


  procedure End_Control is
  begin
    The_Control.Shutdown;
  end End_Control;


  ---------------------------
  -- object event callback --
  ---------------------------
  function On_Object_Event (Event   : Eos.Object_Event;
                            Object  : Eos.Directory_Item;
                            Context : System.Address) return Eos.Error
    with Convention => StdCall;

  function On_Object_Event (Event   : Eos.Object_Event;
                            Object  : Eos.Directory_Item;
                            Context : System.Address) return Eos.Error
  is
    pragma Unreferenced (Context);
    use type Eos.Object_Event;
  begin
  --IO.Put_Line ("[Canon] object event received: " & Eos.Uint32'image (Event));
    if Event = Eos.Object_Event_Dir_Item_Created or else Event = Eos.Object_Event_Dir_Item_Request_Transfer then
    --IO.Put_Line ("[Canon] -> treating this as DirItem event");
      Event_State.Set (Object);
  --else
  --  IO.Put_Line ("[Canon] -> ignoring (volume/folder/etc.)");
    end if;
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

    procedure Raise_Error (Msg : String) with No_Return is
    begin
      Log.Error (Msg);
      raise Canon_Error;
    end Raise_Error;

    ------------------------
    -- Check return codes --
    ------------------------
    procedure Check (Where   : String;
                     Result  : Eos.Error;
                     Logging : Boolean := True) is
      use type Eos.Error;
    begin
      if Logging then
        Log.Write (Where);
      end if;
      if Result /= Eos.OK then
        Raise_Error (Where & " failed, error =" & Eos.Error'image (Result));
      end if;
    end Check;

    -------------------------------------------------
    -- Local mapping for ISO to  Canon enum values --
    -------------------------------------------------
    function To_K_Iso (Item : Sensitivity.Item) return Eos.Uint32 is
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
    function To_K_Tv (Item : Exposure.Item) return Eos.Uint32 is
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
      return 0; --!!!WE!!! GNAT Bug
    end To_K_Tv;


    The_Filename : Text.String   := [Default_Filename];
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


    procedure Command (What  : String;
                       Item  : Eos.Uint32;
                       Param : Eos.Uint32 := 0) is
    begin
      Check (What, Eos.Send_Command (The_Session.Device, Item, Param));
    end Command;


    procedure Disconnect is
      Dummy : Eos.Error;
    begin
      begin
        Dummy := Eos.Send_Status_Command (The_Session.Device, Eos.Camera_Status_UI_Unlock, 0);
      exception
      when others =>
        null;
      end;
      begin
        Dummy := Eos.Close_Session (The_Session.Device);
      exception
      when others =>
        null;
      end;
      begin
        Dummy := Eos.Release (The_Session.Device);
      exception
      when others =>
        null;
      end;
      begin
        Dummy := Eos.Release (The_Session.Device_List);
      exception
      when others =>
        null;
      end;
      begin
        Dummy := Eos.Terminate_SDK;
      exception
      when others =>
        null;
      end;
      Data.Set (Idle);
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

      Check ("UI lock",
             Eos.Send_Status_Command (The_Session.Device,
                                      Eos.Camera_Status_UI_Lock,
                                      0));

      Set (Property    => Eos.Prop_Id_Image_Quality,
           Value       => Eos.Image_Quality_LR,
           Where_Label => "Set image quality to RAW");

      Data.Set (Connected);
    exception
    when others =>
      Disconnect;
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
    The_Item        : Eos.Directory_Item;

    Shutter_Is_On            : Boolean;
    Shutter_Release          : Boolean;
    The_Shutter_Release_Time : RT.Time;

    procedure Start_Capture is
    begin
      Shutter_Is_On := False;
      Shutter_Release := False;
      Open;
      if not The_Iso.Is_From_Camera then
        Set (Property    => Eos.Prop_Id_ISO,
             Value       => To_K_Iso (The_Iso),
             Where_Label => "Set ISO " & The_Iso'image);
      end if;
      case The_Exposure.Mode is
      when Exposure.Tv_Mode =>
        Set (Property    => Eos.Prop_Id_AE_Mode_Select,
             Value       => Eos.K_AE_Mode_Manual,
             Where_Label => "Set AE Mode Select to Manual");
        delay 0.1; -- wait for set
        Set (Property    => Eos.Prop_Id_Tv,
             Value       => To_K_Tv (The_Exposure),
             Where_Label => "Set exposure (Tv) " & The_Exposure'image);
      when Exposure.Timer_Mode =>
        Set (Property    => Eos.Prop_Id_AE_Mode_Select,
             Value       => Eos.K_AE_Mode_Bulb,
             Where_Label => "Set AE mode select to Bulb");
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
                 Eos.Camera_Command_Shutter_Button_Completely_Non_AF);
        Shutter_Is_On := True;
        The_Shutter_Release_Time := RT.Clock + RT.To_Time_Span (Duration(The_Exposure.Time));
        The_Wakeup_Time := RT.Clock;
      when Exposure.From_Camera =>
        null;
      end case;
      The_Start_Time := RT.Clock;
      The_Timeout := Duration(The_Exposure.Time) + 15.0;
      The_Item := Eos.No_Directory;

      Data.Set (Capturing);
    exception
    when others =>
      Disconnect;
    end Start_Capture;


    procedure Continue_Capture is
      use type Eos.Int32;
      use type Eos.Directory_Item;
    begin
      if Shutter_Is_On then
        if Shutter_Release then
          Command ("Release shutter button", Eos.Camera_Command_Shutter_Button_Off);
          Shutter_Release := False;
          Shutter_Is_On := False;
        elsif The_Wakeup_Time > The_Shutter_Release_Time then
          The_Wakeup_Time := The_Shutter_Release_Time;
          Shutter_Release := True;
        end if;
      elsif RT.To_Duration (RT.Clock - The_Start_Time) > The_Timeout then
        if The_Item /= Eos.No_Directory then
          Check ("Delete incomplete file from camera", Eos.Delete_Directory_Item (The_Item));
        end if;
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
          --IO.Put_Line ("received folder item, ignoring: " & String_Of (Info.Sz_File_Name));
            declare
              Dummy : Eos.Error := Eos.Release (The_Item);
            begin
              null;
            end;
            The_Item := Eos.No_Directory;
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

      Check ("Delete file from camera",
              Eos.Delete_Directory_Item (The_Item));
      declare
        Dummy : Eos.Error := Eos.Release (File_Stream_Write);
      begin
        null;
      end;
      Disconnect;
    exception
    when others =>
      Disconnect;
    end Download;

    use type Eos.Directory_Item;

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
          Start_Capture;
        end;
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
        when Stopping =>
          if The_Item /= Eos.No_Directory then
            Check ("Delete file on camera after stop", Eos.Delete_Directory_Item (The_Item));
            The_Item := Eos.No_Directory;
          end if;
          Disconnect;
        when Connected | Downloading | Idle =>
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
