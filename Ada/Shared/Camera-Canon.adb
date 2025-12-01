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

with Ada.Calendar;
with C.Helper;
with Camera.Canon.Eos;
with System;
with Text;
with Traces;

package body Camera.Canon is

  package Log is new Traces ("Canon");


  task type Control is

    entry Define (Filename : String);

    entry Capture_Picture (Time : Exposure.Item;
                           Iso  : Sensitivity.Item);
    entry Shutdown;

  end Control;

  The_Control : access Control;


  procedure Startup is
  begin
    The_Control := new Control;
  end Startup;


  procedure Define (Filename : String) is
  begin
    The_Control.Define (Filename);
  end Define;


  procedure Capture_Picture (Time : Exposure.Item;
                             Iso  : Sensitivity.Item) is
  begin
    The_Control.Capture_Picture (Time, Iso);
  end Capture_Picture;


  procedure Shutdown is
  begin
    The_Control.Shutdown;
  end Shutdown;


  ----------------------------------------------------------
  -- shared state to pass info from callback to main task --
  ----------------------------------------------------------
  protected Event_State is

    procedure Reset;

    procedure Set_Item (Item : Eos.Directory_Item);

    entry Wait_For_Item (Item : out Eos.Directory_Item);

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

    procedure Set_Item (Item : Eos.Directory_Item) is
    begin
      Got_Item := True;
      Dir_Item := Item;
    end Set_Item;

    entry Wait_For_Item (Item : out Eos.Directory_Item) when Got_Item is
    begin
      Item     := Dir_Item;
      Got_Item := False;
      Dir_Item := Eos.No_Directory;
    end Wait_For_Item;

  end Event_State;


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
      Event_State.Set_Item (Object);
  --else
  --  IO.Put_Line ("[Canon] -> ignoring (volume/folder/etc.)");
    end if;
    return Eos.OK;
  end On_Object_Event;

  Handler : constant Eos.Object_Event_Handler := On_Object_Event'access;


  task body Control is

    Default_Filename : constant String := "Raw_Picture.CR2";

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
    function To_K_Iso (Iso : Sensitivity.Iso) return Eos.Uint32 is
    begin
      case Iso is
        when 100   => return Eos.K_ISO_100;
        when 200   => return Eos.K_ISO_200;
        when 400   => return Eos.K_ISO_400;
        when 800   => return Eos.K_ISO_800;
        when 1600  => return Eos.K_ISO_1600;
        when 3200  => return Eos.K_ISO_3200;
        when 6400  => return Eos.K_ISO_6400;
        when 12800 => return Eos.K_ISO_12800;
        when 25600 => return Eos.K_ISO_25600;
      end case;
    end To_K_Iso;

    -----------------------------------------------
    -- Local mapping for Tv to Canon enum values --
    -----------------------------------------------
    function To_K_Tv (Tv : Exposure.Tv) return Eos.Uint32 is
    begin
      case Tv is
        when Exposure.Tv_30_S     => return Eos.K_TV_30;
        when Exposure.Tv_25_S     => return Eos.K_TV_25;
        when Exposure.Tv_20_S     => return Eos.K_TV_20;
        when Exposure.Tv_15_S     => return Eos.K_TV_15;
        when Exposure.Tv_13_S     => return Eos.K_TV_13;
        when Exposure.Tv_10_S     => return Eos.K_TV_10;
        when Exposure.Tv_8_S      => return Eos.K_TV_8;
        when Exposure.Tv_6_S      => return Eos.K_TV_6;
        when Exposure.Tv_5_S      => return Eos.K_TV_5;
        when Exposure.Tv_4_S      => return Eos.K_TV_4;
        when Exposure.Tv_3_2_S    => return Eos.K_TV_3_2;
        when Exposure.Tv_2_5_S    => return Eos.K_TV_2_5;
        when Exposure.Tv_2_S      => return Eos.K_TV_2;
        when Exposure.Tv_1_6_S    => return Eos.K_TV_1_6;
        when Exposure.Tv_1_3_S    => return Eos.K_TV_1_3;
        when Exposure.Tv_1_S      => return Eos.K_TV_1;
        when Exposure.Tv_0_8_S    => return Eos.K_TV_0_8;
        when Exposure.Tv_0_6_S    => return Eos.K_TV_0_6;
        when Exposure.Tv_0_5_S    => return Eos.K_TV_0_5;
        when Exposure.Tv_0_4_S    => return Eos.K_TV_0_4;
        when Exposure.Tv_0_3_S    => return Eos.K_TV_0_3;
        when Exposure.Tv_D_4_S    => return Eos.K_TV_D_4;
        when Exposure.Tv_D_5_S    => return Eos.K_TV_D_5;
        when Exposure.Tv_D_6_S    => return Eos.K_TV_D_6;
        when Exposure.Tv_D_8_S    => return Eos.K_TV_D_8;
        when Exposure.Tv_D_10_S   => return Eos.K_TV_D_10;
        when Exposure.Tv_D_13_S   => return Eos.K_TV_D_13;
        when Exposure.Tv_D_15_S   => return Eos.K_TV_D_15;
        when Exposure.Tv_D_20_S   => return Eos.K_TV_D_20;
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
      end case;
    end To_K_Tv;


    The_Filename : Text.String   := [Default_Filename];
    The_Exposure : Exposure.Item;
    The_Iso      : Sensitivity.Item;

    type Session is record
      Device_List : aliased Eos.Device_List;
      Device      : aliased Eos.Device;
      Device_Info : aliased Eos.Device_Info;
    end record;

    The_Session  : Session;


    procedure Set (Property    : Eos.Property_Id;
                   Value       : Eos.Uint32;
                   Where_Label : String)
    is
      Val : aliased Eos.Uint32 := Value;
      use type Eos.Uint32;
    begin
      Check (Where_Label,
             Eos.Set_Property_Data (Item     => The_Session.Device,
                                    Property => Property,
                                    Param    => 0,
                                    Size     => Eos.Uint32'size / 8,
                                    Data     => Val'address));
    end Set;


    procedure Cleanup is
    begin
      declare
        Dummy : Eos.Error := Eos.Close_Session (The_Session.Device);
      begin
        null;
      exception
        when others => null;
      end;
      declare
        Dummy : Eos.Error := Eos.Release (The_Session.Device);
      begin
        null;
      exception
        when others => null;
      end;
      declare
        Dummy : Eos.Error := Eos.Release (The_Session.Device_List);
      begin
        null;
      exception
        when others => null;
      end;
      declare
        Dummy : Eos.Error := Eos.Terminate_SDK;
      begin
        null;
      exception
        when others => null;
      end;
    end Cleanup;

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
      else
        Check ("Get camera at index 0",
               Eos.Get_Camera_At_Index (List       => The_Session.Device_List,
                                        Index      => 0,
                                        The_Camera => The_Session.Device'access));
        Check ("Get device information",
               Eos.Get_Device_Info (From     => The_Session.Device,
                                    The_Info => The_Session.Device_Info'access));
        Log.Write ("Camera description: " & C.Helper.String_Of (The_Session.Device_Info.Sz_Device_Description));
        Log.Write ("Device subtype:" & The_Session.Device_Info.Device_Sub_Type'image);

        Check ("Open session", Eos.Open_Session (The_Session.Device));

        Set (Property    => Eos.Prop_Id_Image_Quality,
             Value       => Eos.Image_Quality_LR,
             Where_Label => "Set image quality to RAW");
      end if;
    exception
    when others =>
      Cleanup;
      raise;
    end Open;


    -------------------------------------------------------------
    -- Capture Picture                                         --
    --   Triggers one exposure of a picture                    --
    --   Waits for the resulting RAW file directory-item event --
    --   Downloads the RAW file from the camera to Filename    --
    --   Deletes the RAW file in the camera                    --
    -------------------------------------------------------------
    procedure Start_Capture is

      Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Timeout    : constant Duration := Duration(The_Exposure.Time) + 15.0;
      The_Item   : Eos.Directory_Item := Eos.No_Directory;

      use type Eos.Int32;
      use type Ada.Calendar.Time;

    begin -- Capture
      case The_Exposure.Mode is
      when Exposure.Tv_Mode =>
        Set (Property    => Eos.Prop_Id_Tv,
             Value       => To_K_Tv (The_Exposure.Time_Value),
             Where_Label => "Set exposure (Tv) " & The_Exposure'image);
      when Exposure.Timer_Mode =>
        raise Program_Error; -- Bulp not implemented
      when Exposure.From_Camera =>
        null;
      end case;
      if not The_Iso.Is_From_Camera then
        Set (Property    => Eos.Prop_Id_ISO,
             Value       => To_K_Iso (The_Iso.Value),
             Where_Label => "Set ISO " & The_Iso'image);
      end if;
      Event_State.Reset;

      Check ("Register object event handler",
             Eos.Set_Object_Event_Handler (The_Session.Device,
                                           Eos.Object_Event_All,
                                           Handler,
                                           System.Null_Address));

      Check ("Trigger single shot",
             Eos.Send_Command (The_Session.Device,
                               Eos.Camera_Command_Take_Picture,
                               0));

      -- wait (with timeout) for a *file* directory item event
      loop
        Check ("Get event",
               Eos.Get_Event, Logging => False);

        select
          Event_State.Wait_For_Item (The_Item);
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
              exit;
            end if;
          end;
        or
          delay 0.1;
        end select;

        if Ada.Calendar.Clock - Start_Time > Timeout then
          Raise_Error ("Timeout: no directory item event received");
        end if;
      end loop;

      declare
        File_Name         : aliased constant String := The_Filename.To_String & Ascii.Nul;
        The_Info          : aliased Eos.Directory_Item_Info;
        File_Stream_Write : aliased Eos.Stream;
      begin
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
      end;
    exception
    when others =>
      Cleanup;
      raise;
    end Start_Capture;


    procedure Close is
    begin
      Check ("Close session", Eos.Close_Session (The_Session.Device));
      Check ("Release camera handle", Eos.Release (The_Session.Device));
      Check ("Release camera list", Eos.Release (The_Session.Device_List));
      Check ("Terminating SDK", Eos.Terminate_SDK);
      Log.Write ("Finished");
    exception
    when Occurrence : others =>
      Log.Termination (Occurrence);
      Cleanup;
    end Close;

  begin -- Control
    loop
      select
        accept Define (Filename : String) do
          The_Filename := [Filename];
        end;
      or
        accept Capture_Picture (Time : Exposure.Item;
                                Iso  : Sensitivity.Item) do
          The_Exposure := Time;
          The_Iso := Iso;
        end;
        Open;
        Start_Capture;
        Close;
      or
        accept Shutdown;
        Cleanup;
        exit;
      or
        delay 1.0;
      end select;
    end loop;
  exception
  when Occurrance: others =>
    Log.Termination (Occurrance);
    Cleanup;
  end Control;

end Camera.Canon;
