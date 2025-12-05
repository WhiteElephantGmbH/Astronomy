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
with Ada.Text_IO;
with Canon_Interface;
with Exceptions;
with System;

package body Canon is

  package IO renames Ada.Text_IO;
  package CI renames Canon_Interface;

  --------------------------------------------------------
  -- helper: C-style NUL-terminated string → Ada String --
  --------------------------------------------------------
  function String_Of (C_String : String) return String is

    function Last return Natural is
      The_Last : Natural := C_String'first - 1;
    begin
      for Char of C_String loop
        if Char = Ascii.Nul then
          return The_Last;
        end if;
        The_Last := @ + 1;
      end loop;
      return C_String'last;
    end Last;

  begin
    return C_String (C_String'first .. Last);
  end String_Of;

  ----------------------------------------------------------
  -- shared state to pass info from callback to main task --
  ----------------------------------------------------------
  protected Event_State is

    procedure Reset;

    procedure Set_Item (Item : CI.Directory_Item);

    entry Wait_For_Item (Item : out CI.Directory_Item);

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

    procedure Set_Item (Item : CI.Directory_Item) is
    begin
      Got_Item := True;
      Dir_Item := Item;
    end Set_Item;

    entry Wait_For_Item (Item : out CI.Directory_Item) when Got_Item is
    begin
      Item     := Dir_Item;
      Got_Item := False;
      Dir_Item := CI.No_Directory;
    end Wait_For_Item;

  end Event_State;

  ---------------------------
  -- object event callback --
  ---------------------------
  function On_Object_Event (Event   : CI.Eds_Object_Event;
                            Object  : CI.Directory_Item;
                            Context : System.Address) return CI.Eds_Error
    with Convention => StdCall;

  function On_Object_Event (Event   : CI.Eds_Object_Event;
                            Object  : CI.Directory_Item;
                            Context : System.Address) return CI.Eds_Error
  is
    pragma Unreferenced (Context);
    use type CI.Eds_Object_Event;
  begin
    --IO.Put_Line ("[Canon] object event received: " & CI.Eds_Uint32'image (Event));
    if Event = CI.Object_Event_Dir_Item_Created
      or else Event = CI.Object_Event_Dir_Item_Request_Transfer
    then
      --IO.Put_Line ("[Canon] -> treating this as DirItem event");
      Event_State.Set_Item (Object);
    --else
    --  IO.Put_Line ("[Canon] -> ignoring (volume/folder/etc.)");
    end if;
    return CI.EDS_OK;
  end On_Object_Event;

  Handler : constant CI.Object_Event_Handler := On_Object_Event'access;

  -----------
  -- Error --
  -----------
  procedure Raise_Error (Msg : String) with No_Return is
  begin
    IO.Put_Line ("### " & Msg);
    raise Canon_Error;
  end Raise_Error;

  ------------------------
  -- Check return codes --
  ------------------------
  procedure Check (Where   : String;
                   Result  : CI.Eds_Error;
                   Logging : Boolean := True) is
    use type CI.Eds_Error;
  begin
    if Logging then
      IO.Put_Line (Where);
    end if;
    if Result /= CI.EDS_OK then
      Raise_Error (Where & " failed, error =" & CI.Eds_Error'image (Result));
    end if;
  end Check;

  -------------------------------------------------
  -- Local mapping for ISO to  Canon enum values --
  -------------------------------------------------
  function To_Eds_Iso (Iso : Iso_Value) return CI.Eds_Uint32 is
  begin
    case Iso is
      when 100   => return CI.K_ISO_100;
      when 200   => return CI.K_ISO_200;
      when 400   => return CI.K_ISO_400;
      when 800   => return CI.K_ISO_800;
      when 1600  => return CI.K_ISO_1600;
      when 3200  => return CI.K_ISO_3200;
      when 6400  => return CI.K_ISO_6400;
      when 12800 => return CI.K_ISO_12800;
      when 25600 => return CI.K_ISO_25600;
    end case;
  end To_Eds_Iso;

  -----------------------------------------------
  -- Local mapping for Tv to Canon enum values --
  -----------------------------------------------
  --  Only used for Exposure < 1.0 s.
  function To_Eds_Tv (T : Exposure_Time) return CI.Eds_Uint32 is
    Tus : constant Natural := Natural (T * 1_000_000.0); -- in Microseconds
  begin
    case Tus is
      when 800_000 => return CI.K_TV_0_8;
      when 600_000 => return CI.K_TV_0_6;
      when 500_000 => return CI.K_TV_0_5;
      when 400_000 => return CI.K_TV_0_4;
      when 300_000 => return CI.K_TV_0_3;
      when 250_000 => return CI.K_TV_D_4;
      when 200_000 => return CI.K_TV_D_5;
      when 166_666 => return CI.K_TV_D_6;
      when 125_000 => return CI.K_TV_D_8;
      when 100_000 => return CI.K_TV_D_10;
      when  76_923 => return CI.K_TV_D_13;
      when  66_666 => return CI.K_TV_D_15;
      when  50_000 => return CI.K_TV_D_20;
      when  40_000 => return CI.K_TV_D_25;
      when  33_333 => return CI.K_TV_D_30;
      when  25_000 => return CI.K_TV_D_40;
      when  20_000 => return CI.K_TV_D_50;
      when  16_666 => return CI.K_TV_D_60;
      when  12_500 => return CI.K_TV_D_80;
      when  10_000 => return CI.K_TV_D_100;
      when   8_000 => return CI.K_TV_D_125;
      when   6_250 => return CI.K_TV_D_160;
      when   5_000 => return CI.K_TV_D_200;
      when   4_000 => return CI.K_TV_D_250;
      when   3_125 => return CI.K_TV_D_320;
      when   2_500 => return CI.K_TV_D_400;
      when   2_000 => return CI.K_TV_D_500;
      when   1_562 => return CI.K_TV_D_640;
      when   1_250 => return CI.K_TV_D_800;
      when   1_000 => return CI.K_TV_D_1000;
      when     800 => return CI.K_TV_D_1250;
      when     625 => return CI.K_TV_D_1600;
      when     500 => return CI.K_TV_D_2000;
      when     400 => return CI.K_TV_D_2500;
      when     312 => return CI.K_TV_D_3200;
      when     250 => return CI.K_TV_D_4000;
      when others  => Raise_Error ("Not supported exposure time:" & T'image & " seconds");
    end case;
  end To_Eds_Tv;

  ---------------------
  -- Capture Picture --
  ---------------------
  procedure Capture (Filename : String;
                     Exposure : Exposure_Time;
                     Iso      : Iso_Value)
  is
    --  For short exposures, we wait Exposure + 15 s for the file event.
    --  For bulb exposures, we wait a fixed 60 s after BulbEnd.
    Camera_List : aliased CI.Camera_List;
    Camera      : aliased CI.Camera;
    Count       : aliased CI.Eds_Uint32;
    Device_Info : aliased CI.Device_Info;
    Item        : CI.Directory_Item := CI.No_Directory;

    Event_Timeout : Duration;
    Start_Time    : Ada.Calendar.Time;
    Timed_Out     : Boolean := False;

    procedure Set_Uint32_Property (Where_Label : String;
                                   Prop        : CI.Eds_Property_Id;
                                   Value       : CI.Eds_Uint32;
                                   Param       : CI.Eds_Int32 := 0)
    is
      Val : aliased CI.Eds_Uint32 := Value;
      use type CI.Eds_Uint32;
    begin
      IO.Put_Line (Where_Label & " - Propety:" & Prop'image & " - Value:" & Value'image);
      Check (Where_Label,
             CI.Set_Property_Data (Ref           => Camera,
                                   Property_Id   => Prop,
                                   Param         => Param,
                                   Property_Size => CI.Eds_Uint32'size / 8,
                                   Property_Data => Val'address));
    end Set_Uint32_Property;

  begin -- Capture
    Check ("Initialize SDK",
           CI.Initialize_SDK);

    declare
      use type CI.Eds_Int32;
      use type CI.Eds_Uint32;
      use type CI.Directory_Item;
      use type Ada.Calendar.Time;
    begin
      Check ("Get camera list",
             CI.Get_Camera_List (Camera_List'access));
      Check ("Get child count",
             CI.Get_Child_Count (Camera_List, Count'access));
      IO.Put_Line ("Number of detected cameras:" & CI.Eds_Uint32'image (Count));

      if Count = 0 then
        IO.Put_Line ("No Canon cameras found. Check USB connection and that EOS Utility is not running.");
      else
        Check ("Get camera at index 0",
               CI.Get_Camera_At_Index (Camera_List, 0, Camera'access));

        Check ("Get device information",
               CI.Get_Device_Info (Camera, Device_Info'access));
        IO.Put_Line ("Camera description: " & String_Of (Device_Info.Sz_Device_Description));
        IO.Put_Line ("Device subtype:" & Device_Info.Device_Sub_Type'image);

        --Set_Uint32_Property ("Enable Target Property",
        --                     Prop  => 16#0100_0000#,
        --                     Param => 16#00E1_3499#,
        --                     Value => CI.Prop_Id_Mirror_Lock_Up_State);

        --Set_Uint32_Property ("Enable Target Property",
        --                     Prop  => 16#0100_0000#,
        --                     Param => 16#517F_095D#,
        --                     Value => CI.Prop_Id_Mirror_Up_Setting);

        Check ("Open session",
               CI.Open_Session (Camera));

        Check ("UI lock",
               CI.Send_Status_Command (Camera,
                                       CI.Camera_Status_UI_Lock,
                                       0));
        Set_Uint32_Property (Prop        => CI.Prop_Id_Image_Quality,
                             Value       => CI.Image_Quality_LR,
                             Where_Label => "Set image quality to RAW");

        Set_Uint32_Property (Prop        => CI.Prop_Id_ISO,
                             Value       => To_Eds_Iso (Iso),
                             Where_Label => "Set ISO" & Iso'image);

        -- Choose Tv / Bulb depending on Exposure
        if Exposure < 1.0 then
          -- Short exposure: normal Tv value, TakePicture
          Set_Uint32_Property (Prop        => CI.Prop_Id_AE_Mode_Select,
                               Value       => CI.K_AE_Mode_Manual,
                               Where_Label => "Set AE Mode Select to Manual");
          Set_Uint32_Property (Prop        => CI.Prop_Id_Tv,
                               Value       => To_Eds_Tv (Exposure),
                               Where_Label => "Set exposure (Tv)" & Exposure'image);
        else
          -- Long exposure: bulb mode
          Set_Uint32_Property (Prop        => CI.Prop_Id_AE_Mode_Select,
                               Value       => CI.K_AE_Mode_Bulb,
                               Where_Label => "Set AE Mode Select to Bulb");
        end if;

      --Set_Uint32_Property (Prop        => CI.Prop_Id_Mirror_Lock_Up_State,
      --                     Value       => CI.K_Eds_Mirror_Lockup_State_Disable,
      --                     Where_Label => "Disable mirror up settings during shooting");

      --Set_Uint32_Property (Prop        => CI.Prop_Id_Mirror_Up_Setting,
      --                     Value       => CI.K_Eds_Mirror_Up_Setting_On,
      --                     Where_Label => "Mirror up");
      --delay 1.0

        Event_State.Reset;
        Start_Time := Ada.Calendar.Clock;
        Event_Timeout := Duration (Exposure + 15.0);

        Check ("Register object event handler",
               CI.Set_Object_Event_Handler (Camera,
                                            CI.Object_Event_All,
                                            Handler,
                                            System.Null_Address));
        if Exposure < 1.0 then
          Check ("Trigger single shot",
                 CI.Send_Command (Camera,
                                  CI.Camera_Command_Take_Picture,
                                  0));

        else
          Check ("Press shutter button completely non AF",
                 CI.Send_Command (Camera,
                                  CI.Camera_Command_Press_Shutter_Button,
                                  CI.Camera_Command_Shutter_Button_Completely_Non_AF));

          delay Duration (Exposure);

          Check ("Press shutter button off",
                 CI.Send_Command (Camera,
                                  CI.Camera_Command_Press_Shutter_Button,
                                  CI.Camera_Command_Shutter_Button_Off));
        end if;

        -- Now wait (with timeout) for a *file* directory item event
        loop
          Check ("Get event",
                 CI.Get_Event,
                 Logging => False);

          select
            Event_State.Wait_For_Item (Item);
            declare
              Info : aliased CI.Directory_Item_Info;
            begin
              Check ("Get directory item info",
                     CI.Get_Directory_Item_Info (Item, Info'access),
                     Logging => False);

              if Info.Is_Folder /= 0 then
              --IO.Put_Line ("received folder item, ignoring: " & String_Of (Info.Sz_File_Name));
                declare
                  Dummy : CI.Eds_Error := CI.Release (Item);
                begin
                  null;
                end;
                Item := CI.No_Directory;
              else
                IO.New_Line;
                IO.Put_Line ("Received file " & String_Of (Info.Sz_File_Name));
                exit;
              end if;
            end;
          or
            delay 0.1;
          end select;
          IO.Put (".");

          if Item /= CI.No_Directory then
            IO.Put ("???");
            exit;
          end if;

          if Ada.Calendar.Clock - Start_Time > Event_Timeout then
            Timed_Out := True;
            exit;
          end if;
        end loop;

      --Set_Uint32_Property (Prop        => CI.Prop_Id_Mirror_Up_Setting,
      --                     Value       => CI.K_Eds_Mirror_Up_Setting_Off,
      --                     Where_Label => "Mirror down");

        if Timed_Out then
          Raise_Error ("Timeout: no directory item event received");
        else
          declare
            Info              : aliased CI.Directory_Item_Info;
            File_Stream_Write : aliased CI.Stream;
            File_Name         : aliased constant String := Filename & Ascii.Nul;
          begin
            Check ("Get directory item info",
                   CI.Get_Directory_Item_Info (Item, Info'access));

            IO.Put_Line ("RAW size from camera (bytes): " & CI.Eds_Uint64'image (Info.Size));
            IO.Put_Line ("Download " & String_Of (Info.Sz_File_Name) & " to " & Filename);

            Check ("Create file stream (write)",
                   CI.Create_File_Stream (File_Name'address,
                                          CI.File_Create_Always,
                                          CI.Access_Read_Write,
                                          File_Stream_Write'access));

            Check ("Download",
                   CI.Download (Item, Info.Size, File_Stream_Write));

            Check ("Download complete",
                   CI.Download_Complete (Item));

            Check ("Delete file from camera",
                   CI.Delete_Directory_Item (Item));

            declare
              Dummy : CI.Eds_Error := CI.Release (File_Stream_Write);
            begin
              null;
            end;
            Item := CI.No_Directory;
          end;
        end if;

        Check ("UI unlock",
               CI.Send_Status_Command (Camera,
                                       CI.Camera_Status_UI_Unlock,
                                       0));
        Check ("Close session",
               CI.Close_Session (Camera));

        Check ("Release camera handle",
               CI.Release (Camera));
      end if;

      Check ("Release camera list",
             CI.Release (Camera_List));

    exception
      when Occurrence : others =>
        IO.Put_Line (Exceptions.Information_Of (Occurrence));
        declare
          Dummy : CI.Eds_Error;
        begin
          begin
            Dummy := CI.Send_Status_Command (Camera,
                                             CI.Camera_Status_UI_Unlock,
                                             0);
          exception
            when others => null;
          end;
          begin
            Dummy := CI.Close_Session (Camera);
          exception
            when others => null;
          end;
          begin
            Dummy := CI.Release (Camera);
          exception
            when others => null;
          end;
          begin
            Dummy := CI.Release (Camera_List);
          exception
            when others => null;
          end;
        end;
    end;

    Check ("Terminating SDK",
           CI.Terminate_SDK);

    IO.Put_Line ("Finished.");

  end Capture;

end Canon;
