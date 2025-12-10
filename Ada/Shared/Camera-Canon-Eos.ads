-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *                                                                                                                   *
-- *    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General     *
-- *    Public License as published by the Free Software Foundation; either version 2 of the License, or               *
-- *    (at your option) any later version.                                                                            *
-- *                                                                                                                   *
-- *    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the     *
-- *    implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License    *
-- *    for more details.                                                                                              *
-- *                                                                                                                   *
-- *    You should have received a copy of the GNU General Public License along with this program; if not, write to    *
-- *    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with C;
with System;

package Camera.Canon.Eos is

 -- interface from EDSDK.h, EDSDKTypes.h and EDSDKErrors.h

  pragma Linker_Options ("-lEDSDK");

  Byte_Size  : constant := System.Storage_Unit;
  Int32_Size : constant := 32;

  MAX_NAME : constant := 256;

  subtype Name is C.Name(1 .. MAX_NAME);

  --  Basic types
  subtype Int32   is C.Long;
  subtype Uint32  is C.Unsigned_Long;
  subtype Uint64  is C.Unsigned_Long_Long;
  subtype Int64   is C.Long_Long;

  type Ref_Count is new Uint32;

  type Result is new Uint32;

  OK : constant Result := 0;

  --  Access / create disposition
  Access_Read       : constant Int32 := 0;
  Access_Write      : constant Int32 := 1;
  Access_Read_Write : constant Int32 := 2;

  File_Create_New        : constant Int32 := 0;
  File_Create_Always     : constant Int32 := 1;
  File_Open_Existing     : constant Int32 := 2;
  File_Open_Always       : constant Int32 := 3;
  File_Truncate_Existing : constant Int32 := 4;

 --------------------------
  -- Opaque Handle Types --
  -------------------------
  type Device_List    is private;
  type Device         is private;
  type Directory_Item is private;
  type Stream         is private;

  No_Directory : constant Directory_Item;

  ------------------------
  -- SDK Initialisation --
  ------------------------
  function Initialize_SDK return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsInitializeSDK";

  function Terminate_SDK return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsTerminateSDK";

  ------------------------
  -- Camera Enumeration --
  ------------------------
  function Get_Camera_List (The_List : access Device_List) return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsGetCameraList";

  function Get_Child_Count (List      : Device_List;
                            The_Count : access Uint32) return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsGetChildCount";

  function Get_Camera_At_Index (List       : Device_List;
                                Index      : Int32;
                                The_Camera : access Device) return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsGetChildAtIndex";

  ----------------------------
  -- Get Device Information --
  ----------------------------
  type Device_Info is record
    Sz_Port_Name          : aliased Name;
    Sz_Device_Description : aliased Name;
    Device_Sub_Type       : Uint32;
    Reserved              : Uint32;
  end record with
    Pack,
    Convention => C;

  for Device_Info'size use 2 * (MAX_NAME * Byte_Size + Int32_Size);

  function Get_Device_Info (From     : Device;
                            The_Info : access Device_Info) return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsGetDeviceInfo";

  ---------------------
  -- Session Control --
  ---------------------
  function Open_Session (Item : Device) return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsOpenSession";

  function Close_Session (Item : Device) return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsCloseSession";

  --------------------------
  -- Reference Management --
  --------------------------
  function Release (Item : Device_List) return Ref_Count with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsRelease";

  function Release (Item : Device) return Ref_Count with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsRelease";

  function Release (Item : Directory_Item) return Ref_Count with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsRelease";

  function Release (Item : Stream) return Ref_Count with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsRelease";

  function Retain (Item : Device_List) return Ref_Count with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsRetain";

  function Retain (Item : Device) return Ref_Count with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsRetain";

  function Retain (Item : Directory_Item) return Ref_Count with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsRetain";

  function Retain (Item : Stream) return Ref_Count with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsRetain";

  ---------------------
  -- Camera Commands --
  ---------------------
  type Camera_Command is (
    Camera_Command_Take_Picture,         -- kEdsCameraCommand_TakePicture
    Camera_Command_Bulb_Start,           -- kEdsCameraCommand_BulbStart
    Camera_Command_Bulb_End,             -- kEdsCameraCommand_BulbEnd
    Camera_Command_Press_Shutter_Button) -- kEdsCameraCommand_PressShutterButton
    with Size => Uint32'size;

  for Camera_Command use (
    Camera_Command_Take_Picture         => 16#0000_0000#,
    Camera_Command_Bulb_Start           => 16#0000_0002#,
    Camera_Command_Bulb_End             => 16#0000_0003#,
    Camera_Command_Press_Shutter_Button => 16#0000_0004#);

  -- kEdsCameraCommand_ShutterButton_* (second parameter for Press_ShutterButton)
  type Shutter_Button is (
    Camera_Command_Shutter_Button_Off,
    Camera_Command_Shutter_Button_Halfway,
    Camera_Command_Shutter_Button_Completely,
    Camera_Command_Shutter_Button_Halfway_Non_AF,
    Camera_Command_Shutter_Button_Completely_Non_AF)
    with Size => Uint32'size;

  for Shutter_Button use (
    Camera_Command_Shutter_Button_Off               => 16#0000_0000#,
    Camera_Command_Shutter_Button_Halfway           => 16#0000_0001#,
    Camera_Command_Shutter_Button_Completely        => 16#0000_0003#,
    Camera_Command_Shutter_Button_Halfway_Non_AF    => 16#0001_0001#,
    Camera_Command_Shutter_Button_Completely_Non_AF => 16#0001_0003#);

  function Send_Command (To      : Device;
                         Command : Camera_Command;
                         Param   : Int32) return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsSendCommand";

  -------------------
  -- Object Events --
  -------------------
  subtype Object_Event is Uint32;

  Object_Event_All                       : constant Object_Event := 16#00000200#;
  Object_Event_Dir_Item_Created          : constant Object_Event := 16#00000204#;
  Object_Event_Dir_Item_Request_Transfer : constant Object_Event := 16#00000208#;

  type Object_Event_Handler is access function (Event   : Object_Event;
                                                Object  : Directory_Item;
                                                Context : System.Address) return Result
    with Convention => StdCall;

  function Set_Object_Event_Handler (For_Camera : Device;
                                     Event      : Object_Event;
                                     Handler    : Object_Event_Handler;
                                     Context    : System.Address) return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsSetObjectEventHandler";

  -------------------
  -- Event Pumping --
  -------------------
  function Get_Event return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsGetEvent";

  -------------------------
  -- Directory item info --
  -------------------------
  type Directory_Item_Info is record
    Size         : Uint64;
    Is_Folder    : Int32; -- EdsBool is 'int' in C
    Group_Id     : Uint32;
    Option       : Uint32;
    Sz_File_Name : aliased Name;
    Format       : Uint32;
    Date_Time    : Uint32;
  end record with
    Pack,
    Convention => C;

  function Get_Directory_Item_Info (Item     : Directory_Item;
                                    The_Info : access Directory_Item_Info) return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsGetDirectoryItemInfo";

  ---------------------------
  -- Delete Directory Item --
  ---------------------------
  function Delete_Directory_Item (Item : Directory_Item) return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsDeleteDirectoryItem";

  ----------------------
  -- File I/O on host --
  ----------------------
  function Create_File_Stream
    (File_Name   : System.Address;
     Disposition : Int32; -- EdsFileCreateDisposition
     Eds_Access  : Int32;
     Out_Stream  : access Stream) return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsCreateFileStream";

  function Download (Item      : Directory_Item;
                     Read_Size : Uint64;
                     Dest      : Stream) return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsDownload";

  function Download_Complete (Item : Directory_Item) return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsDownloadComplete";

  ----------------
  -- Properties --
  ----------------
  type Property_Id is (
    Prop_Id_Image_Quality,        -- kEdsPropID_ImageQuality
    Prop_Id_AE_Mode,              -- kEdsPropID_AEMode
    Prop_Id_Drive_Mode,           -- kEdsPropID_DriveMode
    Prop_Id_ISO,                  -- kEdsPropID_ISOSpeed
    Prop_Id_Tv,                   -- kEdsPropID_Tv
    Prop_Id_AE_Mode_Select,       -- kEdsPropID_AEModeSelect
    Prop_Id_Evf_Output_Device,    -- kEdsPropID_Evf_OutputDevice
    Prop_Id_Mirror_Lock_Up_State, -- kEdsPropID_MirrorLockUpState
    Prop_Id_Mirror_Up_Setting)    -- kEdsPropID_MirrorUpSetting
    with Size => Uint32'size;

  for Property_Id use (
    Prop_Id_Image_Quality        => 16#0000_0100#,
    Prop_Id_AE_Mode              => 16#0000_0400#,
    Prop_Id_Drive_Mode           => 16#0000_0401#,
    Prop_Id_ISO                  => 16#0000_0402#,
    Prop_Id_Tv                   => 16#0000_0406#,
    Prop_Id_AE_Mode_Select       => 16#0000_0436#,
    Prop_Id_Evf_Output_Device    => 16#0000_0500#,
    Prop_Id_Mirror_Lock_Up_State => 16#0100_0421#,
    Prop_Id_Mirror_Up_Setting    => 16#0100_0438#);

  type Evf_Output_Device is (
     Evf_Output_None, -- EVF disabled
     Evf_Output_TFT,  -- Camera LCD screen
     Evf_Output_PC)   -- LiveView to PC
     with Size => Uint32'size;

  for Evf_Output_Device use (-- code kEdsEvfOutputDevice*
     Evf_Output_None => 0,
     Evf_Output_TFT  => 1,
     Evf_Output_PC   => 2);

  subtype Image_Quality is Uint32;

  -- RAW only (no JPEG) – from EdsImageQuality_LR
  Image_Quality_LR : constant Image_Quality := 16#0064FF0F#;

  type ISO_Speed is (
    K_ISO_6,
    K_ISO_12,
    K_ISO_25,
    K_ISO_50,
    K_ISO_100,
    K_ISO_125,
    K_ISO_160,
    K_ISO_200,
    K_ISO_250,
    K_ISO_320,
    K_ISO_400,
    K_ISO_500,
    K_ISO_640,
    K_ISO_800,
    K_ISO_1000,
    K_ISO_1250,
    K_ISO_1600,
    K_ISO_2000,
    K_ISO_2500,
    K_ISO_3200,
    K_ISO_4000,
    K_ISO_5000,
    K_ISO_6400,
    K_ISO_8000,
    K_ISO_10000,
    K_ISO_12800,
    K_ISO_16000,
    K_ISO_20000,
    K_ISO_25600,
    K_ISO_32000,
    K_ISO_40000,
    K_ISO_51200,
    K_ISO_64000,
    K_ISO_80000,
    K_ISO_102400,
    K_ISO_204800,
    K_ISO_409600,
    K_ISO_819200)
    with Size => Uint32'size;

  for ISO_Speed use (-- codes EdsISOSpeed_*
    K_ISO_6      => 16#28#,
    K_ISO_12     => 16#30#,
    K_ISO_25     => 16#38#,
    K_ISO_50     => 16#40#,
    K_ISO_100    => 16#48#,
    K_ISO_125    => 16#4B#,
    K_ISO_160    => 16#4D#,
    K_ISO_200    => 16#50#,
    K_ISO_250    => 16#53#,
    K_ISO_320    => 16#55#,
    K_ISO_400    => 16#58#,
    K_ISO_500    => 16#5B#,
    K_ISO_640    => 16#5D#,
    K_ISO_800    => 16#60#,
    K_ISO_1000   => 16#63#,
    K_ISO_1250   => 16#65#,
    K_ISO_1600   => 16#68#,
    K_ISO_2000   => 16#6B#,
    K_ISO_2500   => 16#6D#,
    K_ISO_3200   => 16#70#,
    K_ISO_4000   => 16#73#,
    K_ISO_5000   => 16#75#,
    K_ISO_6400   => 16#78#,
    K_ISO_8000   => 16#7B#,
    K_ISO_10000  => 16#7D#,
    K_ISO_12800  => 16#80#,
    K_ISO_16000  => 16#83#,
    K_ISO_20000  => 16#85#,
    K_ISO_25600  => 16#88#,
    K_ISO_32000  => 16#8B#,
    K_ISO_40000  => 16#8D#,
    K_ISO_51200  => 16#90#,
    K_ISO_64000  => 16#93#,
    K_ISO_80000  => 16#95#,
    K_ISO_102400 => 16#98#,
    K_ISO_204800 => 16#A0#,
    K_ISO_409600 => 16#A8#,
    K_ISO_819200 => 16#B0#);

 -- used by Exposure_Time
 type Tv_Value is (
    K_Tv_Bulb,
    K_TV_30,
    K_TV_25,
    K_TV_20,
    K_TV_20_S,
    K_TV_15,
    K_TV_13,
    K_TV_10,
    K_TV_10_S,
    K_TV_8,
    K_TV_6_S,
    K_TV_6,
    K_TV_5,
    K_TV_4,
    K_TV_3_2,
    K_TV_2_5,
    K_TV_2,
    K_TV_1_6,
    K_TV_1_3,
    K_TV_1,
    K_TV_0_8,
    K_TV_0_6,
    K_TV_0_5,
    K_TV_0_4,
    K_TV_0_3,
    K_TV_0_3_S,
    K_TV_D_4,
    K_TV_D_5,
    K_TV_D_6,
    K_TV_D_6_S,
    K_TV_D_8,
    K_TV_D_10_S,
    K_TV_D_10,
    K_TV_D_13,
    K_TV_D_15,
    K_TV_D_20_S,
    K_TV_D_20,
    K_TV_D_25,
    K_TV_D_30,
    K_TV_D_40,
    K_TV_D_50,
    K_TV_D_60,
    K_TV_D_80,
    K_TV_D_100,
    K_TV_D_125,
    K_TV_D_160,
    K_TV_D_200,
    K_TV_D_250,
    K_TV_D_320,
    K_TV_D_400,
    K_TV_D_500,
    K_TV_D_640,
    K_TV_D_800,
    K_TV_D_1000,
    K_TV_D_1250,
    K_TV_D_1600,
    K_TV_D_2000,
    K_TV_D_2500,
    K_TV_D_3200,
    K_TV_D_4000,
    K_TV_D_5000,
    K_TV_D_6000,
    K_TV_D_6400,
    K_TV_D_8000,
    K_TV_D_10000,
    K_TV_D_12800,
    K_TV_D_16000,
    K_TV_D_20000,
    K_TV_D_25600,
    K_TV_D_32000)
    with Size => Uint32'size;

 for Tv_Value use ( -- codes EdsTv_*
    K_Tv_Bulb    => 16#0C#,
    K_TV_30      => 16#10#,
    K_TV_25      => 16#13#,
    K_TV_20      => 16#14#,
    K_TV_20_S    => 16#15#,
    K_TV_15      => 16#18#,
    K_TV_13      => 16#1B#,
    K_TV_10      => 16#1C#,
    K_TV_10_S    => 16#1D#,
    K_TV_8       => 16#20#,
    K_TV_6_S     => 16#23#,
    K_TV_6       => 16#24#,
    K_TV_5       => 16#25#,
    K_TV_4       => 16#28#,
    K_TV_3_2     => 16#2B#,
    K_TV_2_5     => 16#2D#,
    K_TV_2       => 16#30#,
    K_TV_1_6     => 16#33#,
    K_TV_1_3     => 16#35#,
    K_TV_1       => 16#38#,
    K_TV_0_8     => 16#3B#,
    K_TV_0_6     => 16#3D#,
    K_TV_0_5     => 16#40#,
    K_TV_0_4     => 16#43#,
    K_TV_0_3     => 16#44#,
    K_TV_0_3_S   => 16#45#,
    K_TV_D_4     => 16#48#,
    K_TV_D_5     => 16#4B#,
    K_TV_D_6     => 16#4C#,
    K_TV_D_6_S   => 16#4D#,
    K_TV_D_8     => 16#50#,
    K_TV_D_10_S  => 16#53#,
    K_TV_D_10    => 16#54#,
    K_TV_D_13    => 16#55#,
    K_TV_D_15    => 16#58#,
    K_TV_D_20_S  => 16#5B#,
    K_TV_D_20    => 16#5C#,
    K_TV_D_25    => 16#5D#,
    K_TV_D_30    => 16#60#,
    K_TV_D_40    => 16#63#,
    K_TV_D_50    => 16#65#,
    K_TV_D_60    => 16#68#,
    K_TV_D_80    => 16#6B#,
    K_TV_D_100   => 16#6D#,
    K_TV_D_125   => 16#70#,
    K_TV_D_160   => 16#73#,
    K_TV_D_200   => 16#75#,
    K_TV_D_250   => 16#78#,
    K_TV_D_320   => 16#7B#,
    K_TV_D_400   => 16#7D#,
    K_TV_D_500   => 16#80#,
    K_TV_D_640   => 16#83#,
    K_TV_D_800   => 16#85#,
    K_TV_D_1000  => 16#88#,
    K_TV_D_1250  => 16#8B#,
    K_TV_D_1600  => 16#8D#,
    K_TV_D_2000  => 16#90#,
    K_TV_D_2500  => 16#93#,
    K_TV_D_3200  => 16#95#,
    K_TV_D_4000  => 16#98#,
    K_TV_D_5000  => 16#9B#,
    K_TV_D_6000  => 16#9C#,
    K_TV_D_6400  => 16#9D#,
    K_TV_D_8000  => 16#A0#,
    K_TV_D_10000 => 16#A3#,
    K_TV_D_12800 => 16#A5#,
    K_TV_D_16000 => 16#A8#,
    K_TV_D_20000 => 16#AB#,
    K_TV_D_25600 => 16#AD#,
    K_TV_D_32000 => 16#B0#);

  type AE_Mode is (
    K_AE_Mode_Manual,
    K_AE_Mode_Bulb)
    with Size => Uint32'size;

  for AE_Mode use (  -- codes EdsAEMode*
    K_AE_Mode_Manual => 16#03#,
    K_AE_Mode_Bulb   => 16#04#);

  type Mirror_Lockup_State is (
    K_Eds_Mirror_Lockup_State_Disable,
    K_Eds_Mirror_Lockup_State_Enable,
    K_Eds_Mirror_Lockup_During_Shooting)
    with Size => Uint32'size;

  for Mirror_Lockup_State use (         -- codes kEdsMirrorLockupState*
    K_Eds_Mirror_Lockup_State_Disable   => 0,
    K_Eds_Mirror_Lockup_State_Enable    => 1,
    K_Eds_Mirror_Lockup_During_Shooting => 2);

  type Mirror_Up_Setting is (
    K_Eds_Mirror_Up_Setting_Off,
    K_Eds_Mirror_Up_Setting_On)
    with Size => Uint32'size;

  for Mirror_Up_Setting use (   -- codes kEdsMirrorUpSetting*
    K_Eds_Mirror_Up_Setting_Off => 0,
    K_Eds_Mirror_Up_Setting_On  => 1);

  function Get_Property_Data (Item     : Device;
                              Property : Property_Id;
                              Param    : Int32;
                              Size     : Uint32;
                              Data     : System.Address) return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsGetPropertyData";

  function Set_Property_Data (Item     : Device;
                              Property : Property_Id;
                              Param    : Int32;
                              Size     : Uint32;
                              Data     : System.Address) return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsSetPropertyData";

  ---------------------
  -- Status Commands --
  ---------------------
  subtype Camera_Status_Command is Uint32;

  -- kEdsCameraStatusCommand_UI_*
  Camera_Status_UI_Lock   : constant Camera_Status_Command := 16#00000000#;
  Camera_Status_UI_Unlock : constant Camera_Status_Command := 16#00000001#;

  function Send_Status_Command (Item  : Device;
                                State : Camera_Status_Command;
                                Param : Int32) return Result with
    Import        => True,
    Convention    => StdCall,
    External_Name => "EdsSendStatusCommand";

private
  type Device_List    is new System.Address;
  type Device         is new System.Address;
  type Directory_Item is new System.Address;
  type Stream         is new System.Address;

  No_Directory : constant Directory_Item :=
    Directory_Item (System.Null_Address);

end Camera.Canon.Eos;
