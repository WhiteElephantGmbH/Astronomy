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
  subtype Error   is C.Int;
  subtype Int32   is C.Int;
  subtype Uint32  is C.Unsigned_Long;
  subtype Uint64  is C.Unsigned_Long_Long;
  subtype Int64   is C.Long_Long;

  OK : constant Error := 0;

  --  Access / create disposition
  Access_Read       : constant Int32 := 0;
  Access_Write      : constant Int32 := 1;
  Access_Read_Write : constant Int32 := 2;

  File_Create_New        : constant Int32 := 0;
  File_Create_Always     : constant Int32 := 1;
  File_Open_Existing     : constant Int32 := 2;
  File_Open_Always       : constant Int32 := 3;
  File_Truncate_Existing : constant Int32 := 4;

 ---------------------------------
  -- Opaque Handle Types (void*) --
  ---------------------------------

  type Device_List    is private;
  type Device         is private;
  type Directory_Item is private;
  type Stream         is private;

  No_Directory : constant Directory_Item;

  ------------------------
  -- SDK Initialisation --
  ------------------------

  function Initialize_SDK return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsInitializeSDK";

  function Terminate_SDK return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsTerminateSDK";

  ------------------------
  -- Camera Enumeration --
  ------------------------

  function Get_Camera_List (The_List : access Device_List) return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsGetCameraList";

  function Get_Child_Count (List      : Device_List;
                            The_Count : access Uint32) return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsGetChildCount";

  function Get_Camera_At_Index (List       : Device_List;
                                Index      : Int32;
                                The_Camera : access Device) return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsGetChildAtIndex";

  ----------------------------
  -- Get Device Information --
  ----------------------------

  type Device_Info is record
    Sz_Port_Name          : aliased Name;
    Sz_Device_Description : aliased Name;
    Device_Sub_Type       : Uint32;
    Reserved              : Uint32;
  end record
  with
    Pack,
    Convention => C;

  for Device_Info'size use 2 * (MAX_NAME * Byte_Size + Int32_Size);

  function Get_Device_Info (From     : Device;
                            The_Info : access Device_Info) return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsGetDeviceInfo";

  ---------------------
  -- Session Control --
  ---------------------

  function Open_Session (Item : Device) return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsOpenSession";

  function Close_Session (Item : Device) return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsCloseSession";

  --------------------------
  -- Reference Management --
  --------------------------

  function Release (Item : Device_List) return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsRelease";

  function Release (Item : Device) return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsRelease";

  function Release (Item : Directory_Item) return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsRelease";

  function Release (Item : Stream) return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsRelease";

  ---------------------
  -- Camera Commands --
  ---------------------

  Camera_Command_Take_Picture : constant Uint32 := 16#00000000#;
  Camera_Command_Bulb_Start   : constant Uint32 := 16#00000002#;
  Camera_Command_Bulp_End     : constant Uint32 := 16#00000003#;

  function Send_Command (To      : Device;
                         Command : Uint32;
                         Param   : Uint32) return Error
  with
    Import        => True,
    Convention    => C,
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
                                                Context : System.Address) return Error
  with
    Convention => StdCall;

  function Set_Object_Event_Handler (For_Camera : Device;
                                     Event      : Object_Event;
                                     Handler    : Object_Event_Handler;
                                     Context    : System.Address) return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsSetObjectEventHandler";

  -------------------
  -- Event Pumping --
  -------------------

  function Get_Event return Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsGetEvent";

  -------------------------
  -- Directory item info --
  -------------------------

  type Directory_Item_Info is record
    Size         : Uint64;
    Is_Folder    : Int32;  -- EdsBool is 'int' in C
    Group_Id     : Uint32;
    Option       : Uint32;
    Sz_File_Name : aliased Name;
    Format       : Uint32;
    Date_Time    : Uint32;
  end record
  with
    Pack,
    Convention => C;

  function Get_Directory_Item_Info (Item     : Directory_Item;
                                    The_Info : access Directory_Item_Info) return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsGetDirectoryItemInfo";

  ---------------------------
  -- Delete Directory Item --
  ---------------------------

  function Delete_Directory_Item (Item : Directory_Item) return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsDeleteDirectoryItem";

  ----------------------
  -- File I/O on host --
  ----------------------

  function Create_File_Stream
    (File_Name   : System.Address;
     Disposition : Int32; -- EdsFileCreateDisposition
     Eds_Access  : Int32;
     Out_Stream  : access Stream) return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsCreateFileStream";

  function Download (Item      : Directory_Item;
                     Read_Size : Uint64;
                     Dest      : Stream) return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsDownload";

  function Download_Complete (Item : Directory_Item) return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsDownloadComplete";

  ----------------
  -- Properties --
  ----------------

  subtype Property_Id   is Uint32;
  subtype Image_Quality is Uint32;

  --  property ids
  Prop_Id_Tv            : constant Property_Id := 16#0000_0406#;
  Prop_Id_ISO           : constant Property_Id := 16#0000_0402#;
  Prop_Id_Image_Quality : constant Property_Id := 16#0000_0100#;

  -- ISO codes (EdsISOSpeed_*)
  K_ISO_6      : constant Uint32 := 16#28#;
  K_ISO_12     : constant Uint32 := 16#30#;
  K_ISO_25     : constant Uint32 := 16#38#;
  K_ISO_50     : constant Uint32 := 16#40#;
  K_ISO_100    : constant Uint32 := 16#48#;
  K_ISO_125    : constant Uint32 := 16#4B#;
  K_ISO_160    : constant Uint32 := 16#4D#;
  K_ISO_200    : constant Uint32 := 16#50#;
  K_ISO_250    : constant Uint32 := 16#53#;
  K_ISO_320    : constant Uint32 := 16#55#;
  K_ISO_400    : constant Uint32 := 16#58#;
  K_ISO_500    : constant Uint32 := 16#5B#;
  K_ISO_640    : constant Uint32 := 16#5D#;
  K_ISO_800    : constant Uint32 := 16#60#;
  K_ISO_1000   : constant Uint32 := 16#63#;
  K_ISO_1250   : constant Uint32 := 16#65#;
  K_ISO_1600   : constant Uint32 := 16#68#;
  K_ISO_2000   : constant Uint32 := 16#6B#;
  K_ISO_2500   : constant Uint32 := 16#6D#;
  K_ISO_3200   : constant Uint32 := 16#70#;
  K_ISO_4000   : constant Uint32 := 16#73#;
  K_ISO_5000   : constant Uint32 := 16#75#;
  K_ISO_6400   : constant Uint32 := 16#78#;
  K_ISO_8000   : constant Uint32 := 16#7B#;
  K_ISO_10000  : constant Uint32 := 16#7D#;
  K_ISO_12800  : constant Uint32 := 16#80#;
  K_ISO_16000  : constant Uint32 := 16#83#;
  K_ISO_20000  : constant Uint32 := 16#85#;
  K_ISO_25600  : constant Uint32 := 16#88#;
  K_ISO_32000  : constant Uint32 := 16#8B#;
  K_ISO_40000  : constant Uint32 := 16#8D#;
  K_ISO_51200  : constant Uint32 := 16#90#;
  K_ISO_64000  : constant Uint32 := 16#93#;
  K_ISO_80000  : constant Uint32 := 16#95#;
  K_ISO_102400 : constant Uint32 := 16#98#;
  K_ISO_204800 : constant Uint32 := 16#A0#;
  K_ISO_409600 : constant Uint32 := 16#A8#;
  K_ISO_819200 : constant Uint32 := 16#B0#;

  -- Tv codes (EdsTv_*) for the values allowed by Exposure_Time
  K_Tv_Bulb    : constant Uint32 := 16#0C#;
  K_TV_30      : constant Uint32 := 16#10#;
  K_TV_25      : constant Uint32 := 16#13#;
  K_TV_20      : constant Uint32 := 16#15#; -- (1/3)
  K_TV_15      : constant Uint32 := 16#18#;
  K_TV_13      : constant Uint32 := 16#1B#;
  K_TV_10      : constant Uint32 := 16#1D#; -- (1/3)
  K_TV_8       : constant Uint32 := 16#20#;
  K_TV_6       : constant Uint32 := 16#23#; -- (1/3)
  K_TV_5       : constant Uint32 := 16#25#;
  K_TV_4       : constant Uint32 := 16#28#;
  K_TV_3_2     : constant Uint32 := 16#2B#;
  K_TV_2_5     : constant Uint32 := 16#2D#;
  K_TV_2       : constant Uint32 := 16#30#;
  K_TV_1_6     : constant Uint32 := 16#33#;
  K_TV_1_3     : constant Uint32 := 16#35#;
  K_TV_1       : constant Uint32 := 16#38#;
  K_TV_0_8     : constant Uint32 := 16#3B#;
  K_TV_0_6     : constant Uint32 := 16#3D#;
  K_TV_0_5     : constant Uint32 := 16#40#;
  K_TV_0_4     : constant Uint32 := 16#43#;
  K_TV_0_3     : constant Uint32 := 16#45#; -- (1/3)
  K_TV_D_4     : constant Uint32 := 16#48#;
  K_TV_D_5     : constant Uint32 := 16#4B#;
  K_TV_D_6     : constant Uint32 := 16#4D#; -- (1/3)
  K_TV_D_8     : constant Uint32 := 16#50#;
  K_TV_D_10    : constant Uint32 := 16#54#;
  K_TV_D_13    : constant Uint32 := 16#55#;
  K_TV_D_15    : constant Uint32 := 16#58#;
  K_TV_D_20    : constant Uint32 := 16#5C#;
  K_TV_D_25    : constant Uint32 := 16#5D#;
  K_TV_D_30    : constant Uint32 := 16#60#;
  K_TV_D_40    : constant Uint32 := 16#63#;
  K_TV_D_50    : constant Uint32 := 16#65#;
  K_TV_D_60    : constant Uint32 := 16#68#;
  K_TV_D_80    : constant Uint32 := 16#6B#;
  K_TV_D_100   : constant Uint32 := 16#6D#;
  K_TV_D_125   : constant Uint32 := 16#70#;
  K_TV_D_160   : constant Uint32 := 16#73#;
  K_TV_D_200   : constant Uint32 := 16#75#;
  K_TV_D_250   : constant Uint32 := 16#78#;
  K_TV_D_320   : constant Uint32 := 16#7B#;
  K_TV_D_400   : constant Uint32 := 16#7D#;
  K_TV_D_500   : constant Uint32 := 16#80#;
  K_TV_D_640   : constant Uint32 := 16#83#;
  K_TV_D_800   : constant Uint32 := 16#85#;
  K_TV_D_1000  : constant Uint32 := 16#88#;
  K_TV_D_1250  : constant Uint32 := 16#8B#;
  K_TV_D_1600  : constant Uint32 := 16#8D#;
  K_TV_D_2000  : constant Uint32 := 16#90#;
  K_TV_D_2500  : constant Uint32 := 16#93#;
  K_TV_D_3200  : constant Uint32 := 16#95#;
  K_TV_D_4000  : constant Uint32 := 16#98#;
  K_TV_D_5000  : constant Uint32 := 16#9B#;
  K_TV_D_6000  : constant Uint32 := 16#9C#;
  K_TV_D_6400  : constant Uint32 := 16#9D#;
  K_TV_D_8000  : constant Uint32 := 16#A0#;
  K_TV_D_10000 : constant Uint32 := 16#A3#;
  K_TV_D_12800 : constant Uint32 := 16#A5#;
  K_TV_D_16000 : constant Uint32 := 16#A8#;
  K_TV_D_20000 : constant Uint32 := 16#AB#;
  K_TV_D_25600 : constant Uint32 := 16#AD#;
  K_TV_D_32000 : constant Uint32 := 16#B0#;

  -- RAW only (no JPEG) – from EdsImageQuality_LR
  Image_Quality_LR : constant Image_Quality := 16#0064FF0F#;

  function Set_Property_Data (Item     : Device;
                              Property : Property_Id;
                              Param    : Int32;
                              Size     : Uint32;
                              Data     : System.Address) return Error
  with
    Import        => True,
    Convention    => C,
    External_Name => "EdsSetPropertyData";

private

  type Device_List    is new System.Address;
  type Device         is new System.Address;
  type Directory_Item is new System.Address;
  type Stream         is new System.Address;

  No_Directory : constant Directory_Item := Directory_Item (System.Null_Address);

end Camera.Canon.Eos;
