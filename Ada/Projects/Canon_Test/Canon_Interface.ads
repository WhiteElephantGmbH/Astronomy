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

with Interfaces.C;
with System;

package Canon_Interface is

  pragma Linker_Options ("-lEDSDK");

  package C renames Interfaces.C;

  Byte_Size  : constant := System.Storage_Unit;
  Int32_Size : constant := 32;

  EDS_MAX_NAME : constant := 256;

  --  Basic EDSDK integer types
  subtype Eds_Error   is C.int;
  subtype Eds_Int32   is C.int;
  subtype Eds_Uint32  is C.unsigned_long;
  subtype Eds_Uint64  is C.unsigned_long_long;
  subtype Eds_Int64   is C.long_long;

  --  Access / create disposition (from EDSDKTypes.h)
  Access_Read       : constant Eds_Int32 := 0;
  Access_Write      : constant Eds_Int32 := 1;
  Access_Read_Write : constant Eds_Int32 := 2;

  File_Create_New        : constant Eds_Int32 := 0;
  File_Create_Always     : constant Eds_Int32 := 1;
  File_Open_Existing     : constant Eds_Int32 := 2;
  File_Open_Always       : constant Eds_Int32 := 3;
  File_Truncate_Existing : constant Eds_Int32 := 4;

 ---------------------------------
  -- Opaque Handle Types (void*) --
  ---------------------------------

  type Camera_List    is private;
  type Camera         is private;
  type Directory_Item is private;
  type Stream         is private;

  No_Directory : constant Directory_Item;

  ------------------------
  -- SDK Initialisation --
  ------------------------

  function Initialize_SDK return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsInitializeSDK";

  function Terminate_SDK return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsTerminateSDK";

  ------------------------
  -- Camera Enumeration --
  ------------------------

  function Get_Camera_List (Out_List : access Camera_List) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsGetCameraList";

  function Get_Child_Count (List  : Camera_List;
                            Count : access Eds_Uint32) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsGetChildCount";

  function Get_Camera_At_Index (List       : Camera_List;
                                Index      : Eds_Int32;
                                Out_Camera : access Camera) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsGetChildAtIndex";

  ----------------------------
  -- Get Device Information --
  ----------------------------

  type Device_Info is record
    Sz_Port_Name          : aliased String (1 .. EDS_MAX_NAME);
    Sz_Device_Description : aliased String (1 .. EDS_MAX_NAME);
    Device_Sub_Type       : Eds_Uint32;
    Reserved              : Eds_Uint32;
  end record
    with Convention => C;

  for Device_Info use record
    Sz_Port_Name          at 0                                         range 0 .. EDS_MAX_NAME * Byte_Size - 1;
    Sz_Device_Description at EDS_MAX_NAME                              range 0 .. EDS_MAX_NAME * Byte_Size - 1;
    Device_Sub_Type       at EDS_MAX_NAME * 2                          range 0 .. Int32_Size - 1;
    Reserved              at EDS_MAX_NAME * 2 + 32 / Byte_Size         range 0 .. Int32_Size - 1;
  end record;

  for Device_Info'size use 2 * (EDS_MAX_NAME * Byte_Size + Int32_Size);

  function Get_Device_Info (The_Camera : Camera;
                            Out_Info   : access Device_Info) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsGetDeviceInfo";

  ---------------------
  -- Session Control --
  ---------------------

  function Open_Session (Cam : Camera) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsOpenSession";

  function Close_Session (Cam : Camera) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsCloseSession";

  --------------------------
  -- Reference Management --
  --------------------------

  function Release (Ref : Camera_List) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsRelease";

  function Release (Ref : Camera) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsRelease";

  function Release (Ref : Directory_Item) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsRelease";

  function Release (Ref : Stream) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsRelease";

  ---------------------
  -- Camera Commands --
  ---------------------

  Camera_Command_Take_Picture : constant Eds_Uint32 := 16#00000000#;

  function Send_Command (Cam     : Camera;
                         Command : Eds_Uint32;
                         Param   : Eds_Uint32) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsSendCommand";

  -------------------
  -- Object Events --
  -------------------

  subtype Eds_Object_Event is Eds_Uint32;

  Object_Event_All                       : constant Eds_Object_Event := 16#00000200#;
  Object_Event_Dir_Item_Created          : constant Eds_Object_Event := 16#00000204#;
  Object_Event_Dir_Item_Request_Transfer : constant Eds_Object_Event := 16#00000208#;

  type Object_Event_Handler is access function
    (Event   : Eds_Object_Event;
     Object  : Directory_Item;
     Context : System.Address) return Eds_Error
    with Convention => StdCall;

  function Set_Object_Event_Handler (The_Camera : Camera;
                                     Event      : Eds_Object_Event;
                                     Handler    : Object_Event_Handler;
                                     Context    : System.Address) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsSetObjectEventHandler";

  -------------------
  -- Event Pumping --
  -------------------

  function Get_Event return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsGetEvent";

  -------------------------
  -- Directory item info --
  -------------------------

  type Directory_Item_Info is record
    Size         : Eds_Uint64;
    Is_Folder    : Eds_Int32;  -- EdsBool is 'int' in C
    Group_Id     : Eds_Uint32;
    Option       : Eds_Uint32;
    Sz_File_Name : aliased String (1 .. EDS_MAX_NAME);
    Format       : Eds_Uint32;
    Date_Time    : Eds_Uint32;
  end record
    with Convention => C;

  for Directory_Item_Info use record
    Size         at 0                     range 0 .. Eds_Uint64'size - 1;
    Is_Folder    at 8                     range 0 .. Int32_Size - 1;
    Group_Id     at 12                    range 0 .. Int32_Size - 1;
    Option       at 16                    range 0 .. Int32_Size - 1;
    Sz_File_Name at 20                    range 0 .. EDS_MAX_NAME * Byte_Size - 1;
    Format       at 20 + EDS_MAX_NAME     range 0 .. Int32_Size - 1;
    Date_Time    at 20 + EDS_MAX_NAME + 4 range 0 .. Int32_Size - 1;
  end record;

  for Directory_Item_Info'size use
    (8 + 4 + 4 + 4 + EDS_MAX_NAME + 4 + 4) * Byte_Size;

  function Get_Directory_Item_Info (Item : Directory_Item;
                                    Info : access Directory_Item_Info) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsGetDirectoryItemInfo";

  ---------------------------
  -- Delete Directory Item --
  ---------------------------

  function Delete_Directory_Item (Item : Directory_Item) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsDeleteDirectoryItem";

  ----------------------
  -- File I/O on host --
  ----------------------

  function Create_File_Stream
    (File_Name   : System.Address;
     Disposition : Eds_Int32;  -- EdsFileCreateDisposition
     Eds_Access  : Eds_Int32;  -- EdsAccess
     Out_Stream  : access Stream) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsCreateFileStream";

  function Download
    (Item      : Directory_Item;
     Read_Size : Eds_Uint64;
     Dest      : Stream) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsDownload";

  function Download_Complete
    (Item : Directory_Item) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsDownloadComplete";

  ----------------
  -- Properties --
  ----------------

  subtype Eds_Property_Id   is Eds_Uint32;
  subtype Eds_Image_Quality is Eds_Uint32;

  --  property ids
  Prop_Id_Tv            : constant Eds_Property_Id := 16#0000_0406#;
  Prop_Id_ISO           : constant Eds_Property_Id := 16#0000_0402#;
  Prop_Id_Image_Quality : constant Eds_Property_Id := 16#0000_0100#;

  -- ISO codes (EdsISOSpeed_*)
  K_ISO_100   : constant Eds_Uint32 := 16#48#;
  K_ISO_200   : constant Eds_Uint32 := 16#50#;
  K_ISO_400   : constant Eds_Uint32 := 16#58#;
  K_ISO_800   : constant Eds_Uint32 := 16#60#;
  K_ISO_1600  : constant Eds_Uint32 := 16#68#;
  K_ISO_3200  : constant Eds_Uint32 := 16#70#;
  K_ISO_6400  : constant Eds_Uint32 := 16#78#;
  K_ISO_12800 : constant Eds_Uint32 := 16#80#;
  K_ISO_25600 : constant Eds_Uint32 := 16#88#;

  -- Tv codes (EdsTv_*) for the values allowed by Exposure_Time
  K_Tv_30   : constant Eds_Uint32 := 16#10#;
  K_Tv_25   : constant Eds_Uint32 := 16#13#;
  K_Tv_20   : constant Eds_Uint32 := 16#14#;
  K_Tv_15   : constant Eds_Uint32 := 16#15#;
  K_Tv_13   : constant Eds_Uint32 := 16#18#;
  K_Tv_10   : constant Eds_Uint32 := 16#1B#;
  K_Tv_8    : constant Eds_Uint32 := 16#1C#;
  K_Tv_6    : constant Eds_Uint32 := 16#1D#;
  K_Tv_5    : constant Eds_Uint32 := 16#20#;
  K_Tv_4    : constant Eds_Uint32 := 16#23#;
  K_Tv_3_2  : constant Eds_Uint32 := 16#24#;
  K_Tv_2    : constant Eds_Uint32 := 16#28#;
  K_Tv_1    : constant Eds_Uint32 := 16#2D#;

  -- RAW only (no JPEG) – from EdsImageQuality_LR
  Image_Quality_LR : constant Eds_Image_Quality := 16#0064FF0F#;

  function Set_Property_Data
    (Ref           : Camera;
     Property_Id   : Eds_Property_Id;
     Param         : Eds_Int32;
     Property_Size : Eds_Uint32;
     Property_Data : System.Address) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsSetPropertyData";

private

  type Camera_List    is new System.Address;
  type Camera         is new System.Address;
  type Directory_Item is new System.Address;
  type Stream         is new System.Address;

  No_Directory : constant Directory_Item := Directory_Item (System.Null_Address);

end Canon_Interface;
