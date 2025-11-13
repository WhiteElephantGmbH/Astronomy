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

  --  Basic EDSDK integer types (check against EDSDKTypes.h and tweak if needed)
  subtype Eds_Error   is C.int;
  subtype Eds_Int32   is C.int;
  subtype Eds_Uint32  is C.unsigned_long;
  subtype Eds_Uint64  is C.unsigned_long_long;

  ---------------------------------
  -- Opaque handle types (void*) --
  ---------------------------------

  --  In EDSDK headers:
  --    typedef void *EdsBaseRef;
  --    typedef EdsBaseRef EdsCameraListRef;
  --    typedef EdsBaseRef EdsCameraRef;
  --    typedef EdsBaseRef EdsDirectoryItemRef;
  --    typedef EdsBaseRef EdsStreamRef;
  --
  --  We wrap these as private types over System.Address.
  --  You pass them by value to C; for out-parameters, use 'Access.

  type Camera_List    is private;
  type Camera         is private;
  type Directory_Item is private;
  type Stream         is private;

  ---------------------------
  -- SDK initialisation    --
  ---------------------------

  function Initialize_SDK return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsInitializeSDK";

  function Terminate_SDK return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsTerminateSDK";

  ---------------------------
  -- Camera enumeration    --
  ---------------------------

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
    Reserved              at EDS_MAX_NAME * 2 + 32 / Byte_Size range 0 .. Int32_Size - 1;
  end record;
  for Device_Info'size use 2 * (EDS_MAX_NAME * Byte_Size + Int32_Size);

  function Get_Device_Info (The_Camera : Camera;
                            Out_Info   : access Device_Info) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsGetDeviceInfo";


  ---------------------------
  -- Session control       --
  ---------------------------

  function Open_Session (Cam : Camera) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsOpenSession";

  function Close_Session (Cam : Camera) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsCloseSession";

  ---------------------------
  -- Reference management  --
  ---------------------------

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

  ---------------------------
  -- Camera commands       --
  ---------------------------

  --  Only the one you need right now:
  Camera_Command_Take_Picture : constant Eds_Uint32 := 16#00000000#;
  --  (kEdsCameraCommand_TakePicture in EDSDK.h)

  function Send_Command (Cam     : Camera;
                         Command : Eds_Uint32;
                         Param   : Eds_Uint32) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsSendCommand";

  ---------------------------
  -- Image rectangles      --
  ---------------------------

  --  EDSDK compatible structs:
  --
  --  typedef struct {
  --      EdsInt32 x;
  --      EdsInt32 y;
  --      EdsInt32 width;
  --      EdsInt32 height;
  --  } EdsRect;
  --
  --  typedef struct {
  --      EdsInt32 width;
  --      EdsInt32 height;
  --  } EdsSize;

  type Rect is record
     X      : Eds_Int32;
     Y      : Eds_Int32;
     Width  : Eds_Int32;
     Height : Eds_Int32;
  end record
    with Convention => C;

  type Size is record
     Width  : Eds_Int32;
     Height : Eds_Int32;
  end record
    with Convention => C;

  ---------------------------
  -- Image source / types  --
  ---------------------------

  --  Minimal subset for your use case; extend as needed.
  --
  --  enum EdsImageSource {
  --      kEdsImageSrc_FullView = 0,
  --      kEdsImageSrc_Thumbnail,
  --      kEdsImageSrc_Preview,
  --      kEdsImageSrc_RAWThumbnail,
  --      kEdsImageSrc_RAWFullView
  --  };

  type Image_Source is new Eds_Uint32;
  Image_Src_Full_View   : constant Image_Source := 0;
  Image_Src_Thumbnail   : constant Image_Source := 1;
  Image_Src_Preview     : constant Image_Source := 2;
  Image_Src_Raw_Thumb   : constant Image_Source := 3;
  Image_Src_Raw_Full    : constant Image_Source := 4;

  --  TargetImageType for EdsGetImage:
  --
  --  Typically:
  --    kEdsTargetImageType_RGB  = 2
  --    kEdsTargetImageType_RGB16 = 3
  --
  --  Check EDSDKTypes.h for exact values and adjust if required.

  type Target_Image_Type is new Eds_Uint32;
  Target_RGB    : constant Target_Image_Type := 2;
  Target_RGB16  : constant Target_Image_Type := 3;

  ---------------------------
  -- Streams (memory)      --
  ---------------------------

  --  We use a memory stream so the central crop goes straight into RAM.
  --
  --  EdsError EdsCreateMemoryStream(EdsUInt64 inBufferSize,
  --                                 EdsStreamRef *outStream);
  --
  --  EdsError EdsGetLength (EdsStreamRef inStream,
  --                          EdsUInt64  *outLength);
  --
  --  EdsError EdsGetPointer(EdsStreamRef inStream,
  --                          EdsVoid    **outPointer);

  function Create_Memory_Stream (Buffer_Size : Eds_Uint64;
                                 Out_Stream  : access Stream)
                                 return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsCreateMemoryStream";

  function Get_Length (S         : Stream;
                       Out_Len   : access Eds_Uint64)
                       return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsGetLength";

  function Get_Pointer (S        : Stream;
                        Out_Ptr  : access System.Address)
                        return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsGetPointer";

  ---------------------------
  -- Cropped image access  --
  ---------------------------

  --  Core function for your autofocus loop:
  --
  --  EdsError EdsGetImage(
  --      EdsDirectoryItemRef inDirItemRef,
  --      EdsImageSource      inImageSource,
  --      EdsTargetImageType  inTargetImageType,
  --      EdsRect            *inSrcRect,
  --      EdsSize            *inDstSize,
  --      EdsStreamRef        inStream);

  function Get_Image (Item      : Directory_Item;
                      Source    : Image_Source;
                      Target    : Target_Image_Type;
                      Src_Rect  : access Rect;
                      Dst_Size  : access Size;
                      Dest      : Stream) return Eds_Error
    with Import        => True,
         Convention    => C,
         External_Name => "EdsGetImage";

  --  Note: You will obtain Directory_Item values from EDSDK object
  --  events (kEdsObjectEvent_DirItemCreated / RequestTransfer). The
  --  binding for event handlers can be added later once SDK access is
  --  confirmed.

private

  --  Internally, each handle is just the underlying pointer value.
  --  This keeps them opaque at the Ada level but matches the C API.

  type Camera_List    is new System.Address;
  type Camera         is new System.Address;
  type Directory_Item is new System.Address;
  type Stream         is new System.Address;

end Canon_Interface;
