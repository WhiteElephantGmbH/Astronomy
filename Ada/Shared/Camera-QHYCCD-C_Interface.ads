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

package Camera.QHYCCD.C_Interface is

  pragma Linker_Options ("-lqhyccd");

  -- basic C types
  subtype Int    is C.Int;
  subtype Uint8  is C.Uint8;
  subtype Uint32 is C.Uint32;
  subtype Double is C.Double;
  subtype Bool   is C.Bool;

  -- qhyccd_handle is an opaque type in the SDK
  type Handle is private;

  No_Handle : constant Handle;

  -- result codes
  type Result is new Uint32;

  QHY_SUCCESS : constant Result := 0;
  QHY_ERROR   : constant Result := 16#FFFF_FFFF#;

  type Stream_Mode is new Uint8;

  Stream_Single_Frame : constant Stream_Mode := 16#00#;
  Stream_Live         : constant Stream_Mode := 16#01#;

  type Read_Mode is new Uint32;

  Photo_Graphic_DSO_16_BIT : constant Read_Mode := 0;

  -- CONTROL_ID (from SDK qhyccdstruct.h table in the PDF)
  type Control_Id is
    (Control_Brightness,
     Control_Contrast,
     Control_WBR,
     Control_WBB,
     Control_WBG,
     Control_Gamma,
     Control_Gain,
     Control_Offset,
     Control_Exposure,
     Control_Speed,
     Control_Transfer_Bit,
     Control_Channels,
     Control_USB_Traffic,
     Control_CURRTEMP,
     Cam_Color,
     Control_DDR,
     Cam_Single_Frame_Mode,
     Cam_Live_Video_Mode,
     Cam_Is_Color)
    with Size => Int'size;

  for Control_Id use
    (Control_Brightness    => 0,
     Control_Contrast      => 1,
     Control_WBR           => 2,
     Control_WBB           => 3,
     Control_WBG           => 4,
     Control_Gamma         => 5,
     Control_Gain          => 6,
     Control_Offset        => 7,
     Control_Exposure      => 8,
     Control_Speed         => 9,
     Control_Transfer_Bit  => 10,
     Control_Channels      => 11,
     Control_USB_Traffic   => 12,
     Control_CURRTEMP      => 14,
     Cam_Color             => 20,
     Control_DDR           => 48,
     Cam_Single_Frame_Mode => 57,
     Cam_Live_Video_Mode   => 58,
     Cam_Is_Color          => 59);

  ----------------------------------
  -- Resource + Camera open/close --
  ----------------------------------
  function Init_Resource return Result with
    Import,
    Convention    => StdCall,
    External_Name => "InitQHYCCDResource";

  function Release_Resource return Result with
    Import,
    Convention    => StdCall,
    External_Name => "ReleaseQHYCCDResource";

  type Camera_Count is new Uint32;

  function Scan return Camera_Count with
    Import,
    Convention    => StdCall,
    External_Name => "ScanQHYCCD";

  function Get_Id (Index : Uint32;
                   Id    : System.Address) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "GetQHYCCDId";

  function Open (Id : System.Address) return Handle with
    Import,
    Convention    => StdCall,
    External_Name => "OpenQHYCCD";

  function Close (H : Handle) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "CloseQHYCCD";

  ----------------------------------------------------------------
  -- Read Mode (useful for cameras with multiple readout modes) --
  ----------------------------------------------------------------
  function Get_Number_Of_Read_Modes (H          : Handle;
                                     Mode_Count : access Uint32) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "GetQHYCCDNumberOfReadModes";

  function Get_Read_Mode_Name (H          : Handle;
                               Mode       : Read_Mode;
                               Name       : System.Address) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "GetQHYCCDReadModeName";

  function Get_Read_Mode_Resolution (H      : Handle;
                                     Mode   : Uint32;
                                     Width  : access Uint32;
                                     Height : access Uint32) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "GetQHYCCDReadModeResolution";

  function Set_Read_Mode (H    : Handle;
                          Mode : Read_Mode) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "SetQHYCCDReadMode";

  ----------------------------------
  -- Stream / Init / Capabilities --
  ----------------------------------
  function Set_Stream_Mode (H    : Handle;
                            Mode : Stream_Mode) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "SetQHYCCDStreamMode";

  function Init_Camera (H : Handle) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "InitQHYCCD";

  function Set_Debayer_On_Off (H  : Handle;
                               On : Bool) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "SetQHYCCDDebayerOnOff";

  function Is_Control_Available (H  : Handle;
                                 Id : Control_Id) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "IsQHYCCDControlAvailable";

  -------------------------------------
  -- Parameter API (range + get/set) --
  -------------------------------------
  function Get_Param_Min_Max_Step (H     : Handle;
                                   Id    : Control_Id;
                                   Min   : access Double;
                                   Max   : access Double;
                                   Step  : access Double) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "GetQHYCCDParamMinMaxStep";

  function Get_Param (H  : Handle;
                      Id : Control_Id) return Double with
    Import,
    Convention    => StdCall,
    External_Name => "GetQHYCCDParam";

  function Set_Param (H     : Handle;
                      Id    : Control_Id;
                      Value : Double) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "SetQHYCCDParam";

  -----------------------------------
  -- Geometry / Buffers / Exposure --
  -----------------------------------
  function Get_Chip_Info (H      : Handle;
                          Chip_W : access Double;
                          Chip_H : access Double;
                          Img_W  : access Uint32;
                          Img_H  : access Uint32;
                          Pix_W  : access Double;
                          Pix_H  : access Double;
                          Bpp    : access Uint32) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "GetQHYCCDChipInfo";

  function Set_Resolution (H     : Handle;
                           X     : Uint32;
                           Y     : Uint32;
                           Xsize : Uint32;
                           Ysize : Uint32) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "SetQHYCCDResolution";

  function Set_Bin_Mode (H    : Handle;
                         Wbin : Uint32;
                         Hbin : Uint32) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "SetQHYCCDBinMode";

  function Get_Mem_Length (H : Handle) return Uint32 with
    Import,
    Convention    => StdCall,
    External_Name => "GetQHYCCDMemLength";

  function Exp_Single_Frame (H : Handle) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "ExpQHYCCDSingleFrame";

  function Get_Single_Frame (H        : Handle;
                             W        : access Uint32;
                             Ht       : access Uint32;
                             Bpp      : access Uint32;
                             Channels : access Uint32;
                             Img_Data : System.Address) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "GetQHYCCDSingleFrame";

  function Set_Single_Frame_Timeout (H       : Handle;
                                     Time_Ms : Uint32) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "SetQHYCCDSingleFrameTimeOut";

  function Cancel_Exposing_And_Readout (H : Handle) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "CancelQHYCCDExposingAndReadout";

  -------------------------------------------
  -- Versions (handy for first-light logs) --
  -------------------------------------------
  function Get_SDK_Version (Year   : access Uint32;
                            Month  : access Uint32;
                            Day    : access Uint32;
                            Subday : access Uint32) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "GetQHYCCDSDKVersion";

  function Get_FW_Version (H   : Handle;
                           Buf : System.Address) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "GetQHYCCDFWVersion";

private

  type Handle is new System.Address;

  No_Handle : constant Handle := Handle (System.Null_Address);

end Camera.QHYCCD.C_Interface;
