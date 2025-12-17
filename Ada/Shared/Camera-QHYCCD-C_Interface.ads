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

  -- qhyccd_handle is an opaque type in the SDK (typedef void qhyccd_handle;)
  type Handle is private;
  No_Handle : constant Handle;

  -- result codes (SDK uses uint32_t, 0 = success)
  subtype Result is Uint32;
  QHY_SUCCESS : constant Result := 0;
  QHY_ERROR   : constant Result := 16#FFFF_FFFF#;

  -- stream mode (SDK: 0x00 single frame, 0x01 live)
  subtype Stream_Mode is Uint8;
  Stream_Single_Frame : constant Stream_Mode := 16#00#;
  Stream_Live         : constant Stream_Mode := 16#01#;

  -- subset of control IDs we need for first picture
  type Control_Id is (Control_Gain,         -- optional
                      Control_Offset,       -- optional
                      Control_Exposure,     -- in microseconds
                      Control_Transfer_Bit, -- bits per pixel (8/16)
                      Control_Channels)     -- requested channels (1/3)
    with Size => Int'size;

  for Control_Id use (Control_Gain         => 6,
                      Control_Offset       => 7,
                      Control_Exposure     => 8,
                      Control_Transfer_Bit => 10,
                      Control_Channels     => 11);

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

  function Set_Stream_Mode (H : Handle; Mode : Stream_Mode) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "SetQHYCCDStreamMode";

  function Init_Camera (H : Handle) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "InitQHYCCD";

  function Set_Debayer_On_Off (H : Handle; On : Bool) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "SetQHYCCDDebayerOnOff";

  function Is_Control_Available (H : Handle; Id : Control_Id) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "IsQHYCCDControlAvailable";

  function Set_Bits_Mode (H : Handle; Bits : Uint32) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "SetQHYCCDBitsMode";

  function Set_Param (H     : Handle;
                      Id    : Control_Id;
                      Value : Double) return Result with
    Import,
    Convention    => StdCall,
    External_Name => "SetQHYCCDParam";

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

private

  type Handle is new System.Address;
  No_Handle : constant Handle := Handle (System.Null_Address);

end Camera.QHYCCD.C_Interface;
