-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
-- *                                                (Linux simulation)                                                 *
-- *********************************************************************************************************************
pragma Style_Astronomy;

with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with Standard_C_Interface;
with System;

package Gpiod_Interface is

  package IC  renames Interfaces.C;
  package ICE renames IC.Extensions;
  package ICS renames IC.Strings;
  package CI  renames Standard_C_Interface;

  -------------
  -- C Types --
  -------------
  subtype T_Bool          is ICE.bool;
  subtype T_Unsigned      is IC.unsigned;
  subtype T_Unsigned_Long is IC.unsigned_long;
  subtype T_Size          is IC.size_t;
  subtype T_Int64         is Interfaces.Integer_64;
  subtype T_Uint64        is Interfaces.Unsigned_64;

  ------------------
  -- Opaque Types --
  ------------------
  type Chip              is limited private;
  type Line_Settings     is limited private;
  type Line_Config       is limited private;
  type Edge_Event        is limited private;
  type Edge_Event_Buffer is limited private;
  type Request_Config    is limited private;
  type Line_Request      is limited private;

  ------------------
  -- Enumerations --
  ------------------
  type Line_Value is (Error, Inactive, Active) with Convention => C;
  for Line_Value use (
    Error    => -1,
    Inactive =>  0,
    Active   =>  1);

  type Line_Direction is (As_Is, Input, Output) with Convention => C;
  for Line_Direction use (
    As_Is  => 1,
    Input  => 2,
    Output => 3);

  type Line_Edge is (None, Rising, Falling, Both) with Convention => C;
  for Line_Edge use (
    None    => 1,
    Rising  => 2,
    Falling => 3,
    Both    => 4);

  type Line_Bias is (As_Is, Unknown, Disabled, Pull_Up, Pull_Down) with Convention => C;
  for Line_Bias use (
    As_Is     => 1,
    Unknown   => 2,
    Disabled  => 3,
    Pull_Up   => 4,
    Pull_Down => 5);

  type Line_Drive is (Push_Pull, Open_Drain, Open_Source) with Convention => C;
  for Line_Drive use (
    Push_Pull   => 1,
    Open_Drain  => 2,
    Open_Source => 3);

  type Line_Clock is (Monotonic, Realtime, Hte) with Convention => C;
  for Line_Clock use (
    Monotonic => 1,
    Realtime  => 2,
    Hte       => 3);

  type Edge_Event_Type is (Rising_Edge, Falling_Edge) with Convention => C;
  for Edge_Event_Type use (
    Rising_Edge  => 1,
    Falling_Edge => 2);

  ----------
  -- Chip --
  ----------
  function Open (Path : System.Address) return access Chip;

  procedure Close (Device : access Chip);

  function Request_Lines (Device  : access Chip;
                          Request : access Request_Config;
                          Config  : access Line_Config) return access Line_Request;
  ------------------------
  -- Information Events --
  ------------------------
  --  Not currently required.

  -------------------
  -- Line Settings --
  -------------------
  function New_Line_Settings return access Line_Settings;

  procedure Free (Settings : access Line_Settings);

  function Set_Direction (Settings  : access Line_Settings;
                          Direction : Line_Direction) return CI.Return_Code;

  function Set_Output_Value (Settings : access Line_Settings;
                             Value    : Line_Value) return CI.Return_Code;

  function Set_Edge_Detection (Settings : access Line_Settings;
                               Edge     : Line_Edge) return CI.Return_Code;

  function Set_Bias (Settings : access Line_Settings;
                     Bias     : Line_Bias) return CI.Return_Code;

  function Set_Drive (Settings : access Line_Settings;
                      Drive    : Line_Drive) return CI.Return_Code;

  function Set_Event_Clock (Settings : access Line_Settings;
                            Clock    : Line_Clock) return CI.Return_Code;

  ----------------
  -- Edge Event --
  ----------------
  procedure Free (Event : access Edge_Event);

  function Get_Event_Type (Event : access Edge_Event) return Edge_Event_Type;

  function Get_Timestamp_Ns (Event : access Edge_Event) return T_Uint64;

  function Get_Line_Offset (Event : access Edge_Event) return T_Unsigned;

  function Get_Global_Seqno (Event : access Edge_Event) return T_Unsigned_Long;

  function Get_Line_Seqno (Event : access Edge_Event) return T_Unsigned_Long;

  -----------------------
  -- Edge Event Buffer --
  -----------------------
  function New_Edge_Event_Buffer (Capacity : T_Size) return access Edge_Event_Buffer;

  procedure Free (Buffer : access Edge_Event_Buffer);

  function Get_Capacity (Buffer : access Edge_Event_Buffer) return T_Size;

  function Get_Num_Events (Buffer : access Edge_Event_Buffer) return T_Size;

  function Get_Event (Buffer : access Edge_Event_Buffer;
                      Index  : T_Unsigned_Long) return access Edge_Event;

  ------------------------
  -- Line Configuration --
  ------------------------
  function New_Line_Config return access Line_Config;

  procedure Free (Config : access Line_Config);

  procedure Reset (Config : access Line_Config);

  function Add_Line_Settings (Config      : access Line_Config;
                              Offsets     : access T_Unsigned;
                              Num_Offsets : T_Size;
                              Settings    : access Line_Settings) return CI.Return_Code;

  ---------------------------
  -- Request Configuration --
  ---------------------------
  function New_Request_Config return access Request_Config;

  procedure Free (Config : access Request_Config);

  procedure Set_Consumer (Config   : access Request_Config;
                          Consumer : System.Address);

  procedure Set_Event_Buffer_Size (Config            : access Request_Config;
                                   Event_Buffer_Size : T_Size);

  ------------------
  -- Line Request --
  ------------------
  procedure Release (Request : access Line_Request);

  function Reconfigure_Lines (Request : access Line_Request;
                              Config  : access Line_Config) return CI.Return_Code;

  function Get_Fd (Request : access Line_Request) return CI.File_Descriptor;

  function Wait_Edge_Events (Request    : access Line_Request;
                             Timeout_Ns : T_Int64) return CI.Return_Count;

  function Read_Edge_Events (Request    : access Line_Request;
                             Buffer     : access Edge_Event_Buffer;
                             Max_Events : T_Size) return CI.Return_Count;

  function Set_Value (Request : access Line_Request;
                      Offset  : T_Unsigned;
                      Value   : Line_Value) return CI.Return_Code;

  function Get_Value (Request : access Line_Request;
                      Offset  : T_Unsigned) return Line_Value;

private

  type Chip              is limited null record;
  type Line_Settings     is limited null record;
  type Line_Config       is limited null record;
  type Edge_Event        is limited null record;
  type Edge_Event_Buffer is limited null record;
  type Request_Config    is limited null record;
  type Line_Request      is limited null record;

end Gpiod_Interface;
