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

package body Gpiod_Interface is

  ----------
  -- Chip --
  ----------
  function Open (Path : System.Address) return access Chip is
    pragma Unreferenced (Path);
  begin
    return null;
  end Open;

  procedure Close (Device : access Chip) is
  begin
    null;
  end Close;

  function Request_Lines (Device  : access Chip;
                          Request : access Request_Config;
                          Config  : access Line_Config) return access Line_Request is
    pragma Unreferenced (Device, Request, Config);
  begin
    return null;
  end Request_Lines;


  -------------------
  -- Line Settings --
  -------------------
  function New_Line_Settings return access Line_Settings is
  begin
    return null;
  end New_Line_Settings;

  procedure Free (Settings : access Line_Settings) is
  begin
    null;
  end Free;

  function Set_Direction (Settings  : access Line_Settings;
                          Direction : Line_Direction) return CI.Return_Code is
    pragma Unreferenced (Settings, Direction);
  begin
    return CI.Failed;
  end Set_Direction;

  function Set_Output_Value (Settings : access Line_Settings;
                             Value    : Line_Value) return CI.Return_Code is
    pragma Unreferenced (Settings, Value);
  begin
    return CI.Failed;
  end Set_Output_Value;

  function Set_Edge_Detection (Settings : access Line_Settings;
                               Edge     : Line_Edge) return CI.Return_Code is
    pragma Unreferenced (Settings, Edge);
  begin
    return CI.Failed;
  end Set_Edge_Detection;

  function Set_Bias (Settings : access Line_Settings;
                     Bias     : Line_Bias) return CI.Return_Code is
    pragma Unreferenced (Settings, Bias);
  begin
    return CI.Failed;
  end Set_Bias;

  function Set_Drive (Settings : access Line_Settings;
                      Drive    : Line_Drive) return CI.Return_Code is
    pragma Unreferenced (Settings, Drive);
  begin
    return CI.Failed;
  end Set_Drive;

  function Set_Event_Clock (Settings : access Line_Settings;
                            Clock    : Line_Clock) return CI.Return_Code is
    pragma Unreferenced (Settings, Clock);
  begin
    return CI.Failed;
  end Set_Event_Clock;


  ----------------
  -- Edge Event --
  ----------------
  procedure Free (Event : access Edge_Event) is
  begin
    null;
  end Free;

  function Get_Event_Type (Event : access Edge_Event) return Edge_Event_Type is
    pragma Unreferenced (Event);
  begin
    return Rising_Edge;
  end Get_Event_Type;

  function Get_Timestamp_Ns (Event : access Edge_Event) return T_Uint64 is
    pragma Unreferenced (Event);
  begin
    return 0;
  end Get_Timestamp_Ns;

  function Get_Line_Offset (Event : access Edge_Event) return T_Unsigned is
    pragma Unreferenced (Event);
  begin
    return 0;
  end Get_Line_Offset;

  function Get_Global_Seqno (Event : access Edge_Event) return T_Unsigned_Long is
    pragma Unreferenced (Event);
  begin
    return 0;
  end Get_Global_Seqno;

  function Get_Line_Seqno (Event : access Edge_Event) return T_Unsigned_Long is
    pragma Unreferenced (Event);
  begin
    return 0;
  end Get_Line_Seqno;

  -----------------------
  -- Edge Event Buffer --
  -----------------------
  function New_Edge_Event_Buffer (Capacity : T_Size) return access Edge_Event_Buffer is
    pragma Unreferenced (Capacity);
  begin
    return null;
  end New_Edge_Event_Buffer;

  procedure Free (Buffer : access Edge_Event_Buffer) is
  begin
    null;
  end Free;

  function Get_Capacity (Buffer : access Edge_Event_Buffer) return T_Size is
    pragma Unreferenced (Buffer);
  begin
    return 0;
  end Get_Capacity;

  function Get_Num_Events (Buffer : access Edge_Event_Buffer) return T_Size is
    pragma Unreferenced (Buffer);
  begin
    return 0;
  end Get_Num_Events;

  function Get_Event (Buffer : access Edge_Event_Buffer;
                      Index  : T_Unsigned_Long) return access Edge_Event is
    pragma Unreferenced (Buffer, Index);
  begin
    return null;
  end Get_Event;

  ------------------------
  -- Line Configuration --
  ------------------------
  function New_Line_Config return access Line_Config is
  begin
    return null;
  end New_Line_Config;

  procedure Free (Config : access Line_Config) is
  begin
    null;
  end Free;

  procedure Reset (Config : access Line_Config) is
  begin
    null;
  end Reset;

  function Add_Line_Settings (Config      : access Line_Config;
                              Offsets     : access T_Unsigned;
                              Num_Offsets : T_Size;
                              Settings    : access Line_Settings) return CI.Return_Code is
    pragma Unreferenced (Config, Offsets, Num_Offsets, Settings);
  begin
    return CI.Failed;
  end Add_Line_Settings;

  ---------------------------
  -- Request Configuration --
  ---------------------------
  function New_Request_Config return access Request_Config is
  begin
    return null;
  end New_Request_Config;

  procedure Free (Config : access Request_Config) is
  begin
    null;
  end Free;

  procedure Set_Consumer (Config   : access Request_Config;
                          Consumer : System.Address) is
  begin
    null;
  end Set_Consumer;

  procedure Set_Event_Buffer_Size (Config            : access Request_Config;
                                   Event_Buffer_Size : T_Size) is
  begin
    null;
  end Set_Event_Buffer_Size;

  ------------------
  -- Line Request --
  ------------------
  procedure Release (Request : access Line_Request) is
  begin
    null;
  end Release;

  function Reconfigure_Lines (Request : access Line_Request;
                              Config  : access Line_Config) return CI.Return_Code is
    pragma Unreferenced (Request, Config);
  begin
    return CI.Failed;
  end Reconfigure_Lines;

  function Get_Fd (Request : access Line_Request) return CI.File_Descriptor is
    pragma Unreferenced (Request);
  begin
    return 0;
  end Get_Fd;

  function Wait_Edge_Events (Request    : access Line_Request;
                             Timeout_Ns : T_Int64) return CI.Return_Count is
    pragma Unreferenced (Request, Timeout_Ns);
  begin
    return CI.Failed;
  end Wait_Edge_Events;

  function Read_Edge_Events (Request    : access Line_Request;
                             Buffer     : access Edge_Event_Buffer;
                             Max_Events : T_Size) return CI.Return_Count is
    pragma Unreferenced (Request, Buffer, Max_Events);
  begin
    return CI.Failed;
  end Read_Edge_Events;

  function Set_Value (Request : access Line_Request;
                      Offset  : T_Unsigned;
                      Value   : Line_Value) return CI.Return_Code is
    pragma Unreferenced (Request, Offset, Value);
  begin
    return CI.Failed;
  end Set_Value;

  function Get_Value (Request : access Line_Request;
                      Offset  : T_Unsigned) return Line_Value is
    pragma Unreferenced (Request, Offset);
  begin
    return Error;
  end Get_Value;

end Gpiod_Interface;
