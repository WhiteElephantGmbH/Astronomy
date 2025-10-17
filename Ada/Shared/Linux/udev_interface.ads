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
with Interfaces.C.Strings;

package Udev_Interface is

  pragma Linker_Options ("-ludev");

  package ICS renames Interfaces.C.Strings;

  -- Opaque C types
  type Device_Context      is limited private;
  type Device_Enumerator   is limited private;
  type List_Entry          is limited private;
  type Device              is limited private;

  -- udev context
  function Create_Context return access Device_Context
    with Import => True, Convention => C, External_Name => "udev_new";

  procedure Free_Context (Context : access Device_Context)
    with Import => True, Convention => C, External_Name => "udev_unref";

  -- enumeration
  function Create_Enumerator (Context : access Device_Context) return access Device_Enumerator
    with Import => True, Convention => C, External_Name => "udev_enumerate_new";

  procedure Free_Enumerator (Enumerator : access Device_Enumerator)
    with Import => True, Convention => C, External_Name => "udev_enumerate_unref";

  procedure Add_Subsystem_Filter
    (Enumerator : access Device_Enumerator; Subsystem : ICS.chars_ptr)
    with Import => True, Convention => C, External_Name => "udev_enumerate_add_match_subsystem";

  procedure Scan_Devices (Enumerator : access Device_Enumerator)
    with Import => True, Convention => C, External_Name => "udev_enumerate_scan_devices";

  function Get_Device_List (Enumerator : access Device_Enumerator) return access List_Entry
    with Import => True, Convention => C, External_Name => "udev_enumerate_get_list_entry";

  -- list entry iteration
  function Get_Next_Entry (Entry_Node : access List_Entry) return access List_Entry
    with Import => True, Convention => C, External_Name => "udev_list_entry_get_next";

  function Get_Entry_Name (Entry_Node : access List_Entry) return ICS.chars_ptr
    with Import => True, Convention => C, External_Name => "udev_list_entry_get_name";

  -- device creation and attributes
  function Create_Device_From_Path
    (Context : access Device_Context; SysPath : ICS.chars_ptr) return access Device
    with Import => True, Convention => C, External_Name => "udev_device_new_from_syspath";

  procedure Free_Device (Dev : access Device)
    with Import => True, Convention => C, External_Name => "udev_device_unref";

  function Get_Device_Node (Dev : access Device) return ICS.chars_ptr
    with Import => True, Convention => C, External_Name => "udev_device_get_devnode";

  function Get_Parent_Device
    (Dev : access Device; Subsystem, Devtype : ICS.chars_ptr) return access Device
    with Import => True, Convention => C, External_Name => "udev_device_get_parent_with_subsystem_devtype";

  function Get_Attribute_Value
    (Dev : access Device; Attr : ICS.chars_ptr) return ICS.chars_ptr
    with Import => True, Convention => C, External_Name => "udev_device_get_sysattr_value";

private
  type Device_Context    is limited null record;
  type Device_Enumerator is limited null record;
  type List_Entry        is limited null record;
  type Device            is limited null record;
end Udev_Interface;

