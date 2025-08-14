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

with Exceptions;
with Interfaces.C.Strings;
with Text;
with Udev_Interface;
with Unsigned;

package body Serial_Io.Usb is

  package ICS renames Interfaces.C.Strings;
  package UDI renames Udev_Interface;


  function "=" (Left  : ICS.chars_ptr;
                Right : Natural) return Boolean is
    use type ICS.chars_ptr;
    use type Unsigned.Word;
  begin
    if Left = ICS.Null_Ptr then
      return False;
    end if;
    return Unsigned.Word'(Unsigned.Hex_Value_Of (ICS.Value (Left))) = Unsigned.Word(Right);
  exception
  when Item: others =>
    return False;
  end "=";


  function Device_Name_For (Vid : Vendor_Id;
                            Pid : Product_Id) return String is

    Context     : constant access UDI.Device_Context := UDI.Create_Context;
    Enumerator  : access UDI.Device_Enumerator;
    Entry_Node  : access UDI.List_Entry;
    Usb_Device  : access UDI.Device;
    Parent      : access UDI.Device;
    Vendor_Ptr  : ICS.chars_ptr;
    Product_Ptr : ICS.chars_ptr;
    Dev_Node    : ICS.chars_ptr;
    Dev_Count   : Natural := 0;
    Device_Name : Text.String;

  begin
    if Context = null then
      raise Program_Error with "Failed to create udev context.";
    end if;

    Enumerator := UDI.Create_Enumerator (Context);
    UDI.Add_Subsystem_Filter (Enumerator, ICS.New_String ("tty"));
    UDI.Scan_Devices (Enumerator);
    Entry_Node := UDI.Get_Device_List (Enumerator);

    while Entry_Node /= null loop
      declare
        Path : constant ICS.chars_ptr := UDI.Get_Entry_Name (Entry_Node);
      begin
        Usb_Device := UDI.Create_Device_From_Path (Context, Path);
        Parent := UDI.Get_Parent_Device (Usb_Device,
                                         ICS.New_String ("usb"),
                                         ICS.New_String ("usb_device"));

        if Parent /= null then
          Vendor_Ptr  := UDI.Get_Attribute_Value (Parent, ICS.New_String ("idVendor"));
          Product_Ptr := UDI.Get_Attribute_Value (Parent, ICS.New_String ("idProduct"));
          Dev_Node := UDI.Get_Device_Node (Usb_Device);
          if Vendor_Ptr = Natural(Vid) and then Product_Ptr = Natural(Pid) then
            declare
              Name : constant String := ICS.Value (Dev_Node);
              use type Text.String;
            begin
              Device_Name := Text.String_Of (Name);
            end;
            Dev_Count := @ + 1;
          end if;
        end if;

        UDI.Free_Device (Usb_Device);
        Entry_Node := UDI.Get_Next_Entry (Entry_Node);
      end;
    end loop;

    UDI.Free_Enumerator (Enumerator);
    UDI.Free_Context (Context);

    if Dev_Count = 0 then
      raise Device_Not_Found;
    elsif Dev_Count > 1 then
      raise Multiple_Devices;
    else
      return Device_Name.To_String;
    end if;
  end Device_Name_For;

end Serial_Io.Usb;
