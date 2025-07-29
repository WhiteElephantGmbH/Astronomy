with Ada.Text_IO;
with Interfaces.C.Strings;
with Udev_Interface;

procedure Main is

  package TIO renames Ada.Text_IO;
  package ICS renames Interfaces.C.Strings;
  package UDI renames Udev_Interface;

  Context    : access UDI.Device_Context := UDI.Create_Context;
  Enumerator : access UDI.Device_Enumerator;
  Entry_Node : access UDI.List_Entry;
  Device     : access UDI.Device;
  Parent     : access UDI.Device;
  Vendor_Id  : ICS.chars_ptr;
  Product_Id : ICS.chars_ptr;
  Dev_Node   : ICS.chars_ptr;

begin
  if Context = null then
    TIO.Put_Line ("Failed to create udev context.");
    return;
  end if;

  Enumerator := UDI.Create_Enumerator (Context);
  UDI.Add_Subsystem_Filter (Enumerator, ICS.New_String ("tty"));
  UDI.Scan_Devices (Enumerator);
  Entry_Node := UDI.Get_Device_List (Enumerator);

  while Entry_Node /= null loop
    declare
      Path : ICS.chars_ptr := UDI.Get_Entry_Name (Entry_Node);
      use type ICS.Chars_Ptr;
    begin
      Device := UDI.Create_Device_From_Path (Context, Path);
      Parent := UDI.Get_Parent_Device (Device,
                                       ICS.New_String ("usb"),
                                       ICS.New_String ("usb_device"));

      if Parent /= null then
        Vendor_Id  := UDI.Get_Attribute_Value (Parent, ICS.New_String ("idVendor"));
        Product_Id := UDI.Get_Attribute_Value (Parent, ICS.New_String ("idProduct"));
        Dev_Node   := UDI.Get_Device_Node (Device);

        if Vendor_Id /= ICS.Null_Ptr and then Product_Id /= ICS.Null_Ptr then
          if ICS.Value (Vendor_Id) = "15a2" and then ICS.Value (Product_Id) = "a50f" then
            TIO.Put_Line ("Found matching device: " & ICS.Value (Dev_Node));
          end if;
        end if;
      end if;

      UDI.Free_Device (Device);
      Entry_Node := UDI.Get_Next_Entry (Entry_Node);
    end;
  end loop;

  UDI.Free_Enumerator (Enumerator);
  UDI.Free_Context (Context);
end Main;

