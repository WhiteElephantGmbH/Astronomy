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

pragma Build (Description => "Usb test",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");

with Ada.Text_IO;
with Celestron;
with Exceptions;
with Serial_Io.Usb;
with System;
with Text;
with Unsigned;
with Win32.Winbase;
with Win32.Winnt;

procedure Usb_Test is

  Port_Unknown : exception;

  function Port_For (Vid : Serial_Io.Usb.Vendor_Id;
                      Pid : Serial_Io.Usb.Product_Id) return Serial_Io.Port is
    Ports : constant Serial_Io.Usb.Ports := Serial_Io.Usb.Ports_For (Vid, Pid);
  begin
    Ada.Text_IO.Put_Line ("Number of ports:" & Ports'length'image);
    if Ports'length = 1 then
      return Ports(Ports'first);
    else
      raise Port_Unknown;
    end if;
  end Port_For;

  The_Handbox_Port : Serial_Io.Port;

  procedure Get_Handbox_Port is
    ARM_Vendor_Id        : constant Serial_Io.Usb.Vendor_Id := 3368;
    Micro_Bit_Product_Id : constant Serial_Io.Usb.Product_Id := 516;
  begin
    The_Handbox_Port := Port_For (ARM_Vendor_Id, Micro_Bit_Product_Id);
    Ada.Text_IO.Put_Line ("Handbox Port: " & The_Handbox_Port'image);
  end Get_Handbox_Port;


  The_Focuser_Port : Serial_Io.Port;

  procedure Get_Focuser_Port is
    Celestron_Vendor_Id : constant Serial_Io.Usb.Vendor_Id := Celestron.Vendor_Id;
    Focuser_Product_Id : constant Serial_Io.Usb.Product_Id := 42255;
  begin
    The_Focuser_Port := Port_For (Celestron_Vendor_Id, Focuser_Product_Id);
    Ada.Text_IO.Put_Line ("Focuser Port: " & The_Focuser_Port'image);
  end Get_Focuser_Port;


  procedure Get_Version is
    Channel     : Serial_Io.Channel(The_Handbox_Port);
    The_Version : String := "x.xx";
  begin
    --Serial_Io.Set (The_Baudrate => 19200,
    --               On           => Channel);
    Serial_Io.Set_For_Read (The_Timeout => 1.0,
                            On          => Channel);
    Serial_Io.Send (The_Item => 'v',
                    To       => Channel);
    Serial_Io.Receive (The_Item => The_Version,
                       From     => Channel);
    Ada.Text_IO.Put_Line ("Version:" & The_Version);
  end Get_Version;


  procedure Open is

    Port_Name : aliased constant String := "\\.\COM" & Text.Trimmed(The_Focuser_Port'img) & Ascii.Nul;

    use type Unsigned.Longword;

    GENERIC_READ       : constant Unsigned.Longword := Win32.Winnt.GENERIC_READ;
    GENERIC_WRITE      : constant Unsigned.Longword := Win32.Winnt.GENERIC_WRITE;
    GENERIC_READ_WRITE : constant := GENERIC_READ or GENERIC_WRITE;

    --FILE_FLAG_OVERLAPPED : constant := Win32.Winbase.FILE_FLAG_OVERLAPPED;

    The_Device : Win32.Winnt.HANDLE;

    use type System.Address;

  begin
    The_Device := Win32.Winbase.CreateFile (Win32.Addr(Port_Name),
                                            GENERIC_READ_WRITE,
                                            0, null,
                                            Win32.Winbase.OPEN_EXISTING,
                                            0, --FILE_FLAG_OVERLAPPED,
                                            System.Null_Address);
    if The_Device = Win32.Winbase.INVALID_HANDLE_VALUE then
      Ada.Text_IO.Put_Line ("### failed to open focuser COM port - error:" & Win32.Winbase.GetLastError'image);
    else
      Ada.Text_IO.Put_Line ("focuser COM opened.");
    end if;
  end Open;


  procedure Connect is

    Start_Packet : constant Unsigned.Byte := 16#3B#;
    Receive_Id   : constant Unsigned.Byte := 16#12#;
    Transmit_Id  : constant Unsigned.Byte := 16#22#;
    Connect_Id   : constant Unsigned.Byte := 16#FE#;

    Channel : Serial_Io.Channel(The_Focuser_Port);

    function Checksum_Of (Sum : Natural) return Unsigned.Byte is
    begin
      return Unsigned.Byte(256 - (Sum mod 256));
    end Checksum_Of;


    procedure Send (Item : Unsigned.Byte_String) is

      Header : constant Unsigned.Byte_String
             := [Start_Packet, Unsigned.Byte(Item'length + 2), Transmit_Id, Receive_Id];

      The_Data : Unsigned.Byte_String(1 .. Header'length + Item'length + 1);
      The_Sum  : Natural := 0;

    begin
      The_Data(1 .. Header'length) := Header;
      The_Data(Header'length + 1 .. The_Data'last - 1) := Item;
      for Index in The_Data'first + 1 .. The_Data'last - 1 loop
        The_Sum := The_Sum + Natural(The_Data(Index));
      end loop;
      The_Data(The_Data'last) := Checksum_Of (The_Sum);
      Serial_Io.Send (The_Data, Channel);
      Ada.Text_IO.Put_Line ("sent: " & Unsigned.Hex_Image_Of (The_Data));
    end Send;


    function Received_For (Command_Id : Unsigned.Byte) return Unsigned.Byte_String is

      The_Sum   : Natural;
      The_Count : Natural;

      use type Unsigned.Byte;

      function Received_Byte return Unsigned.Byte is
        The_Byte : Unsigned.Byte;
      begin
        Serial_Io.Receive (The_Byte, From => Channel);
        return The_Byte;
      exception
      when Item: others =>
        Ada.Text_IO.Put_Line ("received byte exception: " & Exceptions.Information_Of (Item));
        return 0;
      end Received_Byte;

      procedure Check (Item          : Unsigned.Byte;
                       Error_Message : String) is
        The_Byte : Unsigned.Byte;
      begin
        The_Byte := Received_Byte;
        if The_Byte /= Item then
          Ada.Text_IO.Put_Line ("### " & Error_Message & "(received: " & Unsigned.Image_Of (The_Byte) &
                                                         " - expected: " & Unsigned.Image_Of (Item) & ")");
          raise Program_Error;
        end if;
        The_Sum := The_Sum + Natural(Item);
      end Check;

    begin -- Received_For
      while Received_Byte /= Start_Packet loop
        null;
      end loop;
      The_Count := Natural(Received_Byte);
      The_Sum := The_Count;
      Check (Receive_Id, "incorrect receive Id");
      Check (Transmit_Id, "incorrect transmit Id");
      Check (Command_Id, "incorrect command Id");
      declare
        The_Data : Unsigned.Byte_String(1 .. The_Count - 3);
      begin
        if The_Data'length /= 0 then
          Serial_Io.Receive (The_Data, From => Channel);
          for Item of The_Data loop
            The_Sum := The_Sum + Natural(Item);
          end loop;
        end if;
        Check (Checksum_Of (The_Sum), "incorrect checksum");
        return The_Data;
      end;
    end Received_For;

    function "+" (Left  : String;
                  Right : Unsigned.Byte_String) return String is
    begin
      return Text.Trimmed (Left & Text.Trimmed(Unsigned.Word_Of_Big_Endian(Right)'image));
    end "+";

  begin
    --Serial_Io.Set (The_Baudrate => 19200, On => Channel);
    Serial_Io.Set_For_Read (The_Timeout => 1.0, On => Channel);
    Send ([Connect_Id]);
    declare
      Version : constant Unsigned.Byte_String := Received_For (Connect_Id);
    begin
      Ada.Text_IO.Put_Line ("version: " + [Version(Version'first)] & '.'
                                        + [Version(Version'first + 1)] & '.'
                                        + Version(Version'first + 2 .. Version'first + 3));
    end;
  exception
  when Item: others =>
    Ada.Text_IO.Put_Line (Exceptions.Information_Of (Item));
    Serial_Io.Free (The_Focuser_Port);
  end Connect;


begin -- Usb_Test
  Ada.Text_IO.Put_Line ("USB Test");
  Ada.Text_IO.Put_Line ("========");
  loop
    Ada.Text_IO.Put(">");
    begin
      declare
        Command : constant String := Ada.Text_IO.Get_Line;
      begin
        if Command'length > 0 then
          case Command(Command'first) is
          when 'h' => -- handbox
            Get_Handbox_Port;
          when 'v' => -- version of handbox
            Get_Version;
          when 'f' => -- focuser
            Get_Focuser_Port;
          when 'o' => -- open focuser port
            Open;
          when 'c' => -- connect focuser
            Connect;
          when 'e' => -- exit
            exit;
          when others =>
            Ada.Text_IO.Put("### expected: h, v, c or e");
          end case;
        else
          exit;
        end if;
      exception
      when Port_Unknown =>
        Ada.Text_IO.Put_Line ("### port unknown");
      when Item: others =>
        Ada.Text_IO.Put_Line (Exceptions.Name_Of (Item));
      end;
    end;
  end loop;
exception
when Item: others =>
  Ada.Text_IO.Put_Line (Exceptions.Information_Of (Item));
end Usb_Test;
