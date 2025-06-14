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
with Serial_Io.Usb;
with Traces;
with Unsigned;

package body Focuser is

  package Log is new Traces ("Focuser");

  Vendor_Id  : constant Serial_Io.Usb.Vendor_Id := 5538;
  Product_Id : constant Serial_Io.Usb.Product_Id := 42255;

  Start_Packet : constant Unsigned.Byte := 16#3B#;
  Receive_Id   : constant Unsigned.Byte := 16#12#;
  Transmit_Id  : constant Unsigned.Byte := 16#22#;

  Connect_Id       : constant Unsigned.Byte := 16#FE#;
  Get_Position_Id  : constant Unsigned.Byte := 16#01#;
  Set_Position_Id  : constant Unsigned.Byte := 16#02#;
  Slew_Done_Id     : constant Unsigned.Byte := 16#13#;
  Move_Out_Id      : constant Unsigned.Byte := 16#24#;
  Move_In_Id       : constant Unsigned.Byte := 16#25#;

  type Command is (Move_Out, Move_In);

  Speed_Offset : constant Unsigned.Byte := 6; --> speeds 7 .. 9

  use type Unsigned.Byte;

  task type Control is
    entry Start;
    entry Get (Item : out Distance);
    entry Get (Item : out Status);
    entry Set (Item : Distance);
    entry Execute (Item : Command;
                   Rate : Unsigned.Byte);
    entry Stop;
  end Control;

  The_Control  : access Control;


  procedure Start is
  begin
    The_Control := new Control;
    The_Control.Start;
  end Start;


  function State return Status is
    The_Status : Status;
  begin
    The_Control.Get (The_Status);
    return The_Status;
  end State;


  function Position return Distance is
    The_Position : Distance;
  begin
    The_Control.Get (The_Position);
    return The_Position;
  end Position;


  procedure Move (To : Distance) is
  begin
    The_Control.Set (To);
  end Move;


  procedure Move_In (Item : Speed) is
  begin
    The_Control.Execute (Move_In, Unsigned.Byte(Item) + Speed_Offset);
  end Move_In;


  procedure Move_Out (Item : Speed) is
  begin
    The_Control.Execute (Move_Out, Unsigned.Byte(Item) + Speed_Offset);
  end Move_Out;


  procedure Stop is
  begin
    The_Control.Execute (Move_In, 0);
  end Stop;


   procedure Close is
  begin
    The_Control.Stop;
  end Close;


  task body Control is

    type Channel_Access is access Serial_Io.Channel;

    The_Focuser_Port : Serial_Io.Port;
    The_Channel      : Channel_Access;
    Is_Available     : Boolean := False;
    Is_Moving        : Boolean := False;
    The_Position     : Distance;
    New_Position     : Distance;
    The_Command      : Command;
    The_Speed        : Unsigned.Byte;

    Protocol_Error : exception;


    function Checksum_Of (Sum : Natural) return Unsigned.Byte is
    begin
      return Unsigned.Byte(256 - (Sum mod 256));
    end Checksum_Of;


    procedure Send (Item : Unsigned.Byte_String) is

      Header : constant Unsigned.Byte_String := [Start_Packet, Unsigned.Byte(Item'length + 2), Transmit_Id, Receive_Id];

      The_Data : Unsigned.Byte_String(1 .. Header'length + Item'length + 1);
      The_Sum  : Natural := 0;

    begin
      The_Data(1 .. Header'length) := Header;
      The_Data(Header'length + 1 .. The_Data'last - 1) := Item;
      for Index in The_Data'first + 1 .. The_Data'last - 1 loop
        The_Sum := The_Sum + Natural(The_Data(Index));
      end loop;
      The_Data(The_Data'last) := Checksum_Of (The_Sum);
      Serial_Io.Send (The_Data, The_Channel.all);
    end Send;


    function Received_For (Command_Id : Unsigned.Byte) return Unsigned.Byte_String is

      The_Sum   : Natural;
      The_Count : Natural;

      procedure Check (Item          : Unsigned.Byte;
                       Error_Message : String) is
        The_Byte : Unsigned.Byte;
      begin
        Serial_Io.Receive (The_Byte, From => The_Channel.all);
        if The_Byte /= Item then
          Log.Error (Error_Message & "(expected: " & Unsigned.Image_Of (Item) & ")");
          raise Protocol_Error;
        end if;
        The_Sum := The_Sum + Natural(Item);
      end Check;

    begin -- Received
      while Serial_Io.Byte_Of (The_Channel.all) /= Start_Packet loop
        null;
      end loop;
      The_Count := Natural(Serial_Io.Byte_Of (The_Channel.all));
      The_Sum := The_Count;
      Check (Receive_Id, "incorrect receive Id");
      Check (Transmit_Id, "incorrect transmit Id");
      Check (Command_Id, "incorrect command Id");
      declare
        The_Data : Unsigned.Byte_String(1 .. The_Count - 3);
      begin
        if The_Data'length /= 0 then
          Serial_Io.Receive (The_Data, From => The_Channel.all);
          for Item of The_Data loop
            The_Sum := The_Sum + Natural(Item);
          end loop;
        end if;
        Check (Checksum_Of (The_Sum), "incorrect checksum");
        return The_Data;
      end;
    end Received_For;


    procedure Connect is
      Ports : constant Serial_Io.Usb.Ports := Serial_Io.Usb.Ports_For (Vid => Vendor_Id, Pid => Product_Id);
    begin
      if Ports'length = 1 then
        The_Focuser_Port := Ports(Ports'first);
        The_Channel := new Serial_Io.Channel(The_Focuser_Port);
        Serial_Io.Set (The_Baudrate => 19200, On => The_Channel.all);
        Serial_Io.Set_For_Read (The_Timeout => 1.0, On => The_Channel.all);
        Send ([Connect_Id]);
        declare
          Version : constant Unsigned.Byte_String := Received_For (Connect_Id);
        begin
          Log.Write ("version: " & Unsigned.Hex_Image_Of (Version));
        end;
      end if;
      Is_Available := True;
    exception
    when Item: others =>
      Log.Error ("Connect: " & Exceptions.Name_Of (Item));
      Is_Available := False;
      Serial_Io.Free (The_Focuser_Port);
    end Connect;


    procedure Get_Values is
     use type Unsigned.Byte_String;
    begin
      if Is_Available then
        Send ([Get_Position_Id]);
        The_Position := Distance(Unsigned.Longword_Of_Big_Endian (Received_For (Get_Position_Id)));
        Log.Write ("position: " & The_Position'image);
        Send ([Slew_Done_Id]);
        Is_Moving := Received_For (Slew_Done_Id) /= [16#FF#];
        Log.Write ("moving: " & Is_Moving'image);
      end if;
    exception
    when Item: others =>
      Log.Error ("Get_Position: " & Exceptions.Name_Of (Item));
      Is_Available := False;
      Serial_Io.Free (The_Focuser_Port);
    end Get_Values;


    procedure Start_Moving is
      use type Unsigned.Byte_String;
      P : constant Unsigned.Byte_String := Unsigned.String_Of (Unsigned.Longword(New_Position));
    begin
      if Is_Available then
        Send ([Set_Position_Id, P(P'first + 2), P(P'first + 1), P(P'first)]);
        Is_Moving := Received_For (Set_Position_Id) = [];
        Log.Write ("moving started: " & Is_Moving'image);
      end if;
    exception
    when Item: others =>
      Log.Error ("Start_Moving: " & Exceptions.Name_Of (Item));
      Is_Available := False;
      Serial_Io.Free (The_Focuser_Port);
    end Start_Moving;


    procedure Execute_Command is
      use type Unsigned.Byte_String;
      The_Command_Id : Unsigned.Byte;
    begin
      if Is_Available then
        Log.Write ("Execute: " & The_Command'image);
        case The_Command is
        when Move_In =>
          The_Command_Id := Move_In_Id;
        when Move_Out =>
          The_Command_Id := Move_Out_Id;
        end case;
        Send ([The_Command_Id, The_Speed]);
        if Received_For (The_Command_Id) = [] then
          Log.Write ("execute " & The_Command'image & " with speed" & The_Speed'image);
        end if;
      end if;
    exception
    when Item: others =>
      Log.Error ("Execute_Command: " & Exceptions.Name_Of (Item));
      Is_Available := False;
      Serial_Io.Free (The_Focuser_Port);
    end Execute_Command;

  begin -- Control
    accept Start;
    Connect;
    Get_Values;
    Log.Write ("started");
    loop
      select
        accept Get (Item : out Status) do
          if Is_Available then
            if Is_Moving then
              Item := Moving;
            else
              Item := Stopped;
            end if;
          else
            Item := Disconnected;
          end if;
        end Get;
      or
        accept Get (Item : out Distance) do
          if Is_Available then
            Item := The_Position;
          else
            Item := Distance'last;
          end if;
        end Get;
      or
        accept Set (Item : Distance) do
          New_Position := Item;
        end Set;
        Start_Moving;
      or
        accept Execute (Item : Command;
                        Rate : Unsigned.Byte)
        do
          The_Command := Item;
          The_Speed := Rate;
        end Execute;
        Execute_Command;
      or
        accept Stop;
        exit;
      or
        delay 1.0;
        if not Is_Available then
          Connect;
        end if;
        Get_Values;
      end select;
    end loop;
    if Is_Available then
      Serial_Io.Free (The_Focuser_Port);
      Log.Write ("end");
    end if;
  exception
  when Item: others =>
    Log.Termination (Item);
  end Control;

end Focuser;
