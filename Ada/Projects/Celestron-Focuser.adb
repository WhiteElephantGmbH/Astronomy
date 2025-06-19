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

package body Celestron.Focuser is

  package Log is new Traces ("Focuser");

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

  use type Unsigned.Byte;

  task type Control is
    entry Start;
    entry Request_Exists;
    entry Get_Exists (Item : out Boolean);
    entry Request_Moving_State;
    entry Get_Moving (Item : out Boolean);
    entry Request_Position;
    entry Get_Position (Item : out Distance);
    entry Get_Speed (Item : out Rate);
    entry Set (Item : Distance);
    entry Execute (Item : Command);
    entry Stop;
  end Control;

  The_Control  : access Control;


  procedure Start is
  begin
    The_Control := new Control;
    The_Control.Start;
  end Start;


  function Exists return Boolean is
    The_Flag : Boolean;
  begin
    The_Control.Request_Exists;
    The_Control.Get_Exists (The_Flag);
    return The_Flag;
  end Exists;


  function Moving return Boolean is
    The_Flag : Boolean;
  begin
    The_Control.Request_Moving_State;
    The_Control.Get_Moving (The_Flag);
    return The_Flag;
  end Moving;


  function Position return Distance is
    The_Position : Distance;
  begin
    The_Control.Request_Position;
    The_Control.Get_Position (The_Position);
    return The_Position;
  end Position;


  function Speed return Rate is
    The_Rate : Rate;
  begin
    The_Control.Request_Position;
    The_Control.Get_Speed (The_Rate);
    return The_Rate;
  end Speed;


  procedure Execute (Item : Command) is
  begin
    The_Control.Execute (Item);
  end Execute;


  procedure Move_To (Item : Distance) is
  begin
    The_Control.Set (Item);
  end Move_To;


  procedure Close is
  begin
    The_Control.Stop;
  end Close;


  task body Control is

    Startup_Rate : constant Rate := Rate'last - 1;

    Rate_Offset : constant Unsigned.Byte := 9 - Unsigned.Byte(Rate'last);

    type Channel_Access is access Serial_Io.Channel;

    The_Focuser_Port : Serial_Io.Port;
    The_Channel      : Channel_Access;
    Is_Available     : Boolean := False;
    Is_Moving        : Boolean := False;
    The_Position     : Distance := Distance'last;
    New_Position     : Distance;
    The_Command      : Command;
    The_Rate         : Rate := Startup_Rate;

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
          Log.Error (Error_Message & "(received: " & Unsigned.Image_Of (The_Byte) &
                                   " - expected: " & Unsigned.Image_Of (Item) & ")");
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
        Is_Available := True;
      end if;
    exception
    when others =>
      Is_Available := False;
      Serial_Io.Free (The_Focuser_Port);
    end Connect;


    procedure Get_Available_State is
    begin
      if not Is_Available then
        Connect;
      end if;
    end Get_Available_State;


    procedure Get_Position is
    begin
      if Is_Available then
        Send ([Get_Position_Id]);
        The_Position := Distance(Unsigned.Longword_Of_Big_Endian (Received_For (Get_Position_Id)));
        Log.Write ("position: " & The_Position'image);
      end if;
    end Get_Position;


    procedure Get_Moving_State is
     use type Unsigned.Byte_String;
    begin
      if Is_Available then
        Send ([Slew_Done_Id]);
        Is_Moving := Received_For (Slew_Done_Id) /= [16#FF#];
        Log.Write ("moving: " & Is_Moving'image);
      end if;
    end Get_Moving_State;


    procedure Start_Moving is
      use type Unsigned.Byte_String;
      P : constant Unsigned.Byte_String := Unsigned.String_Of (Unsigned.Longword(New_Position));
    begin
      if Is_Available then
        Send ([Set_Position_Id, P(P'first + 2), P(P'first + 1), P(P'first)]);
        if Received_For (Set_Position_Id) = [] then
          Log.Write ("moving to " & New_Position'image);
        end if;
      end if;
    end Start_Moving;


    procedure Execute_Command is
      use type Unsigned.Byte_String;
      The_Command_Id : Unsigned.Byte;
      The_Parameter  : Unsigned.Byte := Unsigned.Byte(The_Rate) + Rate_Offset;
    begin
      if Is_Available then
        case The_Command is
        when Decrease_Rate =>
          if The_Rate > Rate'first then
            The_Rate := The_Rate - 1;
          end if;
          return;
        when Increase_Rate =>
          if The_Rate < Rate'last then
            The_Rate := The_Rate + 1;
          end if;
          return;
        when Move_In =>
          The_Command_Id := Move_In_Id;
        when Move_Out =>
          The_Command_Id := Move_Out_Id;
        when Stop =>
          The_Parameter := 0;
          The_Command_Id := Move_In_Id;
        end case;
        Send ([The_Command_Id, The_Parameter]);
        if Received_For (The_Command_Id) = [] then
          Log.Write ("execute " & The_Command'image & " with rate" & The_Rate'image);
        end if;
      end if;
    end Execute_Command;

  begin -- Control
    accept Start;
    Log.Write ("started");
    Connect;
    loop
      begin
        select
          accept Request_Exists;
          Get_Available_State;
        or
          accept Get_Exists (Item : out Boolean) do
            Item := Is_Available;
          end Get_Exists;
        or
          accept Request_Moving_State;
          Get_Moving_State;
        or
          accept Get_Moving (Item : out Boolean) do
            if Is_Available then
              Item := Is_Moving;
            else
              Item := False;
            end if;
          end Get_Moving;
        or
          accept Request_Position;
          Get_Position;
        or
          accept Get_Position (Item : out Distance) do
            Item := The_Position;
          end Get_Position;
        or
          accept Get_Speed (Item : out Rate) do
            Item := The_Rate;
          end Get_Speed;
        or
          accept Set (Item : Distance) do
            New_Position := Item;
          end Set;
          Start_Moving;
        or
          accept Execute (Item : Command) do
            The_Command := Item;
          end Execute;
          Execute_Command;
        or
          accept Stop;
          exit;
        or
          delay 1.0;
          Get_Available_State;
        end select;
      exception
      when Item: others =>
        Log.Error (Exceptions.Name_Of (Item));
        Is_Available := False;
        Serial_Io.Free (The_Focuser_Port);
      end;
    end loop;
    if Is_Available then
      Serial_Io.Free (The_Focuser_Port);
    end if;
    Log.Write ("end");
  exception
  when Item: others =>
    Log.Termination (Item);
  end Control;

end Celestron.Focuser;
