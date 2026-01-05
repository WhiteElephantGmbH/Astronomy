-- *********************************************************************************************************************
-- *                       (c) 2025 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Serial_Io;
with Traces;
with Unsigned;

package body Celestron.Focuser is

  package Log is new Traces ("Focuser");

  Product_Id : constant Serial_Io.Product_Id := 42255;

  Start_Packet : constant Unsigned.Byte := 16#3B#;
  Receive_Id   : constant Unsigned.Byte := 16#12#;
  Transmit_Id  : constant Unsigned.Byte := 16#22#;

  Connect_Id      : constant Unsigned.Byte := 16#FE#;
  Get_Backlash_Id : constant Unsigned.Byte := 16#40#;
  Set_In_Lash_Id  : constant Unsigned.Byte := 16#10#;
  Set_Out_Lash_Id : constant Unsigned.Byte := 16#11#;
  Get_Position_Id : constant Unsigned.Byte := 16#01#;
  Set_Position_Id : constant Unsigned.Byte := 16#02#;
  Slew_Done_Id    : constant Unsigned.Byte := 16#13#;
  Move_Out_Id     : constant Unsigned.Byte := 16#24#;
  Move_In_Id      : constant Unsigned.Byte := 16#25#;

  use type Unsigned.Byte;

  task type Control is
    entry Start;
    entry Request_Exists;
    entry Get_Exists (Item : out Boolean);
    entry Request_Moving_State;
    entry Get_Moving (Item : out Boolean);
    entry Request_Backlash;
    entry Get_Backlash (Item : out Lash);
    entry Request_Position;
    entry Get_Position (Item : out Distance);
    entry Get_Speed (Item : out Rate);
    entry Get_Home (Item : out Distance);
    entry Set (Item : Distance);
    entry Set (Item : Lash);
    entry Set_Home (Item : Distance);
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


  function Home_Position return Distance is
    The_Position : Distance;
  begin
    The_Control.Get_Home (The_Position);
    return The_Position;
  end Home_Position;


  function Backlash return Lash is
    The_Backlash : Lash;
  begin
    The_Control.Request_Backlash;
    The_Control.Get_Backlash (The_Backlash);
    return The_Backlash;
  end Backlash;


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


  procedure Set_Home (Item : Distance) is
  begin
    The_Control.Set_Home (Item);
  end Set_Home;


  procedure Set (Item : Lash) is
  begin
     The_Control.Set (Item);
  end Set;


  procedure Finish is
  begin
    The_Control.Stop;
  end Finish;


  task body Control is

    Startup_Rate : constant Rate := Rate'last - 1;

    Rate_Offset : constant Unsigned.Byte := 9 - Unsigned.Byte(Rate'last);

    The_Device    : Serial_Io.Device;
    Is_Available  : Boolean := False;
    Is_Moving     : Boolean := False;
    The_Backlash  : Lash := Lash'last;
    New_Backlash  : Lash := 0;
    Moving_Out    : Boolean := True;
    The_Position  : Distance := Distance'last;
    New_Position  : Distance;
    At_Home       : Distance := Default_Home_Position;
    The_Command   : Command;
    The_Rate      : Rate := Startup_Rate;

    Protocol_Error : exception;

    type Byte is mod 2**8;

    function Checksum_Of (Bytes : Unsigned.Byte_String;
                          Sum   : Byte := 0) return Unsigned.Byte is
      The_Sum  : Byte := Sum;
    begin
      for Item of Bytes loop
        The_Sum := The_Sum + Byte(Item);
      end loop;
      return Unsigned.Byte(not The_Sum + 1);
    end Checksum_Of;


    procedure Send (Item : Unsigned.Byte_String) is

      Header : constant Unsigned.Byte_String := [Start_Packet, Unsigned.Byte(Item'length + 2), Transmit_Id, Receive_Id];

      The_Data : Unsigned.Byte_String(1 .. Header'length + Item'length + 1);

    begin
      The_Data(1 .. Header'length) := Header;
      The_Data(Header'length + 1 .. The_Data'last - 1) := Item;
      The_Data(The_Data'last) := Checksum_Of (The_Data(The_Data'first + 1 .. The_Data'last - 1));
      The_Device.Send (The_Data);
    end Send;


    function Received_For (Command_Id : Unsigned.Byte) return Unsigned.Byte_String is

      The_Sum   : Byte;
      The_Count : Natural;

      procedure Check (Item          : Unsigned.Byte;
                       Error_Message : String) is
        The_Byte : Unsigned.Byte;
      begin
        The_Device.Receive (The_Byte);
        if The_Byte /= Item then
          Log.Error (Error_Message & "(received: " & Unsigned.Image_Of (The_Byte) &
                                   " - expected: " & Unsigned.Image_Of (Item) & ")");
          raise Protocol_Error;
        end if;
        The_Sum := The_Sum + Byte(Item);
      end Check;

    begin -- Received
      while The_Device.Next_Byte /= Start_Packet loop
        null;
      end loop;
      The_Count := Natural(The_Device.Next_Byte);
      The_Sum := Byte(The_Count);
      Check (Receive_Id, "incorrect receive Id");
      Check (Transmit_Id, "incorrect transmit Id");
      Check (Command_Id, "incorrect command Id");
      declare
        The_Data : Unsigned.Byte_String(1 .. The_Count - 3);
      begin
        if The_Data'length /= 0 then
          The_Device.Receive (The_Data);
        end if;
        Check (Checksum_Of (The_Data, The_Sum), "incorrect checksum");
        return The_Data;
      end;
    end Received_For;


    procedure Connect is
    begin
      The_Device.Allocate (Vendor => Vendor_Id, Product => Product_Id);
      The_Device.Set (Serial_Io.B19200);
      The_Device.Set_For_Read (The_Timeout => 1.0);
      Send ([Connect_Id]);
      declare
        Version : constant Unsigned.Byte_String := Received_For (Connect_Id);
      begin
        Log.Write ("version: " & Unsigned.Hex_Image_Of (Version));
      end;
      Is_Available := True;
    exception
    when others =>
      Is_Available := False;
    end Connect;


    procedure Get_Available_State is
    begin
      if not Is_Available then
        Connect;
      end if;
    end Get_Available_State;


    procedure Get_Backlash is
    begin
      if Is_Available then
        Send ([Get_Backlash_Id]);
        The_Backlash := Lash(Unsigned.Word_Of (Received_For (Get_Backlash_Id)));
        Log.Write ("backlash:" & The_Backlash'image);
      end if;
    end Get_Backlash;


    procedure Get_Position is
    begin
      if Is_Available then
        Send ([Get_Position_Id]);
        The_Position := Distance(Unsigned.Longword_Of_Big_Endian (Received_For (Get_Position_Id)));
        Log.Write ("position:" & The_Position'image);
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


    procedure Set_Backlash (Id : Unsigned.Byte) is
      use type Unsigned.Byte_String;
    begin
      if Is_Available then
        Send ([Id, Unsigned.Byte(New_Backlash)]);
        if Received_For (Id) = [] then
          Log.Write ("set backlash for " & Id'image & " to" & New_Backlash'image);
        end if;
      end if;
    end Set_Backlash;


    procedure Set_Backlash is
    begin
      if Is_Available then
        Set_Backlash (Set_In_Lash_Id);
        Set_Backlash (Set_Out_Lash_Id);
      end if;
    end Set_Backlash;


    procedure Start_Moving_To (Pos : Distance) is
      use type Unsigned.Byte_String;
      P : constant Unsigned.Byte_String := Unsigned.String_Of (Unsigned.Longword(Pos));
    begin
      if Is_Available then
        Send ([Set_Position_Id, P(P'first + 2), P(P'first + 1), P(P'first)]);
        if Received_For (Set_Position_Id) = [] then
          Log.Write ("moving to " & Pos'image);
        end if;
      end if;
    end Start_Moving_To;


    procedure Start_Moving is
    begin
      Start_Moving_To (New_Position);
    end Start_Moving;


    procedure Goto_Home_Position is
      Backward_Home : constant Distance := At_Home - Distance(The_Backlash);
    begin
      if Moving_Out then
        if The_Position < At_Home then
          Start_Moving_To (At_Home);
        else
          Start_Moving_To (Backward_Home);
          Moving_Out := False;
        end if;
      else
        if The_Position > Backward_Home then
          Start_Moving_To (Backward_Home);
        else
          Start_Moving_To (At_Home);
          Moving_Out := True;
        end if;
      end if;
    end Goto_Home_Position;


    procedure Execute is
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
        when Home =>
          Goto_Home_Position;
          return;
        when Move_In =>
          The_Command_Id := Move_In_Id;
          Moving_Out := False;
        when Move_Out =>
          The_Command_Id := Move_Out_Id;
          Moving_Out := True;
        when Stop =>
          The_Parameter := 0;
          The_Command_Id := Move_In_Id;
        end case;
        Send ([The_Command_Id, The_Parameter]);
        if Received_For (The_Command_Id) = [] then
          Log.Write ("execute " & The_Command'image & " with rate" & The_Rate'image);
        end if;
      end if;
    end Execute;

  begin -- Control
    accept Start;
    Log.Write ("Control: started");
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
          accept Request_Backlash;
          Get_Backlash;
        or
          accept Get_Backlash (Item : out Lash) do
            Item := The_Backlash;
          end Get_Backlash;
        or
          accept Request_Position;
          Get_Position;
        or
          accept Get_Position (Item : out Distance) do
            if Moving_Out then
              Item := The_Position;
            else
              Item := The_Position + Distance(The_Backlash);
            end if;
          end Get_Position;
        or
          accept Get_Speed (Item : out Rate) do
            Item := The_Rate;
          end Get_Speed;
        or
          accept Get_Home (Item : out Distance) do
            Item := At_Home;
          end Get_Home;
        or
          accept Set_Home (Item : Distance) do
            At_Home := Item;
          end Set_Home;
        or
          accept Set (Item : Lash) do
            New_Backlash := Item;
          end Set;
          Set_Backlash;
        or
          accept Set (Item : Distance) do
            New_Position := Item;
          end Set;
          Start_Moving;
        or
          accept Execute (Item : Command) do
            The_Command := Item;
          end Execute;
          Execute;
        or
          accept Stop;
          exit;
        or
          delay 1.0;
          Get_Available_State;
        end select;
      exception
      when Serial_Io.No_Access =>
        Is_Available := False;
      when Item: others =>
        Log.Error ("Control " & Exceptions.Name_Of (Item));
        Is_Available := False;
      end;
    end loop;
    The_Device.Close;
    Log.Write ("Control: end");
  exception
  when Item: others =>
    Log.Termination (Item);
    The_Device.Close;
  end Control;

end Celestron.Focuser;
