-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Serial_Io.Usb;
with Traces;
with Input;

package body Handbox is

  Vendor_Id  : constant Serial_Io.Usb.Vendor_Id := 3368;
  Product_Id : constant Serial_Io.Usb.Product_Id := 516;
  Version    : constant String := "1.00";

  package Log is new Traces ("Handbox");

  task type Reader is
    entry Start;
    entry Stopped;
  end Reader;

  The_Reader : access Reader;

  The_Handbox_Port         : Serial_Io.Port;
  The_Handbox_Is_Available : Boolean := False;


  procedure Check_Handbox_Version is
  begin
    declare
      The_Version : String := "x.xx";
      Channel     : Serial_Io.Channel(The_Handbox_Port);
    begin
      Serial_Io.Set (The_Baudrate => 19200,
                     On           => Channel);
      Serial_Io.Set_For_Read (The_Timeout => 1.0,
                              On          => Channel);
      Serial_Io.Send (The_Item => 'v',
                      To       => Channel);
      Serial_Io.Receive (The_Item => The_Version,
                         From     => Channel);
      if The_Version /= Version then
        Log.Error ("Incorrect version (" & The_Version & ") for Handbox on port " & The_Handbox_Port'img);
        return;
      end if;
    end;
    Log.Write ("Handbox on Port: " & The_Handbox_Port'img);
    The_Handbox_Is_Available := True;
  exception
  when others =>
    Log.Error ("Handbox port " & The_Handbox_Port'img & " is not available");
  end Check_Handbox_Version;


  procedure Start is
    Ports : constant Serial_Io.Usb.Ports := Serial_Io.Usb.Ports_For (Vid => Vendor_Id, Pid => Product_Id);
  begin
    if Ports'length = 1 then
      The_Handbox_Port := Ports(Ports'first);
      Check_Handbox_Version;
      if The_Handbox_Is_Available then
        Log.Write ("start with " & The_Handbox_Port'img);
        The_Reader := new Reader;
        The_Reader.Start;
      end if;
    end if;
  end Start;


  procedure Close is
  begin
    if The_Handbox_Is_Available then
      Serial_Io.Free (The_Handbox_Port);
      The_Reader.Stopped;
      Log.Write ("end");
    end if;
  end Close;


  task body Reader is

    procedure Execute (Command : Input.Command) is
    begin
      Input.Put (Command, Input.Handbox);
    end Execute;

    Is_Moving           : Boolean := False;
    Center_Is_Pressed   : Boolean := False;
    Left_Action_Pending : Boolean := False;
    Is_Active           : Boolean := False;

    Delay_Time_After_Error : constant Duration := 0.5; -- seconds

  begin -- Reader
    accept Start;
    Log.Write ("started");
    Main: loop
      declare
        Channel       : Serial_Io.Channel(The_Handbox_Port);
        The_Character : Character;
      begin
        Serial_Io.Set (The_Baudrate => 19200,
                       On           => Channel);
        loop
          begin
            Serial_Io.Receive (The_Character, Channel);
            Log.Write ("Input: " & The_Character);
            case The_Character is
            when 'u' =>
              if Center_Is_Pressed then
                Execute (Input.Next_Speed);
                Is_Active := True;
              elsif not Is_Moving then
                Execute (Input.Move_Up);
                Is_Moving := True;
              end if;
            when 'd' =>
              if Center_Is_Pressed then
                Execute (Input.Previous_Speed);
                Is_Active := True;
              elsif not Is_Moving then
                Execute (Input.Move_Down);
                Is_Moving := True;
              end if;
            when 'l' =>
              if Center_Is_Pressed then
                Left_Action_Pending := True;
              elsif not Is_Moving then
                Execute (Input.Move_Left);
                Is_Moving := True;
              end if;
            when 'r' =>
              if Center_Is_Pressed then
                Execute (Input.Spiral_Offset_Next);
                Is_Active := True;
              elsif not Is_Moving then
                Execute (Input.Move_Right);
                Is_Moving := True;
              end if;
            when 'c' =>
              if not Is_Moving then
                Center_Is_Pressed := True;
                Is_Active := False;
              end if;
            when 'U' | 'D' | 'L' | 'R' =>
              if Is_Moving then
                Is_Moving := False;
                Execute (Input.End_Command);
              elsif Left_Action_Pending then
                Execute (Input.Spiral_Offset_Previous);
                Is_Active := True;
              end if;
              Left_Action_Pending := False;
            when 'C' =>
              if not Is_Moving then
                if Left_Action_Pending then
                  Execute (Input.Spiral_Offset_Center);
                  Left_Action_Pending := False;
                elsif not Is_Active then
                  Execute (Input.Go_Back);
                end if;
              end if;
              Center_Is_Pressed := False;
              Is_Active := False;
            when others =>
              Log.Error ("Unknown Input: " & The_Character);
            end case;
          exception
          when Serial_Io.Aborted =>
            exit Main;
          when Serial_Io.No_Access =>
            Log.Error ("No Access");
            delay (Delay_Time_After_Error);
          when Serial_Io.Operation_Failed =>
            Log.Error ("Read Error");
            delay (Delay_Time_After_Error);
          end;
        end loop;
      end;
    end loop Main;
    Log.Write ("stopped");
    accept Stopped;
  exception
  when Item: others =>
    Log.Termination (Item);
    accept Stopped;
  end Reader;

end Handbox;
