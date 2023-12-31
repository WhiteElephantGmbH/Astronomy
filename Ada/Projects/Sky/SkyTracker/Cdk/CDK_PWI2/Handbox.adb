-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Device;
with Serial_Io.Usb;
with Traces;
with User.Input;

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

    procedure Execute (Command : Device.Command) is
    begin
      User.Input.Put (Command, User.Input.Handbox);
    end Execute;

    Arrow_Was_Pressed : Boolean := False;
    Center_Is_Pressed : Boolean := False;
    Is_Changing       : Boolean := False;

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
                Execute (Device.Next_Speed);
                Arrow_Was_Pressed := True;
              else
                Is_Changing := True;
                Execute (Device.Move_Up);
              end if;
            when 'd' =>
              if Center_Is_Pressed then
               Execute (Device.Previous_Speed);
                Arrow_Was_Pressed := True;
              else
                Is_Changing := True;
                Execute (Device.Move_Down);
              end if;
            when 'l' =>
              Is_Changing := True;
              if Center_Is_Pressed then
                Execute (Device.Decrease_Time);
                Arrow_Was_Pressed := True;
              else
                Execute (Device.Move_Left);
              end if;
            when 'r' =>
              Is_Changing := True;
              if Center_Is_Pressed then
                Execute (Device.Increase_Time);
                Arrow_Was_Pressed := True;
              else
                Execute (Device.Move_Right);
              end if;
            when 'c' =>
              Center_Is_Pressed := True;
              Arrow_Was_Pressed := False;
            when 'U' | 'D' | 'L' | 'R' =>
              if Is_Changing then
                Is_Changing := False;
                Execute (Device.End_Command);
              end if;
            when 'C' =>
              Center_Is_Pressed := False;
              if not Arrow_Was_Pressed then
                Execute (Device.Go_Back);
              end if;
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
