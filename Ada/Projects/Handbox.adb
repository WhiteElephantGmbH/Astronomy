-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package body Handbox is

  Vendor_Id  : constant Serial_Io.Usb.Vendor_Id := 3368;
  Product_Id : constant Serial_Io.Usb.Product_Id := 516;

  Version : constant String := "2.00";

  package Log is new Traces ("Handbox");

  task type Reader (Handle : access procedure (Item : Command)) is
    entry Start;
    entry Close;
  end Reader;

  The_Reader : access Reader;

  procedure Start (With_Handler : access procedure (Item : Command)) is
  begin
    The_Reader := new Reader (With_Handler);
    The_Reader.Start;
  end Start;


  The_Port     : Serial_Io.Port;
  Is_Available : Boolean := False;

  procedure Close is
  begin
    if Is_Available then
      Serial_Io.Free (The_Port);
    end if;
    The_Reader.Close;
    Log.Write ("end");
  end Close;


  task body Reader is

    procedure Check_Handbox_Version is
      Ports : constant Serial_Io.Usb.Ports := Serial_Io.Usb.Ports_For (Vid => Vendor_Id, Pid => Product_Id);
    begin
      Is_Available := False;
      if Ports'length = 1 then
        The_Port := Ports(Ports'first);
        declare
          The_Version : String := "x.xx";
          Channel     : Serial_Io.Channel(The_Port);
        begin
          Serial_Io.Set (The_Baudrate => 19200,
                         On           => Channel);
          Serial_Io.Set_For_Read (The_Timeout => 1.0,
                                  On          => Channel);
          Serial_Io.Flush (Channel);
          Serial_Io.Send (The_Item => 'v',
                          To       => Channel);
          Serial_Io.Receive (The_Item => The_Version,
                             From     => Channel);
          if The_Version /= Version then
            Log.Error ("Incorrect version (" & The_Version & ") for port " & The_Port'img);
            return;
          end if;
        end;
        Log.Write ("port: " & The_Port'img);
        Is_Available := True;
      end if;
    exception
    when others =>
      Log.Error ("port " & The_Port'img & " is not available");
    end Check_Handbox_Version;

  begin -- Reader
    accept Start;
    Log.Write ("started");
    Main: loop
      Check_Handbox_Version;
      if Is_Available then
        declare
          Channel       : Serial_Io.Channel(The_Port);
          The_Character : Character;
          The_Command   : Command;
          Unknown_Input : exception;
        begin
          Serial_Io.Set (The_Baudrate => 19200,
                         On           => Channel);
          loop
            begin
              Serial_Io.Receive (The_Character, Channel);
              case The_Character is
              when 'c' =>
                The_Command := Center_Pressed;
              when 'C' =>
                The_Command := Center_Released;
              when 'd' =>
                The_Command := Down_Pressed;
              when 'D' =>
                The_Command := Down_Released;
              when 'l' =>
                The_Command := Left_Pressed;
              when 'L' =>
                The_Command := Left_Released;
              when 'r' =>
                The_Command := Right_Pressed;
              when 'R' =>
                The_Command := Right_Released;
              when 'u' =>
                The_Command := Up_Pressed;
              when 'U' =>
                The_Command := Up_Released;
              when others =>
                raise Unknown_Input;
              end case;
              Log.Write ("Command: " & The_Command'image);
              Handle (The_Command);
            exception
            when Serial_Io.Aborted =>
              accept Close;
              exit Main;
            when Unknown_Input =>
              Log.Error ("Unknown Input: " & The_Character);
              exit;
            when Serial_Io.No_Access | Serial_Io.Operation_Failed=>
              Log.Warning ("Disconnected");
              exit;
            end;
          end loop;
        end;
        Handle (Stop);
        Serial_Io.Free (The_Port);
      end if;
      select
        accept Close;
        exit;
      or
        delay 1.0;
      end select;
    end loop Main;
  exception
  when Item: others =>
    Log.Termination (Item);
    accept Close;
  end Reader;

end Handbox;
