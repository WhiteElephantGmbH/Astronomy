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

with Celestron.Focuser;
with Serial_Io.Usb;
with Traces;

package body Handbox is

  Vendor_Id  : constant Serial_Io.Usb.Vendor_Id := 3368;
  Product_Id : constant Serial_Io.Usb.Product_Id := 516;
  Version    : constant String := "1.00";

  package Log is new Traces ("Handbox");

  package Focuser renames Celestron.Focuser;

  task type Reader is
    entry Start;
    entry Close;
  end Reader;

  The_Reader : access Reader;

  procedure Start is
  begin
    The_Reader := new Reader;
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

    Is_Moving : Boolean := False;

  begin -- Reader
    accept Start;
    Log.Write ("started");
    Main: loop
      Check_Handbox_Version;
      if Is_Available then
        declare
          Channel       : Serial_Io.Channel(The_Port);
          The_Character : Character;
        begin
          Serial_Io.Set (The_Baudrate => 19200,
                         On           => Channel);
          loop
            begin
              Serial_Io.Receive (The_Character, Channel);
              if The_Character in 'c' | 'C' | 'd' |'D' | 'l' |'L' | 'r' |'R' | 'u' | 'U' then
                Log.Write ("input: " & The_Character);
                if Is_Moving then
                  case The_Character is
                  when 'L' | 'R' =>
                    Focuser.Execute (Focuser.Stop);
                    Is_Moving := False;
                  when others =>
                    null;
                  end case;
                else
                  case The_Character is
                  when 'u' =>
                    Focuser.Execute (Focuser.Increase_Rate);
                  when 'd' =>
                    Focuser.Execute (Focuser.Decrease_Rate);
                  when 'l' =>
                    Focuser.Execute (Focuser.Move_In);
                    Is_Moving := True;
                  when 'r' =>
                    Focuser.Execute (Focuser.Move_Out);
                    Is_Moving := True;
                  when others =>
                    null;
                  end case;
                end if;
              else
                Log.Error ("Unknown Input: " & The_Character);
              end if;
            exception
            when Serial_Io.Aborted =>
              accept Close;
              exit Main;
            when Serial_Io.No_Access =>
              exit;
            when Serial_Io.Operation_Failed =>
              Log.Error ("Read Error");
              exit;
            end;
          end loop;
        end;
        Focuser.Execute (Focuser.Stop);
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
