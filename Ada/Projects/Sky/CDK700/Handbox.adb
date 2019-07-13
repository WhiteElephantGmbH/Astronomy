-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
with Parameter;
with Serial_Io;
with Traces;
with User.Input;

package body Handbox is

  package Log is new Traces ("Handbox");

  task type Reader is
    entry Start;
    entry Stopped;
  end Reader;

  The_Reader : access Reader;


  procedure Start is
  begin
    if Parameter.Handbox_Is_Available then
      Log.Write ("start");
      The_Reader := new Reader;
      The_Reader.Start;
    end if;
  end Start;


  procedure Close is
  begin
    if Parameter.Handbox_Is_Available then
      Serial_Io.Free (Parameter.Handbox_Port);
      The_Reader.Stopped;
      Log.Write ("end");
    end if;
  end Close;


  task body Reader is

    procedure Execute (Command : Device.Command) is
    begin
      User.Input.Put (Command, User.Input.Handbox);
    end Execute;

  begin
    accept Start;
    Log.Write ("started");
    Main: loop
      declare
        Channel       : Serial_Io.Channel(Parameter.Handbox_Port);
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
              Execute (Device.Move_Up);
            when 'd' =>
              Execute (Device.Move_Down);
            when 'l' =>
              Execute (Device.Move_Left);
            when 'r' =>
              Execute (Device.Move_Right);
            when 'c' =>
              Execute (Device.Enter);
            when 'U' | 'D' | 'L' | 'R' | 'C' =>
              Execute (Device.No_Command);
            when others =>
              Log.Error ("Unknown Input: " & The_Character);
            end case;
          exception
          when Serial_Io.Aborted =>
            exit Main;
          when Serial_Io.No_Access =>
            Log.Error ("No Access");
          when Serial_Io.Operation_Failed =>
            Log.Error ("Read Error");
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
