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

    Arrow_Was_Pressed : Boolean := False;
    Center_Is_Pressed : Boolean := False;
    Is_Changing       : Boolean := False;

  begin -- Reader
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
              if Center_Is_Pressed then
                if Parameter.Is_Expert_Mode then
                  Execute (Device.Increase_Speed);
                end if;
                Arrow_Was_Pressed := True;
              else
                Is_Changing := True;
                Execute (Device.Move_Up);
              end if;
            when 'd' =>
              if Center_Is_Pressed then
                if Parameter.Is_Expert_Mode then
                  Execute (Device.Decrease_Speed);
                end if;
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
                Execute (Device.No_Command);
              end if;
            when 'C' =>
              Center_Is_Pressed := False;
              if not Arrow_Was_Pressed then
                Execute (Device.Enter);
              end if;
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