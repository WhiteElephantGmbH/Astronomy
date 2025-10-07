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

with Exceptions;
with Serial_Io;
with Traces;

package body Handbox is

  Vendor_Id  : constant Serial_Io.Vendor_Id := 3368;
  Product_Id : constant Serial_Io.Product_Id := 516;

  Version : constant String := "2.00";

  package Log is new Traces ("Handbox");

  task type Reader (Handle : access procedure (Item : Command)) is
    entry Close;
  end Reader;

  The_Reader : access Reader;

  procedure Start (With_Handler : access procedure (Item : Command)) is
  begin
    The_Reader := new Reader (With_Handler);
  end Start;


  The_Device : Serial_Io.Device;

  Is_Available : Boolean := False;

  procedure Close is
  begin
    The_Device.Free;
    The_Reader.Close;
  end Close;


  task body Reader is

    procedure Check_Handbox_Version is
      The_Version : String := "x.xx";
    begin
      Is_Available := False;
      The_Device.Allocate (Vendor  => Vendor_Id,
                           Product => Product_Id);
      The_Device.Set (The_Baudrate => Serial_Io.B19200);
      The_Device.Set_For_Read (The_Timeout => 1.0);
      The_Device.Flush;
      The_Device.Send ('v');
      The_Device.Receive (The_Version);
      if The_Version /= Version then
        Log.Error ("Incorrect version (" & The_Version & ")");
        return;
      end if;
      The_Device.Set_For_Read (The_Timeout => Serial_Io.Infinite);
      Is_Available := True;
    exception
    when Serial_Io.No_Access | Serial_Io.Device_Not_Found=>
      null;
    when Serial_Io.Multiple_Devices=>
      Log.Warning ("More than one Handbox connected.");
    when Item: others =>
      Log.Error ("Check_Handbox_Version " & Exceptions.Name_Of (Item));
    end Check_Handbox_Version;

  begin -- Reader
    Log.Write ("Reader: started");
    Main: loop
      Check_Handbox_Version;
      if Is_Available then
        declare
          The_Character : Character;
          The_Command   : Command;
          Unknown_Input : exception;
        begin
          The_Device.Set (Serial_Io.B19200);
          loop
            begin
              The_Device.Receive (The_Character);
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
      end if;
      select
        accept Close;
        exit;
      or
        delay 1.0;
      end select;
    end loop Main;
    The_Device.Close;
    Log.Write ("Reader: end");
  exception
  when Item: others =>
    Log.Termination (Item);
    accept Close;
    The_Device.Close;
  end Reader;

end Handbox;
