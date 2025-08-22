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

pragma Build (Description => "Focuser test",
              Version     => (1, 0, 0, 2),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWS64", "COLL64"),
              Compiler    => "GNATPRO\23.0");

with Ada.IO_Exceptions;
with Ada.Text_IO;
with AWS.Client;
with AWS.Messages;
with AWS.Response;
with Celestron.Focuser;
with Exceptions;
with GNATCOLL.JSON;
with Handbox;
with Network;
with Text;

procedure Focuser_Test is

  package IO renames Ada.Text_IO;
  package JS renames GNATCOLL.JSON;

  package Focuser renames Celestron.Focuser;

  Host_Address : constant Network.Ip_Address := Network.Ip_Address_Of_Host ("localhost"); --!!! from ini file

  Not_Available : exception;


  function Client_Get (Command   : String := "get_data";
                       Parameter : String := "") return Focuser.Data is

    Host    : constant String := Network.Image_Of (Host_Address);
    Port    : constant String := Text.Trimmed (Focuser.Port_Number'image);
    Address : constant String := "http://" & Host & ":" & Port & "/" & Command;
    Url     : constant String := (if Parameter = "" then Address else Address & "=" & Parameter);

    Response : constant AWS.Response.Data        := AWS.Client.Get (Url);
    Status   : constant AWS.Messages.Status_Code := AWS.Response.Status_Code (Response);

    use type AWS.Messages.Status_Code;

  begin
    if Status /= AWS.Messages.S200 then
      IO.Put_Line ("Client_Get failed - Status: " & Status'image);
      raise Program_Error;
    end if;
    declare
      Result : constant String := AWS.Response.Message_Body (Response);
    begin
      declare
        Data : constant JS.JSON_Value := JS.Read (Result);
      begin
        declare
          Exists   : constant JS.JSON_Value := Data.Get ("exists");
          Moving   : constant JS.JSON_Value := Data.Get ("moving");
          Position : constant JS.JSON_Value := Data.Get ("position");
          Backlash : constant JS.JSON_Value := Data.Get ("backlash");
          Speed    : constant JS.JSON_Value := Data.Get ("speed");
          The_Data : Focuser.Data;
        begin
          The_Data.Exists := Exists.Get;
          The_Data.Moving := Moving.Get;
          The_Data.Position := Position.Get;
          The_Data.Backlash := Focuser.Lash(Natural'(Backlash.Get));
          The_Data.Speed := Speed.Get;
          return The_Data;
        end;
      end;
    end;
  exception
  when Ada.IO_Exceptions.Device_Error =>
    raise Not_Available;
  end Client_Get;


  The_Data : Focuser.Data;

  procedure Execute (Item : Focuser.Command) is
  begin
    The_Data := Client_Get (Command   => Focuser.Execute_Parameter,
                            Parameter => Text.Trimmed(Focuser.Command'pos(Item)'image));
  exception
  when Not_Available =>
    IO.Put_Line ("device not available");
  end Execute;


  procedure Move (To : Focuser.Distance) is
  begin
    The_Data := Client_Get (Command   => Focuser.Move_To_Parameter,
                            Parameter => Text.Trimmed(To'image));
  exception
  when Not_Available =>
    IO.Put_Line ("device not available");
  end Move;


  Is_Executing : Boolean := True;

  procedure Execute (The_Command: Handbox.Command) is
    use all type Focuser.Command;
  begin
    case The_Command is
    when Handbox.Up_Pressed =>
      Execute (Increase_Rate);
    when Handbox.Down_Pressed =>
      Execute (Decrease_Rate);
    when Handbox.Left_Pressed =>
      Execute (Move_In);
    when Handbox.Right_Pressed =>
      Execute (Move_Out);
    when Handbox.Up_Released =>
      null;
    when Handbox.Down_Released =>
      null;
    when Handbox.Left_Released | Handbox.Right_Released =>
      Execute (Stop);
    when Handbox.Center_Pressed | Handbox.Stop =>
      Execute (Stop);
    when Handbox.Center_Released =>
      Is_Executing := False;
    end case;
  end Execute;

  Has_Moved        : Boolean := False;
  Was_Disconnected : Boolean := False;

begin
  IO.Put_Line ("Focuser Test");
  IO.Put_Line ("============");
  Handbox.Start (Execute'access);
  while Is_Executing loop
    delay 1.0;
    The_Data := Client_Get;
    IO.Put_Line ("Rate:" & The_Data.Speed'image &
                 " - Backlash:" & The_Data.Backlash'image &
                 " - Position:" & The_Data.Position'image);
  end loop;
  loop
    if The_Data.Exists then
      if The_Data.Moving then
        IO.Put_Line ("Focuser Moving");
        Has_Moved := True;
      else
        IO.Put_Line ("Focuser Stopped at " & The_Data.Position'image);
        if Has_Moved then
          exit when Was_Disconnected;
        else
          if Was_Disconnected then
            Move (To => 10000);
          else
            Move (To => 9000);
          end if;
        end if;
      end if;
    else
      Has_Moved := False;
      Was_Disconnected := True;
      IO.Put_Line ("Focuser Disconnected");
    end if;
    delay 1.0;
    The_Data := Client_Get;
  end loop;
  IO.Put_Line ("Close Handbox");
  Handbox.Close;
  IO.Put_Line ("Complete");
exception
when Item: others =>
  IO.Put_Line ("Exception: " & Exceptions.Information_Of (Item));
  Handbox.Close;
end Focuser_Test;
