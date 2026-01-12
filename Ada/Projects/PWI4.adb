-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with AWS.Client;
with AWS.Response;
with Os.Process;
with PWI4.Protocol;
with Text;
with Traces;

package body PWI4 is

  package Log is new Traces ("PWI");

  use type Text.String;

  The_Ip_Address_And_Port : Text.String := ["127.0.0.1:8220"]; -- Default

  The_Process_Id : Os.Process.Id;


  function Image_Of (Item : Arc_Second) return String is
  begin
    return Text.Trimmed (Item'image);
  end Image_Of;


  function Image_Of (Item : Degrees) return String is
  begin
    return Text.Trimmed (Item'image);
  end Image_Of;


  function Image_Of (Item : Device_Index) return String is
  begin
    return Text.Trimmed (Item'image);
  end Image_Of;


  function Image_Of (Item : Hours) return String is
  begin
    return Text.Trimmed (Item'image);
  end Image_Of;


  function Image_Of (Item : Microns) return String is
  begin
    return Text.Trimmed (Integer(Item)'image);
  end Image_Of;


  function Image_Of (Item : Points) return String is
  begin
    return Text.Trimmed (Item'image);
  end Image_Of;


  function Image_Of (Item : Update_Count) return String is
  begin
    return Text.Trimmed (Item'image);
  end Image_Of;


  function Startup (Filename   : String;
                    Ip_Address : String) return Boolean is
  begin
    The_Process_Id := Os.Process.Created (Filename);
    The_Ip_Address_And_Port := [Ip_Address];
    return True;
  exception
  when others =>
    return False;
  end Startup;


  procedure Shutdown is
  begin
    Os.Process.Terminate_With (The_Process_Id);
  exception
  when others =>
    Log.Write ("already terminated");
  end Shutdown;


  The_Error_Info : Text.String;

  procedure Set_Error_Info (Data : AWS.Response.Data) is
  begin
    The_Error_Info := [Text.Trimmed (AWS.Response.Message_Body (Data))];
    Log.Error (Error_Info);
  end Set_Error_Info;

  function Error_Info return String is (The_Error_Info.S);


  procedure Execute (Item : String) is
    Url : constant String := "http://" & The_Ip_Address_And_Port;
  begin
    Log.Write ("Execute " & Item);
    declare
      Data   : constant AWS.Response.Data := AWS.Client.Get (Url & '/' & Item);
      Status : constant AWS.Messages.Status_Code := AWS.Response.Status_Code (Data);
    begin
      case Status is
      when AWS.Messages.Informational =>
        Log.Write ("Informational response: " & Status'image);
      when AWS.Messages.Success =>
        null;
      when AWS.Messages.Client_Error =>
        Log.Error ("Client error: " & Status'image);
        Set_Error_Info (Data);
        Protocol.Set_Error (Status);
        return;
      when AWS.Messages.Server_Error =>
        Log.Error ("Server error: " & Status'image);
        Set_Error_Info (Data);
        Protocol.Set_Error (Status);
        return;
      when others =>
        Log.Error ("Unexpected response: " & Status'image);
        Set_Error_Info (Data);
        Protocol.Set_Error (Status);
        return;
      end case;
      Protocol.Parse (AWS.Response.Message_Body (Data));
    end;
  end Execute;


  procedure Get_System is
  begin
    Execute ("status");
  end Get_System;


  procedure Test_Crash is
  begin
    Execute ("/internal/crash");
  end Test_Crash;


  procedure Test_Error is
  begin
    Execute ("/command/notfound");
  end Test_Error;


  procedure Execute (Device     : String;
                     Command    : String;
                     Parameters : Parameter := "") is
  begin
    Execute (Device & "/" & Command & (if Parameters = "" then "" else "?" & String(Parameters)));
  end Execute;


  function "/" (Left  : String;
                Right : String) return Parameter is
  begin
    return Parameter (String'(Left & '=' & Right));
  end "/";


  function "+" (Left  : Parameter;
                Right : Parameter) return Parameter is
  begin
    if Left = "" then
      return Right;
    elsif Right = "" then
      return Left;
    else
      return Left & '&' & Right;
    end if;
  end "+";


  function Image_Of (The_Port : Port) return Character is
  begin
    case The_Port is
    when Port_1 =>
      return '1';
    when Port_2 =>
      return '2';
    end case;
  end Image_Of;

end PWI4;
