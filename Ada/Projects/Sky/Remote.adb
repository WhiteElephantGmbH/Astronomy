-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Persistent_String;
with Strings;
with Traces;

package body Remote is

  package Log is new Traces ("Remote");

  package Persistent_Key is new Persistent_String ("Remote_Key");

  Remote_Key : Persistent_Key.Data;

  function Actual_Key return String is
  begin
    if Remote_Key.Item = "" then
      Remote_Key.Store ("ZzRW8sYHdHrgZGG3");
    end if;
    return Remote_Key.Item;
  end Actual_Key;

  Key : constant String := Actual_Key;

  type Telescope_State is (Unknown, On_Target, Not_On_Target);


  task type Handler is

    entry Start (Telescope_Name : String;
                 Ip_Address     : Network.Ip_Address;
                 Port           : Network.Port_Number);

    entry Execute (The_Command : Command);

    entry Define (Target : String);

    entry Define (State : Telescope_State);

    entry Close;
  end Handler;

  The_Handler : access Handler;


  procedure Start (Telescope_Name : String;
                   Ip_Address     : Network.Ip_Address;
                   Port           : Network.Port_Number) is
  begin
    Log.Write ("start");
    The_Handler := new Handler;
    The_Handler.Start (Telescope_Name, Ip_Address, Port);
  end Start;


  procedure Close is
  begin
    if The_Handler /= null then
      Log.Write ("close");
      The_Handler.Close;
    end if;
  end Close;


  procedure Execute (The_Command : Command) is
  begin
    if The_Handler /= null then
      The_Handler.Execute (The_Command);
    end if;
  end Execute;


  procedure Define (Target : String) is
  begin
    if The_Handler /= null then
      The_Handler.Define (Target);
    end if;
  end Define;


  protected Actual_State is

    procedure Check (State       :     Telescope_State;
                     Has_Changed : out Boolean);

  private
    Last_State : Telescope_State:= Unknown;
  end Actual_State;


  protected body Actual_State is

    procedure Check (State       :     Telescope_State;
                     Has_Changed : out Boolean) is
    begin
      Has_Changed := Last_State /= State;
      Last_State := State;
    end Check;

  end Actual_State;


  procedure Define (Is_On_Target : Boolean) is
    Has_Changed : Boolean;
    State       : constant Telescope_State := (if Is_On_Target then On_Target else Not_On_Target);
  begin
    if The_Handler /= null then
      Actual_State.Check (State, Has_Changed);
      if Has_Changed then
        The_Handler.Define (State);
      end if;
    end if;
  end Define;


  task body Handler is

     The_Telescope_Name : Strings.Element;
     The_Remote_Address : Network.Ip_Address;
     The_Remote_Port    : Network.Port_Number;

     use type Strings.Element;

    procedure Send (Info : String) is

      Telescope_Name : constant String := +The_Telescope_Name;
      Remote_Address : constant String := Network.Image_Of (The_Remote_Address);
      Remote_Port    : constant String := Network.Image_Of (The_Remote_Port);
      Parameters     : constant String := "?tele=" & Telescope_Name & "&" & Info;
      URL            : constant String := "http://" & Remote_Address & ':' & Remote_Port & '/' & Key & Parameters;

      Timeouts : constant AWS.Client.Timeouts_Values := AWS.Client.Timeouts (Connect  => 1.0,
                                                                             Send     => 1.0,
                                                                             Receive  => 1.0,
                                                                             Response => 1.0);
    begin -- Send
      Log.Write ("Send " & Info & " from " & Telescope_Name);
      Log.Write ("URL: " & URL);
      declare
        Response : constant String := AWS.Response.Message_Body (AWS.Client.Get (URL, Timeouts => Timeouts));
      begin
        if Response = "ok" then
          Log.Write ("Information accepted");
        else
          declare
            Last : Natural := Strings.Location_Of ("Call stack", Response);
          begin
            if Last = Strings.Not_Found then
              Last := Response'last;
            else
              Last := Last - 1;
            end if;
            Log.Error (Strings.Trimmed (Response(Response'first .. Last)));
          end;
        end if;
      end;
    exception
    when Occurrence: others =>
      Log.Termination (Occurrence);
    end Send;


    procedure Send_Command (Item : String) is
    begin
      Send ("command=" & Item);
    end Send_Command;


    The_State          : Telescope_State := Unknown;
    Is_Cleared         : Boolean := True;
    The_Actual_Command : Command;
    The_Actual_Target  : Strings.Element;

    procedure Send_Actual (Clear : Boolean := False) is
    begin
      if The_State = On_Target then
        if Clear or Is_Cleared then
          Is_Cleared := False;
          Send ("target=" & The_Actual_Target);
        end if;
      elsif Clear then
        Send ("target=");
        Is_Cleared := True;
      end if;
    end Send_Actual;

  begin -- Handler
    accept Start (Telescope_Name : String;
                  Ip_Address     : Network.Ip_Address;
                  Port           : Network.Port_Number)
    do
      The_Telescope_Name := [Telescope_Name];
      The_Remote_Address := Ip_Address;
      The_Remote_Port := Port;
    end Start;
    Log.Write ("started");
    loop
      select
        accept Execute (The_Command : Command) do
          The_Actual_Command := The_Command;
        end Execute;
        case The_Actual_Command is
        when Start_Session =>
          Send_Command ("Start");
        when Generate_Qr_Code =>
          Send_Command ("QR-Code");
        when End_Session =>
          Send_Command ("End");
        end case;
      or
        accept Define (State  : Telescope_State) do
          The_State := State;
        end Define;
        Send_Actual;
      or
        accept Define (Target : String) do
          The_Actual_Target := [Target];
        end Define;
      or
        accept Close;
        exit;
      or
        delay 3.0;
        Send_Actual (Clear => True);
      end select;
    end loop;
    Log.Write ("closed");
  exception
  when Item: others =>
    Log.Termination (Item);
  end Handler;

end Remote;
