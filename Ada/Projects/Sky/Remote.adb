-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
with Network;
with Parameter;
with Persistent_String;
with Strings;
with Text;
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

  Is_Tracking       : Boolean := False;
  The_Actual_Target : Text.String;


  procedure Send (Info : String) is

    Telescope_Name : constant String := Parameter.Telescope_Name;
    Remote_Address : constant String := Network.Image_Of (Parameter.Remote_Address);
    Remote_Port    : constant String := Network.Image_Of (Parameter.Remote_Port);
    Parameters     : constant String := "?tele=" & Telescope_Name & "&" & Info;
    URL            : constant String := "http://" & Remote_Address & ':' & Remote_Port & '/' & Key & Parameters;

  begin -- Send
    Log.Write ("Send " & Info & " from " & Telescope_Name);
    Log.Write ("URL: " & URL);
    declare
      Response : constant String := AWS.Response.Message_Body (AWS.Client.Get (URL));
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
          Log.Write (Strings.Trimmed (Response(Response'first .. Last)));
        end;
      end if;
    end;
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Send;


  procedure Send_Actual is
  begin
    Send ("target=" & Text.String_Of (The_Actual_Target));
  end Send_Actual;


  procedure Define (Target : String;
                    State  : Telescope.State) is
    use type Telescope.State;
  begin
    if Is_Tracking then
      if not (State in Telescope.Tracking | Telescope.Positioned) then
        Text.Clear (The_Actual_Target);
        Is_Tracking := False;
        Send_Actual;
      end if;
    else
      if State in Telescope.Tracking | Telescope.Positioned then
        The_Actual_Target := Text.String_Of (Target);
        Is_Tracking := True;
        Send_Actual;
      end if;
    end if;
  end Define;


  procedure Execute (The_Command : Command) is

    procedure Send_Command (Item : String) is
    begin
      Send ("command=" & Item);
    end Send_Command;

  begin -- Execute
    case The_Command is
    when Start_Session =>
      Send_Command ("Start");
      Send_Actual;
    when Generate_Qr_Code =>
      Send_Command ("QR-Code");
    when End_Session =>
      Send_Command ("End");
    end case;
  end Execute;

end Remote;
