-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.IO_Exceptions;
with AWS.Client;
with AWS.Response;
with GNATCOLL.JSON;
with Text;
with Traces;

package body ENC_2302_Client is

  package Log is new Traces ("ENC_2302_Client");

  package JS renames GNATCOLL.JSON;

  function Client_Get (Host : Network.Ip_Address;
                       P    : String := "";
                       S    : String := "") return Switches is

    H   : constant String := Network.Image_Of (Host);
    Url : constant String := (if P = "" then "http://" & H & "/statusjsn.js?components=1"
                                        else "http://" & H & "/statusjsn.js?components=1&cmd=1&p=" & P & "&s=" & S);
  begin
    Log.Write ("Client_Get");
    Log.Write ("URL: <<<" & Url & ">>>");
    declare
      Result  : constant String := AWS.Response.Message_Body (AWS.Client.Get (Url));
      Data    : constant JS.JSON_Value := JS.Read (Result);
      Outputs : constant JS.JSON_Value := Data.Get ("outputs");
    begin
      Log.Write ("OUT<<<" & Outputs.Write & ">>>");
      declare
        Array_Items  : constant JS.JSON_Array := Outputs.Get;
        The_Switches : Switches;
        The_Port     : Port := Port'first;
      begin
        for Item of Array_Items loop
          declare
            Device_Name  : constant JS.JSON_Value := Item.Get ("name");
            Device_State : constant JS.JSON_Value := Item.Get ("state");
            The_Switch   : constant Switch := Switch'val(Integer'(Device_State.Get));
          begin
            Log.Write ("Device_Name  : " & Device_Name.Get);
            Log.Write ("Device_State : " & The_Switch'image);
            The_Switches(The_Port) := The_Switch;
          end;
          exit when The_Port = Port'last;
          The_Port := Port'succ(The_Port);
        end loop;
        return The_Switches;
      end;
    end;
  exception
  when Ada.IO_Exceptions.Device_Error =>
    Log.Warning ("device not available");
    raise Not_Available;
  end Client_Get;


  function Switches_Of (Host : Network.Ip_Address) return Switches is
  begin
    Log.Write ("Switches_Of");
    return Client_Get (Host);
  end Switches_Of;


  procedure Set (The_Port   : Port;
                 The_Switch : Switch;
                 Host       : Network.Ip_Address) is
    Port_Number  : constant Natural := Port'pos(The_Port) + 1;
    Switch_Value : constant Natural := Switch'pos(The_Switch);
    The_Switches : Switches;
  begin
    Log.Write ("Set " & The_Port'image & " to " & The_Switch'image);
    The_Switches := Client_Get (Host,
                                P => Text.Trimmed(Port_Number'image),
                                S => Text.Trimmed(Switch_Value'image));
    if The_Switches(The_Port) /= The_Switch then
      raise Not_Set;
    end if;
  exception
  when others =>
    raise Not_Set;
  end Set;

end ENC_2302_Client;
