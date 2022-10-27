-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with AWS.Client;
with AWS.Response;
with GNATCOLL.JSON;
with Strings;
with Traces;

package body ENC_2302_Client is

  package JS renames GNATCOLL.JSON;

  package Log is new Traces ("ENC_2302_Client");

  function Client_Get (Ip : String;
                       P  : String := "";
                       S  : String := "") return Switches is

    Url : constant String := (if P = "" then "http://" & Ip & "/statusjsn.js?components=513"
                                        else "http://" & Ip & "/statusjsn.js?components=513&cmd=1&p=" & P & "&s=" & S);

    Result  : constant String := AWS.Response.Message_Body (AWS.Client.Get (Url));
    Data    : constant JS.JSON_Value := JS.Read (Result);
    Outputs : constant JS.JSON_Value := Data.Get ("outputs");

  begin -- Client
    Log.Write ("URL<<<" & Url & ">>>");
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
  end Client_Get;


  function Switches_Of (Host : Ip_Address) return Switches is
  begin
    Log.Write ("Switches_Of " & Host);
    return Client_Get (Host);
  end Switches_Of;


  procedure Set (The_Port   : Port;
                 The_Switch : Switch;
                 Host       : Ip_Address) is
    Port_Number  : constant Natural := Port'pos(The_Port) + 1;
    Switch_Value : constant Natural := Switch'pos(The_Switch);
    The_Switches : Switches;
  begin
    Log.Write ("Set " & The_Port'image & " to " & The_Switch'image & "(IP address: " & Host & ")");
    The_Switches := Client_Get (Ip => Host,
                                P  => Strings.Trimmed(Port_Number'image),
                                S  => Strings.Trimmed(Switch_Value'image));
    if The_Switches(The_Port) /= The_Switch then
      raise Not_Set;
    end if;
  exception
  when others =>
    raise Not_Set;
  end Set;

end ENC_2302_Client;
