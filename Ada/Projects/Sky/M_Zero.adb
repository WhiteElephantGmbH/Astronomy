-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Network.Tcp;
with Traces;

package body M_Zero is

  package Log is new Traces ("M_Zero");

  Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;

--Server_Address  : constant String := "10.0.0.1";
  Server_Port     : constant Network.Port_Number := 4030;
  Receive_Timeout : constant Duration := 3.0;

  Command_Start : constant Character := ':';
  Terminator    : constant Character := '#';

  The_Socket : Network.Tcp.Socket;

  Slewing_Complete_Handler : Slewing_Complete_Handling;


  procedure Connect (Slewing_Complete : Slewing_Complete_Handling;
                     Server_Address   : String) is
  begin
    Slewing_Complete_Handler := Slewing_Complete;
    The_Socket := Network.Tcp.Socket_For (The_Address     => Network.Ip_Address_Of (Server_Address),
                                          The_Port        => Server_Port,
                                          The_Protocol    => Socket_Protocol,
                                          Receive_Timeout => Receive_Timeout);
  exception
  when Item: others =>
    Log.Error ("Connect: " & Network.Net.Resolve_Exception (Item)'img);
    raise No_Connection;
  end Connect;


  Slewing_Is_Started  : Boolean := False;


  function Received_String return String is
    Reply_Is_Ready : Boolean := False;
  begin
    loop
      declare
        Reply : constant String := Network.Tcp.Raw_String_From (The_Socket, Terminator => Terminator);
      begin
        Log.Write ("Reply " & Reply);
        if Slewing_Is_Started then
           if Reply = "ge" & Terminator then
             Slewing_Complete_Handler (Is_Ok => True);
             Slewing_Is_Started := False;
           else
             Reply_Is_Ready := True;
           end if;
        else
          Reply_Is_Ready := True;
        end if;
        if Reply_Is_Ready then
          return Reply (Reply'first .. Reply'last - 1);
        end if;
      end;
    end loop;
  end Received_String;


  function Reply_For (Command : String) return String is
  begin
    Log.Write ("Command " & Command_Start & Command & Terminator);
    begin
      Network.Tcp.Send (The_String  => Command_Start & Command & Terminator,
                        Used_Socket => The_Socket);
    exception
    when Item: others =>
      Log.Error ("Reply_For: " & Network.Net.Resolve_Exception (Item)'img);
      raise No_Connection;
    end;
    if Command = "" then
      raise Program_Error;
    end if;
    case Command(Command'first) is
    when 'A' | 'M' | 'Q' | 'R' =>
      if Command in "MS" then
        declare
          Reply : constant String := Received_String;
        begin
          if Reply = "0" then
            Slewing_Is_Started := True;
            return "0";
          else
            Log.Warning ("Slewing: " & Reply);
            Slewing_Complete_Handler (Is_Ok => False);
          end if;
        end;
      end if;
      return "";
    when 'S' =>
      case Command(Command'first + 1) is
      when 'g' | 't' =>
        loop
          if Slewing_Is_Started then
            raise Program_Error;
          end if;
          declare
            Reply : constant String := Received_String;
          begin
            if Reply /= "" then
              return Reply;
            end if;
          end;
        end loop;
      when others =>
        declare
          Reply : constant Character := Network.Tcp.Raw_Character_From (The_Socket);
        begin
          Log.Write ("Reply " & Reply);
          return "" & Reply;
        end;
      end case;
    when others =>
      return Received_String;
    end case;
  end Reply_For;


  procedure Disconnect is
  begin
    Network.Tcp.Close (The_Socket);
  end Disconnect;

end M_Zero;
