-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Network.Tcp;
with PWI.XML;
with Traces;

package body PWI is

  package Log is new Traces ("Pwi");


  procedure Execute (Item : String) is

    Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;
    Server_Address  : constant String := "127.0.0.1";
    Server_Port     : constant Network.Port_Number := 8080;

    The_Socket : Network.Tcp.Socket;

  begin
    The_Socket := Network.Tcp.Socket_For (Server_Address, Server_Port, Socket_Protocol);
    Log.Write ("Execute " & Item);
    begin
      Network.Tcp.Send ("GET /?" & Item & Ascii.Cr & Ascii.Lf & Ascii.Cr & Ascii.Lf, The_Socket);
      for Unused_Count in 1 .. 2 loop
        declare
          Unused : constant String := Network.Tcp.Raw_String_From (The_Socket, Terminator => Ascii.Lf);
        begin
          null;
        end;
      end loop;
      XML.Parse (Network.Tcp.Raw_String_From (The_Socket, Terminator => Ascii.Lf));
      Network.Tcp.Close (The_Socket);
    exception
    when Occurrence: others =>
      Network.Tcp.Close (The_Socket);
      Log.Termination (Occurrence);
      raise Command_Failed;
    end;
  exception
  when Command_Failed =>
    raise;
  when others =>
    Log.Error ("no server");
    raise No_Server;
  end Execute;


  procedure Get_System is
  begin
    Execute ("cmd=getsystem");
  end Get_System;


  procedure Execute (Device     : String;
                     Command    : String;
                     Parameters : String := "") is
  begin
    Execute ("device=" & Device & "&cmd=" & Command & (if Parameters /= "" then "&" & Parameters else ""));
  end Execute;

end PWI;
