-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Os.Process;
with PWI.XML;
with Traces;

package body PWI is

  package Log is new Traces ("Pwi");

  The_Process_Id : Os.Process.Id;

  function Startup (Filename : String) return Boolean is
  begin
    Os.Process.Create (Filename, The_Process_Id);
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


  New_Socket : Open_Socket_Handler;

  procedure Install (Handler : Open_Socket_Handler) is
  begin
    New_Socket := Handler;
  end Install;


  procedure Execute (Item : String) is

    Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;
    Server_Address  : constant String := "127.0.0.1";
    Server_Port     : constant Network.Port_Number := 8080;

    The_Socket : Network.Tcp.Socket;

    procedure Open_Socket is
    begin
      if New_Socket = null then
        The_Socket := Network.Tcp.Socket_For (Server_Address, Server_Port, Socket_Protocol);
      else
        The_Socket := New_Socket.all;
      end if;
    exception
    when others =>
      raise No_Server;
    end Open_Socket;

    procedure Close_Socket is
    begin
      Network.Tcp.Close (The_Socket);
    exception
    when others =>
      null;
    end Close_Socket;

    function String_From_Socket return String is
    begin
      return Network.Tcp.Raw_String_From (The_Socket, Terminator => Ascii.Lf);
    exception
    when others =>
      Close_Socket;
      raise No_Server;
    end String_From_Socket;

    procedure Send_Item is
    begin
      Network.Tcp.Send ("GET /?" & Item & Ascii.Cr & Ascii.Lf & Ascii.Cr & Ascii.Lf, The_Socket);
    exception
    when others =>
      Close_Socket;
      raise No_Server;
    end Send_Item;

  begin -- Execute
    Log.Write ("Execute " & Item);
    Open_Socket;
    Send_Item;
    for Unused_Count in 1 .. 2 loop
      declare
        Unused : constant String := String_From_Socket;
      begin
        null;
      end;
    end loop;
    XML.Parse (String_From_Socket);
    Close_Socket;
  exception
  when No_Server =>
    Log.Error ("no server");
    raise;
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
