-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Network.Tcp;

package PWI is

  No_Server : exception;

  type Arc_Second is delta 0.0000001 range -9999999.9999999 .. 9999999.9999999;

  type Open_Socket_Handler is access function return Network.Tcp.Socket;

  function Startup (Filename : String) return Boolean;

  procedure Shutdown;

  procedure Install (Handler : Open_Socket_Handler);

  procedure Get_System;

private
  procedure Execute (Device     : String;
                     Command    : String;
                     Parameters : String := "");

end PWI;
