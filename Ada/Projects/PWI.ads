-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Network.Tcp;

package PWI is

  No_Server : exception;

  type Open_Socket_Handler is access function return Network.Tcp.Socket;

  procedure Install (Handler : Open_Socket_Handler);

  procedure Get_System;

private
  procedure Execute (Device     : String;
                     Command    : String;
                     Parameters : String := "");

end PWI;
