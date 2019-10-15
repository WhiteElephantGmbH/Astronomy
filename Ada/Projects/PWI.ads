-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Network.Tcp;

package PWI is

  No_Server : exception;

  type Arc_Second is delta 0.000_000_1 range -9_999_999.999_999_9 .. 9_999_999.999_999_9;

  type Encoder_Degrees is delta 0.000_000_000_000_1 range -999.999_999_999_999_9 .. 999.999_999_999_999_9;

  type Encoder_Data is record
    Azm : Encoder_Degrees;
    Alt : Encoder_Degrees;
  end record;

  type M3_Port is (Unknown, Between, Port_1, Port_2);

  subtype Port is M3_Port range Port_1 .. Port_2;

  type Microns is range -999_999 .. 999_999;

  type Open_Socket_Handler is access function return Network.Tcp.Socket;

  function Startup (Filename : String) return Boolean;

  procedure Shutdown;

  procedure Install (Handler : Open_Socket_Handler);

  procedure Get_System;

private
  procedure Execute (Device     : String;
                     Command    : String;
                     Parameters : String := "");

  function Image_Of (The_Port : Port) return Character;

end PWI;
