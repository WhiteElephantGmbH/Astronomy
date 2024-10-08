-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Network;

package ENC_2302_Client is

  subtype Ip_Address is String; -- local 127.0.0.1

  type Port is (Port_1, Port_2, Port_3, Port_4);

  type Switch is (Off, On);

  type Switches is array (Port) of Switch;

  All_Off : constant Switches := [others => Off];

  function Switches_Of (Host : Network.Ip_Address) return Switches;

  Not_Available : exception;

  procedure Set (The_Port   : Port;
                 The_Switch : Switch;
                 Host       : Network.Ip_Address);

  Not_Set : exception;

end ENC_2302_Client;
