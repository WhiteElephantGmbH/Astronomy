-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with PWI2.XML;

package body PWI2.Fans is

  procedure Execute (Command_Name : String) is
  begin
    Execute (Device  => "fans",
             Command => Command_Name);
  end Execute;


  procedure Turn_On is
  begin
    Execute ("turnon");
  end Turn_On;


  procedure Turn_Off is
  begin
    Execute ("turnoff");
  end Turn_Off;


  function Turned_On return Boolean is
  begin
    return PWI2.XML.Fans.Turned_On;
  end Turned_On;

end PWI2.Fans;
