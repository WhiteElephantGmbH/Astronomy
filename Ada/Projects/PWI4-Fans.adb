-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package body PWI4.Fans is

  procedure Execute (Command_Name : String) is
  begin
    Execute (Device  => "fans",
             Command => Command_Name);
  end Execute;


  procedure Turn_On is
  begin
    Execute ("on");
  end Turn_On;


  procedure Turn_Off is
  begin
    Execute ("off");
  end Turn_Off;

end PWI4.Fans;
