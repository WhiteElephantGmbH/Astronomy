-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with PWI4.Protocol;

package body PWI4.Rotator is

  function Exists return Boolean is
  begin
    return Protocol.Rotator.Info.Exists;
  end Exists;


  procedure Execute (Command_Name : String) is
  begin
    Execute (Device  => "rotator",
             Command => Command_Name);
  end Execute;


  procedure Enable is
  begin
    Execute ("enable");
  end Enable;


  procedure Disable is
  begin
    Execute ("disable");
  end Disable;

end PWI4.Rotator;
