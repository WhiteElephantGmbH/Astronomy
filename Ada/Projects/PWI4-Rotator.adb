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


  procedure Connect is
  begin
    Execute ("connect");
  end Connect;


  procedure Disconnect is
  begin
    Execute ("disconnect");
  end Disconnect;


  procedure Find_Home is
  begin
    Execute ("find_home");
  end Find_Home;


  procedure Start is
  begin
    Execute ("derotatestart");
  end Start;


  procedure Stop is
  begin
    Execute ("stop");
  end Stop;

end PWI4.Rotator;
