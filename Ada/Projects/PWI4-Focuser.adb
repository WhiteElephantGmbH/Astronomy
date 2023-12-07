-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with PWI4.Protocol;

package body PWI4.Focuser is

  function Exists return Boolean is
  begin
    return Protocol.Focuser.Info.Exists;
  end Exists;


  procedure Execute (Command_Name : String;
                     Parameters   : String := "") is
  begin
    Execute (Device     => "focuser",
             Command    => Command_Name,
             Parameters => Parameters);
  end Execute;


  procedure Connect is
  begin
    Execute ("connect");
  end Connect;


  procedure Disconnect is
  begin
    Execute ("disconnect");
  end Disconnect;


  procedure Enable is
  begin
    Execute ("enable");
  end Enable;


  procedure Disable is
  begin
    Execute ("disable");
  end Disable;


  procedure Go_To (Position : Microns) is
  begin
    Execute (Command_Name => "goto",
             Parameters   => "target=" & Protocol.Image_Of (Position));
  end Go_To;


  procedure Stop is
  begin
    Execute ("stop");
  end Stop;

end PWI4.Focuser;
