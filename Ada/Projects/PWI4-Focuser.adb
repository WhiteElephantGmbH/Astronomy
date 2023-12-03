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


  procedure Enable is
  begin
    Execute ("enable");
  end Enable;


  procedure Go_To (Position : Microns) is
  begin
    Execute (Command_Name => "goto",
             Parameters   => "target=" & Protocol.Image_Of (Position));
  end Go_To;


  procedure Disable is
  begin
    Execute ("disable");
  end Disable;

end PWI4.Focuser;
