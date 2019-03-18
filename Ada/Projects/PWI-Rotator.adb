-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with PWI.XML;

package body PWI.Rotator is

  function Status_Of (Info  : XML.Rotator_Info;
                      Info1 : XML.Rotator_Info) return State is
  begin
    if not Info1.Connected then
      return Disconnected;
    elsif Info.Alt_Az_Derotate then
      return Started;
    elsif Info1.Finding_Home or Info1.Goto_Complete then
      return Homing;
    else
      return Connected;
    end if;
  end Status_Of;


  function Status return State is
  begin
    return Status_Of (XML.Rotator.Info,
                      XML.Rotator.Info1);
  end Status;


  procedure Find_Home is
  begin
    Execute (Device  => "rotator1",
             Command => "findhome");
  end Find_Home;


  procedure Start is
  begin
    Execute (Device  => "rotator",
             Command => "derotatestart");
  end Start;


  procedure Stop is
  begin
    Execute (Device  => "rotator",
             Command => "stop");
  end Stop;

end PWI.Rotator;
