-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with PWI.XML;

package body PWI.Rotator is

  function Status_Of (Info : XML.Rotator_Info) return State is
  begin
    if not Info.Connected then
      return Disconnected;
    elsif Info.Alt_Az_Derotate then
      return Started;
    elsif Info.Finding_Home or Info.Goto_Complete then
      return Homing;
    else
      return Connected;
    end if;
  end Status_Of;


  function Status return State is
  begin
    return Status_Of (XML.Rotator.Info);
  end Status;


  procedure Find_Home (On : Port) is
  begin
    Execute (Device  => "rotator" & Image_Of (On),
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
