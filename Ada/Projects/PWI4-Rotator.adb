-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with PWI4.Protocol;

package body PWI4.Rotator is

  function Moving return Boolean is
  begin
    return Protocol.Rotator.Info.Is_Moving;
  end Moving;


  function Slewing return Boolean is
  begin
    return Protocol.Rotator.Info.Is_Slewing;
  end Slewing;


  function Field_Angle return Degrees is
  begin
    return Protocol.Rotator.Info.Field_Angle;
  end Field_Angle;


  function Mech_Position return Degrees is
  begin
    return Protocol.Rotator.Info.Mech_Position;
  end Mech_Position;


  procedure Execute (Command_Name : String;
                     Parameters   : String := "") is
  begin
    Execute (Device     => "rotator",
             Command    => Command_Name,
             Parameters => Parameters);
  end Execute;


  procedure Find_Home is
  begin
    Execute ("find_home");
  end Find_Home;


  procedure Goto_Mech (Position : Degrees) is
  begin
    Execute (Command_Name => "goto_mech",
             Parameters   => "degs=" & Image_Of (Position));
  end Goto_Mech;


  procedure Goto_Field (Position : Degrees) is
  begin
    Execute (Command_Name => "goto_field",
             Parameters   => "degs=" & Image_Of (Position));
  end Goto_Field;


  procedure Goto_Offset (Distance : Degrees) is
  begin
    Execute (Command_Name => "offset",
             Parameters   => "degs=" & Image_Of (Distance));
  end Goto_Offset;


  procedure Start is
  begin
    Execute ("derotatestart");
  end Start;


  procedure Stop is
  begin
    Execute ("stop");
  end Stop;

end PWI4.Rotator;
