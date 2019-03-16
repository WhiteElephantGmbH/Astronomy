-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with PWI.XML;
with Strings;

package body PWI.M3 is

  procedure Execute (Command_Name : String;
                     Parameters   : String := "") is
  begin
    Execute (Device     => "m3",
             Command    => Command_Name,
             Parameters => Parameters);
  end Execute;


  procedure Turn (To : Port) is
  begin
    Execute (Command_Name => "select",
             Parameters   => "port=" & Strings.Trimmed (Port'pos(To)'img));
  end Turn;


  function Actual_Position return Position is
  begin
    return Position'val(PWI.XML.M3.Port);
  exception
  when others =>
    return Between;
  end Actual_Position;

end PWI.M3;
