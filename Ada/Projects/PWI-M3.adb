-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with PWI.XML;
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
             Parameters   => "port=" & (case To is when Port_1 => "1", when Port_2 => "2"));
  end Turn;


  function Actual_Port return M3_Port is
    Data : constant XML.M3_Info := XML.M3.Info;
  begin
    if Data.Connected then
      case Data.Port is
      when 0 =>
        return Between;
      when 1 =>
        return Port_1;
      when 2 =>
        return Port_2;
      when others =>
        null;
      end case;
    end if;
    return Unknown;
  end Actual_Port;

end PWI.M3;
