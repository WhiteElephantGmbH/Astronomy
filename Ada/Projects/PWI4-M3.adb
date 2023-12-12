-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with PWI4.Protocol;

with Traces;

package body PWI4.M3 is

  package Log is new Traces ("PWI.M3");


  function Exists return Boolean is
  begin
    return Protocol.M3.Info.Exists;
  end Exists;


  procedure Execute (Command_Name : String;
                     Parameters   : String := "") is
  begin
    Execute (Device     => "m3",
             Command    => Command_Name,
             Parameters => Parameters);
  end Execute;


  procedure Turn (To : Port) is
  begin
    Execute (Command_Name => "goto",
             Parameters   => "port=" & (case To is when Port_1 => "1", when Port_2 => "2"));
  end Turn;


  function Actual_Port return M3_Port is
    Data : constant Protocol.M3_Info := Protocol.M3.Info;
  begin
    if Data.Exists then
      case Data.Port is
      when 0 =>
        return Between;
      when 1 =>
        return Port_1;
      when 2 =>
        return Port_2;
      when others =>
        Log.Warning ("Unknown Port: " & Protocol.Image_Of (Data.Port));
      end case;
    end if;
    return Unknown;
  end Actual_Port;

end PWI4.M3;
