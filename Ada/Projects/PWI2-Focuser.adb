-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with PWI2.XML;
with Strings;

package body PWI2.Focuser is

  procedure Execute (Command_Name : String;
                     The_Port     : Port;
                     Parameters   : String := "") is
  begin
    Execute (Device     => "focuser" & Image_Of (The_Port),
             Command    => Command_Name,
             Parameters => Parameters);
  end Execute;


  procedure Connect (To : Port) is
  begin
    Execute ("connect", To);
  end Connect;


  procedure Disconnect (From : Port) is
  begin
    Execute ("disconnect", From);
  end Disconnect;


  procedure Move (On          : Port;
                  To_Position : Microns) is
  begin
    Execute ("move", On, Parameters => "position=" & Strings.Trimmed (To_Position'img));
  end Move;


  function Position (On : Port) return Microns is
  begin
    case On is
    when Port_1 =>
      return XML.Focuser.Info1.Position;
    when Port_2 =>
      return XML.Focuser.Info2.Position;
    end case;
  end Position;

end PWI2.Focuser;
