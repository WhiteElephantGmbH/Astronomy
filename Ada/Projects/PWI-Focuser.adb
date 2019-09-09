-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package body PWI.Focuser is

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
    Execute ("move", On, Parameters => "position=" & To_Position'img);
  end Move;

end PWI.Focuser;
