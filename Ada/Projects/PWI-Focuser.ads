-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package PWI.Focuser is

  procedure Connect (To : Port);

  procedure Disconnect (From : Port);

  procedure Move (On          : Port;
                  To_Position : Microns);

  function Position (On : Port) return Microns;

end PWI.Focuser;
