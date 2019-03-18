-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package PWI.M3 is

  type Position is (Unknown, Between, Port_1, Port_2);

  subtype Port is Position range Port_1 .. Port_2;

  procedure Turn (To : Port);

  function Actual_Position return Position;

end PWI.M3;
