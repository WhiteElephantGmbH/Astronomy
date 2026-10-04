-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

package Random is

  procedure Initialize (The_Seed : Natural);

  function Value (The_Maximum : Natural) return Natural;

end Random;
