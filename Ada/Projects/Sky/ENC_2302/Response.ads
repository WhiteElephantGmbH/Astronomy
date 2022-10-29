-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with ENC_2302_Client;

package Response is

  package ENC renames ENC_2302_Client;

  function Item (With_Switches : ENC.Switches;
                 Components    : String) return String;

end Response;
