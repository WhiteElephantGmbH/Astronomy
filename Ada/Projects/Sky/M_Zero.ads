-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package M_Zero is

  No_Connection : exception;

  type Slewing_Complete_Handling is access procedure (Is_Ok : Boolean);

  procedure Connect (Slewing_Complete : Slewing_Complete_Handling;
                     Server_Address   : String);

  function Reply_For (Command : String) return String;

  procedure Disconnect;

end M_Zero;
