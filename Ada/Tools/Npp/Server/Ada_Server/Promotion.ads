-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Server;

package Promotion is

  Error : exception;

  procedure Start (Name : String;
                   Kind : Server.Promotion_Kind);

  type Color is (Black, Blue, Green, Orange, Red);

  procedure Define_Next_Message_Color (Item : Color);

  procedure Set_Message (Item : String);

  procedure Set_Error (Item      : String;
                       File      : String := "";
                       At_Line   : Server.Line_Number := Server.Line_Number'first;
                       At_Column : Server.Column_Range := Server.Column_Range'first) with No_Return;

  procedure Complete;

  function Message_Ready return Boolean; -- awaits result

  function Error_Message_Ready return Boolean; -- awaits result

  function Message return String;

  function Filename return String;

  function Line return Server.Line_Number;

  function Column return Server.Column_Range;

end Promotion;
