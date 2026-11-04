-- *********************************************************************************************************************
-- *                           (c) 2010 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant

with Ada_95.Token;

package Ada_95.Format is

  procedure Initialize (Item : Token.Handle);

  procedure Next;

  function Actual_Image return String;

  function Actual_Kind return Format_Kind;

  function Actual_Data_Kind return Data_Kind;

  function Actual_Position return Natural;

end Ada_95.Format;
