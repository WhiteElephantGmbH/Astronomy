-- *********************************************************************************************************************
-- *                   (c) 2008 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Error;
with Ada_95.Library;
with Ada_95.Token;
with Server;
with Text;

package Ada_95.Project is

  type Error_Kind is new Error.Kind;

  subtype Token_Kind is Standard.Server.Token_Kind;

  procedure Initialize (Work_Path : Text.List);

  procedure Finalize;

  function Token_Handle_Of (Filename : String;
                            Content  : String := "") return Token.Handle;

  function Token_Handle_Of (Filename : String;
                            Line     : Token.Line_Number;
                            Column   : Token.Column_Position;
                            Content  : String) return Token.Handle;

  function Full_Name_Of (Item : Token.Handle) return String;

  function Local_Unit return Library.Handle;

  function Unit_Of (Filename : String;
                    Content  : String) return Library.Handle;

  function Reference_Of (Filename : String;
                         Line     : Token.Line_Number;
                         Column   : Token.Column_Position;
                         Content  : String := "") return Token.Handle;


  function Token_Kind_Of (Item : Token.Handle) return Token_Kind;


  function First_Error_Of (Filename : String;
                           Content  : String := "") return Token.Handle;

  function Next_Error return Token.Handle;

  function Error_Kind_Of (Item : Token.Handle) return Error_Kind;

end Ada_95.Project;
