-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;
pragma Console_Application;

with Request;

procedure Request_Test is
begin
  Request.Send ("M42");
end Request_Test;
