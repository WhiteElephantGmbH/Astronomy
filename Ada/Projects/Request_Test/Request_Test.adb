-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "Request test program",
              Version     => (1, 0, 0, 1),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWS"),
              Compiler    => "GNAT\2021");
with Request;

procedure Request_Test is
begin
  Request.Send ("M42");
end Request_Test;
