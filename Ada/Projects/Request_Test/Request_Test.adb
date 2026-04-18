-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

pragma Build (Description => "Request test program",
              Version     => (1, 0, 0, 6),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWS64"),
              Compiler    => "GNATPRO\23.0");
with Request;

procedure Request_Test is
begin
  Request.Work;
end Request_Test;
