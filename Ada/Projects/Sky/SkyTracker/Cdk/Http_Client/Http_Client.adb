-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

pragma Build (Description => "ENC 2302 Simulator",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWS64", "COLL64"),
              Compiler    => "GNATPRO\23.0");
with Client;

procedure Http_Client is
begin
  Client.Execute;
end Http_Client;
