-- *********************************************************************************************************************
-- *                      (c) 2022 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                         *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

pragma Build (Description => "Telnet Server",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWS64"),
              Compiler    => "GNATPRO\23.0");
with Work;

procedure Http_Client is
begin
  Work.Execute;
end Http_Client;
