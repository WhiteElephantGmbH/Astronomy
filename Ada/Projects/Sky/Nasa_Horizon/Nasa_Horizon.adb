-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "Get ephimerides from NASA horizon",
              Version     => (1, 1, 0, 0),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWS64"),
              Compiler    => "GNATPRO\23.0");
with Request;

procedure Nasa_Horizon is
begin
  Request.Work;
end Nasa_Horizon;
