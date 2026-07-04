-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

pragma Build (Description => "Get ephimerides from NASA horizon",
              Version     => (1, 1, 0, 5),
              Kind        => Console,
              Libraries   => ("AWS", "COLL"),
              Compiler    => "GNAT\14.2");
with Request;

procedure Nasa_Horizon is
begin
  Request.Work;
end Nasa_Horizon;
