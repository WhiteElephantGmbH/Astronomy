-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

pragma Build (Description => "GM HPS Simulator",
              Version     => (3, 1, 10, 5),
              Kind        => Console,
              Compiler    => "GNAT\14.2");
with Simulator;

procedure GM_HPS is
begin
  Simulator.Execute;
end GM_HPS;
