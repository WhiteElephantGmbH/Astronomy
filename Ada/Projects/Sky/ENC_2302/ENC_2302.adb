-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

pragma Build (Description => "ENC 2302 Simulator",
              Version     => (1, 0, 0, 3),
              Kind        => Console,
              Libraries   => ("AWS", "GNATCOLL"),
              Compiler    => "GNAT\14.2");
with Simulator;

procedure ENC_2302 is
begin
  Simulator.Execute;
end ENC_2302;
