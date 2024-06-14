-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "GM HPS Simulator",
              Version     => (3, 1, 10, 3),
              Kind        => Console,
              Compiler    => "GNATPRO\23.0");
with Simulator;

procedure GM_HPS is
begin
  Simulator.Execute;
end GM_HPS;
