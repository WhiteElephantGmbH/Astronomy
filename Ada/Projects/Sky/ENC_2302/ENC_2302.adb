-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "ENC 2302 Simulator",
              Version     => (1, 0, 0, 1),
              Kind        => Console,
              Libraries   => ("AWS64", "COLL64"),
              Compiler    => "GNATPRO\23.0");
with Simulator;

procedure ENC_2302 is
begin
  Simulator.Execute;
end ENC_2302;
