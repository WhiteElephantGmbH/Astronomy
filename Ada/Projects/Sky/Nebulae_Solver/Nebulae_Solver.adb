-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "Generate JSON file for Stellariums Nebulae",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");
with Nebulae;

procedure Nebulae_Solver is
begin
  Nebulae.Solve;
end Nebulae_Solver;
