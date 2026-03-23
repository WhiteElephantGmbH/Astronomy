-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

pragma Build (Description => "Generate JSON file for Stellariums Nebulae",
              Version     => (1, 0, 0, 1),
              Kind        => Console,
              Compiler    => "GNATPRO\23.0");
with Nebulae;

procedure Nebulae_Solver is
begin
  Nebulae.Solve;
end Nebulae_Solver;
