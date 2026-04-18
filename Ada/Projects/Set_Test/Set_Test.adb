-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

pragma Build (Description => "Test for the generic package Discrete_Sets",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");
with Test;

procedure Set_Test is
begin
  Test.Execute;
end Set_Test;
