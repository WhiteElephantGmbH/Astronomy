-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

pragma Build (Description => "M-Zero Simulator",
              Version     => (1, 3, 0, 4),
              Kind        => Console,
              Compiler    => "GNAT\14.2");
with Test;

procedure M_Zero_Simulator is
begin
  Test.Execute;
end M_Zero_Simulator;
