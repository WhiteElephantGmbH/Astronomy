-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

pragma Build (Description => "PWI test program",
              Version     => (2, 3, 2, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNAT\14.2");
with Test;

procedure PWI2_Test is
begin
  Test.Work;
end PWI2_Test;
