-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

pragma Build (Description => "PWI4 test program",
              Version     => (4, 0, 0, 3),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWS"),
              Compiler    => "GNAT\14.2");
with Test;

procedure PWI4_Test is
begin
  Test.Work;
end PWI4_Test;
