-- *********************************************************************************************************************
-- *                       (c) 2024 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

pragma Build (Description => "Test for the package Moon",
              Version     => (1, 0, 0, 1),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNAT\14.2");
with Test;

procedure Moon_Test is
begin
  Test.Execute;
end Moon_Test;
