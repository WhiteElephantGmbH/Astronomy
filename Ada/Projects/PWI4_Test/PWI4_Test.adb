-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "PWI4 test program",
              Version     => (4, 0, 0, 3),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWS64"),
              Compiler    => "GNATPRO\23.0");
with Test;

procedure PWI4_Test is
begin
  Test.Work;
end PWI4_Test;
