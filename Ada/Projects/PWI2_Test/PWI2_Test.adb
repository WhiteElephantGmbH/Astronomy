-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "PWI test program",
              Version     => (2, 3, 2, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");
with Test;

procedure PWI2_Test is
begin
  Test.Work;
end PWI2_Test;
