-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "PWI test program",
              Version     => (2, 0, 0, 18),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");
with Test;

procedure PWI_Test is
begin
  Test.Work;
end PWI_Test;
