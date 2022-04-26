-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "10micron test",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\22.1");
with Test;

procedure Ten_Micron is
begin
  Test.Execute;
end Ten_Micron;
