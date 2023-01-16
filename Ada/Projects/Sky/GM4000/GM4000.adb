-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "GM4000 Simulator",
              Version     => (2, 15, 1, 8),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");
with Test;

procedure GM4000 is
begin
  Test.Execute;
end GM4000;
