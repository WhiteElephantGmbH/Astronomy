-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "Test for the package Strings",
              Version     => (1, 0, 0, 1),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");
with Test;

procedure Strings_Test is
begin
  Test.Execute;
end Strings_Test;
