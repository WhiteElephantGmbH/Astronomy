-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "GM HPS Simulator",
              Version     => (3, 1, 10, 1),
              Kind        => Console,
              Compiler    => "GNATPRO\23.0");
with Test;

procedure GM_HPS is
begin
  Test.Execute;
end GM_HPS;
