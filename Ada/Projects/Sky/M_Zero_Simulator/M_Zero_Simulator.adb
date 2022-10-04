-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "M-Zero Simulator",
              Version     => (1, 3, 0, 2),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\22.2");
with Test;

procedure M_Zero_Simulator is
begin
  Test.Execute;
end M_Zero_Simulator;
