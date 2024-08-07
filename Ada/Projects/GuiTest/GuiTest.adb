-- *********************************************************************************************************************
-- *                           (c) 2015 .. 2022 by Soudronic AG, Bergdietikon, Switzerland                             *
-- *                      Developed by White Elephant GmbH, Switzerland (www.white-elephant.ch)                        *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "GUI test",
              Version     => (1, 1, 0, 3),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");
with Test;

procedure GuiTest is
begin
  Test.Work;
end GuiTest;