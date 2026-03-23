-- *********************************************************************************************************************
-- *                           (c) 2015 .. 2026 by Soudronic AG, Bergdietikon, Switzerland                             *
-- *                      Developed by White Elephant GmbH, Switzerland (www.white-elephant.ch)                        *
-- *********************************************************************************************************************
pragma Style_Astronomy;

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