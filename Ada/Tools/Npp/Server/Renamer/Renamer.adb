-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

pragma Build (Description => "Ada Renamer",
              Version     => (2, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNAT\14.2");

with Work;

procedure Renamer is
begin
  Work.Start;
end Renamer;

