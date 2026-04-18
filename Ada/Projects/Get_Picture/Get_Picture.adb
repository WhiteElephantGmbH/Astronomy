-- *********************************************************************************************************************
-- *                       (c) 2025 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

pragma Build (Description => "Get newest picture from a directory",
              Version     => (1, 3, 0, 0),
              Kind        => Windows,
              Compiler    => "GNATPRO\23.0");
with Picture;

procedure Get_Picture is
begin
  Picture.Get;
end Get_Picture;
