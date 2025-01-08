-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "Get newest picture from a directory",
              Version     => (1, 0, 0, 0),
              Kind        => Windows,
              Compiler    => "GNATPRO\23.0");
with Picture;

procedure Get_Picture is
begin
  Picture.Get;
end Get_Picture;
