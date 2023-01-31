-- *********************************************************************************************************************
-- *                       (c) 2010 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
-->Style: White_Elephant

with Coordinate;
with Data;
with Eps;

generic
  with procedure Error (Message : String);
package Map is

  Aborted : exception;

  package Star is new Data (Error);

  type Star_Brightness is record
    Limit  : Star.Magnitude;
    Filter : Star.Magnitude;
    Min    : Eps.Value;
    Max    : Eps.Value;
  end record;

  procedure Draw (Declination              : Coordinate.Declination;
                  Margin                   : Eps.Value;
                  Map_Size                 : Eps.Value;
                  Constellation_Line_Width : Eps.Value;
                  Ecliptic_Line_Width      : Eps.Value;
                  Equator_Line_Width       : Eps.Value;
                  Visibility_Line_Width    : Eps.Value;
                  Brightness               : Star_Brightness);
end Map;