-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *                                                                                                                   *
-- *    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General     *
-- *    Public License as published by the Free Software Foundation; either version 2 of the License, or               *
-- *    (at your option) any later version.                                                                            *
-- *                                                                                                                   *
-- *    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the     *
-- *    implied warranty of MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE. See the GNU General Public License    *
-- *    for more details.                                                                                              *
-- *                                                                                                                   *
-- *    You should have received a copy of the GNU General Public License along with this program; if not, write to    *
-- *    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

-------------------------------------------------
-- High-level Canon EOS 6D control (via EDSDK) --
-------------------------------------------------
package Canon is

  -- ISO values supported by your EOS 6D (FW 1.1.9)
  type Iso_Value is new Natural range 100 .. 25600 with
    Static_Predicate => Iso_Value in 100 | 200 | 400 | 800 | 1600 | 3200 | 6400 | 12800 | 25600;

  -- Exposure times for EOS 6D astrophotography
  Exposure_Delta : constant := 0.000_001;
  type Exposure_Time is delta Exposure_Delta range Exposure_Delta .. 4194.0 with Small => Exposure_Delta, Size  => 32,
    Static_Predicate => Exposure_Time in -- Seconds
     0.8        | 0.6        | 0.5        | 0.4        | 0.3        |
     1.0 / 4    | 1.0 / 5    | 1.0 / 6    | 1.0 / 8    |
     1.0 / 10   | 1.0 / 13   | 1.0 / 15   | 1.0 / 20   | 1.0 / 25   | 1.0 / 30   |
     1.0 / 40   | 1.0 / 50   | 1.0 / 60   | 1.0 / 80   |
     1.0 / 100  | 1.0 / 125  | 1.0 / 160  | 1.0 / 200  | 1.0 / 250  | 1.0 / 320  |
     1.0 / 400  | 1.0 / 500  | 1.0 / 640  | 1.0 / 800  |
     1.0 / 1000 | 1.0 / 1250 | 1.0 / 1600 | 1.0 / 2000 | 1.0 / 2500 | 1.0 / 3200 |
     1.0 / 4000 or Exposure_Time >= 1.0;

  ----------------------------------------------------------------------
  --  Capture Picture                                                 --
  --                                                                  --
  --  High-level one-shot API:                                        --
  --                                                                  --
  --    * Initializes EDSDK                                           --
  --    * Finds the first connected Canon EOS (EOS 6D in your setup)  --
  --    * Opens a session                                             --
  --    * Sets:                                                       --
  --        - Image quality to RAW only (no JPEG)                     --
  --        - ISO according to Iso                                    --
  --        - Shutter mode:                                           --
  --            * Exposure < 1.0 s  -> normal Tv + TakePicture        --
  --            * Exposure >= 1.0 s -> Bulb (Tv = Bulb, BulbStart/End)--
  --    * Triggers one exposure                                       --
  --    * Waits for the resulting RAW file directory-item event       --
  --    * Downloads the RAW file from the camera to Filename          --
  --    * Closes the session and terminates EDSDK                     --
  --                                                                  --
  --  Parameters:                                                     --
  --                                                                  --
  --    Filename:                                                    --
  --       Path of the RAW file on the host, e.g. "img_0001.cr2".     --
  --       The implementation will create/overwrite this file.        --
  --                                                                  --
  --    Exposure:                                                    --
  --       Desired shutter time in seconds.                           --
  --       * If Exposure <  1.0: uses standard Tv + TakePicture.      --
  --       * If Exposure >= 1.0: uses BulbStart/BulbEnd and keeps     --
  --         the shutter open for Exposure seconds.                   --
  --                                                                  --
  --    Iso:                                                         --
  --       Desired ISO value (including the expanded H = 25600).      --
  --       The static predicate guarantees that only supported        --
  --       values can be passed.                                      --
  ----------------------------------------------------------------------
  procedure Capture (Filename : String;
                     Exposure : Exposure_Time;
                     Iso      : Iso_Value);

  Canon_Error : exception;

end Canon;
