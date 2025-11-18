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
  type Iso_Value is new Natural range 100 .. 25600
    with Static_Predicate => Iso_Value in 100 | 200 | 400 | 800 | 1600 | 3200 | 6400 | 12800 | 25600;

  -- Exposure times for EOS 6D astrophotography (in 1/3 EV steps)
  type Exposure_Time is new Natural range 1 .. 30
    with Static_Predicate => Exposure_Time in 1 | 2 | 3 | 4 | 5 | 6 | 8 | 10 | 13 | 15 | 20 | 25 | 30;

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
  --        - Shutter speed according to Exposure                     --
  --        - ISO according to Iso                                    --
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
  --       Desired shutter speed in seconds,                          --
  --       chosen from the discrete set enforced by the static        --
  --       predicate of Exposure_Time.  A value of 0.0 seconds is     --
  --       interpreted as Bulb.                                       --
  --                                                                  --
  --    Iso:                                                         --
  --       Desired ISO value (including the expanded H = 25600).      --
  --       The static predicate guarantees that only supported        --
  --       values can be passed.                                      --
  --                                                                  --
  --    Timeout:                                                     --
  --       Maximum time spent *waiting for the camera to deliver and  --
  --       download the RAW file*, starting from the moment after the --
  --       exposure has been triggered.                               --
  --                                                                  --
  --       In other words:                                            --
  --         * The physical exposure duration itself is determined    --
  --           solely by Exposure_Time and is not limited by Timeout. --
  --         * Timeout only bounds the time for:                      --
  --             - The directory-item event to arrive, and            --
  --             - The subsequent RAW download to the host.           --
  --                                                                  --
  --       If this period is exceeded, Canon_Error is raised.         --
  ----------------------------------------------------------------------
  procedure Capture (Filename : String;
                     Exposure : Exposure_Time;
                     Iso      : Iso_Value);

  Canon_Error : exception;

end Canon;
