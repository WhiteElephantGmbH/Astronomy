-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
pragma Style_Astronomy;

with Siril_Interface;
with Space;
with Units;

package Siril is

  package SI renames Siril_Interface;

  subtype Percent is SI.Percent;

  Start    : constant Percent := Percent'first;
  Complete : constant Percent := Percent'last;

  subtype Progress_Handler is SI.Progress_Handler;

  procedure Open (Progress : Progress_Handler := null);
  -- Starts Siril and establishes the command interface.

  procedure Change_Directory (Name : String);
  -- Changes Siril's current working directory.
  -- raises Name_Error if the source directory does not exist

  procedure Convert_Light (Destination : String);
  -- Converts the light images to FITS files.
  -- raises Name_Error if the destination directory does not exist

  procedure Calibrate_Light (Dark : String;
                             Flat : String);
  -- Calibrates the light images using the specified dark and flat masters.
  -- raises Name_Error Dark or Flat files do not exist

  procedure Register_Light;
  -- Registers the calibrated light images.

  procedure Stack_Light;
  -- Stacks the registered light images.

  procedure Load_Stacked_Light;
  -- Loads the stacked light image for further processing.

  procedure Plate_Solve (Direction    : Space.Direction;
                         Focal_Length : Units.Focal_Length;
                         Pixel_Size   : Units.Pixel_Size);
  -- Determines the image position and scale by plate solving the loaded image.

  procedure Remove_Background;
  -- Removes the background gradient from the image.

  procedure Calibrate_Color;
  -- Performs spectrophotometric color calibration.

  procedure Transfer (Tones : Units.Tones);
  -- Applies the specified tone transfer to the image.

  procedure Set (Saturation : Units.Saturation);
  -- Applies the specified saturation settings to the image.

  procedure Save_Jpeg (File_Name : String);
  -- Saves the processed image as a JPEG file.

  procedure Close;
  -- Closes Siril and terminates the Siril session.

  Name_Error : exception;

end Siril;
