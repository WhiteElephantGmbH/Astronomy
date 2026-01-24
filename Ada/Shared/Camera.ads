-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Exceptions;
with Exposure;
with Sensitivity;
private with Traces;
private with Text;

package Camera is

  Max_With_Or_Height : constant := 9600; -- maximum for Canon QHY600C
  Min_With_Or_Height : constant := 3464; -- minimum for Canon 60D

  type Model is (Unknown, Canon_Eos_6D, Canon_Eos_60D, QHY600C);

  subtype Canon_Model is Model range Canon_Eos_6D .. Canon_Eos_60D;

  type Status is (Idle, Connecting, Connected, Capturing, Captured, Downloading, Cropping, Cropped, Stopping, Failed);

  type Columns is range 1 .. Max_With_Or_Height;
  type Rows    is range 1 .. Max_With_Or_Height;

  type Information is record
    State  : Status;
    Camera : Model;
    Height : Rows;
    Width  : Columns;
  end record;

  Pixel_Bits : constant := 16;

  type Pixel is new Natural range 0 .. 2 ** Pixel_Bits - 1 with Size => Pixel_Bits;

  type Raw_Grid is array (Rows range <>, Columns range <>) of Pixel;

  type Square_Size is new Natural range 2 .. Min_With_Or_Height with Dynamic_Predicate => Square_Size mod 2 = 0;

  procedure Start;

  function Actual_Information return Information;

  function Model_Image return String;

  procedure Capture (Filename  : String;
                     Time      : Exposure.Item;
                     Parameter : Sensitivity.Item);

  procedure Capture (Filename : String);

  procedure Capture (Size      : Square_Size;
                     Time      : Exposure.Item;
                     Parameter : Sensitivity.Item);

  procedure Capture (Size : Square_Size);

  function Captured return Raw_Grid;

  procedure Stop;

  function Has_Error return Boolean;

  function Error_Message return String;

  procedure Finish;

private

  Camera_Id : constant String := "Camera";

  package Log is new Traces (Camera_Id);

  The_Exposure_Parameter    : Exposure.Item;
  The_Sensitivity_Parameter : Sensitivity.Item;

  Camera_Error : exception;

  procedure Raise_Error (Message : String) with No_Return;

  protected Camera_Data is

    procedure Set (State : Status);

    procedure Set (Item : Model);

    procedure Set (Height : Rows);

    procedure Set (Width : Columns);

    function Actual return Information;

    procedure Check (Item : Status);

    procedure Set_Error (Message : String);

    procedure Set_Fatal (Item : Exceptions.Occurrence);

    function Last_Error return String;

    procedure Reset_Error;

  private
    The_Information : Information;
    The_Last_Error  : Text.String;
  end Camera_Data;

end Camera;
