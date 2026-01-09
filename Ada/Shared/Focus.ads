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
pragma Style_White_Elephant;

with Focuser;
with Camera;
private with Exceptions;
private with Text;

package Focus is

  subtype Distance is Focuser.Distance;

  type Lash_Correction is range -2**7 .. 2**7 - 1;

  type Diameter is new Natural range 0 .. Camera.Min_With_Or_Height;

  type Status is (No_Focuser, Undefined, Positioning, Capturing, Evaluated, Failed);

  procedure Start (Device : Focuser.Object_Access);

  function Actual_State return Status;

  function Focuser_Image return String;

  procedure Evaluate;

  type Result is record
    Half_Flux : Camera.Pixel := 0;
    HFD       : Diameter := 0;
    Position  : Distance := 0;
  end record;

  function Evaluation_Result return Result;

  procedure Stop;

  function Error_Message return String;

  procedure Finish;

private

  Focus_Error : exception;

  procedure Error (Message : String);

  procedure Raise_Error (Message : String) with No_Return;

  protected Focus_Data is

    procedure Set (Item : Status);

    procedure Set (First_Position  : Distance;
                   First_Increment : Distance;
                   Square_Size     : Camera.Square_Size);

    function State return Status;

    function Start_Position return Distance;

    function Start_Increment return Distance;

    function Grid_Size return Camera.Square_Size;

    procedure Set (Half_Flux : Camera.Pixel);

    procedure Set (Half_Flux_Diameter : Diameter);

    procedure Set (Position : Distance);

    function Evaluation return Result;

    procedure Check (Item : Status);

    procedure Set_Error (Message : String);

    procedure Set_Fatal (Item : Exceptions.Occurrence);

    function Last_Error return String;

    procedure Reset_Error;

  private
    The_State           : Status := No_Focuser;
    The_Start_Position  : Distance := 12000;
    The_Start_Increment : Distance := 100;
    The_Grid_Size       : Camera.Square_Size := 1000;
    The_Result          : Result;
    The_Last_Error      : Text.String;
  end Focus_Data;

end Focus;
