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

with Exceptions;
with Focuser;
with Text;

package Focus is

  subtype Distance is Focuser.Distance;

  type Lash_Correction is range -2**7 .. 2**7 - 1;

  type Status is (Stopped, Starting, Positioning, Positioned, Evaluating, Evaluated, Error);

  procedure Start (Device : Focuser.Object_Access);
  
  function Actual_State return Status;

  function Focuser_Image return String;

  procedure Evaluate;

  procedure Stop;

  function Error_Message return String;

  procedure Finish;

private

  Focus_Error : exception;

  procedure Error (Message : String);

  procedure Raise_Error (Message : String) with No_Return;

  protected Focus_Data is

    procedure Set (Item : Status);

    procedure Set (First_Position : Distance;
                   With_Increment : Distance);

    function State return Status;

    function Start_Position return Distance;

    function Increment return Distance;

    procedure Check (Item : Status);

    procedure Set_Error (Message : String);

    procedure Set_Fatal (Item : Exceptions.Occurrence);

    function Last_Error return String;

    procedure Reset_Error;
    
  private
    The_State          : Status := Stopped;
    The_Start_Position : Distance := 12000;
    The_Increment      : Distance := 100;
    The_Last_Error     : Text.String;
  end Focus_Data;

end Focus;
