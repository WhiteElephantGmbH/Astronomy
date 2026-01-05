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
with Text;

package Focus is

  subtype Distance is Natural range 0 .. 2**24 - 1;

  type Lash is new Distance range 0 .. 2**8 - 1;

  type Status is (Undefined, Positioning, Evaluating, Positioned, Error);

  type Focuser_Model is (Unknown, Celestron);

  type Information is record
    State    : Status;
    Focuser  : Focuser_Model;
    Position : Distance;
    Backlash : Lash;
  end record;

  procedure Start;

  function Actual_Information return Information;

  function Focuser_Image return String;

  procedure Evaluate;

  procedure Stop;

  function Error_Message return String;

  procedure Finish;

private

  Focus_Error : exception;

  procedure Raise_Error (Message : String) with No_Return;

  protected Focus_Data is

    procedure Set (State : Status);

    procedure Set (Item : Focuser_Model);

    procedure Set (Start_Position : Distance);

    procedure Set (Backlash : Lash);

    function Actual return Information;

    procedure Check (Item : Status);

    procedure Set_Error (Message : String);

    procedure Set_Fatal (Item : Exceptions.Occurrence);

    function Last_Error return String;

    procedure Reset_Error;

  private
    The_Information : Information;
    The_Last_Error  : Text.String;
  end Focus_Data;

end Focus;
