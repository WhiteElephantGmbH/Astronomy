-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Angle;
with Device;
with Network.Udp;
with Serial_Io;

package Parameter is

  Acceleration_Unit : constant String := "/s²";
  Speed_Unit        : constant String := "/s";

  Datagram_Timeout : constant Duration := 0.1;

  type Connection_Kind is (Is_Serial, Is_Simulated, Is_Udp);

  type Connection (Kind : Connection_Kind := Is_Simulated) is record
    case Kind is
    when Is_Udp =>
      Socket  : Network.Udp.Socket;
      Address : Network.Address;
    when Is_Serial =>
      Port : Serial_Io.Port;
    when Is_Simulated =>
      null;
    end case;
  end record;

  procedure Read (Is_Stepper_Driver : Boolean);


  ----------
  -- Site --
  ----------

  type Location is (Home, Sternwarte_Schaffhausen, Unknown);

  function Default_Location return Location;

  function Latitude return Angle.Value;

  function Longitude return Angle.Value;

  function Altitude return Integer; -- in meters above see level

  function Sky_Line return String;


  ------------
  -- Device --
  ------------

  function Telescope_Name return String;

  function Is_Setup_Mode return Boolean;

  function Telescope_Connection return Connection;

  function Steps_Per_Revolution return Device.Steps_Per_Revolution;

  function Clocks_Per_Second return Positive;

  function Park_Azimuth return Angle.Value;

  function Park_Altitude return Angle.Value;

  function Pole_Height return Angle.Value;

  function Is_Azimuthal_Mount return Boolean;

  function Maximum_Speed return Angle.Value; -- in angle / s

  function Moving_Speeds return Angle.Values; -- in angle / s

  function First_Acceleration return Angle.Value; -- in angle / s²

  function Second_Acceleration return Angle.Value; -- in angle / s²

  function First_Upper_Limit return Angle.Degrees;

  function First_Lower_Limit return Angle.Degrees;

  function Second_Upper_Limit return Angle.Degrees;

  function Second_Lower_Limit return Angle.Degrees;


  -------------
  -- Servers --
  -------------

  function Lx200_Port return Network.Port_Number;

  function Stellarium_Port return Network.Port_Number;

end Parameter;
