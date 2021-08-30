-- *********************************************************************************************************************
-- *                       (c) 2016 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package body Network is

  function Image_Of (Port : Port_Number) return String is
    Image : constant String := Port'image;
  begin
    return Image(Image'first + 1 .. Image'last);
  end Image_Of;

  function Ip_Address_Of_Host (Host_Name : String) return Ip_Address is
  begin
    return Net.Addresses (Net.Get_Host_By_Name (Host_Name));
  end;

  function Address_Of (Addr : Ip_Address;
                       Port : Port_Number) return Address is
  begin
    return Address'(Family => Net.Family_Inet,
                    Addr   => Addr,
                    Port   => Port);
  end Address_Of;

  function Home_Addresses return Ip_Addresses is
    Host          : constant Net.Host_Entry_Type := Net.Get_Host_By_Name (Net.Host_Name);
    The_Addresses : Ip_Addresses (1 .. Net.Addresses_Length (Host));
  begin
    for The_Index in The_Addresses'range loop
      The_Addresses(The_Index) := Net.Addresses (Host, The_Index);
    end loop;
    return The_Addresses;
  end Home_Addresses;

end Network;
