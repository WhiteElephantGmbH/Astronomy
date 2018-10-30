-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Finalization;
with Unsigned;
package Serial_Io is

  type Port is (Com1, Com2, Com3, Com4, Com5, Com6, Com7, Com8);

  type Channel (The_Port : Port) is tagged limited private;

  type Baudrate is new Natural range 110 .. 256000;

  type Parity is (None, Odd, Even, Mark, Space);

  type Stop_Bits is (One, One_And_A_Half, Two);

  type Flow_Control is (None, Cts, Dsr, Cts_And_Dsr);

  type Byte_Size is (Eight_Bit_Bytes, Seven_Bit_Bytes, Six_Bit_Bytes, Five_Bit_Bytes);

  Default_Baudrate      : constant := 19200;
  Default_Byte_Size     : constant Byte_Size := Eight_Bit_Bytes;
  Default_Parity        : constant Parity := None;
  Default_Stop_Bits     : constant Stop_Bits := One;
  Default_Flow_Control  : constant Flow_Control := None;
  Default_Flush_Timeout : constant Duration := 0.1;

  Infinite : constant Duration := 0.0;


  function Is_Available (The_Port : Port) return Boolean;

  procedure Free (The_Port : Port);


  procedure Set (The_Baudrate : Baudrate;
                 On           : Channel);

  function Baudrate_Of (The_Channel : Channel) return Baudrate;

  function Max_Baudrate_Of (The_Channel : Channel) return Baudrate;


  procedure Set (The_Parity  : Parity;
                 On          : Channel);

  function Parity_Of (The_Channel : Channel) return Parity;


  procedure Set (The_Stop_Bits : Stop_Bits;
                 On            : Channel);

  function Stop_Bits_Of (The_Channel : Channel) return Stop_Bits;


  procedure Set (The_Flow_Control : Flow_Control;
                 On               : Channel);

  function Flow_Control_Of (The_Channel : Channel) return Flow_Control;


  procedure Set (The_Timeout : Duration;
                 On          : Channel);

  procedure Set_For_Read (The_Timeout : Duration;
                          On          : Channel);

  procedure Set_For_Write (The_Timeout : Duration;
                           On          : Channel);

  function Read_Timeout_Of (The_Channel : Channel) return Duration;

  function Write_Timeout_Of (The_Channel : Channel) return Duration;


  procedure Set (The_Byte_Size : Byte_Size;
                 On            : Channel);

  function Byte_Size_Of (The_Channel : Channel) return Byte_Size;


  procedure Send (The_Item : String;
                  To       : Channel);

  procedure Send (The_Item : Character;
                  To       : Channel);

  procedure Send (The_Item : Unsigned.Byte_String;
                  To       : Channel);

  procedure Send (The_Item : Unsigned.Byte;
                  To       : Channel);


  procedure Receive (The_Item : out String;
                     From     : Channel);

  procedure Receive (The_Item : out Character;
                     From     : Channel);

  procedure Receive (The_Item : out Unsigned.Byte_String;
                     From     : Channel);

  procedure Receive (The_Item : out Unsigned.Byte;
                     From     : Channel);


  function Character_Of (The_Channel : Channel) return Character;

  function Byte_Of (The_Channel : Channel) return Unsigned.Byte;


  procedure Flush (The_Channel : Channel;
                   The_Timeout : Duration := Default_Flush_Timeout);


  Aborted           : exception;
  No_Access         : exception;
  Operation_Failed  : exception;
  Timeout           : exception;
  Illegal_Baudrate  : exception;
  Illegal_Byte_Size : exception;

private

  type Data;

  type Data_Pointer is access Data;

  type Channel (The_Port : Port) is new Ada.Finalization.Limited_Controlled with record
    The_Data : Data_Pointer;
  end record;

  procedure Initialize (The_Channel : in out Channel);

  procedure Finalize (The_Channel : in out Channel);

end Serial_Io;

