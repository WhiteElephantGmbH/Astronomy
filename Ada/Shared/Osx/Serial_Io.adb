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

package body Serial_Io is

  type Data is null record;
  

  procedure Initialize (The_Channel : in out Channel) is
  begin
    null;
  end Initialize;

  
  procedure Finalize (The_Channel : in out Channel) is
  begin
    null;
  end Finalize;
  
------------------------------------------------------------------------------------------------------------------------

  function Is_Available (The_Port : Port) return Boolean is
    pragma Unreferenced (The_Port);
  begin
    return False;
  end Is_Available;


  procedure Free (The_Port : Port) is
  begin
    null;
  end Free;


  procedure Set (The_Baudrate : Baudrate;
                 On           : Channel) is
  begin
    null;
  end Set;
  
   
  function Baudrate_Of (The_Channel : Channel) return Baudrate is
    pragma Unreferenced (The_Channel);
  begin
    return Baudrate'first;
  end Baudrate_Of;

  
  function Max_Baudrate_Of (The_Channel : Channel) return Baudrate is
    pragma Unreferenced (The_Channel);
  begin
    return Baudrate'first;
  end Max_Baudrate_Of;


  procedure Set (The_Parity  : Parity;
                 On          : Channel) is
  begin
    null;
  end Set;
  

  function Parity_Of (The_Channel : Channel) return Parity is
    pragma Unreferenced (The_Channel);
  begin
    return None;
  end Parity_Of;


  procedure Set (The_Stop_Bits : Stop_Bits;
                 On            : Channel) is
  begin
    null;
  end Set;

  
  function Stop_Bits_Of (The_Channel : Channel) return Stop_Bits is
    pragma Unreferenced (The_Channel);
  begin
    return One;
  end Stop_Bits_Of;


  procedure Set (The_Flow_Control : Flow_Control;
                 On               : Channel) is
  begin
    null;
  end Set;
  

  function Flow_Control_Of (The_Channel : Channel) return Flow_Control is
    pragma Unreferenced (The_Channel);
  begin
    return None;
  end Flow_Control_Of;


  procedure Set (The_Timeout : Duration;
                 On          : Channel) is
  begin
    null;
  end Set;
  

  procedure Set_For_Read (The_Timeout : Duration;
                          On          : Channel) is
  begin
    null;
  end Set_For_Read;
  

  procedure Set_For_Write (The_Timeout : Duration;
                           On          : Channel) is
  begin
    null;
  end Set_For_Write;
  

  function Read_Timeout_Of (The_Channel : Channel) return Duration is
    pragma Unreferenced (The_Channel);
  begin
    return 0.0;
  end Read_Timeout_Of;
  

  function Write_Timeout_Of (The_Channel : Channel) return Duration is
    pragma Unreferenced (The_Channel);
  begin
    return 0.0;
  end Write_Timeout_Of;


  procedure Set (The_Byte_Size : Byte_Size;
                 On            : Channel) is
  begin
    null;
  end Set;

  
  function Byte_Size_Of (The_Channel : Channel) return Byte_Size is
    pragma Unreferenced (The_Channel);
  begin
    return Eight_Bit_Bytes;
  end Byte_Size_Of;


  procedure Send (The_Item : String;
                  To       : Channel) is
  begin
    null;
  end Send;
  

  procedure Send (The_Item : Character;
                  To       : Channel) is
  begin
    null;
  end Send;
  

  procedure Send (The_Item : Unsigned.Byte_String;
                  To       : Channel) is
  begin
    null;
  end Send;
  

  procedure Send (The_Item : Unsigned.Byte;
                  To       : Channel) is
  begin
    null;
  end Send;


  procedure Receive (The_Item : out String;
                     From     : Channel) is
  begin
    null;
  end Receive;
  

  procedure Receive (The_Item : out Character;
                     From     : Channel) is
  begin
    null;
  end Receive;
  

  procedure Receive (The_Item : out Unsigned.Byte_String;
                     From     : Channel) is
  begin
    null;
  end Receive;
  

  procedure Receive (The_Item : out Unsigned.Byte;
                     From     : Channel) is
  begin
    null;
  end Receive;


  function Character_Of (The_Channel : Channel) return Character is
    pragma Unreferenced (The_Channel);
  begin
    return Ascii.Nul;
  end Character_Of;
  

  function Byte_Of (The_Channel : Channel) return Unsigned.Byte is
    pragma Unreferenced (The_Channel);
  begin
    return 0;
  end Byte_Of;


  procedure Flush (The_Channel : Channel;
                   The_Timeout : Duration := Default_Flush_Timeout) is
  begin
    null;
  end Flush;

end Serial_Io;
