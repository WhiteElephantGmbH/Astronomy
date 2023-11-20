-- *********************************************************************************************************************
-- *                       (c) 2017 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Network.Tcp.Server;
with Sky_Line;
with Strings;
with Traces;
with Time;
with User.Input;

package body Lx200_Server is

  package Log is new Traces ("Lx200");

  package Server renames Network.Tcp.Server;

  Command_Terminator : constant Character := '#';

  Goto_Location : Goto_Handler;
  The_Goto_Dec  : Angle.Value;
  The_Goto_Ra   : Angle.Value;

  The_Actual_Dec : Angle.Value;
  The_Actual_Ra  : Angle.Value;


  procedure Message_Handler (Data : String;
                             From : Network.Tcp.Socket) is

    function Angle_Image (Unit_1, Unit_2, Unit_3 : Character) return String is
      The_Image : String := Data(Data'first + 3 .. Data'last); -- format: "sDD:DD:DD#"
    begin
      The_Image(The_Image'last - 6) := Unit_1;
      The_Image(The_Image'last - 3) := Unit_2;
      The_Image(The_Image'last) := Unit_3;
      return The_Image;
    end Angle_Image;


    function Image_Of (The_Value : Angle.Value;
                       Unit      : Angle.Units) return String is

      use type Angle.Units;

      The_Image : String := Strings.Ansi_Of (Angle.Image_Of (The_Value   => The_Value,
                                                             Unit        => Unit,
                                                             Decimals    => 0,
                                                             Show_Signed => Unit = Angle.In_Degrees));
    begin
      The_Image(The_Image'last - 6) := ':';
      The_Image(The_Image'last - 3) := ':';
      The_Image(The_Image'last) := '#';

      if The_Image(The_Image'first) in '+' | '-' then
        if The_Image(The_Image'first + 2) = ':' then -- check for "sD:DD:DD#"
          return The_Image(The_Image'first) & '0' & The_Image(The_Image'first + 1 .. The_Image'last);
        end if;
      elsif The_Image(The_Image'first + 1) = ':' then -- check for "D:DD:DD#"
        return '0' & The_Image;
      end if;
      return The_Image;
    end Image_Of;


    procedure Execute (Command : Device.Command) is
    begin
      User.Input.Put (Command, User.Input.Lx200);
    end Execute;

  begin -- Message_Handler
    Log.Write ("Data: " & Data);
    if Data'length > 3  and then Data(Data'last) = Command_Terminator then
      declare
        Command : constant String := Data(Data'first .. Data'first + 2);
      begin
        if Command = ":GD" then
          Server.Reply (Image_Of (The_Actual_Dec, Angle.In_Degrees), To => From);
        elsif Command = ":GR" then
          Server.Reply (Image_Of (The_Actual_Ra, Angle.In_Hours), To => From);
        elsif Command = ":Sr" then
          The_Goto_Ra := Angle.Value_Of (Angle_Image ('h', 'm', 's'), With_Units => Angle.In_Hours);
          Server.Reply ("1", To => From);
        elsif Command = ":Sd" then
          The_Goto_Dec := Angle.Value_Of (Angle_Image ('d', ''', '"'), With_Units => Angle.In_Degrees);
          Server.Reply ("1", To => From);
        elsif Command = ":MS" then
          if Goto_Location = null then
            Server.Reply ("2not reachable#", To => From);
          else
            declare
              Object_Direction : constant Space.Direction := Space.Direction_Of (Ra  => The_Goto_Ra,
                                                                                 Dec => The_Goto_Dec);
            begin
              if Sky_Line.Is_Above (Object_Direction, Time.Lmst) then
                Goto_Location (Space.Direction_Of (Ra  => The_Goto_Ra,
                                                   Dec => The_Goto_Dec));
                Server.Reply ("0", To => From); -- goto OK
              else
                Server.Reply ("1below horizon#", To => From);
              end if;
            end;
          end if;
        elsif Command = ":CM" then
          Execute (Device.Enter);
          Server.Reply ("#", To => From);
        elsif Command = ":Me" then
          Execute (Device.Move_Left);
        elsif Command = ":Mn" then
          Execute (Device.Move_Up);
        elsif Command = ":Ms" then
          Execute (Device.Move_Down);
        elsif Command = ":Mw" then
          Execute (Device.Move_Right);
        elsif Command = ":Qe" then
          Execute (Device.No_Command);
        elsif Command = ":Qn" then
          Execute (Device.No_Command);
        elsif Command = ":Qs" then
          Execute (Device.No_Command);
        elsif Command = ":Qw" then
          Execute (Device.No_Command);
        elsif Command = ":RC" then
          Execute (Device.Set_Centering_Rate);
        elsif Command = ":RG" then
          Execute (Device.Set_Guiding_Rate);
        elsif Command = ":RM" then
          Execute (Device.Set_Finding_Rate);
        elsif Command = ":RS" then
          Execute (Device.Set_Slewing_Rate);
        else
          Log.Error ("Unknown Command");
        end if;
      end;
    elsif Data = ":Q#" then
      Execute (Device.Stop);
    else
      Log.Error ("Unknown Data");
    end if;
  end Message_Handler;


  procedure Start (Used_Port : Port_Number) is
  begin
    Log.Write ("start - port:" & Used_Port'img);
    Server.Start (Message_Handler'access, Used_Port, Command_Terminator);
  end Start;


  procedure Define_Handler (The_Handler : Goto_Handler) is
  begin
    Goto_Location := The_Handler;
  end Define_Handler;


  procedure Set (Direction : Space.Direction) is
  begin
    The_Actual_Dec := Space.Dec_Of (Direction);
    The_Actual_Ra := Space.Ra_Of (Direction);
  end Set;


  procedure Close is
  begin
    Server.Close;
    Log.Write ("end");
  end Close;

end Lx200_Server;
