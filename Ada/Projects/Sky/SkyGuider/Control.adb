-- *********************************************************************************************************************
-- *                               (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with Angle;
with Earth;
with Exceptions;
with M_Zero;
with Network;
with Objects;
with Site;
with Space;
with Strings;
with Time;
with Log;

package body Control is

  procedure Put (Item : String) renames Ada.Text_IO.Put_Line;

  function Get return String renames Ada.Text_IO.Get_Line;

  function Image_Of (Item : Angle.Value) return String is
  begin
    return Strings.Ansi_Of (Angle.Image_Of (The_Value   => Item,
                                            Unit        => Angle.In_Degrees,
                                            Decimals    => 3,
                                            Show_Signed => True));
  end Image_Of;

  function Direction_For (Alt : Angle.Degrees;
                          Az  : Angle.Degrees) return Space.Direction is
    use type Angle.Value;
    Location : constant Earth.Direction := Earth.Direction_Of (Alt => +Alt, Az  => +Az);
  begin
    return Objects.Direction_Of (Location, Time.Universal);
  end Direction_For;


  function Object return Space.Direction is
  begin
    return Direction_For (Alt => 30.0, Az => 90.0);
  end Object;


  function North return Space.Direction is
  begin
    return Direction_For (Alt => 0.0, Az => 0.0);
  end North;


  function East return Space.Direction is
  begin
    return Direction_For (Alt => 0.0, Az => 90.0);
  end East;


  function South return Space.Direction is
  begin
    return Direction_For (Alt => 0.0, Az => 180.0);
  end South;


  function West return Space.Direction is
  begin
    return Direction_For (Alt => 0.0, Az => 270.0);
  end West;


  procedure Client (Server : String) is
    The_Information : M_Zero.Information;
  begin
    declare
      Ip_Address : constant Network.Ip_Address := Network.Ip_Address_Of (Server);
    begin
      loop
        Ada.Text_IO.Put (">");
        begin
          declare
            Command : constant String := Strings.Lowercase_Of (Get);
          begin
            exit when Command = "";
            case Command(Command'first) is
            when 'c' =>
              M_Zero.Connect (Ip_Address);
              case M_Zero.Get.Status is
              when M_Zero.Connected =>
                Put (">>> connected to " & Server);
              when M_Zero.Error =>
                Put (">>> connect failed with " & M_Zero.Error_Message);
              when others =>
                null;
              end case;
            when 'i' =>
              M_Zero.Initialize;
            when 'g' =>
              The_Information := M_Zero.Get;
              Put ("Status: " & The_Information.Status'image);
              if Space.Direction_Is_Known (The_Information.Direction) then
                Put ("RA    : " & Space.Ra_Image_Of (The_Information.Direction));
                Put ("DEC   : " & Space.Dec_Image_Of (The_Information.Direction));
              end if;
            when 'm' =>
              if Command'length = 1 then
                Put ("Unknown Direction");
              else
                case Command(Command'first + 1) is
                when 'u' =>
                  M_Zero.Start_Move (M_Zero.Up);
                when 'd' =>
                  M_Zero.Start_Move (M_Zero.Down);
                when 'l' =>
                  M_Zero.Start_Move (M_Zero.Left);
                when 'r' =>
                  M_Zero.Start_Move (M_Zero.Right);
                when others =>
                  Put ("Unknown Direction <u, d, l or r>");
                end case;
              end if;
            when 'q' =>
              if Command'length = 1 then
                M_Zero.Stop_Move;
              else
                case Command(Command'first + 1) is
                when 'u' =>
                  M_Zero.Stop_Move (M_Zero.Up);
                when 'd' =>
                  M_Zero.Stop_Move (M_Zero.Down);
                when 'l' =>
                  M_Zero.Stop_Move (M_Zero.Left);
                when 'r' =>
                  M_Zero.Stop_Move (M_Zero.Right);
                when others =>
                  Put ("Unknown Direction <u, d, l or r>");
                end case;
              end if;
            when 'a' =>
              if Command'length = 1 then
                M_Zero.Synch_To (Object);
              else
                case Command(Command'first + 1) is
                when 'n' =>
                  M_Zero.Synch_To (North);
                when 'e' =>
                  M_Zero.Synch_To (East);
                when 's' =>
                  M_Zero.Synch_To (South);
                when 'w' =>
                  M_Zero.Synch_To (West);
                when others =>
                  Put ("Unknown Direction <n, e, s or w>");
                end case;
              end if;
            when 's' =>
              if Command'length = 1 then
                M_Zero.Slew_To (Object);
              else
                case Command(Command'first + 1) is
                when 'n' =>
                  M_Zero.Slew_To (North);
                when 'e' =>
                  M_Zero.Slew_To (East);
                when 's' =>
                  M_Zero.Slew_To (South);
                when 'w' =>
                  M_Zero.Slew_To (West);
                when others =>
                  Put ("Unknown Direction <n, e, s or w>");
                end case;
              end if;
            when 'd' =>
              M_Zero.Disconnect;
              Put (">>> Disconnected");
            when 'e' =>
              Put ("<error> " & M_Zero.Error_Message);
            when others =>
              Put ("<unknown command>");
            end case;
          end;
        exception
        when Item: others =>
          Log.Write (Item);
          Put ("<unexpected exception> " & Ada.Exceptions.Exception_Message(Item));
          exit;
        end;
      end loop;
      M_Zero.Disconnect;
    end;
  end Client;


  procedure Start is
  begin
    case Ada.Command_Line.Argument_Count is
    when 0 =>
      Put ("IP Address expected");
    when 1 =>
      if Site.Is_Defined then
        Put ("Elevation :" & Site.Elevation'image & 'm');
        Put ("Latitude  : " & Image_Of ( Site.Latitude));
        Put ("Longitude : " & Image_Of (Site.Longitude));
        Put ("Time      : " & Time.Image_Of (Time.Universal));
        Client (Ada.Command_Line.Argument(1));
      else
        Put ("Site not Defined");
      end if;
    when others =>
      Put ("Too many arguments");
    end case;
  exception
  when Item: others =>
    Put ("<Traceback> " & Exceptions.Information_Of (Item));
  end Start;

end Control;
