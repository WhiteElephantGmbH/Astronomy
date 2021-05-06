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
with Ada.Text_IO;
with Angle;
with Earth;
with Exceptions;
with M_Zero;
with Objects;
with Site;
with Space;
with Strings;
with Time;

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

  function Home_Direction return Space.Direction is
    East : constant Earth.Direction := Earth.Direction_Of (Alt => Angle.Zero, Az => Angle.Quadrant);
  begin
    return Objects.Direction_Of (East, Time.Universal);
  end Home_Direction;

  Is_Complete : Boolean;

  procedure Slewing_Completion (Is_Ok : Boolean) is
  begin
    Is_Complete := Is_Ok;
  end Slewing_Completion;


  procedure Client (Server : String) is
  begin
    Put ("Home RA : " & Space.Ra_Image_Of (Home_Direction));
    M_Zero.Connect (Slewing_Completion'access, Server);
    Put (">>> connected to " & Server);
    loop
      Ada.Text_IO.Put (">");
      Is_Complete := False;
      declare
        Command : constant String := Get;
      begin
        exit when Command = "";
        declare
          Replay : constant String := M_Zero.Reply_For (Command);
        begin
          if Replay /= "" then
            Put (Replay);
          end if;
        end;
        if Command = "MS" then
          Put ("slewing");
          while not Is_Complete loop
            Put (M_Zero.Reply_For ("GD"));
            Put (M_Zero.Reply_For ("GR"));
            delay 1.0;
          end loop;
          Put ("tracking");
        end if;
      exception
      when M_Zero.No_Connection =>
        Put ("<not connected>");
      end;
    end loop;
    M_Zero.Disconnect;
  exception
  when others =>
    M_Zero.Disconnect;
    raise;
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
