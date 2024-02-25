-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with ENC_2302_Client;
with Traces;

package body Cdk_700 is

  package Log is new Traces ("Cdk_700");

  package ENC renames ENC_2302_Client;

  use all type ENC.Switch;

  Expected_Switches : constant ENC.Switches := [On, On, Off, On];

  Startup_Called     : Boolean  := False;
  Had_Restart        : Boolean  := False;
  Is_Simulation_Mode : Boolean := False;


  function Switches return ENC.Switches is
  begin
    return ENC.Switches_Of (The_Ip_Address);
  exception
  when ENC_2302_Client.Not_Available =>
    raise;
  when others =>
    return ENC.All_Off;
  end Switches;


  procedure Startup is
    use type ENC.Switches;
    use type Network.Ip_Address;
  begin
    Log.Write ("Startup");
    Had_Restart := False;
    Is_Simulation_Mode := The_Ip_Address = Network.Loopback_Address;
    if Is_Simulation_Mode then
      Log.Warning ("is in simulation mode");
    end if;
    declare
      Actual_Switches : constant ENC.Switches := Switches;
    begin
      if Actual_Switches /= Expected_Switches then
        for Port in ENC.Port loop
          if Actual_Switches(Port) /= Expected_Switches(Port) then
            ENC.Set (Port, Expected_Switches(Port), The_Ip_Address);
          end if;
        end loop;
        Progress.Start (The_Restart_Duration);
        Had_Restart := True;
      end if;
    end;
    Startup_Called := True;
  exception
  when ENC.Not_Available =>
    raise ENC_Not_Available;
  when others =>
    raise Startup_Failed;
  end Startup;


  function Startup_Progress return Progress.Percent renames Progress.In_Percent;


  function Had_Powerup return Boolean is
  begin
    if not Startup_Called then
      raise Program_Error;
    end if;
    return Had_Restart;
  end Had_Powerup;


  function Is_Started return Boolean is
  begin
    if not Startup_Called then
      raise Program_Error;
    end if;
    return not Progress.Is_Active;
  end Is_Started;


  function Is_Simulated return Boolean is
  begin
    if not Startup_Called then
      raise Program_Error;
    end if;
    return Is_Simulation_Mode;
  end Is_Simulated;

end Cdk_700;
