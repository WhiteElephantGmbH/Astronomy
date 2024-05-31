-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Unchecked_Deallocation;
with Angle;
with Astro;
with Error;
with Norad;
with Objects;
with Satellite;
with Site;
with Sky.Data;
with Sky_Line;
with Traces;
with Values;

package body Neo is

  package Log is new Traces ("Neo");

  use type Time.Ut;

  function Exists (Item : String) return Boolean is
  begin
    return Satellite.Exists (Item);
  end Exists;


  type Element is record
    Ut  : Time.Ut     := Time.In_The_Past;
    Ra  : Angle.Value := Angle.Zero;
    Dec : Angle.Value := Angle.Zero;
  end record;

  type Trajectory is array (Positive range <>) of Element;

  The_Trajectory : Trajectory(1 .. 10000);
  The_Last       : Natural := 0;

  The_Wrap_Location : Earth.Direction;


  procedure Read (Target : String) is

    Start_Time : constant Time.Ut := Time.Synchronized_Universal (Base => 1.0);
    The_Entry  : Natural := 0;

    Norad_Lines : Norad.Two_Line;

    procedure Find_Norad_Entries is

      use Astro;

      Rsphi, Rcphi : REAL;

      procedure Evaluate_Site is
        use all type Angle.Value;
      begin
        SPHLIB.SITE (PHI   => +Site.Latitude,
                     RCPHI => Rcphi,
                     RSPHI => Rsphi);
      end Evaluate_Site;

      The_Time      : Time.Ut;
      The_Lmst      : Time.Value;
      The_Direction : Space.Direction;
      The_Location  : Earth.Direction;

      --procedure Write (Text  : String;
      --                 Value : VECTOR) is
      --begin
      --  Log.Write (The_Time'img & " " & Text & ": x=" & Value(X)'img & ", y=" & Value(Y)'img & ", z=" & Value(Z)'img);
      --end Write;

      --Last_Geozentric_Vector  : VECTOR := (0.0, 0.0, 0.0);

      procedure Evaluate_Location is

        use MATLIB;

        The_Dec, The_Ra, R : REAL;

        The_Geozentric_Velocity : VECTOR;
        The_Geozentric_Vector   : VECTOR;
        The_Location_Vector     : VECTOR;
        The_Topocentric_Vector  : VECTOR;

      begin
        Norad.SGP (Norad_Lines, The_Time, The_Geozentric_Vector, The_Geozentric_Velocity);
        --Write ("Position", The_Geozentric_Vector);
        --Write ("Diff    ", The_Geozentric_Vector - Last_Geozentric_Vector);
        --Last_Geozentric_Vector := The_Geozentric_Vector;
        --Write ("Speed   ", The_Geozentric_Velocity);
        declare
          use all type Angle.Value;
          Er   : constant REAL := Earth_Equatorial_Radius + (REAL(Site.Elevation) / 1000.0);
          Lmst : constant REAL := +The_Lmst;
        begin
          The_Location_Vector := [X => Er * Rcphi * CS (Lmst),
                                  Y => Er * Rcphi * SN (Lmst),
                                  Z => Er * Rsphi];
        end;
        The_Topocentric_Vector := The_Geozentric_Vector - The_Location_Vector;
        POLAR (The_Topocentric_Vector, R, The_Dec, The_Ra);
        The_Direction := Space.Direction_Of (Dec => The_Dec,
                                             Ra  => The_Ra);
        The_Location := Objects.Direction_Of (The_Direction, The_Lmst);
      end Evaluate_Location;

      procedure Log_Image_Of (Lable : String;
                              Item  : Element) is
      begin
        Log.Write (Lable & Time.Image_Of (Item.Ut) & " - RA : " & Angle.Hours_Image_Of (Item.Ra) &
                                                     " - DEC: " & Angle.Signed_Degrees_Image_Of (Item.Dec));
      end Log_Image_Of;

      Search_Step   : constant Duration := Time.One_Second * 12.0;
      Maximum_Pause : constant Duration := Time.One_Minute * 3.0;

      The_Step  : Duration;
      The_Pause : Duration;

      The_Max_Alt : Angle.Degrees;

    begin -- Find_Norad_Entries
      if Log.Is_Enabled then
        Log.Write ("NORAD: " & Norad_Lines(1));
        Log.Write ("       " & Norad_Lines(2));
      end if;
      begin
        Evaluate_Site;
        The_Time := Start_Time;
        The_Step := Search_Step;
        while The_Time < (Start_Time + Time.One_Day) loop
          The_Time := The_Time + The_Step;
          The_Lmst := Time.Lmst_Of (The_Time);
          Evaluate_Location;
          if Sky_Line.Is_Above (The_Location) then
            if The_Step = Time.One_Second then
              The_Max_Alt := 0.0;
              loop
                The_Entry := @ + 1;
                if The_Entry > The_Trajectory'last then
                  Log.Warning ("slow object");
                  The_Last := 0;
                  exit;
                end if;
                The_Trajectory(The_Entry).Ut := The_Time;
                The_Trajectory(The_Entry).Ra := Space.Ra_Of (The_Direction);
                The_Trajectory(The_Entry).Dec := Space.Dec_Of (The_Direction);
                The_Time := The_Time + Time.One_Second;
                The_Lmst := Time.Lmst_Of (The_Time);
                Evaluate_Location;
                declare
                  use type Angle.Signed;
                  use type Angle.Value;
                  Alt : constant Angle.Degrees := +Angle.Signed'(+Earth.Alt_Of (The_Location));
                begin
                  if Alt > The_Max_Alt then
                    The_Max_Alt := Alt;
                    The_Wrap_Location := The_Location;
                  end if;
                end;
                if Sky_Line.Is_Above (The_Location) then
                  The_Last := The_Entry;
                  The_Pause := Maximum_Pause;
                else
                  The_Pause := The_Pause - The_Step;
                  if The_Pause <= 0.0 then
                    exit;
                  end if;
                end if;
              end loop;
              if The_Last > The_Trajectory'first then
                if Log.Is_Enabled then
                  Log_Image_Of ("start: ", The_Trajectory(The_Trajectory'first));
                  Log_Image_Of ("end  : ", The_Trajectory(The_Last));
                  Log.Write ("Max Alt: " & Earth.Alt_Image_Of (The_Wrap_Location));
                  Log.Write ("Wrap Az: " & Earth.Az_Image_Of (The_Wrap_Location));
                end if;
                return;
              end if;
              exit;
            else
              The_Time := The_Time - The_Step;
              The_Step := Time.One_Second;
            end if;
          end if;
        end loop;
      exception
      when Norad.Bad_Data =>
        Log.Warning ("Bad NORAD data");
      when Error.Occurred =>
        raise;
      when Occurrence: others =>
        Log.Termination (Occurrence);
        Error.Raise_With ("NORAD calculation failed");
      end;
      The_Last := 0;
    end Find_Norad_Entries;

  begin -- read
    The_Last := 0;
    The_Entry := 0;
    Log.Write ("NEO READ - " & Target);
    Norad_Lines := Satellite.Tle_Of (Target);
    Find_Norad_Entries;
  end Read;


  type Trajectory_Access is access Trajectory;

  type Data is record
    List : Trajectory_Access;
    Wrap : Earth.Direction;
  end record;

  The_Trajectories : array (1..500) of Data;


  procedure Add_Objects is

    procedure Process (Target : String) is
      The_Index : Positive;
    begin
      Read (Target);
      if The_Last > 0 then
        The_Index := Sky.Data.New_Neo_Object_For (Item        => Target,
                                                  Description => "");
        if The_Index > The_Trajectories'last then
          Error.Raise_With ("Too many near earth objects");
        end if;
        The_Trajectories(The_Index).List := new Trajectory'(The_Trajectory(1..The_Last));
        The_Trajectories(The_Index).Wrap := The_Wrap_Location;
      end if;
    end Process;

  begin -- Add_Objects
    Satellite.Read_Stellarium_Data;
    for Target of Satellite.Names loop
      Process (Target);
    end loop;
  end Add_Objects;


  function Direction_Of (Item : Name.Id;
                         Ut   : Time.Ut) return Space.Direction is

    Index : constant Positive := Sky.Data.Neo_Index_Of (Name.Object_Of (Item));

    The_Data : Trajectory_Access renames The_Trajectories(Index).List;

  begin
    if The_Data /= null then
      for The_Index in The_Data.all'range loop
        if The_Index < The_Data.all'last and then The_Data(The_Index + 1).Ut >= Ut then
          return Space.Direction_Of (Ra => Values.Interpolation_Of (T  => Ut,
                                                                    T1 => The_Data(The_Index).Ut,
                                                                    T2 => The_Data(The_Index + 1).Ut,
                                                                    V1 => The_Data(The_Index).Ra,
                                                                    V2 => The_Data(The_Index + 1).Ra),
                                     Dec => Values.Interpolation_Of (T  => Ut,
                                                                     T1 => The_Data(The_Index).Ut,
                                                                     T2 => The_Data(The_Index + 1).Ut,
                                                                     V1 => The_Data(The_Index).Dec,
                                                                     V2 => The_Data(The_Index + 1).Dec));
        end if;
      end loop;
    end if;
    return Space.Unknown_Direction;
  end Direction_Of;


  procedure Dispose is new Ada.Unchecked_Deallocation (Trajectory, Trajectory_Access);

  function Is_Arriving (Item : Name.Id) return Boolean is
    Index    : constant Positive := Sky.Data.Neo_Index_Of (Name.Object_Of (Item));
    The_Data : Data renames The_Trajectories(Index);
  begin
    if The_Data.List = null then
      return False;
    elsif The_Data.List(The_Data.List'last).Ut < Time.Universal then
      Dispose (The_Data.List);
      Read (Name.Image_Of (Item));
      if The_Last = 0 then
        The_Data.List := null;
        return False;
      end if;
      The_Data.List := new Trajectory'(The_Trajectory(1..The_Last));
      The_Data.Wrap := The_Wrap_Location;
    end if;
    return The_Data.List(The_Data.List'first).Ut < Time.Universal + Time.One_Minute * 15;
  end Is_Arriving;


  function Tracking_Period_Of (Item : Name.Id) return Time.Period is
    Index    : constant Positive := Sky.Data.Neo_Index_Of (Name.Object_Of (Item));
    The_Data : Data renames The_Trajectories(Index);
  begin
    if The_Data.List = null then
      return Time.Undefined;
    else
      return (Arrival_Time => The_Data.List(The_Data.List'first).Ut,
              Leaving_Time => The_Data.List(The_Data.List'last).Ut);
    end if;
  end Tracking_Period_Of;


  function Wrap_Location_Of (Item : Name.Id) return Earth.Direction is
    Index    : constant Positive := Sky.Data.Neo_Index_Of (Name.Object_Of (Item));
    The_Data : Data renames The_Trajectories(Index);
  begin
    return The_Data.Wrap;
  end Wrap_Location_Of;

end Neo;
