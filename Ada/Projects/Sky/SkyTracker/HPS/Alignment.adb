-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Alignment.Stars;
with Earth;
with Objects;
with Sky.Data;
with Sky_Line;

package body Alignment is

  Pear_Side_Undefined : constant Character := ' ';

  type Point is record
    Star              : Stars.Id;
    Mount_Direction   : Space.Direction;
    Picture_Direction : Space.Direction;
    Mount_Pier_Side   : Character;
    Sideral_Time      : Time.Value;
  end record;

  subtype Index is Stars.Count range 1 .. Stars.Count'last;

  type Points is array (Index) of Point;

  type Deltas is record
    Ra  : Angle.Degrees;
    Dec : Angle.Degrees;
  end record;

  type Deltas_Table is array (Index) of Deltas;

  use type Angle.Degrees;

  Ideal_Deltas : constant Deltas_Table := [
    1 => (Ra => 135.0, Dec =>  0.0),
    2 => (Ra =>  90.0, Dec => 20.0),
    3 => (Ra => 180.0, Dec => 20.0),
    4 => (Ra => -90.0, Dec => 20.0),
    5 => (Ra => -90.0, Dec => -90.0),
    6 => (Ra => -90.0, Dec => 10.0),
    others => (90.0, 30.0)];

  Ideal_Ra  : Angle.Value := Angle.Zero;
  Ideal_Dec : Angle.Value := Angle.Zero;

  use type Stars.Set;

  The_Star_Count : Stars.Count := 0;
  The_Points     : Points;
  Actual_Star    : Stars.Id;
  The_Stars      : Stars.Set := [];
  Visited_Stars  : Stars.Set := [];
  Had_Error      : Boolean := False;

  The_Alignment_Info : Information;


  procedure Clear is
  begin
    The_Stars := [];
    Visited_Stars := [];
    The_Star_Count := 0;
    Had_Error := False;
    Ideal_Ra := Angle.Zero;
    Ideal_Dec := Angle.Zero;
  end Clear;


  function Align_More return Boolean is
  begin
    return The_Star_Count < The_Alignment_Stars;
  end Align_More;


  function Meridian_North return Angle.Value is
    North : constant Space.Direction := Objects.Direction_Of (Direction => Earth.Zero_Direction, Ut => Time.Universal);
  begin
    return Space.Ra_Of (North);
  end Meridian_North;


  function Next_Star return Space.Direction is

    Ra_North : constant Angle.Value := Meridian_North;

    Best_Star         : Stars.Id;
    Best_Direction    : Space.Direction;
    Shortest_Distance : Angle.Degrees := Angle.Degrees'last;

    use type Angle.Value;

    procedure Evaluate_Quality_Of (The_Star      : Stars.Id;
                                   The_Direction : Space.Direction) is

      function Distance_Between (V1 : Angle.Value;
                                 V2 : Angle.Value) return Angle.Degrees is
      begin
        return abs (V1 - V2);
      end Distance_Between;

      The_Distance : Angle.Degrees;

    begin -- Evaluate_Quality_Of
      The_Distance := Distance_Between (Space.Ra_Of (The_Direction) - Ra_North, Ideal_Ra) +
                      1.5 * Distance_Between (Space.Dec_Of (The_Direction), Ideal_Dec);
      if The_Distance < Shortest_Distance then
        Best_Star := The_Star;
        Best_Direction := The_Direction;
        Shortest_Distance := The_Distance;
      end if;
    end Evaluate_Quality_Of;

    Ideal_Delta : constant Deltas := Ideal_Deltas(The_Star_Count + 1);

  begin -- Next_Star
    Ideal_Ra := @ + Ideal_Delta.Ra;
    Ideal_Dec := @ + Ideal_Delta.Dec;
    loop
      for The_Star in Stars.Id loop
        if not (The_Star < Visited_Stars) then
          declare
            Direction : constant Space.Direction := Sky.Data.Direction_Of (Stars.Object_Of(The_Star), Time.Universal);
           begin
            if Sky_Line.Is_Above (Direction, Time.Lmst, Use_Upper => True) then
              Evaluate_Quality_Of (The_Star, Direction);
            end if;
          end;
        end if;
      end loop;
      if Space.Direction_Is_Known (Best_Direction) then
        Log.Write ("Next_Star " & Stars.Image_Of (Best_Star) & " at " & Space.Image_Of (Best_Direction));
        Actual_Star := Best_Star;
        Visited_Stars := @ + Actual_Star;
        declare
          P : Point renames The_Points(The_Star_Count + 1);
        begin
          P.Star := Best_Star;
          P.Mount_Direction := Best_Direction;
          P.Mount_Pier_Side := Pear_Side_Undefined;
        end;
        Ideal_Ra := Angle.Value'(Space.Ra_Of (Best_Direction) - Ra_North);
        Ideal_Dec := Space.Dec_Of (Best_Direction);
        Had_Error := False;
        exit;
      else
        exit when Visited_Stars = The_Stars;
        Visited_Stars := The_Stars;
      end if;
    end loop;
    return Best_Direction;
  end Next_Star;


  function Star_Count return Natural is (The_Star_Count);


  procedure Define (Direction : Space.Direction;
                    Lmst      : Time.Value;
                    Pier_Side : Character) is
    P : Point renames The_Points(The_Star_Count + 1);
  begin
    Log.Write ("Define " & Stars.Image_Of (P.Star) & " at " & Space.Image_Of (Direction)
                         & " (Lmst:" & Time.Image_Of (Lmst) & ")");
    The_Stars := @ + Actual_Star;
    The_Star_Count := @ + 1;
    P.Picture_Direction := Direction;
    P.Sideral_Time := Lmst;
    P.Mount_Pier_Side := Pier_Side;
  end Define;


  function Ready return Boolean is
  begin
    return The_Star_Count > 0 and not Had_Error;
  end Ready;


  procedure Generate is
  begin
    Log.Write ("Generate for " & The_Stars'image);
    Ten_Micron.Start_Alignment;
    for The_Index in Index'first .. The_Star_Count loop
      declare
        P : Point renames The_Points(The_Index);
        N : Natural;
      begin
        Ten_Micron.Add_Alignment_Point (Mount   => P.Mount_Direction,
                                        Picture => P.Picture_Direction,
                                        Side    => P.Mount_Pier_Side,
                                        Lmst    => P.Sideral_Time,
                                        Points  => N);
        if The_Index /= N then
          Log.Warning ("add alignment failed on point" & The_Index'image);
          The_Star_Count := The_Index - 1;
          return;
        end if;
      end;
    end loop;
    if Ten_Micron.End_Alignment then
      Clear;
      The_Alignment_Info := Ten_Micron.Alignment_Info;
    else
      Had_Error := True;
      Log.Warning ("alignment computation failed");
    end if;
  end Generate;


  procedure Update_Info is
  begin
    if Ten_Micron.Has_New_Alignment_Info then
      The_Alignment_Info := Ten_Micron.Alignment_Info;
    end if;
  end Update_Info;


  function Info return Information is
  begin
    return The_Alignment_Info;
  end Info;

end Alignment;
