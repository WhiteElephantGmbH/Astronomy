-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Directories;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Angle;
with Astro;
with Data;
with Earth;
with Error;
with File;
with Norad;
with Satellite;
with Site;
with Sky_Line;
with Strings;
with Traces;
with Values;

package body Neo is

  package Log is new Traces ("Neo");

  Directory : constant String := Satellite.Directory;
  Extension : constant String := Satellite.Extension;

  function Filename_Of (Item : String) return String is
  begin
    return File.Composure (Directory, Item, Extension);
  end Filename_Of;


  function Exists (Item : String) return Boolean is
  begin
    return File.Exists (Filename_Of (Item));
  end Exists;


  type Element is record
    Ut  : Time.Ut     := Time.In_The_Past;
    Ra  : Angle.Value := Angle.Zero;
    Dec : Angle.Value := Angle.Zero;
  end record;

  type List is array (Positive range <>) of Element;

  The_List : List(1 .. 10000);
  The_Last : Natural := 0;


  procedure Read (Target : String) is

    Start_Time : constant Time.Ut := Time.Synchronized_Universal (Base => 1.0);
    The_Entry  : Natural := 0;

    procedure Read_Sssb (Object : String) is

      type Header is (Unused_Universal_Time,
                      Julian_Date,
                      Unused_Solar_Presence,
                      Unused_Lunar_Presence,
                      Right_Ascension,
                      Declination,
                      Azimuth,
                      Altitude,
                      Unused_Line_End);

      The_File : Ada.Text_IO.File_Type;

      In_Data_Part : Boolean := False;

      The_Data_Below_Horizon : Element := (others => <>);
      Az_Below_Horizon       : Angle.Value;
      Alt_Below_Horizon      : Angle.Value;
      Has_Data_Below_Horizon : Boolean := False;

    begin -- Read_Objects
      Ada.Text_IO.Open (The_File, Ada.Text_IO.In_File, Filename_Of (Object));
      while not Ada.Text_IO.End_Of_File (The_File) loop
        declare

          Line  : constant String := Strings.Trimmed (Ada.Text_IO.Get_Line (The_File));
          Parts : constant Strings.Item := Strings.Item_Of (Line, ',');

          function Image_Of (Item : Header) return String is
          begin
            return Strings.Trimmed (Parts(Strings.First_Index + Header'pos(Item)));
          end Image_Of;

          use all type Angle.Value;

        begin
          if Line = "$$SOE" then
            In_Data_Part := True;
          elsif Line = "$$EOE" then
            In_Data_Part := True;
          elsif In_Data_Part and then Parts.Count = (Header'pos(Header'last) + 1) then
            declare
              Ut  : constant Time.Ut     := Time.Ut_Of (Image_Of (Julian_Date));
              Ra  : constant Angle.Value := +Angle.Degrees'value(Image_Of (Right_Ascension));
              Dec : constant Angle.Value := +Angle.Degrees'value(Image_Of (Declination));
              Az  : constant Angle.Value := +Angle.Degrees'value(Image_Of (Azimuth));
              Alt : constant Angle.Value := +Angle.Degrees'value(Image_Of (Altitude));

              function First_Above_Horizon return Element is
                The_Ut  : Time.Ut;
                The_Az  : Angle.Value;
                The_Alt : Angle.Value;
              begin
                if Has_Data_Below_Horizon then
                  The_Ut := The_Data_Below_Horizon.Ut;
                  The_Az := Az_Below_Horizon;
                  The_Alt := Alt_Below_Horizon;
                  while The_Ut < Ut loop
                    The_Az := Values.Interpolation_Of (T  => The_Ut,
                                                       T1 => The_Data_Below_Horizon.Ut,
                                                       T2 => Ut,
                                                       V1 => Az_Below_Horizon,
                                                       V2 => Az);
                    The_Alt := Values.Interpolation_Of (T  => The_Ut,
                                                        T1 => The_Data_Below_Horizon.Ut,
                                                        T2 => Ut,
                                                        V1 => Alt_Below_Horizon,
                                                        V2 => Alt);
                    if Sky_Line.Is_Above (Earth.Direction_Of (Alt => The_Alt, Az => The_Az)) then
                      exit;
                    end if;
                    The_Ut := The_Ut + Time.One_Second;
                  end loop;
                  return (Ut  => The_Ut,
                          Ra => Values.Interpolation_Of (T  => The_Ut,
                                                         T1 => The_Data_Below_Horizon.Ut,
                                                         T2 => Ut,
                                                         V1 => The_Data_Below_Horizon.Ra,
                                                         V2 => Ra),
                          Dec => Values.Interpolation_Of (T  => The_Ut,
                                                          T1 => The_Data_Below_Horizon.Ut,
                                                          T2 => Ut,
                                                          V1 => The_Data_Below_Horizon.Dec,
                                                          V2 => Dec));
                else
                  return (Ut => Ut, Ra => Ra, Dec => Dec);
                end if;
              end First_Above_Horizon;

            begin
              if The_Entry > The_List'last - 3 then
                Log.Warning ("SSSB object too slow");
                The_Entry := The_List'first;
                exit;
              end if;
              if Ut >= Start_Time and then Sky_Line.Is_Above (Earth.Direction_Of (Alt => Alt, Az => Az)) then
                if Has_Data_Below_Horizon then
                  The_Entry := The_Entry + 1;
                  The_List(The_Entry) := First_Above_Horizon; -- Previous Data added;
                  Has_Data_Below_Horizon := False;
                end if;
                The_Entry := The_Entry + 1;
                declare
                  The : Element renames The_List(The_Entry);
                begin
                  The.Ut := Ut;
                  The.Ra := Ra;
                  The.Dec := Dec;
                end;
              else
                The_Data_Below_Horizon := (Ut => Ut, Ra => Ra, Dec => Dec);
                Az_Below_Horizon := Az;
                Alt_Below_Horizon := Alt;
                Has_Data_Below_Horizon := True;
                if The_Entry > 0 then
                  The_Entry := The_Entry + 1;
                  The_List(The_Entry) := The_Data_Below_Horizon; -- Next Data added
                  exit;
                end if;
              end if;
            end;
          elsif The_Entry > 0 and then Line'length > 0 and then Line(Line'first) = '>' then
            -- elevation cut-off requested
            if The_Entry = The_List'first then
              The_Entry := 0; -- at least two entries in a section
              Has_Data_Below_Horizon := False;
            else
              exit;
            end if;
          end if;
        end;
      end loop;
      Ada.Text_IO.Close (The_File);
      if The_Entry > The_List'first then
        The_Last := The_Entry;
      end if;
      return;
    exception
    when Error.Occurred =>
      raise;
    when others =>
      Error.Raise_With ("Incorrect Data in " & Object);
    end Read_Sssb;


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
      end Evaluate_Location;

      procedure Log_Image_Of (Lable : String;
                              Item  : Element) is
      begin
        Log.Write (Lable & Time.Image_Of (Item.Ut) & " - RA : " & Angle.Hours_Image_Of (Item.Ra) &
                                                     " - DEC: " & Angle.Signed_Degrees_Image_Of (Item.Dec));
      end Log_Image_Of;

      Search_Step   : constant Time.Ut := Time.One_Second * 12.0;
      Maximum_Pause : constant Time.Ut := Time.One_Minute * 3.0;

      The_Step  : Time.Ut;
      The_Pause : Time.Ut;

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
          if Sky_Line.Is_Above (The_Direction, The_Lmst) then
            if The_Step = Time.One_Second then
              loop
                The_Entry := The_Entry + 1;
                if The_Entry > The_List'last then
                  Log.Warning ("slow object");
                  The_Last := 0;
                  exit;
                end if;
                The_List(The_Entry).Ut := The_Time;
                The_List(The_Entry).Ra := Space.Ra_Of (The_Direction);
                The_List(The_Entry).Dec := Space.Dec_Of (The_Direction);
                The_Time := The_Time + Time.One_Second;
                The_Lmst := Time.Lmst_Of (The_Time);
                Evaluate_Location;
                if Sky_Line.Is_Above (The_Direction, The_Lmst) then
                  The_Last := The_Entry;
                  The_Pause := Maximum_Pause;
                else
                  The_Pause := The_Pause - The_Step;
                  if The_Pause <= 0.0 then
                    exit;
                  end if;
                end if;
              end loop;
              if The_Last > The_List'first then
                if Log.Is_Enabled then
                  Log_Image_Of ("start: ", The_List(The_List'first));
                  Log_Image_Of ("end  : ", The_List(The_Last));
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

    The_File : Ada.Text_IO.File_Type;

  begin -- read
    The_Last := 0;
    The_Entry := 0;
    Log.Write ("NEO READ - " & Target);
    Ada.Text_IO.Open (The_File, Ada.Text_IO.In_File, Filename_Of (Target));
    begin
      Norad_Lines(1) := Ada.Text_IO.Get_Line (The_File);
      Norad_Lines(2) := Ada.Text_IO.Get_Line (The_File);
      Ada.Text_IO.Close (The_File);
    exception
    when others => -- No NORAD Data
      Ada.Text_IO.Close (The_File);
      Read_Sssb (Target);
      return;
    end;
    Find_Norad_Entries;
  end Read;


  type List_Access is access List;

  The_Lists : array (1..500) of List_Access;


  procedure Add_Objects is

    procedure Process_File (Directory_Entry : Ada.Directories.Directory_Entry_Type) is
      The_Index : Positive;
    begin
      declare
        Filename : constant String := Ada.Directories.Simple_Name (Directory_Entry);
        Object   : constant String := Filename(Filename'first .. Filename'last - Neo.Extension'length - 1);
      begin
        Read (Object);
        if The_Last > 0 then
          The_Index := Data.New_Neo_Object_For (Name        => Object,
                                                Description => "",
                                                The_Type    => Data.Satellite);
          if The_Index > The_Lists'last then
            Error.Raise_With ("Too many near earth objects");
          end if;
          The_Lists(The_Index) := new List'(The_List(1..The_Last));
        end if;
      end;
    end Process_File;

  begin -- Add_Near_Earth_Objects
    Satellite.Read_Stellarium_Data;
    if Ada.Directories.Exists (Neo.Directory) then
      Ada.Directories.Search (Directory => Neo.Directory,
                              Pattern   => "*." & Neo.Extension,
                              Filter    => [Ada.Directories.Ordinary_File => True, others => False],
                              Process   => Process_File'access);
    end if;
  end Add_Objects;


  function Direction_Of (Item : Name.Id;
                         Ut   : Time.Ut) return Space.Direction is

    Index : constant Positive := Data.Neo_Index_Of (Name.Object_Of (Item));

    Data  : List_Access renames The_Lists(Index);

  begin
    if Data /= null then
      for The_Index in Data.all'range loop
        if The_Index < Data.all'last and then Data(The_Index + 1).Ut >= Ut then
          return Space.Direction_Of (Ra => Values.Interpolation_Of (T  => Ut,
                                                                    T1 => Data(The_Index).Ut,
                                                                    T2 => Data(The_Index + 1).Ut,
                                                                    V1 => Data(The_Index).Ra,
                                                                    V2 => Data(The_Index + 1).Ra),
                                     Dec => Values.Interpolation_Of (T  => Ut,
                                                                     T1 => Data(The_Index).Ut,
                                                                     T2 => Data(The_Index + 1).Ut,
                                                                     V1 => Data(The_Index).Dec,
                                                                     V2 => Data(The_Index + 1).Dec));
        end if;
      end loop;
    end if;
    return Space.Unknown_Direction;
  end Direction_Of;


  procedure Dispose is new Ada.Unchecked_Deallocation (List, List_Access);

  function Is_Arriving (Item : Name.Id) return Boolean is
    Index : constant Positive := Data.Neo_Index_Of (Name.Object_Of (Item));
    Data  : List_Access renames The_Lists(Index);
  begin
    if Data = null then
      return False;
    elsif Data(Data'last).Ut < Time.Universal then
      Dispose (Data);
      Read (Name.Image_Of (Item));
      if The_Last = 0 then
        Data := null;
        return False;
      end if;
      Data := new List'(The_List(1..The_Last));
    end if;
    return Data(Data'first).Ut < Time.Universal + 15.0 * Time.One_Minute;
  end Is_Arriving;


  function Tracking_Period_Of (Item : Name.Id) return Time.Period is
    Index : constant Positive := Data.Neo_Index_Of (Name.Object_Of (Item));
    Data  : List_Access renames The_Lists(Index);
  begin
    if Data = null then
      return Time.Undefined;
    else
      return (Arrival_Time => Data(Data'first).Ut,
              Leaving_Time  => Data(Data'last).Ut);
    end if;
  end Tracking_Period_Of;

end Neo;
