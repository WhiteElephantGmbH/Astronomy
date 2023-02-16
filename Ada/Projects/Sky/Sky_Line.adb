-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Directories;
with Ada.Text_IO;
with Application;
with Error;
with File;
with Objects;
with Strings;
with Traces;
with Values;

package body Sky_Line is

  package Log is new Traces ("Sky_Line");

  type Element is record
    Azimuth      : Angle.Value;
    Altitude     : Angle.Value;
    Top_Altitude : Angle.Value;
  end record;

  Angle_Divisor : constant := 2**12;

  type Index is mod Angle_Divisor;

  type Horizon is array (Index) of Angle.Value;

  use type Angle.Value;

  Azimuth_Increment : constant Angle.Value := +Angle.Unsigned(2**32 / Angle_Divisor);

  The_Lower_Horizon : Horizon;
  The_Upper_Horizon : Horizon;


  function "<" (Left, Right : Element) return Boolean is
  begin
    return Left.Azimuth < Right.Azimuth;
  end "<";


  function Lower_Interpolation (Azimuth : Angle.Value;
                                First   : Element;
                                Second  : Element) return Angle.Value is
  begin
    return Values.Interpolation_Of (Az   => Azimuth,
                                    Az1  => First.Azimuth,
                                    Alt1 => First.Altitude,
                                    Az2  => Second.Azimuth,
                                    Alt2 => Second.Altitude);
  end Lower_Interpolation;


  function Upper_Interpolation (Azimuth : Angle.Value;
                                First   : Element;
                                Second  : Element) return Angle.Value is
  begin
    return Values.Interpolation_Of (Az   => Azimuth,
                                    Az1  => First.Azimuth,
                                    Alt1 => First.Top_Altitude,
                                    Az2  => Second.Azimuth,
                                    Alt2 => Second.Top_Altitude);
  end Upper_Interpolation;


  package Elements is new Ada.Containers.Doubly_Linked_Lists (Element);

  package Element_Sorting is new Elements.Generic_Sorting;

  procedure Sort (The_List : in out Elements.List) renames Element_Sorting.Sort;

  The_Element_List : Elements.List;

  Directory : constant String := Application.Composure ("Sky Line");
  Extension : constant String := "csv";


  function Actual_Filename return String is
  begin
    return File.Composure (Directory, "Horizon", Extension);
  end Actual_Filename;


  The_File : Ada.Text_IO.File_Type;

  procedure Create_File (Filename : String) is
  begin
    Log.Write ("Create; " & Filename);
    Ada.Directories.Create_Path (Directory);
    Ada.Text_IO.Create (The_File, Name => Filename);
    Ada.Text_IO.Put (The_File, Strings.Bom_8);
  end Create_File;


  procedure Put (Line : String) is
  begin
    Ada.Text_IO.Put_Line (The_File, Line);
  end Put;


  procedure Close_File is
  begin
    Ada.Text_IO.Close (The_File);
  end Close_File;


  procedure Read is

    Filename : constant String := Actual_Filename;

    procedure Create_Default_Sky_Line is
    begin
        Create_File (Filename);
        Put ("0°00', 2°00'");
        Close_File;
    exception
    when others =>
      Error.Raise_With ("Can't create " & Filename);
    end Create_Default_Sky_Line;


    procedure Store (Line : String) is

      The_Index : Natural := Line'first;

      function Next return String is
        First    : constant Natural := The_Index;
        The_Last : Natural := 0;
      begin
        while The_Index <= Line'last loop
          if Line(The_Index) = ',' then
            The_Index := The_Index + 1;
            exit;
          end if;
          The_Last := The_Index;
          The_Index := The_Index + 1;
        end loop;
        return Strings.Trimmed(Line(First .. The_Last));
      end Next;

      The_Azimuth      : Angle.Value;
      The_Altitude     : Angle.Value;
      The_Top_Altitude : Angle.Value;

    begin -- Store
      if Strings.Trimmed (Line) /= "" then
        begin
          The_Azimuth := Angle.Value_Of (Next);
        exception
        when others =>
          Error.Raise_With ("Incorrect azimuth in " & Filename & ": " & Line);
        end;
        begin
          The_Altitude := Angle.Value_Of (Next);
        exception
        when others =>
          Error.Raise_With ("Incorrect altitude in " & Filename & ": " & Line);
        end;
        declare
          Top_Altitude_Image : constant String := Next;
        begin
          if Top_Altitude_Image = "" then
            The_Top_Altitude := The_Altitude;
          else
            The_Top_Altitude := Angle.Value_Of (Top_Altitude_Image);
          end if;
        exception
        when others =>
          Error.Raise_With ("Incorrect top altitude in " & Filename & ": " & Line);
        end;
        The_Element_List.Append (Element'(Azimuth      => The_Azimuth,
                                          Altitude     => The_Altitude,
                                          Top_Altitude => The_Top_Altitude));
      end if;
    end Store;

    The_Index   : Index := Index'first;
    The_Last    : Element;
    The_Azimuth : Angle.Value := Angle.Zero;

    procedure Create_Horizon is
    begin
      if The_Element_List.Is_Empty then
        The_Lower_Horizon := [others => Angle.Zero];
        The_Upper_Horizon := [others => Angle.Zero];
        return;
      elsif Natural(The_Element_List.Length) = 1 then
        The_Lower_Horizon := [others => The_Element_List.First_Element.Altitude];
        The_Upper_Horizon := [others => The_Element_List.First_Element.Top_Altitude];
      else
        The_Last := The_Element_List.Last_Element;
        for The_Element of The_Element_List loop
          while The_Azimuth < The_Element.Azimuth loop
            The_Lower_Horizon(The_Index) := Lower_Interpolation (The_Azimuth, The_Last, The_Element);
            The_Upper_Horizon(The_Index) := Upper_Interpolation (The_Azimuth, The_Last, The_Element);
            The_Index := The_Index + 1;
            if The_Index = Index'first then
              return;
            end if;
            The_Azimuth := The_Azimuth + Azimuth_Increment;
          end loop;
          The_Last := The_Element;
        end loop;
        loop
          The_Lower_Horizon(The_Index) := Lower_Interpolation (The_Azimuth, The_Last, The_Element_List.First_Element);
          The_Upper_Horizon(The_Index) := Upper_Interpolation (The_Azimuth, The_Last, The_Element_List.First_Element);
          The_Index := The_Index + 1;
          exit when The_Index = Index'first;
          The_Azimuth := The_Azimuth + Azimuth_Increment;
        end loop;
      end if;
      Log.Write ("Horizon created");
    end Create_Horizon;

  begin -- Read
    if not File.Exists (Filename) then
      Create_Default_Sky_Line;
    end if;
    The_Element_List.Clear;
    if File.Exists (Filename) then
      Ada.Text_IO.Open (The_File, Ada.Text_IO.In_File, Filename);
      if Ada.Text_IO.End_Of_File (The_File) then
        Error.Raise_With (Filename & " is empty");
      end if;
      declare
        Has_Bom : constant Boolean := Strings.Has_Skipped_Bom_8 (The_File);
      begin
        while not Ada.Text_IO.End_Of_File (The_File) loop
          declare
            Line : constant String := Ada.Text_IO.Get_Line (The_File);
          begin
            Store ((if Has_Bom then Line else Strings.Utf8_Of (Line)));
          end;
        end loop;
      end;
      Ada.Text_IO.Close (The_File);
    end if;
    Sort (The_Element_List);
    Create_Horizon;
  end Read;


  procedure Create is
  begin
    Create_File (Actual_Filename);
  end Create;


  procedure Append (Direction : Earth.Direction;
                    Top_Alt   : Angle.Value := No_Top_Altitude) is
  begin
    Put (Earth.Az_Image_Of (Direction) & ", " & Earth.Alt_Image_Of (Direction)
         & (if Top_Alt = No_Top_Altitude then "" else ", " & Angle.Image_Of (Top_Alt)));
  end Append;


  procedure Close is
  begin
    Close_File;
  end Close;


  procedure Clear is
  begin
    File.Delete (Actual_Filename);
  end Clear;


  procedure Add (Direction : Earth.Direction) is
    Filename : constant String := Actual_Filename;
  begin
    if Earth.Direction_Is_Known (Direction) then
      if File.Exists (Filename) then
        Ada.Text_IO.Open (The_File, Ada.Text_IO.Append_File, Filename);
      else
        Create_File (Filename);
      end if;
      Append (Direction);
      Close_File;
      Read;
    end if;
  exception
  when others =>
    Log.Error ("Add failed");
    Clear;
  end Add;


  function Is_Defined return Boolean is
  begin
    return File.Exists (Actual_Filename);
  end Is_Defined;


  function Is_Above (Direction : Earth.Direction;
                     Use_Upper : Boolean := False) return Boolean is
    use type Angle.Unsigned;
  begin
    if Earth.Is_Below_Horizon (Direction) then
      return False;
    end if;
    declare
      Az    : constant Angle.Value    := Earth.Az_Of (Direction);
      Inc   : constant Angle.Unsigned := +Azimuth_Increment;
      I1    : constant Index          := Index(Angle.Unsigned'(+Az) / Inc);
      I2    : constant Index          := I1 + 1;
      Az1   : constant Angle.Value    := +(Angle.Unsigned(I1) * Inc);
      Limit : constant Angle.Value
        := Values.Interpolation_Of (Az   => Az,
                                    Az1  => Az1,
                                    Alt1 => (if Use_Upper then The_Upper_Horizon(I1) else The_Lower_Horizon(I1)),
                                    Az2  => Az1 + Azimuth_Increment,
                                    Alt2 => (if Use_Upper then The_Upper_Horizon(I2) else The_Lower_Horizon(I2)));
    begin
      return Limit < Earth.Alt_Of (Direction);
    end;
  end Is_Above;


  function Is_Above (Direction : Space.Direction;
                     Lmst      : Time.Value;
                     Use_Upper : Boolean := False) return Boolean is
  begin
    return Is_Above (Objects.Direction_Of (Direction, Lmst), Use_Upper);
  end Is_Above;

end Sky_Line;
