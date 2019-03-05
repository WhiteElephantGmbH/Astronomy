-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Angle;
with Application;
with Definite_Doubly_Linked_Lists;
with Parameter;
with Error;
with File;
with Numerics;
with Stellarium;
with Strings;
with Traces;

package body Sky_Line is

  package Log is new Traces ("Sky_Line");

  type Element is record
    Azimuth  : Angle.Value;
    Altitude : Angle.Value;
  end record;

  Angle_Divisor : constant := 2**12;

  type Index is mod Angle_Divisor;

  type Horizon is array (Index) of Angle.Value;

  use type Angle.Value;

  Azimuth_Increment : constant Angle.Value := +Angle.Unsigned(2**32 / Angle_Divisor);

  The_Horizon : Horizon;


  function "<" (Left, Right : Element) return Boolean is
  begin
    return Left.Azimuth < Right.Azimuth;
  end "<";


  function Interpolation_Of (Azimuth : Angle.Value;
                             First   : Element;
                             Second  : Element) return Angle.Value is
  begin
    return Numerics.Interpolation_Of (Az   => Azimuth,
                                      Az1  => First.Azimuth,
                                      Alt1 => First.Altitude,
                                      Az2  => Second.Azimuth,
                                      Alt2 => Second.Altitude);
  end Interpolation_Of;


  package Element_List is new Definite_Doubly_Linked_Lists (Element);

  package Element_Sorting is new Element_List.Generic_Sorting;

  procedure Sort (The_List : in out Element_List.Item) renames Element_Sorting.Sort;

  The_Element_List : Element_List.Item;

  Directory : constant String := Application.Composure ("Sky Line");
  Extension : constant String := "csv";


  function Actual_Filename return String is
    Name      : constant String := Parameter.Sky_Line;
    Landscape : constant String := (if Name = "" then Stellarium.Landscape else Name);
  begin
    if Landscape = "" then
      return "";
    else
      return File.Composure (Directory, Landscape, Extension);
    end if;
  end Actual_Filename;


  The_File : Ada.Text_IO.File_Type;

  procedure Create_File (Filename : String) is
  begin
    Log.Write ("Create; " & Filename);
    Ada.Directories.Create_Path (Directory);
    Ada.Text_IO.Create (The_File, Name => Filename);
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

      use all type Parameter.Location;

    begin -- Create_Default_Sky_Line
      if Parameter.Default_Location = Home and then Parameter.Sky_Line = "Home" then
        Create_File (Filename);
        Put (Strings.Bom_8 & "0°00', 17°20'");
        Put ("5°00', 18°00'");
        Put ("10°00', 19°20'");
        Put ("15°00', 20°00'");
        Put ("20°00', 20°40'");
        Put ("25°00', 21°20'");
        Put ("30°20', 21°40'");
        Put ("35°00', 22°00'");
        Put ("40°00', 22°20'");
        Put ("47°00', 22°00'");
        Put ("48°00', 28°30'");
        Put ("50°30', 39°00'");
        Put ("53°00', 28°30'");
        Put ("54°20', 21°20'");
        Put ("60°00', 21°00'");
        Put ("62°00', 22°00'");
        Put ("63°20', 22°00'");
        Put ("64°00', 20°20'");
        Put ("70°00', 19°10'");
        Put ("75°00', 18°50'");
        Put ("80°00', 17°20'");
        Put ("85°00', 16°00'");
        Put ("90°00', 14°40'");
        Put ("94°00', 13°30'");
        Put ("94°10', 9°00'");
        Put ("100°00', 8°00'");
        Put ("105°00', 7°00'");
        Put ("110°00', 6°00'");
        Put ("115°00', 5°00'");
        Put ("118°00', 8°00'");
        Put ("120°00', 7°40'");
        Put ("122°00', 8°30'");
        Put ("125°00', 8°00'");
        Put ("126°00', 8°00'");
        Put ("128°00', 3°30'");
        Put ("130°00', 3°40'");
        Put ("135°00', 5°40'");
        Put ("140°00', 6°00'");
        Put ("145°00', 6°00'");
        Put ("150°00', 5°20'");
        Put ("153°00', 5°00'");
        Put ("153°10', 8°00'");
        Put ("155°00', 9°00'");
        Put ("157°00', 7°00'");
        Put ("160°00', 10°30'");
        Put ("165°00', 10°20'");
        Put ("170°00', 11°00'");
        Put ("175°00', 12°30'");
        Put ("180°00', 12°00'");
        Put ("182°00', 9°30'");
        Put ("189°00', 10°00'");
        Put ("192°00', 8°30'");
        Put ("192°30', 6°30'");
        Put ("198°30', 6°30'");
        Put ("200°00', 9°00'");
        Put ("205°00', 13°00'");
        Put ("210°00', 14°00'");
        Put ("215°00', 13°00'");
        Put ("220°00', 12°00'");
        Put ("221°00', 21°00'");
        Put ("225°00', 26°00'");
        Put ("230°00', 23°00'");
        Put ("235°00', 8°00'");
        Put ("240°00', 9°00'");
        Put ("245°00', 13°30'");
        Put ("250°00', 20°30'");
        Put ("255°00', 12°00'");
        Put ("260°00', 16°00'");
        Put ("265°00', 20°00'");
        Put ("268°00', 22°00'");
        Put ("272°00', 20°00'");
        Put ("275°00', 22°40'");
        Put ("277°00', 22°40'");
        Put ("278°00', 20°00'");
        Put ("285°00', 24°00'");
        Put ("290°00', 23°40'");
        Put ("295°00', 22°00'");
        Put ("300°00', 17°00'");
        Put ("303°00', 17°20'");
        Put ("308°00', 16°00'");
        Put ("310°00', 17°00'");
        Put ("315°00', 18°00'");
        Put ("320°00', 16°00'");
        Put ("325°00', 17°00'");
        Put ("330°00', 15°00'");
        Put ("331°00', 12°00'");
        Put ("340°00', 11°00'");
        Put ("344°00', 10°00'");
        Put ("350°00', 15°00'");
        Put ("355°00', 16°00'");
        Close_File;
      elsif Parameter.Default_Location = Sternwarte_Schaffhausen and then Parameter.Sky_Line = "Newton" then
        Create_File (Filename);
        Put (Strings.Bom_8 & "000°00', +22°40'");
        Put ("007°00', +21°50'");
        Put ("007°01', +13°40'");
        Put ("008°50', +13°40'");
        Put ("009°00', +10°20'");
        Put ("011°00', +10°20'");
        Put ("012°00', +10°40'");
        Put ("015°00', +10°40'");
        Put ("015°20', +11°40'");
        Put ("017°00', +13°00'");
        Put ("018°00', +13°00'");
        Put ("018°20', +12°20'");
        Put ("020°40', +12°20'");
        Put ("021°40', +12°50'");
        Put ("024°00', +12°40'");
        Put ("024°20', +11°00'");
        Put ("030°00', +12°00'");
        Put ("035°00', +12°50'");
        Put ("040°00', +13°40'");
        Put ("045°00', +14°20'");
        Put ("050°00', +15°00'");
        Put ("055°00', +15°30'");
        Put ("060°00', +16°00'");
        Put ("065°40', +16°20'");
        Put ("066°00', +5°00'");
        Put ("069°20', +5°00'");
        Put ("071°30', +6°40'");
        Put ("073°20', +5°00'");
        Put ("096°30', +5°00'");
        Put ("096°40', +15°40'");
        Put ("111°40', +14°20'");
        Put ("115°00', +15°00'");
        Put ("120°00', +16°10'");
        Put ("125°00', +17°10'");
        Put ("130°00', +18°10'");
        Put ("135°00', +19°00'");
        Put ("140°00', +19°40'");
        Put ("145°00', +20°10'");
        Put ("150°00', +20°50'");
        Put ("155°00', +21°10'");
        Put ("160°00', +21°20'");
        Put ("165°00', +21°20'");
        Put ("170°00', +21°10'");
        Put ("175°00', +21°00'");
        Put ("180°00', +20°40'");
        Put ("185°00', +20°20'");
        Put ("190°00', +19°40'");
        Put ("195°00', +19°00'");
        Put ("200°00', +18°00'");
        Put ("205°00', +17°00'");
        Put ("210°00', +16°00'");
        Put ("215°00', +14°50'");
        Put ("220°00', +13°20'");
        Put ("225°00', +12°00'");
        Put ("227°40', +11°00'");
        Put ("227°41', +12°00'");
        Put ("237°20', +12°40'");
        Put ("240°40', +10°20'");
        Put ("240°41', +7°00'");
        Put ("256°00', +7°00'");
        Put ("256°10', +5°40'");
        Put ("277°40', +5°40'");
        Put ("279°20', +7°00'");
        Put ("281°20', +6°40'");
        Put ("282°40', +6°20'");
        Put ("282°50', +10°00'");
        Put ("289°40', +9°40'");
        Put ("290°00', +10°20'");
        Put ("295°00', +11°10'");
        Put ("296°20', +11°20'");
        Put ("296°21', +18°00'");
        Put ("300°00', +18°40'");
        Put ("305°00', +19°50'");
        Put ("310°00', +20°50'");
        Put ("315°00', +21°40'");
        Put ("320°00', +22°20'");
        Put ("325°00', +23°00'");
        Put ("330°00', +23°20'");
        Put ("335°00', +23°30'");
        Put ("340°00', +23°40'");
        Put ("345°00', +23°30'");
        Put ("350°00', +23°20'");
        Put ("355°00', +23°00'");
        Close_File;
      end if;
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

      The_Azimuth  : Angle.Value;
      The_Altitude : Angle.Value;

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
        The_Element_List.Append (Element'(Azimuth => The_Azimuth,
                                          Altitude => The_Altitude));
      end if;
    end Store;

    The_Index   : Index := Index'first;
    The_Last    : Element;
    The_Azimuth : Angle.Value := Angle.Zero;

    procedure Create_Horizon is
    begin
      if The_Element_List.Is_Empty then
        The_Horizon := (others => Angle.Zero);
        return;
      elsif The_Element_List.Count = 1 then
        The_Horizon := (others => The_Element_List.First_Element.Altitude);
      else
        The_Last := The_Element_List.Last_Element;
        for The_Element of The_Element_List loop
          while The_Azimuth < The_Element.Azimuth loop
            The_Horizon(The_Index) := Interpolation_Of (The_Azimuth, The_Last, The_Element);
            The_Index := The_Index + 1;
            if The_Index = Index'first then
              return;
            end if;
            The_Azimuth := The_Azimuth + Azimuth_Increment;
          end loop;
          The_Last := The_Element;
        end loop;
        loop
          The_Horizon(The_Index) := Interpolation_Of (The_Azimuth, The_Last, The_Element_List.First_Element);
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


  procedure Clear is
  begin
    File.Delete (Actual_Filename);
  end Clear;


  procedure Add (Direction : Earth.Direction) is
    Filename : constant String := Actual_Filename;
  begin
    declare
      Line : constant String := Earth.Az_Image_Of (Direction) & ", " & Earth.Alt_Image_Of (Direction);
    begin
      if File.Exists (Filename) then
        Ada.Text_IO.Open (The_File, Ada.Text_IO.Append_File, Filename);
        Put (Line);
      else
        Create_File (Filename);
        Put (Strings.Bom_8 & Line);
      end if;
    end;
    Close_File;
    Read;
  exception
  when others =>
    Log.Error ("Add failed");
    Clear;
  end Add;


  function Is_Defined return Boolean is
  begin
    return File.Exists (Actual_Filename);
  end Is_Defined;


  function Is_Above (Direction : Earth.Direction) return Boolean is
    use type Angle.Unsigned;
  begin
    if Earth.Is_Below_Horizon (Direction) then
      return False;
    end if;
    declare
      Az    : constant Angle.Value    := Earth.Az_Of (Direction);
      Inc   : constant Angle.Unsigned := +Azimuth_Increment;
      I1    : constant Index          := Index(Angle.Unsigned'(+Az) / Inc);
      Az1   : constant Angle.Value    := +(Angle.Unsigned(I1) * Inc);
      Limit : constant Angle.Value    := Numerics.Interpolation_Of (Az   => Az,
                                                                    Az1  => Az1,
                                                                    Alt1 => The_Horizon(I1),
                                                                    Az2  => Az1 + Azimuth_Increment,
                                                                    Alt2 => The_Horizon(I1 + 1));
    begin
      return Limit < Earth.Alt_Of (Direction);
    end;
  end Is_Above;


  function Is_Above (Direction : Space.Direction;
                     Lmst      : Time.Value) return Boolean is
  begin
    return Is_Above (Numerics.Direction_Of (Direction, Lmst));
  end Is_Above;

end Sky_Line;
