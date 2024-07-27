-- *********************************************************************************************************************
-- *                           (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
with Lexicon;
with Error;
with Sky.Catalog;
with Text;
with Traces;

package body Sky.Data is

  package Log is new Traces ("Sky.Data");

  use Astro;
  use Astro.PNULIB;
  use Astro.SPHLIB;

  Pn_Mat  : REAL33;
  Ve      : VECTOR;

  Max_Star_Magnitude : constant Magnitude := 7.0; -- limits the size of the selection tables in the GUI;

  First_Moon_Feature : constant Object := Catalog.Number'last + 1;

  Last_Moon_Feature : constant Object := First_Moon_Feature + Moon_Feature'pos(Moon_Feature'last);

  subtype Moon_Object is Object range First_Moon_Feature .. Last_Moon_Feature;

  Number_Of_Extension_Objects : constant := 1000;

  First_Extension : constant Object := Last_Moon_Feature + 1;

  subtype Extension_Object is Object range First_Extension .. First_Extension + Number_Of_Extension_Objects - 1;

  type Information is record
    Name        : Text.String;
    Descriptor  : Text.String;
    Object_Kind : Object_Type;
    Ra_J2000    : Angle.Degrees;
    Dec_J2000   : Angle.Degrees;
  end record;

  use type Text.String;

  type Extension_Objects is array (Extension_Object) of Information;

  The_Extension_Table : Extension_Objects;

  The_Last_Extension : Object := First_Extension - 1;

  The_Last_Neo  : Object := Undefined;
  The_First_Neo : Object := The_Last_Neo + 1; -- no objects


  function Find_Element (From_Direction : Space.Direction;
                         With_Tolerance : Space.Distance) return Element is

    The_Object   : Index;
    The_Distance : Space.Distance;

    function Found return Boolean is
      The_Direction : constant Space.Direction := Direction_Of (The_Object, Time.Universal);
    begin
      The_Distance := Space.Angle_Between (From_Direction, The_Direction);
      if The_Distance <= With_Tolerance then
        return True;
      end if;
      return False;
    end Found;

    Dec : constant Angle.Value := Space.Dec_Of (From_Direction);

    use type Angle.Signed;

    Start_Object : constant Index := Catalog.Found_For (+Angle.Signed'(+Dec));
    The_Offset   : Integer := 0;

    procedure Next is
    begin
      if The_Offset > 0 then
        The_Offset := - @;
      else
        The_Offset := (abs @) + 1;
      end if;
      The_Object := Index(Integer(Start_Object) + The_Offset);
    end Next;

    The_Best_Object : Object := Undefined;
    Min_Distance    : Space.Distance := Space.Distance'last;

    use type Angle.Value;

  begin -- Find_Element
    The_Object := Start_Object;
    Search:
    loop
      while not Found loop
        Next;
        exit Search when The_Offset > Max_Search_Distance;
      end loop;
      if Min_Distance > The_Distance then
        Min_Distance := The_Distance;
        The_Best_Object := The_Object;
      end if;
      Next;
    end loop Search;
    if The_Best_Object = Undefined then
      Log.Write ("Not found in tolerance " & Angle.Image_Of (+With_Tolerance));
      return No_Element;
    end if;
    declare
      Main_Id : constant Simbad_Catalog := Catalog.Main_Catalog_Of (The_Best_Object);
    begin
      Log.Write ("Found: direction : " & Space.Image_Of (From_Direction));
      Log.Write ("       tolerance : " & Angle.Image_Of (+With_Tolerance));
      Log.Write ("       object    :"  & The_Best_Object'image);
      return (Catalog => Main_Id,
              Number  => Catalog.Position_Of (Main_Id, The_Best_Object));
    exception
    when others =>
      return No_Element;
    end;
  end Find_Element;


  Delta_Factor : constant REAL := Time.T_Second / 36000.0;

  function Direction_Of (Ra_J2000   : REAL;
                         Dec_J2000  : REAL;
                         Ra_Motion  : REAL;
                         Dec_Motion : REAL;
                         Ut         : Time.Ut;
                         Is_J2000   : Boolean) return Space.Direction is
    Ra, Dec : REAL;
    Delta_T : constant REAL := REAL(Ut) * Delta_Factor;
  begin
    Ra := Ra_J2000 + Ra_Motion * Delta_T;
    Dec := Dec_J2000 + Dec_Motion * Delta_T;
    if not Is_J2000 then
      Apparent (Ra, Dec);
    end if;
    return Space.Direction_Of (Dec => Dec,
                               Ra  => Ra);
  end Direction_Of;


  function Moon_Feature_Name_Of (Item : Moon_Feature) return String is
  begin
    return Lexicon.Image_Of (Lexicon.Word'value(Item'image));
  end Moon_Feature_Name_Of;


  function Moon_Feature_Name_Of (Id : Index) return String is
  begin
    return Moon_Feature_Name_Of (Moon_Feature_Of (Id));
  end Moon_Feature_Name_Of;


  function Name_Of (Id : Index) return String is
  begin
    if Id in Catalog.Index then
      return Catalog.Name_Of (Catalog.Index(Id));
    elsif Id in Moon_Object then
      return Moon_Feature_Name_Of (Id);
    else
      return +The_Extension_Table(Id).Name;
    end if;
  end Name_Of;


  function Name_Of (Item     : Positive;
                    The_Kind : Extended_Catalogs) return String is

    Number : constant Object := Object_Of (Item, The_Kind);

  begin -- Name_Of
    case The_Kind is
    when Neo =>
      return +The_Extension_Table(Number).Name;
    when Moon =>
      return Moon_Feature_Name_Of (Number);
    when Catalogs =>
      return Catalog.Name_Of (Item, The_Kind);
    end case;
  end Name_Of;


  function Descriptor_Of (Id : Index) return String is
  begin
    if Id in Catalog.Index then
      return Catalog.Descriptor_Of (Id);
    else
      return +The_Extension_Table(Id).Descriptor;
    end if;
  end Descriptor_Of;


  function Object_Type_Of (Id : Index) return Object_Type is
  begin
    if Id in Catalog.Index then
      return Catalog.Kind_Of (Id);
    elsif Id in Moon_Object then
      return Moon;
    else
      return The_Extension_Table(Id).Object_Kind;
    end if;
  end Object_Type_Of;


  function Ra_J2000_Of (Id : Index) return REAL with Inline is
  begin
    if Id in Catalog.Index then
      return Catalog.Ra_J2000_Of (Id);
    else
      return The_Extension_Table(Id).Ra_J2000;
    end if;
  end Ra_J2000_Of;


  function Dec_J2000_Of (Id : Index) return REAL with Inline is
  begin
    if Id in Catalog.Index then
      return Catalog.Dec_J2000_Of (Id);
    else
      return The_Extension_Table(Id).Dec_J2000;
    end if;
  end Dec_J2000_Of;


  function Ra_Motion_Of (Id : Index) return REAL with Inline is
  begin
    if Id in Catalog.Index then
      return Catalog.Ra_Motion_Of (Id);
    else
      return 0.0;
    end if;
  end Ra_Motion_Of;


  function Dec_Motion_Of (Id : Index) return REAL with Inline is
  begin
    if Id in Catalog.Index then
      return Catalog.Dec_Motion_Of (Id);
    else
      return 0.0;
    end if;
  end Dec_Motion_Of;


  function Magnitude_Of (Id : Index) return Sky.Magnitude is
  begin
    if Id in Catalog.Index then
      return Catalog.Magnitude_Of (Id);
    else
      return Sky.Unknown_Magnitude;
    end if;
  end Magnitude_Of;


  function Moon_Feature_Of (Id : Object) return Moon_Feature is
  begin
    if Id in Moon_Object then
      return Moon_Feature'val(Id - First_Moon_Feature);
    else
      raise Program_Error;
    end if;
  end Moon_Feature_Of;


  function Moon_Feature_Number_Of (Item : String) return Positive is
  begin
    return Moon_Feature'pos(Moon_Feature'value(Lexicon.Word_Of (Item)'image)) + 1;
  end Moon_Feature_Number_Of;


  function Object_Of (Item     : Positive;
                      The_Kind : Extended_Catalogs) return Object is
    The_Object : Object;
  begin
    case The_Kind is
    when Catalogs =>
      The_Object := Catalog.Object_Of (Item, The_Kind);
    when Moon =>
      The_Object := First_Moon_Feature + Object(Item) - 1;
      if The_Object > Last_Moon_Feature then
        The_Object := Undefined;
      end if;
    when Neo =>
      The_Object := The_First_Neo + Object(Item) - 1;
      if The_Object > The_Last_Neo then
        The_Object := Undefined;
      end if;
    end case;
    return The_Object;
  end Object_Of;


  function Next_Of (Item     : Natural;
                    The_Kind : Extended_Catalogs) return Natural is
    The_Item   : Natural := Item;
    The_Object : Object;
  begin
    loop
      loop
        The_Item := @ + 1;
        The_Object := Object_Of (The_Item, The_Kind);
        if The_Kind in Neo | Moon then
          if The_Object = Undefined then
            return No_More;
          end if;
          return The_Item;
        end if;
        exit when The_Object /= Undefined;
      end loop;
      if not (The_Kind in Hd | Hip) -- not large catalogs
        or else Catalog.Kind_Of (The_Object) /= Star
        or else Catalog.Magnitude_Of (The_Object) <= Max_Star_Magnitude
      then
        return The_Item;
      end if;
    end loop;
  exception
  when others =>
    return No_More;
  end Next_Of;


  function Direction_Of (Id       : Index;
                         Ut       : Time.Ut;
                         Is_J2000 : Boolean := False) return Space.Direction is
  begin
    return Direction_Of (Ra_J2000   => Ra_J2000_Of (Id),
                         Dec_J2000  => Dec_J2000_Of (Id),
                         Ra_Motion  => Ra_Motion_Of (Id),
                         Dec_Motion => Dec_Motion_Of (Id),
                         Ut         => Ut,
                         Is_J2000   => Is_J2000);
  end Direction_Of;


  function New_Object_For (Item        : String;
                           Description : String;
                           Object_Kind : Object_Type := Unknown;
                           Direction   : Space.Direction := Space.Unknown_Direction) return Index is
    use all type Angle.Value;
  begin
    if The_Last_Extension = The_Extension_Table'last then
      Error.Raise_With ("Too many Objects declared - " & Item & " not stored");
    end if;
    The_Last_Extension := The_Last_Extension + 1;
    if Space.Direction_Is_Known (Direction) then
      The_Extension_Table (The_Last_Extension) :=
       (Name        => [Item],
        Descriptor  => [Description],
        Ra_J2000    => Angle.Degrees'(+Space.Ra_Of (Direction)),
        Dec_J2000   => Angle.Degrees'(+Space.Dec_Of (Direction)),
        Object_Kind => Object_Kind);
    else
      The_Extension_Table (The_Last_Extension) :=
       (Name        => [Item],
        Descriptor  => [Description],
        Ra_J2000    => 0.0,
        Dec_J2000   => 0.0,
        Object_Kind => Object_Kind);
    end if;
    return The_Last_Extension;
  end New_Object_For;


  function Neo_Object_Of (Item : String) return Index is
  begin
    for The_Index in The_First_Neo .. The_Last_Neo loop
      if The_Extension_Table(The_Index).Name = Item then
        return The_Index;
      end if;
    end loop;
    raise Program_Error;
  end Neo_Object_Of;


  function New_Neo_Object_For (Item        : String;
                               Description : String) return Positive is
  begin
    if The_Last_Neo = Undefined then
      The_First_Neo := The_Last_Extension + 1;
    end if;
    The_Last_Neo := New_Object_For (Item        => Item,
                                    Description => Description,
                                    Object_Kind => Satellite);
    return Neo_Index_Of (The_Last_Neo);
  end New_Neo_Object_For;


  function Neo_Index_Of (Id : Index) return Positive is
  begin
    return Positive(Id + 1 - The_First_Neo);
  end Neo_Index_Of;


  procedure Apparent (Ra  : in out REAL;
                      Dec : in out REAL) is
  begin
    APPARENT (Pn_Mat, Ve, Ra, Dec);
  end Apparent;

  T : constant Time.T := Time.Tut;

begin
  PN_MATRIX (Time.T_J2000, T, Pn_Mat);
  ABERRAT (T, Ve);
end Sky.Data;
