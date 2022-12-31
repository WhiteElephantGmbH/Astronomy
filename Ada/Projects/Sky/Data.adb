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

with Angle;
with Error;

package body Data is

  use Astro;
  use Astro.PNULIB;
  use Astro.SPHLIB;

  Pn_Mat  : REAL33;
  Ve      : VECTOR;

  Number_Of_Extension_Objects : constant := 1000;

  use type Object;

  First_Extension : constant Object := Catalog.Last_Index + 1;

  subtype Extension_Object is Object range First_Extension .. First_Extension + Number_Of_Extension_Objects - 1;

  type Extension_Objects is array (Extension_Object) of Catalog.Information;

  The_Extension_Table : Extension_Objects;

  The_Last_Extension : Object := First_Extension - 1;

  The_Last_Neo  : Object := Undefined;
  The_First_Neo : Object := The_Last_Neo + 1; -- no objects


  procedure Apparent (Ra  : in out REAL;
                      Dec : in out REAL) is
  begin
    APPARENT (Pn_Mat, Ve, Ra, Dec);
  end Apparent;


  Delta_Factor : constant REAL := Time.T_Second / 36000.0;

  function Direction_Of (Ra_J2000   : REAL;
                         Dec_J2000  : REAL;
                         Ra_Motion  : REAL;
                         Dec_Motion : REAL;
                         Ut         : Time.Ut) return Space.Direction is
    Ra, Dec : REAL;
    Delta_T : constant REAL := REAL(Ut) * Delta_Factor;
  begin
    Ra := Ra_J2000 + Ra_Motion * Delta_T;
    Dec := Dec_J2000 + Dec_Motion * Delta_T;
    Apparent (Ra, Dec);
    return Space.Direction_Of (Dec => Dec,
                               Ra  => Ra);
  end Direction_Of;


  function Table_Of (Id : Object) return Catalog.Information with Inline is
  begin
    if Id <= Catalog.Last_Index then
      return Catalog.Data_Of (Id);
    else
      return The_Extension_Table(Id);
    end if;
  end Table_Of;


  function Ra_J2000_Of (Id : Object) return REAL with Inline is
  begin
    return REAL(Table_Of(Id).Ra_J2000);
  end Ra_J2000_Of;


  function Dec_J2000_Of (Id : Object) return REAL with Inline is
  begin
    return REAL(Table_Of(Id).Dec_J2000);
  end Dec_J2000_Of;


  function Ra_Motion_Of (Id : Object) return REAL with Inline is
  begin
    return REAL(Table_Of(Id).Ra_Motion);
  end Ra_Motion_Of;


  function Dec_Motion_Of (Id : Object) return REAL with Inline is
  begin
    return REAL(Table_Of(Id).Dec_Motion);
  end Dec_Motion_Of;


  function Value_Of (Item : String) return Natural is
  begin
    for The_Index in Item'range loop
      if Item(The_Index) = '.' then
        return Natural'value(Item(Item'first .. The_Index - 1));
      end if;
    end loop;
    return Natural'value(Item);
  end Value_Of;


  function Object_Of (Item     : Positive;
                      The_Kind : Kind) return Object is
  begin
    case The_Kind is
    when Caldwell =>
      return Catalog.Caldwell_Id (Item);
    when Hip =>
      return Catalog.Hip_Id (Item);
    when Hr =>
      return Catalog.Hr_Id (Item);
    when Messier =>
      return Catalog.Messier_Id (Item);
    when Neo =>
      return The_First_Neo + Object(Item) - 1;
    when Ngc =>
      return Catalog.Ngc_Id (Item);
    when Ocl =>
      return Catalog.Ocl_Id (Item);
    when Quasars =>
      return Catalog.Quasars_Id (Item);
    when Favorites =>
      raise Program_Error;
    end case;
  end Object_Of;


  function Next_Of (Item     : Natural;
                    The_Kind : Kind) return Natural is
    The_Item : Natural := Item + 1;
  begin
    case The_Kind is
    when Caldwell =>
      while Catalog.Caldwell_Id (The_Item) = Undefined loop
        The_Item := The_Item + 1;
      end loop;
    when Hip =>
      while Catalog.Hip_Id (The_Item) = Undefined loop
        The_Item := The_Item + 1;
      end loop;
    when Hr =>
      while Catalog.Hr_Id (The_Item) = Undefined loop
        The_Item := The_Item + 1;
      end loop;
    when Messier =>
      while Catalog.Messier_Id (The_Item) = Undefined loop
        The_Item := The_Item + 1;
      end loop;
    when Neo =>
      if The_Item > Natural(The_Last_Neo + 1 - The_First_Neo) then
        raise End_Of_List;
      end if;
    when Ngc =>
      while Catalog.Ngc_Id (The_Item) = Undefined loop
        The_Item := The_Item + 1;
      end loop;
    when Ocl =>
      while Catalog.Ocl_Id (The_Item) = Undefined loop
        The_Item := The_Item + 1;
      end loop;
    when Quasars =>
      while Catalog.Quasars_Id (The_Item) = Undefined loop
        The_Item := The_Item + 1;
      end loop;
    when Favorites =>
      raise Program_Error;
    end case;
    return The_Item;
  exception
  when others =>
    raise End_Of_List;
  end Next_Of;


  function Name_Of (Id : Object) return String is
  begin
    return Table_Of(Id).Name.all;
  end Name_Of;


  function Descriptor_Of (Id : Object) return String is
  begin
    return Table_Of(Id).Descriptor.all;
  end Descriptor_Of;


  function Direction_Of (Id : Object;
                         Ut : Time.Ut) return Space.Direction is
  begin
    return Direction_Of (Ra_J2000   => Ra_J2000_Of (Id),
                         Dec_J2000  => Dec_J2000_Of (Id),
                         Ra_Motion  => Ra_Motion_Of (Id),
                         Dec_Motion => Dec_Motion_Of (Id),
                         Ut         => Ut);
  end Direction_Of;


  function Magnitude_Of (Id : Object) return Float is
  begin
    return Float(Table_Of(Id).Vmag);
  end Magnitude_Of;


  function Type_Of (Id : Object) return Object_Type is
  begin
    return Object_Type(Table_Of(Id).Kind);
  end Type_Of;


  function New_Object_For (Name        : String;
                           Description : String;
                           The_Type    : Object_Type := Star;
                           Direction   : Space.Direction := Space.Unknown_Direction) return Object is
    use all type Angle.Value;
  begin
    if The_Last_Extension = The_Extension_Table'last then
      Error.Raise_With ("Too many Objects declared - " & Name & " not stored");
    end if;
    The_Last_Extension := The_Last_Extension + 1;
    if Space.Direction_Is_Known (Direction) then
      The_Extension_Table (The_Last_Extension) :=
       (Name       => new String'(Name),
        Descriptor => new String'(Description),
        Ra_J2000   => Catalog.Degrees(Angle.Degrees'(+Space.Ra_Of (Direction))),
        Dec_J2000  => Catalog.Degrees(Angle.Degrees'(+Space.Dec_Of (Direction))),
        Ra_Motion  => 0.0,
        Dec_Motion => 0.0,
        Vmag       => 0.0,
        Kind       => Catalog.Object_Type(The_Type));
    else
      The_Extension_Table (The_Last_Extension) :=
       (Name       => new String'(Name),
        Descriptor => new String'(Description),
        Ra_J2000   => 0.0,
        Dec_J2000  => 0.0,
        Ra_Motion  => 0.0,
        Dec_Motion => 0.0,
        Vmag       => 0.0,
        Kind       => Catalog.Object_Type(The_Type));
    end if;
    return The_Last_Extension;
  end New_Object_For;


  function New_Neo_Object_For (Name        : String;
                               Description : String;
                               The_Type    : Object_Type) return Positive is
  begin
    if The_Last_Neo = Undefined then
      The_First_Neo := The_Last_Extension + 1;
    end if;
    The_Last_Neo := New_Object_For (Name        => Name,
                                    Description => Description,
                                    The_Type    => The_Type);
    return Neo_Index_Of (The_Last_Neo);
  end New_Neo_Object_For;


  function Neo_Object_Of (Name : String) return Object is
  begin
    for The_Index in The_First_Neo .. The_Last_Neo loop
      if The_Extension_Table(The_Index).Name.all = Name then
        return The_Index;
      end if;
    end loop;
    raise Program_Error;
  end Neo_Object_Of;


  function Neo_Index_Of (Id : Object) return Positive is
  begin
    return Natural(Id + 1 - The_First_Neo);
  end Neo_Index_Of;

  T : constant Time.T := Time.Tut;

begin
  PN_MATRIX (Time.T_J2000, T, Pn_Mat);
  ABERRAT (T, Ve);
end Data;
