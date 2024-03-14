-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Sky.Catalog;
with Error;
with Strings;

package body Data is

  use Astro;
  use Astro.PNULIB;
  use Astro.SPHLIB;

  Pn_Mat  : REAL33;
  Ve      : VECTOR;

  Number_Of_Extension_Objects : constant := 1000;

  use type Object;

  First_Extension : constant Object := Sky.Catalog.Number'last + 1;

  subtype Extension_Object is Object range First_Extension .. First_Extension + Number_Of_Extension_Objects - 1;

  type Information is record
    Name        : Text.String;
    Descriptor  : Text.String;
    Object_Kind : Object_Type;
    Ra_J2000    : Angle.Degrees;
    Dec_J2000   : Angle.Degrees;
  end record;

  type Extension_Objects is array (Extension_Object) of Information;

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


  function Name_Of (Id : Index) return String is
    use type Text.String;
  begin
    if Id in Sky.Catalog.Index'first .. Sky.Catalog.Index'last then
      return Sky.Catalog.Name_Of (Sky.Catalog.Index(Id));
    else
      return +The_Extension_Table(Id).Name;
    end if;
  end Name_Of;


  function Descriptor_Of (Id : Index) return String is
    use type Text.String;
  begin
    if Id in Catalog.Object then
      return Catalog.Descriptor_Of (Id);
    else
      return +The_Extension_Table(Id).Descriptor;
    end if;
  end Descriptor_Of;


  function Object_Type_Of (Id : Index) return Object_Type is
  begin
    if Id in Catalog.Index'range then
      return Catalog.Object_Type_Of (Id);
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


  function New_Object_For (Name        : String;
                           Description : String;
                           The_Type    : Object_Type := Star;
                           Direction   : Space.Direction := Space.Unknown_Direction) return Index is
    use all type Angle.Value;
  begin
    if The_Last_Extension = The_Extension_Table'last then
      Error.Raise_With ("Too many Objects declared - " & Name & " not stored");
    end if;
    The_Last_Extension := The_Last_Extension + 1;
    if Space.Direction_Is_Known (Direction) then
      The_Extension_Table (The_Last_Extension) :=
       (Name        => [Name],
        Descriptor  => [Description],
        Ra_J2000    => Angle.Degrees'(+Space.Ra_Of (Direction)),
        Dec_J2000   => Angle.Degrees'(+Space.Dec_Of (Direction)),
        Object_Kind => The_Type);
    else
      The_Extension_Table (The_Last_Extension) :=
       (Name        => [Name],
        Descriptor  => [Description],
        Ra_J2000    => 0.0,
        Dec_J2000   => 0.0,
        Object_Kind => The_Type);
    end if;
    return The_Last_Extension;
  end New_Object_For;


  function Neo_Object_Of (Name : String) return Index is
    use type Text.String;
  begin
    for The_Index in The_First_Neo .. The_Last_Neo loop
      if The_Extension_Table(The_Index).Name = Name then
        return The_Index;
      end if;
    end loop;
    raise Program_Error;
  end Neo_Object_Of;


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


  function Neo_Index_Of (Id : Index) return Positive is
  begin
    return Positive(Id + 1 - The_First_Neo);
  end Neo_Index_Of;

  T : constant Time.T := Time.Tut;

begin
  PN_MATRIX (Time.T_J2000, T, Pn_Mat);
  ABERRAT (T, Ve);
end Data;
