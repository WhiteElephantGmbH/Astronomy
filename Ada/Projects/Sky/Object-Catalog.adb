-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Traces;

package body Object.Catalog is

  package Log is new Traces ("Object.Catalog");

  type HD_List  is array (HD) of Star_Id;
  type HIP_List is array (HIP) of Star_Id;
  type HR_List  is array (HR) of Star_Id;

  subtype Star_Range is Database.Stars.Data_Range;


  function HD_Stars return HD_List is
    The_List : HD_List := [others => Unknown_Id];
  begin
    for The_Star in Star loop
      declare
        Id : constant HD_Id := HD_Id(Database.Stars.List(Star_Range(The_Star)).HD_Number);
      begin
        if Id /= Unknown_Id then
          if The_List(Id) = Unknown_Id then
            The_List(Id) := The_Star;
          else
            Log.Error ("HD id" & Id'image & " defined twice");
          end if;
        end if;
      end;
    end loop;
    return The_List;
  end HD_Stars;


  function HIP_Stars return HIP_List is
    The_List : HIP_List := [others => Unknown_Id];
  begin
    for The_Star in Star loop
      declare
        Id : constant HIP_Id := HIP_Id(Database.Stars.List(Star_Range(The_Star)).HIP_Number);
      begin
        if Id /= Unknown_Id then
          if The_List(Id) = Unknown_Id then
            The_List(Id) := The_Star;
          else
            Log.Error ("HIP id" & Id'image & " defined twice");
          end if;
        end if;
      end;
    end loop;
    return The_List;
  end HIP_Stars;


  function HR_Stars return HR_List is
    The_List : HR_List := [others => Unknown_Id];
  begin
    for The_Star in Star loop
      declare
        Id : constant HR_Id := HR_Id(Database.Stars.List(Star_Range(The_Star)).HR_Number);
      begin
        if Id /= Unknown_Id then
          if The_List(Id) = Unknown_Id then
            The_List(Id) := The_Star;
          else
            Log.Error ("HR id" & Id'image & " defined twice");
          end if;
        end if;
      end;
    end loop;
    return The_List;
  end HR_Stars;


  HD_Map : constant HD_List  := HD_Stars;

  function Star_Of (Id : HD) return Star_Id is
  begin
    return HD_Map(Id);
  end Star_Of;


  HIP_Map : constant HIP_List := HIP_Stars;

  function Star_Of (Id : HIP) return Star_Id is
  begin
    return HIP_Map(Id);
  end Star_Of;


  HR_Map : constant HR_List  := HR_Stars;

  function Star_Of (Id : HR) return Star_Id is
  begin
    return HR_Map(Id);
  end Star_Of;


  function Ra_J2000_Of (Id : Star) return Angle.Degrees is
  begin
    return Angle.Degrees(Database.Stars.List(Star_Range(Id)).Info.Ra_J2000);
  end Ra_J2000_Of;


  function Dec_J2000_Of (Id : Star) return Angle.Degrees is
  begin
    return Angle.Degrees(Database.Stars.List(Star_Range(Id)).Info.Dec_J2000);
  end Dec_J2000_Of;


  function Ra_Motion_Of (Id : Star) return Angle.Degrees is
  begin
    return Angle.Degrees(Database.Stars.List(Star_Range(Id)).Info.Ra_Pm);
  end Ra_Motion_Of;


  function Dec_Motion_Of (Id : Star) return Angle.Degrees is
  begin
    return Angle.Degrees(Database.Stars.List(Star_Range(Id)).Info.Dec_Pm);
  end Dec_Motion_Of;


  function V_Mag_Of (Id : Star) return Magnitude is
  begin
    return Magnitude(Database.Stars.List(Star_Range(Id)).Info.Vmag);
  end V_Mag_Of;


  function Spec_Type_Of (Id : Star) return Star_Spec_Type is
  begin
    return Database.Stars.List(Star_Range(Id)).Info.Stype;
  end Spec_Type_Of;

end Object.Catalog;
