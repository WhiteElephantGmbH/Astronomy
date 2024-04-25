-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Unchecked_Conversion;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Text;
with Traces;

package body Simbad.Catalog is

  package Log is new Traces ("Simbad.Catalog");

  type HD_List  is array (HD_Index) of Number;
  type HIP_List is array (HIP_Index) of Number;
  type HR_List  is array (HR_Index) of Number;
  type M_List   is array (M_Index) of Number;
  type NGC_List is array (NGC_Index) of Number;
  type IC_List  is array (IC_Index) of Number;
  type OCL_List is array (OCL_Index) of Number;

  use type Number;

  Star_Image : constant String := Database.Star_Image;


  function Image_Of (Item : Natural) return String is
  begin
    if Item = Unknown then
      raise No_Image;
    end if;
    return Positive'image(Item);
  end Image_Of;


  function Main_Id_Of (Item : Index) return Id is
    use type Database.Objects.Catalog_Id;
  begin
    case Database.Objects.List(Item).Catalog_Index is
    when 1 =>
      return Star_Id;
    when 2 =>
      return Name_Id;
    when others =>
      return Id'val(Database.Objects.List(Item).Catalog_Index - 1);
    end case;
  end Main_Id_Of;


  function Image_Of (Item : Index) return String is
  begin
    case Main_Id_Of (Item) is
    when Name_Id =>
      return Name_Image_Of (Item);
    when Star_Id =>
      return Star_Image_Of (Item);
    when HD =>
      return HD_Image_Of (Item);
    when HIP =>
      return HIP_Image_Of (Item);
    when HR =>
      return HR_Image_Of (Item);
    when M =>
      return M_Image_Of (Item);
    when NGC =>
      return NGC_Image_Of (Item);
    when IC =>
      return IC_Image_Of (Item);
    when Ocl =>
      return OCL_Image_Of (Item);
    end case;
  end Image_Of;


  use type Database.Id;

  package Star_Ids is new Ada.Containers.Ordered_Maps (Key_Type     => Database.Id,
                                                       Element_Type => Index);

  function All_Star_Ids return Star_Ids.Map is
    The_Map : Star_Ids.Map;
  begin
    for The_Index in Index loop
      declare
        Id : constant Database.Id := Database.Objects.List(The_Index).Star_Id;
      begin
        if Id /= Unknown then
          The_Map.Insert (Key => Id, New_Item => The_Index);
        end if;
      exception
      when others =>
        Log.Error ("Star id" & Id'image & " defined twice");
      end;
    end loop;
    return The_Map;
  end All_Star_Ids;

  Stars_Map : constant Star_Ids.Map := All_Star_Ids;


  type Star_Element is record
    Info   : Star_Info;
    Number : Index;
  end record;

  function "<" (Left, Right : Star_Element) return Boolean is
    use type DB.Constellation;
    use type DB.Star_Count_Type;
    use type DB.Star_Number;
    use type DB.Star_Index;
  begin
    if Left.Info.C_Id < Right.Info.C_Id then
      return True;
    elsif Left.Info.C_Id = Right.Info.C_Id then
      if Left.Info.Kind < Right.Info.Kind then
        return True;
      elsif Left.Info.Kind = Right.Info.Kind then
        if Left.Info.Count < Right.Info.Count then
          return True;
        elsif Left.Info.Count = Right.Info.Count then
          return Left.Info.Index < Right.Info.Index;
        end if;
      end if;
    end if;
    return False;
  end "<";

  package Star_Information is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                          Element_Type => Star_Element);

  package Star_Tool is new Star_Information.Generic_Sorting;

  function Star_List return Star_Information.Vector is
    function Convert is new Ada.Unchecked_Conversion (Database.Id, Star_Info);
    The_List : Star_Information.Vector;
  begin
    for The_Index in Index loop
      declare
        Id : constant Database.Id := Database.Objects.List(The_Index).Star_Id;
      begin
        if Id /= Unknown then
          The_List.Append (New_Item => Star_Element'(Convert(Id), The_Index));
        end if;
      end;
    end loop;
    Star_Tool.Sort (The_List);
    return The_List;
  exception
  when others =>
    Log.Error ("Star_List incomplete");
    return The_List;
  end Star_List;

  Star_Data : constant Star_Information.Vector := Star_List;


  package Star_Numbers is new Ada.Containers.Ordered_Maps (Key_Type     => Index,
                                                           Element_Type => Positive);

  function All_Star_Numbers return Star_Numbers.Map is
    The_Map : Star_Numbers.Map;
  begin
    for The_Number in Star_Data.First_Index .. Star_Data.Last_Index loop
      declare
        Data : constant Star_Element := Star_Data(The_Number);
      begin
        The_Map.Insert (Key => Data.Number, New_Item => The_Number);
      exception
      when others =>
        Log.Error ("Star number" & Data.Number'image & " defined twice");
      end;
    end loop;
    return The_Map;
  end All_Star_Numbers;

  Star_Positions : constant Star_Numbers.Map := All_Star_Numbers;


  function Number_Of_Name (Item : String) return Number is
  begin
    return Index_Of (Lexicon.Word_Of (Item));
  exception
  when others =>
    return Unknown;
  end Number_Of_Name;


  function Star_Number_Of (Item : Positive) return Number is
    Data : constant Star_Element := Star_Data(Item);
  begin
    return Data.Number;
  end Star_Number_Of;


  function Number_Of_Star (Item : String) return Number is
    Id : constant Database.Id := Database.Star_Id_Of (Item);
  begin
    return Stars_Map.Element (Id);
  end Number_Of_Star;


  function Number_Of (The_Catalog : Catalog_Id;
                      The_Index   : Positive) return Number is
  begin
    case The_Catalog is
    when Star_Id =>
      return Star_Number_Of (The_Index);
    when HD =>
      return Number_Of (HD_Index(The_Index));
    when HIP =>
      return Number_Of (HIP_Index(The_Index));
    when HR =>
      return Number_Of (HR_Index(The_Index));
    when M =>
      return Number_Of (M_Index(The_Index));
    when NGC =>
      return Number_Of (NGC_Index(The_Index));
    when IC =>
      return Number_Of (IC_Index(The_Index));
    when Ocl =>
      return Number_Of (OCL_Index(The_Index));
    end case;
  end Number_Of;


  function Number_Of (Image : String) return Number is
    Parts : constant Text.Strings := Text.Strings_Of (Image, ' ');
  begin
    case Parts.Count is
    when 0 =>
      return Unknown;
    when 1 =>
      return Number_Of_Name (Image);
    when others =>
      declare
        Catalog_Image : constant String := Parts(1);
        Number_Image  : constant String := Parts(2);
      begin
        if Catalog_Image = Star_Image then
          return Number_Of_Star (Image);
        else
          declare
            Cat_Id : constant Catalog_Id := Catalog_Id'value(Catalog_Image);
          begin
            return Number_Of (Cat_Id, Positive'value(Number_Image));
          end;
        end if;
      exception
      when others =>
        return Number_Of_Name (Image);
      end;
    end case;
  end Number_Of;


  function Index_Of (Item : Lexicon.Word) return Number is
  begin
    case Item is
    when Lexicon.Simbad_Names =>
      return (Index(Database.Objects.Name_Links(Database.Objects.Names'val(Lexicon.Word'pos(Item)))));
    when Lexicon.Eskimo_Nebula =>
      return Number_Of (Eskimo_Nebula_Index);
    when Lexicon.Hubbles_Nebula =>
      return Number_Of (Hubbles_Nebula_Index);
    when Lexicon.Persei_Clusters =>
      return Number_Of (Persei_Clusters_Index);
    when Lexicon.East_Veil_Nebula =>
      return Number_Of (East_Veil_Nebula_Index);
    when Lexicon.Veil_Nebula =>
      return Number_Of (Veil_Nebula_Index);
    when others =>
      return Unknown;
    end case;
  end Index_Of;


  function Has_Name (Item : Index) return Boolean is
    use type Database.Objects.Name_Id;
  begin
    return Database.Objects.List(Item).Name_Index /= Unknown;
  end Has_Name;


  function Name_Of (Item : Index) return Lexicon.Word is
    use type Database.Objects.Name_Id;
  begin
    return Lexicon.Word'val(Database.Objects.List(Item).Name_Index - 1);
  end Name_Of;


  function Name_Image_Of (Item : Index) return String is
  begin
    if not Has_Name (Item) then
      raise No_Image;
    end if;
    return Lexicon.Image_Of (Name_Of (Item));
  end Name_Image_Of;


  function Index_Of (Item : Star_Info) return Number is
    function Convert is new Ada.Unchecked_Conversion (Star_Info, Database.Id);
  begin
    return Stars_Map.Element (Convert (Item));
  exception
  when others =>
    return Unknown;
  end Index_Of;


  function Has_Star_Info (Item : Index) return Boolean is
  begin
    return Database.Objects.List(Item).Star_Id /= Unknown;
  end Has_Star_Info;


  function Star_Info_Of (Item : Index) return Star_Info is
    function Convert is new Ada.Unchecked_Conversion (Database.Id, Star_Info);
  begin
    return Convert (Database.Objects.List(Item).Star_Id);
  end Star_Info_Of;


  function Star_Image_Of (Item : Star_Info) return String is

    function Number_Image_Of (Count_Type : Database.Star_Count_Type;
                              The_Number : Database.Star_Number) return String is
      use all type Database.Star_Count_Type;
    begin
      case Count_Type is
      when Alphabetic =>
        return [Character'val(Character'pos('A') + Natural(The_Number))];
      when Greek =>
        return Database.Greek_Alphabet(Natural(The_Number));
      when Numeric =>
        return Text.Trimmed (The_Number'image);
      when others =>
        raise Program_Error;
      end case;
    end Number_Image_Of;


    function Constellation_Image_Of (C_Id : Database.Constellation) return String is
      Image : constant String := C_Id'image;
      use all type Database.Constellation;
    begin
      case C_Id is
      when C_Cvn => return "CVn";
      when C_Cma => return "CMa";
      when C_Cmi => return "CMi";
      when C_Cra => return "CrA";
      when C_Crb => return "CrB";
      when C_Lmi => return "LMi";
      when C_Psa => return "PsA";
      when C_Tra => return "TrA";
      when C_Uma => return "UMa";
      when C_Umi => return "UMi";
      when C_LMC => return "LMC";
      when C_SMC => return "SMC";
      when others =>
        return Text.Legible_Of (Image(Image'first + 1 ..Image'last));
      end case;
    end Constellation_Image_Of;


    function Index_Image_Of (The_Index : Database.Star_Index) return String is
      use type Database.Star_Index;
    begin
      if The_Index = 0 then
        return "";
      end if;
      return Text.Trimmed (The_Index'image);
    end Index_Image_Of;

    Number_Image : constant String := Number_Image_Of (Item.Kind, Item.Count);
    Index_Image  : constant String := Index_Image_Of (Item.Index);
    C_Id_Image   : constant String := Constellation_Image_Of (Item.C_Id);

  begin -- Star_Image_Of;
    return Star_Image & " " & Number_Image & Index_Image & " " & C_Id_Image;
  end Star_Image_Of;


  function Star_Of (Item : Index) return Positive is
  begin
    return Star_Positions(Item);
  end Star_Of;


  function Star_Image_Of (Item : Index) return String is
  begin
    if not Has_Star_Info (Item) then
      raise No_Image;
    end if;
    return Star_Image_Of (Star_Info_Of (Item));
  end Star_Image_Of;


  function HD_Stars return HD_List is
    The_List : HD_List := [others => Unknown];
  begin
    for The_Index in Index loop
      declare
        Id : constant HD_Number := HD_Number(Database.Objects.List(The_Index).HD_Number);
      begin
        if Id /= Unknown then
          if The_List(Id) = Unknown then
            The_List(Id) := The_Index;
          else
            Log.Error ("HD id" & Id'image & " defined twice");
          end if;
        end if;
      end;
    end loop;
    return The_List;
  end HD_Stars;


  function HIP_Stars return HIP_List is
    The_List : HIP_List := [others => Unknown];
  begin
    for The_Index in Index loop
      declare
        Id : constant HIP_Number := HIP_Number(Database.Objects.List(The_Index).HIP_Number);
      begin
        if Id /= Unknown then
          if The_List(Id) = Unknown then
            The_List(Id) := The_Index;
          else
            Log.Error ("HIP id" & Id'image & " defined twice");
          end if;
        end if;
      end;
    end loop;
    return The_List;
  end HIP_Stars;


  function HR_Stars return HR_List is
    The_List : HR_List := [others => Unknown];
  begin
    for The_Index in Index loop
      declare
        Id : constant HR_Number := HR_Number(Database.Objects.List(The_Index).HR_Number);
      begin
        if Id /= Unknown then
          if The_List(Id) = Unknown then
            The_List(Id) := The_Index;
          else
            Log.Error ("HR id" & Id'image & " defined twice");
          end if;
        end if;
      end;
    end loop;
    return The_List;
  end HR_Stars;


  function M_Objects return M_List is
    The_List : M_List := [others => Unknown];
  begin
    for The_Index in Index loop
      declare
        Id : constant M_Number := M_Number(Database.Objects.List(The_Index).M_Number);
      begin
        if Id /= Unknown then
          if The_List(Id) = Unknown then
            The_List(Id) := The_Index;
          else
            Log.Error ("M id" & Id'image & " defined twice");
          end if;
        end if;
      end;
    end loop;
    return The_List;
  end M_Objects;


  function NGC_Objects return NGC_List is
    The_List : NGC_List := [others => Unknown];
  begin
    for The_Index in Index loop
      declare
        Id : constant NGC_Number := NGC_Number(Database.Objects.List(The_Index).NGC_Number);
      begin
        if Id /= Unknown then
          if The_List(Id) = Unknown then
            The_List(Id) := The_Index;
          else
            Log.Error ("NGC id" & Id'image & " defined twice");
          end if;
        end if;
      end;
    end loop;
    return The_List;
  end NGC_Objects;


  function IC_Objects return IC_List is
    The_List : IC_List := [others => Unknown];
  begin
    for The_Index in Index loop
      declare
        Id : constant IC_Number := IC_Number(Database.Objects.List(The_Index).IC_Number);
      begin
        if Id /= Unknown then
          if The_List(Id) = Unknown then
            The_List(Id) := The_Index;
          else
            Log.Error ("IC id" & Id'image & " defined twice");
          end if;
        end if;
      end;
    end loop;
    return The_List;
  end IC_Objects;


  function OCL_Objects return OCL_List is
    The_List : OCL_List := [others => Unknown];
  begin
    for The_Index in Index loop
      declare
        Id : constant OCL_Number := OCL_Number(Database.Objects.List(The_Index).OCL_Number);
      begin
        if Id /= Unknown then
          if The_List(Id) = Unknown then
            The_List(Id) := The_Index;
          else
            Log.Error ("OCL id" & Id'image & " defined twice");
          end if;
        end if;
      end;
    end loop;
    return The_List;
  end OCL_Objects;


  HD_Map : constant HD_List  := HD_Stars;

  function Number_Of (Item : HD_Index) return Number is
  begin
    return HD_Map(Item);
  end Number_Of;


  function HD_Of (Item : Index) return HD_Number is
  begin
    return HD_Number(Database.Objects.List(Item).HD_Number);
  end HD_Of;


  function HD_Image_Of (Item : Index) return String is
  begin
    return "HD" & Image_Of (Natural(HD_Of (Item)));
  end HD_Image_Of;


  HIP_Map : constant HIP_List := HIP_Stars;

  function Number_Of (Item : HIP_Index) return Number is
  begin
    return HIP_Map(Item);
  end Number_Of;


  function HIP_Of (Item : Index) return HIP_Number is
  begin
    return HIP_Number(Database.Objects.List(Item).HIP_Number);
  end HIP_Of;


  function HIP_Image_Of (Item : Index) return String is
  begin
    return "HIP" & Image_Of (Natural(HIP_Of (Item)));
  end HIP_Image_Of;


  HR_Map : constant HR_List  := HR_Stars;

  function Number_Of (Item : HR_Index) return Number is
  begin
    return HR_Map(Item);
  end Number_Of;


  function HR_Of (Item : Index) return HR_Number is
  begin
    return HR_Number(Database.Objects.List(Item).HR_Number);
  end HR_Of;


  function HR_Image_Of (Item : Index) return String is
  begin
    return "HR" & Image_Of (Natural(HR_Of (Item)));
  end HR_Image_Of;


  M_Map : constant M_List  := M_Objects;

  function Number_Of (Item : M_Index) return Number is
  begin
    return M_Map(Item);
  end Number_Of;


  function M_Of (Item : Index) return M_Number is
  begin
    return M_Number(Database.Objects.List(Item).M_Number);
  end M_Of;


  function M_Image_Of (Item : Index) return String is
  begin
    return "M" & Image_Of (Natural(M_Of (Item)));
  end M_Image_Of;


  NGC_Map : constant NGC_List  := NGC_Objects;

  function Number_Of (Item : NGC_Index) return Number is
  begin
    return NGC_Map(Item);
  end Number_Of;


  function NGC_Of (Item : Index) return NGC_Number is
  begin
    return NGC_Number(Database.Objects.List(Item).NGC_Number);
  end NGC_Of;


  function NGC_Image_Of (Item : Index) return String is
  begin
    return "NGC" & Image_Of (Natural(NGC_Of (Item)));
  end NGC_Image_Of;


  IC_Map : constant IC_List  := IC_Objects;

  function Number_Of (Item : IC_Index) return Number is
  begin
    return IC_Map(Item);
  end Number_Of;


  function IC_Of (Item : Index) return IC_Number is
  begin
    return IC_Number(Database.Objects.List(Item).IC_Number);
  end IC_Of;


  function IC_Image_Of (Item : Index) return String is
  begin
    return "IC" & Image_Of (Natural(IC_Of (Item)));
  end IC_Image_Of;


  OCL_Map : constant OCL_List  := OCL_Objects;

  function Number_Of (Item : OCL_Index) return Number is
  begin
    return OCL_Map(Item);
  end Number_Of;


  function OCL_Of (Item : Index) return OCL_Number is
  begin
    return OCL_Number(Database.Objects.List(Item).OCL_Number);
  end OCL_Of;


  function OCL_Image_Of (Item : Index) return String is
  begin
    return "OCl" & Image_Of (Natural(OCL_Of (Item)));
  end OCL_Image_Of;


  function Ra_J2000_Of (Item : Index) return Angle.Degrees is
  begin
    return Angle.Degrees(Database.Objects.List(Item).Ra_J2000);
  end Ra_J2000_Of;


  function Dec_J2000_Of (Item : Index) return Angle.Degrees is
  begin
    return Angle.Degrees(Database.Objects.List(Item).Dec_J2000);
  end Dec_J2000_Of;


  function Ra_Motion_Of (Item : Index) return Angle.Degrees is
  begin
    return Angle.Degrees(Database.Objects.List(Item).Ra_PM);
  end Ra_Motion_Of;


  function Dec_Motion_Of (Item : Index) return Angle.Degrees is
  begin
    return Angle.Degrees(Database.Objects.List(Item).Dec_PM);
  end Dec_Motion_Of;


  function Magnitude_Of (Item : Index) return Magnitude is
  begin
    return Magnitude(Database.Objects.List(Item).Mag);
  end Magnitude_Of;


  function Distance_Of (Item : Index) return Light_Years is
  begin
    return Database.Objects.List(Item).Distance;
  end Distance_Of;


  function Parallax_Of (Item : Index) return Parallax is
  begin
    return Database.Objects.List(Item).Plx;
  end Parallax_Of;


  function Spec_Type_Of (Item : Index) return Spectral_Type is
  begin
    return Database.Objects.List(Item).Stype;
  end Spec_Type_Of;


  function Object_Type_Of (Item : Index) return Object_Type is
  begin
    return Object_Type'val(Database.Objects.List(Item).Otype);
  end Object_Type_Of;


  function Found_For (Dec : Angle.Degrees) return Index is
    Dec_J2000 : constant Database.Degrees_Dec := Database.Degrees_Dec(Dec);
    use type Database.Degrees_Dec;
  begin
    Log.Write ("Start Find");
    for The_Index in Simbad.Index loop
      if Database.Objects.List(The_Index).Dec_J2000 > Dec_J2000 then
        Log.Write ("Found Index" & The_Index'image);
        if The_Index < Sky.Max_Search_Distance then
          return Sky.Max_Search_Distance;
        elsif The_Index > Index'last - Sky.Max_Search_Distance then
          return Index'last - Sky.Max_Search_Distance;
        end if;
        return The_Index;
      end if;
    end loop;
    raise Program_Error;
  end Found_For;

end Simbad.Catalog;
