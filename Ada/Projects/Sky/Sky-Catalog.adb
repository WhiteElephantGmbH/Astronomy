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

with Ada.Containers.Ordered_Maps;
with Simbad.Catalog;
with Strings;
with Traces;

package body Sky.Catalog is

  package Log is new Traces ("Sky.Catalog");

  package SC renames Simbad.Catalog;

  Quasar_3C_Id  : constant Positive     := 273;
  Quasar_HIP_Id : constant SC.HIP_Index := SC.HIP_Index(60936);
  Quasar_Index  : constant Index := Index(SC.Number_Of (Quasar_HIP_Id));

  type C_Number is range 0 .. 109;

  subtype Caldwell_Index is C_Number range 1 .. C_Number'last;

  type Caldwell_Item is record
    Catalog : SC.Id;
    Index   : Positive;
  end record;

  type Caldwell_List is array (Caldwell_Index) of Caldwell_Item;

  C14_Id : constant Positive := Positive(SC.Persei_Clusters_Index);
  C33_Id : constant Positive := Positive(SC.East_Veil_Nebula_Index);
  C34_Id : constant Positive := Positive(SC.Veil_Nebula_Index);
  C39_Id : constant Positive := Positive(SC.Eskimo_Nebula_Index);
  C46_Id : constant Positive := Positive(SC.Hubbles_Nebula_Index);

  Caldwell_Element : constant Caldwell_List := [
    (SC.NGC,    188),  --   1
    (SC.NGC,     40),  --   2
    (SC.NGC,   4236),  --   3
    (SC.NGC,   7023),  --   4
    (SC.IC,     342),  --   5
    (SC.NGC,   6543),  --   6
    (SC.NGC,   2403),  --   7
    (SC.NGC,    559),  --   8
    (SC.HIP, 113249),  --   9
    (SC.NGC,    663),  --  10
    (SC.NGC,   7635),  --  11
    (SC.NGC,   6946),  --  12
    (SC.NGC,    457),  --  13
    (SC.HD,  C14_Id),  --  14
    (SC.NGC,   6826),  --  15
    (SC.NGC,   7243),  --  16
    (SC.NGC,    147),  --  17
    (SC.NGC,    185),  --  18
    (SC.IC,    5146),  --  19
    (SC.NGC,   7000),  --  20
    (SC.NGC,   4449),  --  21
    (SC.NGC,   7662),  --  22
    (SC.NGC,    891),  --  23
    (SC.NGC,   1275),  --  24
    (SC.NGC,   2419),  --  25
    (SC.NGC,   4244),  --  26
    (SC.NGC,   6888),  --  27
    (SC.NGC,    752),  --  28
    (SC.NGC,   5005),  --  29
    (SC.NGC,   7331),  --  30
    (SC.IC,     405),  --  31
    (SC.NGC,   4631),  --  32
    (SC.NGC, C33_Id),  --  33
    (SC.NGC, C34_Id),  --  34
    (SC.NGC,   4889),  --  35
    (SC.NGC,   4559),  --  36
    (SC.NGC,   6885),  --  37
    (SC.NGC,   4565),  --  38
    (SC.NGC, C39_Id),  --  39
    (SC.NGC,   3626),  --  40
    (SC.HIP,  20885),  --  41
    (SC.NGC,   7006),  --  42
    (SC.NGC,   7814),  --  43
    (SC.NGC,   7479),  --  44
    (SC.NGC,   5248),  --  45
    (SC.NGC, C46_Id),  --  46
    (SC.NGC,   6934),  --  47
    (SC.NGC,   2775),  --  48
    (SC.NGC,   2238),  --  49
    (SC.NGC,   2244),  --  50
    (SC.IC,    1613),  --  51
    (SC.NGC,   4697),  --  52
    (SC.NGC,   3115),  --  53
    (SC.NGC,   2506),  --  54
    (SC.NGC,   7009),  --  55
    (SC.NGC,    246),  --  56
    (SC.NGC,   6822),  --  57
    (SC.NGC,   2360),  --  58
    (SC.NGC,   3242),  --  59
    (SC.NGC,   4038),  --  60
    (SC.NGC,   4039),  --  61
    (SC.NGC,    247),  --  62
    (SC.NGC,   7293),  --  63
    (SC.NGC,   2362),  --  64
    (SC.NGC,    253),  --  65
    (SC.NGC,   5694),  --  66
    (SC.NGC,   1097),  --  67
    (SC.NGC,   6729),  --  68
    (SC.NGC,   6302),  --  69
    (SC.NGC,    300),  --  70
    (SC.NGC,   2477),  --  71
    (SC.NGC,     55),  --  72
    (SC.NGC,   1851),  --  73
    (SC.NGC,   3132),  --  74
    (SC.NGC,   6124),  --  75
    (SC.NGC,   6231),  --  76
    (SC.NGC,   5128),  --  77
    (SC.NGC,   6541),  --  78
    (SC.NGC,   3201),  --  79
    (SC.NGC,   5139),  --  80
    (SC.NGC,   6352),  --  81
    (SC.NGC,   6193),  --  82
    (SC.NGC,   4945),  --  83
    (SC.NGC,   5286),  --  84
    (SC.IC,    2391),  --  85
    (SC.NGC,   6397),  --  86
    (SC.NGC,   1261),  --  87
    (SC.NGC,   5823),  --  88
    (SC.NGC,   6087),  --  89
    (SC.NGC,   2867),  --  90
    (SC.NGC,   3532),  --  91
    (SC.NGC,   3372),  --  92
    (SC.NGC,   6752),  --  93
    (SC.NGC,   4755),  --  94
    (SC.NGC,   6025),  --  95
    (SC.NGC,   2516),  --  96
    (SC.NGC,   3766),  --  97
    (SC.NGC,   4609),  --  98
    (SC.HIP,  62651),  --  99
    (SC.IC,    2944),  -- 100
    (SC.NGC,   6744),  -- 101
    (SC.IC,    2602),  -- 102
    (SC.NGC,   2070),  -- 103
    (SC.NGC,    362),  -- 104
    (SC.NGC,   4833),  -- 105
    (SC.NGC,    104),  -- 106
    (SC.NGC,   6101),  -- 107
    (SC.NGC,   4372),  -- 108
    (SC.NGC,   3195)]; -- 109

  function Caldwell_Id (Item : Positive) return Object is
    C : constant Caldwell_Item := Caldwell_Element(Caldwell_Index(Item));
  begin
    return Object(SC.Number_Of (C.Catalog, C.Index));
  end Caldwell_Id;


  package C_Index is new Ada.Containers.Ordered_Maps (Key_Type     => Index,
                                                      Element_Type => Caldwell_Index);

  function Caldwell_Map return C_Index.Map is
    The_Map : C_Index.Map;
  begin
    for The_Index in Caldwell_Index'range loop
      The_Map.Insert (Key => Caldwell_Id (Positive(The_Index)), New_Item => The_Index);
    end loop;
    return The_Map;
  end Caldwell_Map;


  C_Map : constant C_Index.Map := Caldwell_Map;

  function C_Number_Of (Item : Index) return C_Number is
  begin
    return C_Map (Item);
  exception
  when others =>
    return Undefined;
  end C_Number_Of;


  function Corrected_Name_Of (Name : String) return String is
  begin
    if Name'length < 2 then
      return "  ";
    elsif Name(Name'first + 1) in '1' .. '9' then                        -- M1, C99 ...
      return Name(Name'first) & ' ' & Name(Name'first + 1 .. Name'last); -- := M 1, C 99 ...
    elsif Name(Name'last - 1 .. Name'last) = ".0" then                   -- Ocl 113.0 ...
      return Name(Name'first .. Name'last - 2);                          -- := Ocl 113 ...
    end if;
    return Name;
  end Corrected_Name_Of;


  function Image_Of (Id : Catalog_Id) return String is
  begin
    case Id is
    when Favorites =>
      return Lexicon.Image_Of (Lexicon.Favorites);
    when Caldwell =>
      null;
    when Name =>
      return Lexicon.Image_Of (Lexicon.Names);
    when Star =>
      return "*";
    when Hd =>
      return "HD";
    when Hip =>
      return "HIP";
    when Hr =>
      return "HR";
    when Messier =>
      null;
    when Neo =>
      return "NEO";
    when Ngc =>
      return "NGC";
    when Ic =>
      return "IC";
    when Ocl =>
      return "OCl";
    when Quasars =>
      return Lexicon.Image_Of (Lexicon.Quasars);
    end case;
    return Strings.Legible_Of (Id'img);
  end Image_Of;


  function Position_Of (Id   : Simbad_Catalog;
                        Item : Index) return Positive is
  begin
    case Id is
    when Star =>
      return SC.Star_Of (Simbad.Index(Item));
    when Hd =>
      return Positive(SC.HD_Of (Simbad.Index(Item)));
    when Hip =>
      return Positive(SC.HIP_Of (Simbad.Index(Item)));
    when Hr =>
      return Positive(SC.HR_Of (Simbad.Index(Item)));
    when Messier =>
      return Positive(SC.M_Of (Simbad.Index(Item)));
    when Ngc =>
      return Positive(SC.NGC_Of (Simbad.Index(Item)));
    when Ic =>
      return Positive(SC.IC_Of (Simbad.Index(Item)));
    when Ocl =>
      return Positive(SC.OCL_Of (Simbad.Index(Item)));
    end case;
  end Position_Of;


  function Object_Of (Item : String) return Object is
    Object_Name   : constant String := Corrected_Name_Of (Item);
    Catalog_Id    : constant String := Object_Name(Object_Name'first .. Object_Name'first + 1);
    Catalog_Index : constant String := Object_Name(Object_Name'first + 2 .. Object_Name'last);
  begin
    if Catalog_Id = "C " then
      return Caldwell_Id (Positive'value(Catalog_Index));
    elsif Catalog_Id = "3C" then
      return Quasars_Id (Positive'value(Catalog_Index));
    end if;
    return Object(SC.Number_Of (Object_Name));
  exception
  when others =>
    return Undefined;
  end Object_Of;


  function Main_Name_Of (Item : Index) return String is
    Messier_Number  : constant SC.M_Number := SC.M_Of (Simbad.Index(Item));
    use type SC.M_Number;
  begin
    if Item = Quasar_Index then
      return "3C" & Quasar_3C_Id'image;
    elsif Messier_Number /= Simbad.Unknown then
      return "M" & Strings.Trimmed (Messier_Number'image);
    end if;
    declare
      Caldwell_Number : constant C_Number := C_Number_Of (Item);
    begin
      if Caldwell_Number /= Undefined then
        return "C" & Strings.Trimmed (Caldwell_Number'image);
      end if;
    end;
    return SC.Image_Of (Simbad.Index(Item));
  end Main_Name_Of;


  function Object_Image_Of (Item     : Index;
                            New_Name : String := "") return String is
    Main_Image : constant String := Main_Name_Of (Item);
  begin
    if New_Name /= "" and then Main_Image /= New_Name then
      return Main_Image & " [" & New_Name & "]";
    else
      declare
        Name_Image : constant String := Name_Of (Item);
      begin
        if Name_Image /= "" and then Main_Image /= Name_Image then
          return Main_Image & " [" & Name_Image & "]";
        end if;
      end;
    end if;
    return Main_Image;
  end Object_Image_Of;


  function Name_Of (Item     : Positive;
                    The_Kind : Catalogs) return String is

    The_Object : constant Object := Object_Of (Item, The_Kind);

    Name_Image : constant String := Name_Of (The_Object);

    function Optional_Name return String is
    begin
      if Name_Image = "" then
        return "";
      end if;
      return " [" & Name_Image & "]";
    end Optional_Name;

    function Id_And_Name return String is
      Image  : constant String := Item'img;
    begin
      if The_Object = Undefined then
        return Image(Image'first + 1 .. Image'last);
      else
        return Image(Image'first + 1 .. Image'last) & Optional_Name;
      end if;
    end Id_And_Name;

  begin -- Name_Of
    case The_Kind is
    when Name =>
      return Name_Image;
    when Caldwell =>
      return "C" & Id_And_Name;
    when Star =>
      return SC.Image_Of (Simbad.Index(The_Object)) & Optional_Name;
    when Hd =>
      return "HD " & Id_And_Name;
    when Hip =>
      return "HIP " & Id_And_Name;
    when Hr =>
      return "HR " & Id_And_Name;
    when Messier =>
      return "M" & Id_And_Name;
    when Ngc =>
      return "NGC " & Id_And_Name;
    when Ic =>
      return "IC " & Id_And_Name;
    when Ocl =>
      return "OCl " & Id_And_Name;
    when Quasars =>
      return "3C " & Id_And_Name;
    end case;
  end Name_Of;


  function Object_Of (Item : Lexicon.Word) return Object is
  begin
    return Object(Simbad.Catalog.Index_Of (Item));
  end Object_Of;


  function Object_Of (Letter : Greek_Letter;
                      Const  : Constellation) return Object is
    use all type Database.Star_Count_Type;
  begin
    return Object(SC.Index_Of (SC.Star_Info'(Kind  => Greek,
                                             Count => Greek_Letter'pos(Letter),
                                             Index => 0,
                                             C_Id  => Const)));
  end Object_Of;


  function Object_Of (Item     : Positive;
                      The_Kind : Catalogs) return Object is
  begin
    case The_Kind is
    when Caldwell =>
      return Caldwell_Id (Item);
    when Name =>
      return Name_Id (Item);
    when Star =>
      return Star_Id (Item);
    when Hd =>
      return Hd_Id (Item);
    when Hip =>
      return Hip_Id (Item);
    when Hr =>
      return Hr_Id (Item);
    when Messier =>
      return Messier_Id (Item);
    when Ngc =>
      return Ngc_Id (Item);
    when Ic =>
      return Ngc_Id (Item);
    when Ocl =>
      return Ocl_Id (Item);
    when Quasars =>
      return Quasars_Id (Item);
    end case;
  end Object_Of;


  function Name_Id (Item : Positive) return Object is
  begin
    return Object(SC.Index_Of(Lexicon.Word'val(Item - 1)));
  end Name_Id;


  function Star_Id (Item : Positive) return Object is
  begin
    return Object(SC.Star_Number_Of (Item));
  end Star_Id;


  function Hd_Id (Item : Positive) return Object is
  begin
    return Object(SC.Number_Of (SC.HD_Index(Item)));
  end Hd_Id;


  function Hip_Id (Item : Positive) return Object is
  begin
    return Object(SC.Number_Of (SC.HIP_Index(Item)));
  end Hip_Id;


  function Hr_Id (Item : Positive) return Object is
  begin
    return Object(SC.Number_Of (SC.HR_Index(Item)));
  end Hr_Id;


  function Messier_Id (Item : Positive) return Object is
  begin
    return Object(SC.Number_Of (SC.M_Index(Item)));
  end Messier_Id;


  function Ngc_Id (Item : Positive) return Object is
  begin
    return Object(SC.Number_Of (SC.NGC_Index(Item)));
  end Ngc_Id;


  function Ocl_Id (Item : Positive) return Object is
  begin
    return Object(SC.Number_Of (SC.OCL_Index(Item)));
  end Ocl_Id;


  function Quasars_Id (Item : Positive) return Object is
  begin
    if Item = Quasar_3C_Id then
      return Object(Quasar_Index);
    elsif Item > Quasar_3C_Id then
      raise Constraint_Error; --> end of list
    end if;
    return Undefined;
  end Quasars_Id;


  C14_Index : constant Object := Caldwell_Id (14);
  C33_Index : constant Object := Caldwell_Id (33);
  C34_Index : constant Object := Caldwell_Id (34);
  C39_Index : constant Object := Caldwell_Id (39);
  C46_Index : constant Object := Caldwell_Id (46);

  function Name_Of (Item : Index) return String is
  begin
    if Item = C14_Index then
      return Lexicon.Image_Of (Lexicon.Persei_Clusters);
    elsif Item = C33_Index then
      return Lexicon.Image_Of (Lexicon.East_Veil_Nebula);
    elsif Item = C34_Index then
      return Lexicon.Image_Of (Lexicon.Veil_Nebula);
    elsif Item = C39_Index then
      return Lexicon.Image_Of (Lexicon.Eskimo_Nebula);
    elsif Item = C46_Index then
      return Lexicon.Image_Of (Lexicon.Hubbles_Nebula);
    end if;
    return SC.Name_Image_Of (Simbad.Index(Item));
  exception
  when others =>
    return "";
  end Name_Of;


  function Descriptor_Of (Item : Index) return String is
  begin
    if Item = C14_Index then
      return Lexicon.Image_Of (Lexicon.Open_Clusters);
    elsif Item in C33_Index | C34_Index then
      return Lexicon.Image_Of (Lexicon.Supernova_Remnant);
    end if;
    return Lexicon.Image_Of (Lexicon.Word'value(SC.Object_Type_Of (Simbad.Index(Item))'image));
  end Descriptor_Of;


  function Main_Catalog_Of (Item : Index) return Simbad_Catalog is
    Id : constant SC.Id := SC.Main_Id_Of (Simbad.Index(Item));
  begin
    case Id is
    when SC.Name_Id =>
      Log.Warning ("Name not supported as main catalog");
      return Hd;
    when SC.Star_Id =>
      return Star;
    when SC.HD =>
      return Hd;
    when SC.HIP =>
      return Hip;
    when SC.HR =>
      return Hr;
    when SC.M =>
      return Messier;
    when SC.NGC =>
      return Ngc;
    when SC.IC =>
      return Ic;
    when SC.Ocl =>
      return Ocl;
    end case;
  end Main_Catalog_Of;


  function Kind_Of (Item : Index) return Kind is
  begin
    case SC.Object_Type_Of (Simbad.Index(Item)) is
    when Simbad.Star =>
      return Star;
    when Simbad.Multiple_Star =>
      return Double;
    when Simbad.Stars =>
      return Stars;
    when Simbad.Globular_Cluster =>
      return Cluster;
    when Simbad.Galaxy =>
      if Item = Quasar_Index then
        return Quasar;
      end if;
      return Galaxy;
    when Simbad.Nebula =>
      return Nebula;
    when Simbad.Spectral =>
      return Star;
    end case;
  end Kind_Of;


  function Ra_J2000_Of (Item : Index) return Angle.Degrees is
  begin
    return SC.Ra_J2000_Of (Simbad.Index(Item));
  end Ra_J2000_Of;


  function Dec_J2000_Of (Item : Index) return Angle.Degrees is
  begin
    return SC.Dec_J2000_Of (Simbad.Index(Item));
  end Dec_J2000_Of;


  function Ra_Motion_Of (Item : Index) return Angle.Degrees is
  begin
    return SC.Ra_Motion_Of (Simbad.Index(Item));
  end Ra_Motion_Of;


  function Dec_Motion_Of (Item : Index) return Angle.Degrees is
  begin
    return SC.Dec_Motion_Of (Simbad.Index(Item));
  end Dec_Motion_Of;


  function Magnitude_Of (Item : Index) return Sky.Magnitude is
  begin
    return SC.Magnitude_Of (Simbad.Index(Item));
  end Magnitude_Of;


  function Found_For (Dec : Angle.Degrees) return Index is
  begin
    return Index(SC.Found_For (Dec));
  end Found_For;

end Sky.Catalog;
