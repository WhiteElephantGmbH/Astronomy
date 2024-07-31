-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Earth;
with Lexicon;
with Moon;
with Objects;
with Site;
with Sky.Data;
with Sky_Line;
with Solar_System;
with Sssb;
with Sun;
with Traces;

package body Targets is

  package Log is new Traces ("Targets");

  function Image_Of (The_Selection : Selection) return String is

    type Names is array (Selection) of Lexicon.Word;

    Name_Of : constant Names := [All_Objects        => Lexicon.All_Objects,
                                 Solar_System       => Lexicon.Solar_System,
                                 Clusters           => Lexicon.Globular_Cluster,
                                 Open_Clusters      => Lexicon.Open_Clusters,
                                 Nebulas            => Lexicon.Nebula,
                                 Galaxies           => Lexicon.Galaxies,
                                 Stars              => Lexicon.Stars,
                                 Multiple_Stars     => Lexicon.Multiple_Stars,
                                 Near_Earth_Objects => Lexicon.Neos];
  begin
    return Lexicon.Image_Of (Name_Of(The_Selection));
  end Image_Of;


  function Selection_Of (Image : String) return Selection is
    Selection_Name : constant Lexicon.Word := Lexicon.Word_Of (Image);
  begin
    case Selection_Name is
    when Lexicon.All_Objects      => return All_Objects;
    when Lexicon.Solar_System     => return Solar_System;
    when Lexicon.Globular_Cluster => return Clusters;
    when Lexicon.Open_Clusters    => return Open_Clusters;
    when Lexicon.Nebula           => return Nebulas;
    when Lexicon.Galaxies         => return Galaxies;
    when Lexicon.Stars            => return Stars;
    when Lexicon.Multiple_Stars   => return Multiple_Stars;
    when Lexicon.Neos             => return Near_Earth_Objects;
    when others =>
      raise Program_Error;
    end case;
  end Selection_Of;


  function Image_Of (The_Feature_Selection : Moon_Feature_Selection) return String is

    type Names is array (Moon_Feature_Selection) of Lexicon.Word;

    Name_Of : constant Names := [All_Features       => Lexicon.All_Objects,
                                 Bays               => Lexicon.Bays,
                                 Cliffs             => Lexicon.Cliffs,
                                 Craters            => Lexicon.Craters,
                                 Rilles             => Lexicon.Rilles,
                                 Lakes              => Lexicon.Lakes,
                                 Mountains          => Lexicon.Mountains,
                                 Oceans             => Lexicon.Oceans,
                                 Seas               => Lexicon.Seas,
                                 Swamps             => Lexicon.Swamps,
                                 Swirls             => Lexicon.Swirls,
                                 Valleys            => Lexicon.Valleys];
  begin
    return Lexicon.Image_Of (Name_Of(The_Feature_Selection));
  end Image_Of;


  function Feature_Of (Image : String) return Moon_Feature_Selection is
    Feature_Name : constant Lexicon.Word := Lexicon.Word_Of (Image);
  begin
    case Feature_Name is
    when Lexicon.All_Objects => return All_Features;
    when Lexicon.Bays        => return Bays;
    when Lexicon.Cliffs      => return Cliffs;
    when Lexicon.Craters     => return Craters;
    when Lexicon.Rilles      => return Rilles;
    when Lexicon.Lakes       => return Lakes;
    when Lexicon.Mountains   => return Mountains;
    when Lexicon.Oceans      => return Oceans;
    when Lexicon.Seas        => return Seas;
    when Lexicon.Swamps      => return Swamps;
    when Lexicon.Swirls      => return Swirls;
    when Lexicon.Valleys     => return Valleys;
    when others =>
      raise Program_Error;
    end case;
  end Feature_Of;


  task type Handler (Clear  : access procedure;
                     Define : access procedure (List : Name.Id_List_Access);
                     Update : access procedure) is

    entry Define_Catalog;

    entry Set (The_Range : Az_Range);

    entry Set (The_Selection : Selection);

    entry Set (The_Selection : Moon_Feature_Selection);

    entry Set (Sorted : Switch);

    entry Update_List;

    entry Get_For (Target_Name :     String;
                   Target_Id   : out Name.Id);

    entry Get_For (The_Direction :     Space.Direction;
                   Tolerance     :     Space.Distance;
                   Target_Id     : out Name.Id);

    entry Stop;

  end Handler;


  The_Handler : access Handler;

  Is_Arriving : Arriving_Handling;


  procedure Start (Clear    : access procedure;
                   Define   : access procedure (List : Name.Id_List_Access);
                   Update   : access procedure;
                   Arriving : Arriving_Handling := null) is
  begin
    The_Handler := new Handler (Clear  => Clear,
                                Define => Define,
                                Update => Update);
    Is_Arriving := Arriving;
  end Start;


  procedure Define_Catalog is
  begin
    The_Handler.Define_Catalog;
  end Define_Catalog;


  procedure Set (The_Range : Az_Range) is
  begin
    The_Handler.Set (The_Range);
  end Set;


  procedure Set (The_Selection : Selection) is
  begin
    The_Handler.Set (The_Selection);
  end Set;


  procedure Set (The_Feature : Moon_Feature_Selection) is
  begin
    The_Handler.Set (The_Feature);
  end Set;


  procedure Set (Sorted : Switch) is
  begin
    The_Handler.Set (Sorted);
  end Set;


  procedure Update_List is
  begin
    The_Handler.Update_List;
  end Update_List;


  procedure Get_For (Target_Name :     String;
                     Target_Id   : out Name.Id) is
  begin
    The_Handler.Get_For (Target_Name, Target_Id);
  end Get_For;


  procedure Get_For (The_Direction :     Space.Direction;
                     Tolerance     :     Space.Distance;
                     Target_Id     : out Name.Id) is
  begin
    The_Handler.Get_For (The_Direction, Tolerance, Target_Id);
  end Get_For;


  procedure Stop is
  begin
    The_Handler.Stop;
  end Stop;


  task body Handler is

    The_Targets     : aliased Name.Id_List;
    Targets_Defined : Boolean := False;
    New_List        : Boolean := False;

    The_Actual_Az_Range   : Az_Range;
    The_Actual_Selection  : Selection;
    The_Feature_Selection : Moon_Feature_Selection := All_Features;
    Is_Az_Sorted          : Boolean := False;

    function Is_Selected (The_Objects : Object_Kind) return Boolean is
    begin
      case The_Actual_Selection is
      when All_Objects =>
        return True;
      when Stars =>
        case The_Objects is
        when Stars | Multiple_Stars  =>
          return True;
        when others =>
          return False;
        end case;
      when others =>
        return The_Objects = The_Actual_Selection;
      end case;
    end Is_Selected;


    function Is_Selected (The_Feature : Moon.Feature_Type) return Boolean is
      use all type Moon.Feature_Type;
    begin
      case The_Feature_Selection is
      when All_Features =>
        return True;
      when Bays =>
        return The_Feature = Sinus;
      when Cliffs =>
        return The_Feature = Rupes;
      when Craters =>
        case The_Feature is
        when Crater | Catena  =>
          return True;
        when others =>
          return False;
        end case;
      when Lakes =>
        return The_Feature = Lacus;
      when Mountains =>
        case The_Feature is
        when Mons | Promontorium  =>
          return True;
        when others =>
          return False;
        end case;
      when Oceans =>
        return The_Feature = Oceanus;
      when Rilles =>
        return The_Feature = Rima;
      when Seas =>
        return The_Feature = Mare;
      when Swamps =>
        return The_Feature = Palus;
      when Swirls =>
        return The_Feature = Swirl;
      when Valleys =>
        return The_Feature = Vallis;
      end case;
    end Is_Selected;


    procedure Define_Targets is

      Sun_Is_Visible : constant Boolean := Sun.Is_Visible;

      function Is_Visible (Direction : Space.Direction) return Boolean is
        Position : constant Earth.Direction := Objects.Direction_Of (Direction, Time.Lmst);
      begin
        return Angle.In_Range (Earth.Az_Of (Position), The_Actual_Az_Range.From, The_Actual_Az_Range.To)
               and then Sky_Line.Is_Above (Direction => Position);
      end Is_Visible;

      function Is_Selected_And_Visible (Item      : Selection;
                                        Direction : Space.Direction) return Boolean is
      begin
        return Space.Direction_Is_Known (Direction) and then Is_Selected (Item) and then Is_Visible (Direction);
      end Is_Selected_And_Visible;

      function Is_To_Add (Item      : Selection;
                          Direction : Space.Direction) return Boolean is
      begin
         if Is_Selected_And_Visible (Item, Direction) then
           return not Sun_Is_Visible or else Sun.Is_In_Safe_Distance (To_Target => Direction);
         end if;
         return False;
      end Is_To_Add;

      The_Changes : Natural := 0;

      Ut : constant Time.Ut := Time.Universal;

      procedure Add_Visible (Item : in out Name.Id) is

        function Is_To_Add (Direction : Space.Direction) return Boolean is
        begin
          if not Space.Direction_Is_Known (Direction) then
            return False;
          elsif not Sun_Is_Visible or else Sun.Is_In_Safe_Distance (To_Target => Direction) then
            return Is_Visible (Direction);
          end if;
          return False;
        end Is_To_Add;

        function Is_To_Add return Boolean is
        begin
          case Name.Kind_Of (Item) is
          when Name.Moon =>
            case The_Feature_Selection is
            when All_Features =>
              return Is_To_Add (Moon.Direction_Of (Item));
            when others =>
              declare
                Feature  : Moon.Feature;
                The_Type : Moon.Feature_Type;
              begin
                if Moon.Has_Feature (Item, Feature, The_Type) and then Is_Selected (The_Type) then
                  return Is_To_Add (Moon.Direction_Of (Item));
                end if;
                return False;
              end;
            end case;
          when Name.Planet =>
            return Is_To_Add (Solar_System, Solar_System_Direction_Of (Item, Ut));
          when Name.Sun =>
            return Is_Selected_And_Visible (Solar_System, Solar_System_Direction_Of (Item, Ut));
          when Name.Small_Solar_System_Body =>
            return Is_To_Add (Solar_System, Sssb.Direction_Of (Item, Ut));
          when Name.Near_Earth_Object =>
            return Is_Selected (Near_Earth_Objects) and then Is_Arriving (Item) and then Sun.Protection_Is_Disabled;
          when Name.Axis_Position =>
            return The_Actual_Selection = All_Objects;
          when Name.Landmark =>
            return The_Actual_Selection = All_Objects;
          when Name.Sky_Object =>
            declare
              Object : constant Sky.Object := Name.Object_Of (Item);
            begin
              case Sky.Data.Object_Type_Of (Object) is
              when Sky.Quasar  =>
                return Is_To_Add (Galaxies, Sky.Data.Direction_Of (Object, Ut));
              when Sky.Galaxy  =>
                return Is_To_Add (Galaxies, Sky.Data.Direction_Of (Object, Ut));
              when Sky.Nebula  =>
                return Is_To_Add (Nebulas, Sky.Data.Direction_Of (Object, Ut));
              when Sky.Cluster =>
                return Is_To_Add (Clusters, Sky.Data.Direction_Of (Object, Ut));
              when Sky.Stars =>
                return Is_To_Add (Open_Clusters, Sky.Data.Direction_Of (Object, Ut));
              when Sky.Double =>
                return Is_To_Add (Multiple_Stars, Sky.Data.Direction_Of (Object, Ut));
              when Sky.Star =>
                return Is_To_Add (Stars, Sky.Data.Direction_Of (Object, Ut));
              when Sky.Moon | Sky.Satellite =>
                raise Program_Error;
              when Sky.Unknown =>
                return False;
              end case;
            end;
          end case;
        end Is_To_Add;

      begin -- Add_Visible
        if Name.Visibility_Changed_For (Item, Is_To_Add) then
          Log.Write ("XXX Visibility_Changed_For " & Name.Image_Of (Item));
          The_Changes := The_Changes + 1;
        end if;
      end Add_Visible;

    begin -- Define_Targets
      if Is_Az_Sorted then
        Name.Sort (The_Targets);
        New_List := True;
      end if;
      Moon.Define (Time.Universal);
      Name.For_All (In_List => The_Targets, Handle => Add_Visible'access);
      if New_List or (The_Changes > 100) then
        New_List := False;
        Clear.all;
        Name.Clear_History_For (The_Targets);
        Define (The_Targets'unchecked_access);
        Targets_Defined := True;
      elsif Targets_Defined then
       Update.all;
      end if;
    end Define_Targets;

    The_Found_Id : Name.Id;

    use type Name.Id;

  begin -- Handler
    Log.Write ("handler start");
    loop
      select
        accept Define_Catalog;
        Clear.all;
        The_Targets := Name.Actual_List;
        New_List := True;
        if Site.Is_Defined then
          Define_Targets;
        else
          Log.Warning ("Site not defined");
        end if;
      or
        accept Set (The_Range : Az_Range) do
          The_Actual_Az_Range := The_Range;
        end Set;
      or
        accept Set (The_Selection : Selection) do
          The_Actual_Selection := The_Selection;
        end Set;
      or
        accept Set (The_Selection : Moon_Feature_Selection) do
          The_Feature_Selection := The_Selection;
        end Set;
      or
        accept Set (Sorted : Switch) do
          Is_Az_Sorted := Sorted = On;
        end Set;
      or
        accept Update_List;
        if Site.Is_Defined then
          Define_Targets;
        else
          New_List := True;
          Clear.all;
        end if;
      or
        accept Get_For (Target_Name :     String;
                        Target_Id   : out Name.Id)
        do
          Target_Id := Name.Item_Of (The_Targets, Target_Name);
          if Target_Id = Name.No_Id then
            Target_Id := The_Found_Id;
          end if;
          The_Found_Id := Name.No_Id;
        end Get_For;
      or
        accept Get_For (The_Direction :     Space.Direction;
                        Tolerance     :     Space.Distance;
                        Target_Id     : out Name.Id)
        do
          Target_Id := Name.Item_Of (The_Targets, The_Direction, Tolerance);
          The_Found_Id := Target_Id;
        end Get_For;
      or
        accept Stop;
        exit;
      or delay 20.0;
        if Site.Is_Defined then
          Define_Targets;
        end if;
      end select;
    end loop;
    Log.Write ("handler end");
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Handler;


  function J2000_Direction_Of (Id : Name.Id) return Space.Direction is
    use type Name.Id;
  begin
    if Id = Name.No_Id then
      return Space.Unknown_Direction;
    else
      declare
        Ut : constant Time.Ut := Time.Universal;
      begin
        case Name.Kind_Of (Id) is
        when Name.Moon =>
          return Moon_Direction_Of (Id, Ut);
        when Name.Planet =>
          return Solar_System_Direction_Of (Id, Ut);
        when Name.Sky_Object =>
          return Sky.Data.Direction_Of (Name.Object_Of (Id), Ut, Is_J2000 => True);
        when others =>
          return Space.Unknown_Direction;
        end case;
      end;
    end if;
  end J2000_Direction_Of;


  function Text_Of (Visible_In : Duration) return String is

    Visible : constant String := Lexicon.Image_Of (Lexicon.Visible);
    In_Time : constant String := Lexicon.Image_Of (Lexicon.In_Time);

    function Image_Of (Value : Natural;
                       Unit  : String) return String is
      Image : constant String := Value'img & Unit;
    begin
      if Value = 0 then
        return "";
      else
        return Image;
      end if;
    end Image_Of;

    Second : constant String := "s";
    Minute : constant String := "m";
    Hour   : constant String := "h";

    function Duration_Text_Of (Value      : Natural;
                               Upper_Unit : String;
                               Lower_Unit : String) return String is
    begin
      return Visible & ' ' & In_Time  & Image_Of (Value / 60, Upper_Unit) & Image_Of (Value mod 60, Lower_Unit);
    end Duration_Text_Of;

    Delta_Time : constant Natural := Natural(Visible_In);

  begin -- Text_Of
    if Delta_Time = 0 then
      return Visible;
    elsif Delta_Time < 3600 then
      return Duration_Text_Of (Delta_Time, Minute, Second);
    else
      return Duration_Text_Of ((Delta_Time + 59) / 60, Hour, Minute);
    end if;
  end Text_Of;


  function Moon_Direction_Of (Id : Name.Id := Name.No_Id;
                              UT : Time.Ut) return Space.Direction is (Moon.Direction_Of (Id, UT));


  function Solar_System_Direction_Of (Item : Name.Id;
                                      Ut   : Time.Ut) return Space.Direction is
    Planet_Name : constant String := Name.Image_Of (Item);
  begin
    declare
      E : constant Lexicon.Word := Lexicon.Word_Of (Planet_Name);
      P : constant Standard.Solar_System.Body_Name := Standard.Solar_System.Body_Name'value(E'img);
    begin
      return Standard.Solar_System.Direction_Of (P, Ut);
    end;
  exception
  when others =>
    Log.Warning ("Unknown Planet: " & Planet_Name);
    return Space.Unknown_Direction;
  end Solar_System_Direction_Of;


  function Description_Of (Id : Name.Id) return String is
    use type Name.Id;
  begin
    if Id /= Name.No_Id then
      case Name.Kind_Of (Id) is
      when Name.Sky_Object =>
        return Sky.Data.Descriptor_Of (Name.Object_Of (Id));
      when Name.Moon =>
        return Moon.Feature_Kind_Of (Id);
      when Name.Planet =>
        if Name.Image_Of (Id) = "Pluto" then
          return "Dwarf Planet"; -- Zwergplanet
        else
          return "Planet";
        end if;
      when others =>
        null;
      end case;
    end if;
    return "";
  end Description_Of;

end Targets;
