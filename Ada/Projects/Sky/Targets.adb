-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Data;
with Moon;
with Neo;
with Site;
with Sky_Line;
with Solar_System;
with Sssb;
with Time;
with Traces;

package body Targets is

  package Log is new Traces ("Targets");

  task type Handler (Clear  : access procedure;
                     Define : access procedure (List : Name.Id_List_Access);
                     Update : access procedure) is

    entry Define_Catalog;

    entry Set (The_Selection : Selection);

    entry Update_List;

    entry Get_For (Target_Name :     String;
                   Target_Id   : out Name.Id);

    entry Get_For (The_Direction :     Space.Direction;
                   Tolerance     :     Space.Distance;
                   Target_Id     : out Name.Id);

    entry Stop;

  end Handler;


  The_Handler : access Handler;


  procedure Start (Clear  : access procedure;
                   Define : access procedure (List : Name.Id_List_Access);
                   Update : access procedure) is
  begin
    The_Handler := new Handler (Clear  => Clear,
                                Define => Define,
                                Update => Update);
  end Start;


  procedure Define_Catalog is
  begin
    The_Handler.Define_Catalog;
  end Define_Catalog;


  procedure Set (The_Selection : Selection) is
  begin
    The_Handler.Set (The_Selection);
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

    The_Targets : aliased Name.Id_List;
    New_List    : Boolean := False;

    The_Actual_Selection : Selection;

    function Is_Selected (The_Objects : Objects) return Boolean is
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


    procedure Define_Targets is

      function Is_Visible (Direction : Space.Direction) return Boolean is
      begin
        return Sky_Line.Is_Above (Direction => Direction,
                                  Lmst      => Time.Lmst);
      end Is_Visible;

      function Is_To_Add (Item      : Selection;
                          Direction : Space.Direction) return Boolean is
      begin
        return Space.Direction_Is_Known (Direction) and then Is_Selected (Item) and then Is_Visible (Direction);
      end Is_To_Add;

      The_Changes : Natural := 0;

      Ut : constant Time.Ut := Time.Universal;

    begin -- Define_Targets
      for Index in The_Targets.Ids'first .. The_Targets.Last loop
        declare

          Item : Name.Id renames The_Targets.Ids(Index);

          function Is_To_Add return Boolean is
          begin
            case Name.Kind_Of (Item) is
            when Name.Moon =>
              return Is_To_Add (Solar_System, Moon.Direction_Of (Item, Ut));
            when Name.Sun | Name.Planet =>
              return Is_To_Add (Solar_System, Standard.Solar_System.Direction_Of (Item, Ut));
            when Name.Small_Solar_System_Body =>
              return Is_To_Add (Solar_System, Sssb.Direction_Of (Item, Ut));
            when Name.Near_Earth_Object =>
              return Is_Selected (Near_Earth_Objects) and then Neo.Is_Arriving (Item);
            when Name.Landmark =>
              return The_Actual_Selection = All_Objects;
            when Name.Sky_Object =>
              declare
                Object : constant Data.Object := Name.Object_Of (Item);
              begin
                case Data.Type_Of (Object) is
                when Data.Landmark  =>
                  raise Program_Error;
                when Data.Quasar  =>
                  return Is_To_Add (Galaxies, Data.Direction_Of (Object, Ut));
                when Data.Galaxy  =>
                  return Is_To_Add (Galaxies, Data.Direction_Of (Object, Ut));
                when Data.Nebula  =>
                  return Is_To_Add (Nebulas, Data.Direction_Of (Object, Ut));
                when Data.Cluster =>
                  return Is_To_Add (Clusters, Data.Direction_Of (Object, Ut));
                when Data.Stars =>
                  return Is_To_Add (Open_Clusters, Data.Direction_Of (Object, Ut));
                when Data.Double =>
                  return Is_To_Add (Multiple_Stars, Data.Direction_Of (Object, Ut));
                when Data.Star =>
                  return Is_To_Add (Stars, Data.Direction_Of (Object, Ut));
                when Data.Satellite =>
                  raise Program_Error;
                when Data.Unknown =>
                  return False;
                end case;
              end;
            end case;
          end Is_To_Add;

        begin
          if Name.Visibility_Changed_For (Item, Is_To_Add) then
            The_Changes := The_Changes + 1;
          end if;
        end;
      end loop;
      if New_List or (The_Changes > 100) then
        New_List := False;
        Clear.all;
        Name.Clear_History_For (The_Targets);
        Define (The_Targets'unchecked_access);
      else
        Update.all;
      end if;
    end Define_Targets;

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
        end if;
      or
        accept Set (The_Selection : Selection) do
          The_Actual_Selection := The_Selection;
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
        end Get_For;
      or
        accept Get_For (The_Direction :     Space.Direction;
                        Tolerance     :     Space.Distance;
                        Target_Id     : out Name.Id)
        do
          Target_Id := Name.Item_Of (The_Targets, The_Direction, Tolerance);
        end Get_For;
      or
        accept Stop;
        exit;
      or delay 10.0;
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

end Targets;
