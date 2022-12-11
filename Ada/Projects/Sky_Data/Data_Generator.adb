-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.IO_Exceptions;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with Strings;

package body Data_Generator is

  Undefined : constant := 0;

  Symbad_Folder : constant String := "H:\Source\Astronomy\Ada\Projects\Sky_Data\";

  Sky_Data_Folder : constant String := "H:\Source\Astronomy\Ada\SkyData\";

  type Object is new Natural range Undefined .. 16400;

  type Object_Type is (Star, Double, Stars, Cluster, Galaxy, Nebula, Quasar, Unknown);

  type Degrees   is delta 0.00000001 digits 12;
  type Motion    is delta 0.01       digits 7;
  type Magnitude is delta 0.001      digits 5;

  type Information is record
    Name        : access constant String := new String'[];
    Descriptor  : access constant String := new String'[];
    Ra_J2000    : Degrees     := 0.0;
    Dec_J2000   : Degrees     := 0.0;
    Ra_Motion   : Motion      := 0.0;
    Dec_Motion  : Motion      := 0.0;
    Vmag        : Magnitude   := 0.0;
    Kind        : Object_Type := Unknown;
  end record;

  type Objects is array (Object) of Information;

  The_Objects     : Objects;
  The_Last_Object : Object;

  type Object_Array is array (Positive range <>) of Object;

  The_Caldwell_Catalog : Object_Array(1..109)    := [others => Undefined];
  The_Hr_Catalog       : Object_Array(1..9999)   := [others => Undefined];
  The_Hip_Catalog      : Object_Array(1..118400) := [others => Undefined];
  The_Messier_Catalog  : Object_Array(1..110)    := [others => Undefined];
  The_Ngc_Catalog      : Object_Array(1..9999)   := [others => Undefined];
  The_Ocl_Catalog      : Object_Array(1..1200)   := [others => Undefined];
  The_Quasars_Catalog  : Object_Array(1..999)    := [others => Undefined];

  Unknown_Value : exception;


  function Motion_Of (Image : String) return Motion is
  begin
    if Strings.Trimmed (Image) = "~" then
      return 0.0;
    else
      return Motion'value (Image);
    end if;
  exception
  when others =>
    Ada.Text_IO.Put_Line ("Motion_Of:" & Image);
    raise Unknown_Value;
  end Motion_Of;


  function Magnitude_Of (Image : String) return Magnitude is
  begin
    if Strings.Trimmed (Image) = "~" then
      return 12.0; -- default
    else
      return Magnitude'value (Image);
    end if;
  exception
  when others =>
    Ada.Text_IO.Put_Line ("Magnitude_Of:" & Image);
    raise Unknown_Value;
  end Magnitude_Of;


  function Value_Of (Item : String) return Natural is
    Image : constant String := Strings.Trimmed (Item);
  begin
    for Index in Image'range loop
      if Image(Index) = '.' then
        return Natural'value(Image(Image'first .. Index - 1));
      end if;
    end loop;
    return Natural'value(Image);
  exception
  when others =>
    raise Unknown_Value;
  end Value_Of;


  File : Ada.Text_IO.File_Type;

  procedure Put (Item : String) is
  begin
    Ada.Text_IO.Put_Line (File, Item);
  end Put;


  function Right_In (Item : String;
                     Size : Natural) return String is
    Image : constant String := "               " & Item;
  begin
    return Image(Image'last - Size + 1 .. Image'last);
  end Right_In;


  procedure Read_Objects is

    Filename : constant String := Symbad_Folder & "simbad.txt";

    The_Object        : Object := Undefined;
    The_Actual_Object : Object := Undefined;
    The_Name          : Strings.Element;

    type Header is (Id_1, Id_2, Id_3, Id_4, Id_5, Id_6, Object_Kind, Loc_J2000, Pm, Vmag);

    Error : exception;

  begin -- Read_Objects
    Ada.Text_IO.Put_Line ("Generate sky data from " & Filename);
    Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Filename);
    while not Ada.Text_IO.End_Of_File (File) loop
      if The_Actual_Object = The_Object then
        The_Object := Object'succ(The_Object);
      end if;
      The_Actual_Object := Undefined;
      declare

        Line  : constant String := Ada.Text_IO.Get_Line (File);
        Parts : constant Strings.Item := Strings.Item_Of (Line, '|');

        function Image_Of (Item : Header) return String is
        begin
          return Strings.Trimmed (Parts(Strings.First_Index + Header'pos(Item)));
        end Image_Of;

        function Parts_Of (Item : Header) return Strings.Item is
          Image : constant String := Image_Of (Item);
        begin
          return Strings.Item_Of (Image, ' ', Purge => True);
        end Parts_Of;

        procedure Get_Actual_For (Id  : Header) is

          Actual_Parts : constant Strings.Item := Parts_Of (Id);

          procedure Assign_Actual_Object (Actual_Object : Object) is
          begin
            if Actual_Object /= Undefined then
              if The_Actual_Object = Undefined then
                The_Actual_Object := Actual_Object;
              elsif The_Actual_Object /= Actual_Object then
                Ada.Text_IO.Put_Line ("Data Inconsistency at: " & Actual_Object'img);
              end if;
            end if;
          end Assign_Actual_Object;

          function Index return Natural is
          begin
            return Value_Of (Actual_Parts(Strings.First_Index + 1));
          end Index;

        begin -- Get_Actual_For
          if Actual_Parts.Count > 1 then
            declare
              Catalog : constant String := Strings.Trimmed (Actual_Parts(Strings.First_Index));
            begin
              if Catalog = "NAME" then
                Strings.Clear (The_Name);
                for The_Index in Strings.First_Index + 1 .. Actual_Parts.Count loop
                  Strings.Append (The_Name, Strings.Legible_Of (Actual_Parts(The_Index)));
                  if The_Index < Actual_Parts.Count then
                    Strings.Append (The_Name, ' ');
                  end if;
                end loop;
              elsif Actual_Parts.Count = 2 then
                if Catalog = "C" then
                  Assign_Actual_Object (The_Caldwell_Catalog(Index));
                elsif Catalog = "HR" then
                  Assign_Actual_Object (The_Hr_Catalog(Index));
                elsif Catalog = "HIP" then
                  Assign_Actual_Object (The_Hip_Catalog(Index));
                elsif Catalog = "M" then
                  Assign_Actual_Object (The_Messier_Catalog(Index));
                elsif Catalog = "NGC" then
                  Assign_Actual_Object (The_Ngc_Catalog(Index));
                elsif Catalog = "OCl" then
                  Assign_Actual_Object (The_Ocl_Catalog(Index));
                elsif Catalog = "3C" then
                  Assign_Actual_Object (The_Quasars_Catalog(Index));
                end if;
              end if;
            end;
          end if;
        exception
        when others =>
          Ada.Text_IO.Put_Line ("Unknown Object: " & Image_Of (Id));
        end Get_Actual_For;

        procedure Handle (Id  : Header) is

          Unknown_Index : exception;

          The_Parts : constant Strings.Item := Parts_Of (Id);

          function Index return Natural is
          begin
            return Value_Of (The_Parts(Strings.First_Index + 1));
          exception
          when others =>
            raise Unknown_Index;
          end Index;

        begin -- Handle
          if The_Parts.Count = 2 then
            declare
              Catalog : constant String := Strings.Trimmed (The_Parts(Strings.First_Index));
            begin
              if Catalog = "C" then
                The_Caldwell_Catalog(Index) := The_Actual_Object;
              elsif Catalog = "HR" then
                The_Hr_Catalog(Index) := The_Actual_Object;
              elsif Catalog = "HIP" then
                The_Hip_Catalog(Index) := The_Actual_Object;
              elsif Catalog = "M" then
                The_Messier_Catalog(Index) := The_Actual_Object;
              elsif Catalog = "NGC" then
                The_Ngc_Catalog(Index) := The_Actual_Object;
              elsif Catalog = "OCl" then
                The_Ocl_Catalog(Index) := The_Actual_Object;
              elsif Catalog = "3C" then
                The_Quasars_Catalog(Index) := The_Actual_Object;
              end if;
            end;
          end if;
        exception
        when Unknown_Index =>
          null;
        when others =>
          Ada.Text_IO.Put_Line ("Can't store: " & Image_Of (Id));
          raise Error;
        end Handle;

        Description : constant String := Image_Of (Object_Kind);
        Loc_Parts   : constant Strings.Item := Parts_Of (Loc_J2000);
        Pm_Parts    : constant Strings.Item := Parts_Of (Pm);
        Vmag_Image  : constant String := Image_Of (Vmag);
        Object_Info : Information renames The_Objects(The_Object);

        function Found (Item : String) return Boolean is
        begin
          return Strings.Location_Of (Item, Strings.Lowercase_Of (Description)) /= Strings.Not_Found;
        end Found;

        use type Strings.Element;

      begin
        Strings.Clear (The_Name);
        Get_Actual_For (Id_1);
        Get_Actual_For (Id_2);
        Get_Actual_For (Id_3);
        Get_Actual_For (Id_4);
        Get_Actual_For (Id_5);
        Get_Actual_For (Id_6);
        if The_Actual_Object = Undefined then -- new Object
          The_Actual_Object := The_Object;
          Object_Info.Name := new String'(+The_Name);
          Object_Info.Descriptor := new String'(Description);
          Object_Info.Ra_J2000 := Degrees'value((Loc_Parts(Strings.First_Index)));
          Object_Info.Dec_J2000 := Degrees'value((Loc_Parts(Strings.First_Index + 1)));
          Object_Info.Ra_Motion := Motion_Of ((Pm_Parts(Strings.First_Index)));
          Object_Info.Dec_Motion := Motion_Of ((Pm_Parts(Strings.First_Index + 1)));
          Object_Info.Vmag := Magnitude_Of (Vmag_Image);
          if Found ("quasar") or Found ("blazar") then
            Object_Info.Kind := Quasar;
          elsif Found ("galaxy") or Found ("galaxies") then
            Object_Info.Kind := Galaxy;
          elsif Found ("cluster") then
            if Found ("open") or Found ("cluster of stars") then
              Object_Info.Kind := Stars;
            else
              Object_Info.Kind := Cluster;
            end if;
          elsif Found ("star") or Found ("binary") or Found ("variable") or Found ("cepheid") or Found ("dwarf") then
            if Found ("double") then
              Object_Info.Kind := Double;
            else
              Object_Info.Kind := Star;
            end if;
          else
            Object_Info.Kind := Nebula;
          end if;
        end if;
        Handle (Id_1);
        Handle (Id_2);
        Handle (Id_3);
        Handle (Id_4);
        Handle (Id_5);
        Handle (Id_6);
      exception
      when Unknown_Value =>
        null;
      when others =>
        Ada.Text_IO.Put_Line ("Line: " & Line);
        raise;
      end;
    end loop;
    Ada.Text_IO.Close (File);
    The_Last_Object := The_Object;
    Ada.Text_IO.Put_Line ("Last Object:" & The_Object'img);
  end Read_Objects;


  procedure Create_Data_Base is
  begin
    Ada.Text_IO.Create (File, Name => Sky_Data_Folder & "Catalog-Base.ads");
    Put ("pragma Restrictions (No_Elaboration_Code);");
    Put ("");
    Put ("private package Catalog.Base is");
    Put ("");
    Put ("  subtype Object_Index is Object range 0 .." & The_Last_Object'img & ";");
    Put ("");
    Put ("  Table : constant Objects (Object_Index) := [");
    for Index in 0 .. The_Last_Object loop
      declare

        Actual_Object : constant Information := The_Objects(Index);

        function Separator return String is
        begin
          if Index = The_Last_Object then
            return "];";
          else
            return ", ";
          end if;
        end Separator;

      begin
        Put (Right_In (Index'img, 7) & " => (new String'(""" & Actual_Object.Name.all & """),");
        Put ("            new String'(""" & Actual_Object.Descriptor.all & """),");
        Put (Right_In (Actual_Object.Ra_J2000'img, 24) & ", " &
             Right_In (Actual_Object.Dec_J2000'img, 12)  & ", " &
             Right_In (Actual_Object.Ra_Motion'img, 8)  & ", " &
             Right_In (Actual_Object.Dec_Motion'img, 8)  & ", " &
             Right_In (Actual_Object.Vmag'img, 6)  & ", " &
             Right_In (Strings.Legible_Of (Actual_Object.Kind'img), 7) & ")" & Separator);
      end;
    end loop;
    Put ("");
    Put ("end Catalog.Base;");
    Ada.Text_IO.Close (File);
  end Create_Data_Base;


  The_Object_Count : Natural;

  function Image_Of (The_Object : Object) return String is
  begin
    if The_Object = Undefined then
      return "Undefined";
    else
      The_Object_Count := The_Object_Count + 1;
      return Right_In (The_Object'img, 9);
    end if;
  end Image_Of;


  function Last_Index_Of (The_Array : Object_Array) return Natural is
  begin
    for Index in reverse The_Array'range loop
      if The_Array(Index) /= Undefined then
        return Index;
      end if;
    end loop;
    return 0;
  end Last_Index_Of;


  function Size_Of (Value : Natural) return Natural is
    The_Value : Natural := Value;
    The_Size  : Natural := 1;
  begin
    while The_Value >= 10 loop
      The_Value := The_Value / 10;
      The_Size := The_Size + 1;
    end loop;
    return The_Size;
  end Size_Of;


  procedure Create_Catalog_Hr is
    Last_Index : constant Natural := Last_Index_Of (The_Hr_Catalog);
    Index_Size : constant Natural := Size_Of(Last_Index) + 2;
  begin
    Ada.Text_IO.Create (File, Name => Sky_Data_Folder & "Catalog-Hr.ads");
    Put ("pragma Restrictions (No_Elaboration_Code);");
    Put ("");
    Put ("private package Catalog.Hr is");
    Put ("");
    Put ("  type Object_List is array (1 .." & Last_Index'img & ") of Object;");
    Put ("");
    Put ("  Id : constant Object_List := [");
    The_Object_Count := 0;
    for Index in 1 .. Last_Index loop
      declare

        function Separator return String is
        begin
          if Index = Last_Index then
            return "];";
          else
            return ",";
          end if;
        end Separator;

      begin
        Put ("  " & Right_In (Index'img, Index_Size) & " => " & Image_Of (The_Hr_Catalog(Index)) & Separator);
      end;
    end loop;
    Put ("");
    Put ("  Count : constant Natural :=" & The_Object_Count'img & ";");
    Put ("");
    Put ("end Catalog.Hr;");
    Ada.Text_IO.Close (File);
  end Create_Catalog_Hr;


  procedure Create_Catalog_Caldwell is
    Last_Index : constant Natural := Last_Index_Of (The_Caldwell_Catalog);
    Index_Size : constant Natural := Size_Of(Last_Index) + 2;
  begin
    Ada.Text_IO.Create (File, Name => Sky_Data_Folder & "Catalog-Caldwell.ads");
    Put ("pragma Restrictions (No_Elaboration_Code);");
    Put ("");
    Put ("private package Catalog.Caldwell is");
    Put ("");
    Put ("  type Object_List is array (1 .." & Last_Index'img & ") of Object;");
    Put ("");
    Put ("  Id : constant Object_List := [");
    The_Object_Count := 0;
    for Index in 1 .. Last_Index loop
      declare

        function Separator return String is
        begin
          if Index = Last_Index then
            return "];";
          else
            return ",";
          end if;
        end Separator;

      begin
        Put ("  " & Right_In (Index'img, Index_Size) & " => " & Image_Of (The_Caldwell_Catalog(Index)) & Separator);
      end;
    end loop;
    Put ("");
    Put ("  Count : constant Natural :=" & The_Object_Count'img & ";");
    Put ("");
    Put ("end Catalog.Caldwell;");
    Ada.Text_IO.Close (File);
  end Create_Catalog_Caldwell;


  procedure Create_Catalog_Hip is
    Last_Index : constant Natural := Last_Index_Of (The_Hip_Catalog);
    Index_Size : constant Natural := Size_Of(Last_Index) + 2;
  begin
    Ada.Text_IO.Create (File, Name => Sky_Data_Folder & "Catalog-Hip.ads");
    Put ("pragma Restrictions (No_Elaboration_Code);");
    Put ("");
    Put ("private package Catalog.Hip is");
    Put ("");
    Put ("  type Object_List is array (1 .." & Last_Index'img & ") of Object;");
    Put ("");
    Put ("  Id : constant Object_List := [");
    The_Object_Count := 0;
    for Index in 1 .. Last_Index loop
      declare

        function Separator return String is
        begin
          if Index = Last_Index then
            return "];";
          else
            return ",";
          end if;
        end Separator;

      begin
        Put ("  " & Right_In (Index'img, Index_Size) & " => " & Image_Of (The_Hip_Catalog(Index)) & Separator);
      end;
    end loop;
    Put ("");
    Put ("  Count : constant Natural :=" & The_Object_Count'img & ";");
    Put ("");
    Put ("end Catalog.Hip;");
    Ada.Text_IO.Close (File);
  end Create_Catalog_Hip;


  procedure Create_Catalog_Messier is
    Last_Index : constant Natural := Last_Index_Of (The_Messier_Catalog);
    Index_Size : constant Natural := Size_Of(Last_Index) + 2;
  begin
    Ada.Text_IO.Create (File, Name => Sky_Data_Folder & "Catalog-Messier.ads");
    Put ("pragma Restrictions (No_Elaboration_Code);");
    Put ("");
    Put ("private package Catalog.Messier is");
    Put ("");
    Put ("  type Object_List is array (1 .." & Last_Index'img & ") of Object;");
    Put ("");
    Put ("  Id : constant Object_List := [");
    The_Object_Count := 0;
    for Index in 1 .. Last_Index loop
      declare

        function Separator return String is
        begin
          if Index = Last_Index then
            return "];";
          else
            return ",";
          end if;
        end Separator;

      begin
        Put ("  " & Right_In (Index'img, Index_Size) & " => " & Image_Of (The_Messier_Catalog(Index)) & Separator);
      end;
    end loop;
    Put ("");
    Put ("  Count : constant Natural :=" & The_Object_Count'img & ";");
    Put ("");
    Put ("end Catalog.Messier;");
    Ada.Text_IO.Close (File);
  end Create_Catalog_Messier;


  procedure Create_Catalog_Ngc is
    Last_Index : constant Natural := Last_Index_Of (The_Ngc_Catalog);
    Index_Size : constant Natural := Size_Of(Last_Index) + 2;
  begin
    Ada.Text_IO.Create (File, Name => Sky_Data_Folder & "Catalog-Ngc.ads");
    Put ("pragma Restrictions (No_Elaboration_Code);");
    Put ("");
    Put ("private package Catalog.Ngc is");
    Put ("");
    Put ("  type Object_List is array (1 .." & Last_Index'img & ") of Object;");
    Put ("");
    Put ("  Id : constant Object_List := [");
    The_Object_Count := 0;
    for Index in 1 .. Last_Index loop
      declare

        function Separator return String is
        begin
          if Index = Last_Index then
            return "];";
          else
            return ",";
          end if;
        end Separator;

      begin
        Put ("  " & Right_In (Index'img, Index_Size) & " => " & Image_Of (The_Ngc_Catalog(Index)) & Separator);
      end;
    end loop;
    Put ("");
    Put ("  Count : constant Natural :=" & The_Object_Count'img & ";");
    Put ("");
    Put ("end Catalog.Ngc;");
    Ada.Text_IO.Close (File);
  end Create_Catalog_Ngc;


  procedure Create_Catalog_Ocl is
    Last_Index : constant Natural := Last_Index_Of (The_Ocl_Catalog);
    Index_Size : constant Natural := Size_Of(Last_Index) + 2;
  begin
    Ada.Text_IO.Create (File, Name => Sky_Data_Folder & "Catalog-Ocl.ads");
    Put ("pragma Restrictions (No_Elaboration_Code);");
    Put ("");
    Put ("private package Catalog.Ocl is");
    Put ("");
    Put ("  type Object_List is array (1 .." & Last_Index'img & ") of Object;");
    Put ("");
    Put ("  Id : constant Object_List := [");
    The_Object_Count := 0;
    for Index in 1 .. Last_Index loop
      declare

        function Separator return String is
        begin
          if Index = Last_Index then
            return "];";
          else
            return ",";
          end if;
        end Separator;

      begin
        Put ("  " & Right_In (Index'img, Index_Size) & " => " & Image_Of (The_Ocl_Catalog(Index)) & Separator);
      end;
    end loop;
    Put ("");
    Put ("  Count : constant Natural :=" & The_Object_Count'img & ";");
    Put ("");
    Put ("end Catalog.Ocl;");
    Ada.Text_IO.Close (File);
  end Create_Catalog_Ocl;


  procedure Create_Catalog_Quasars is
    Last_Index : constant Natural := Last_Index_Of (The_Quasars_Catalog);
    Index_Size : constant Natural := Size_Of(Last_Index) + 2;
  begin
    Ada.Text_IO.Create (File, Name => Sky_Data_Folder & "Catalog-Quasars.ads");
    Put ("pragma Restrictions (No_Elaboration_Code);");
    Put ("");
    Put ("private package Catalog.Quasars is");
    Put ("");
    Put ("  type Object_List is array (1 .." & Last_Index'img & ") of Object;");
    Put ("");
    Put ("  Id : constant Object_List := [");
    The_Object_Count := 0;
    for Index in 1 .. Last_Index loop
      declare

        function Separator return String is
        begin
          if Index = Last_Index then
            return "];";
          else
            return ",";
          end if;
        end Separator;

      begin
        Put ("  " & Right_In (Index'img, Index_Size) & " => " & Image_Of (The_Quasars_Catalog(Index)) & Separator);
      end;
    end loop;
    Put ("");
    Put ("  Count : constant Natural :=" & The_Object_Count'img & ";");
    Put ("");
    Put ("end Catalog.Quasars;");
    Ada.Text_IO.Close (File);
  end Create_Catalog_Quasars;


  procedure Execute is
  begin
    Read_Objects;
    Create_Data_Base;
    Create_Catalog_Caldwell;
    Create_Catalog_Hr;
    Create_Catalog_Hip;
    Create_Catalog_Messier;
    Create_Catalog_Ngc;
    Create_Catalog_Ocl;
    Create_Catalog_Quasars;
    Ada.Text_IO.Put_Line ("Complete");
  exception
  when Ada.IO_Exceptions.Name_Error =>
    Ada.Text_IO.Put_Line ("File not Found");
  when Occurrence: others =>
    declare
      Exception_Info : constant String := GNAT.Traceback.Symbolic.Symbolic_Traceback (Occurrence);
      Lines          : constant Strings.Item := Strings.Item_Of (Exception_Info, Ascii.Lf);
    begin
      for Line of Lines loop
        Ada.Text_IO.Put_Line (Line);
      end loop;
    end;
  end Execute;

end Data_Generator;
