-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada_95.File;
with Ada_95.Project;
with Build;
with Build_Parser;
with Configuration;
with Directory;
with File;
with Files;
with Log;
with Promotion;
with Project.Gpr;
with Project.Resource;
with Server;
with Target;

package body Project is

  function "=" (Unused_Left, Unused_Right : Text.List) return Boolean is (False);

  package Installations is new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                                       Element_Type => Text.List);

  Language : constant String := "Ada";

  Body_Extension : constant String := ".adb";

  Default_Ada_Version : constant String := "gnat2022";
  Legacy_Ada_Version  : constant String := "gnat2012";

  use type Text.Strings;

  Leaf_Directory_Exceptions : constant Text.Strings := ["$Osx", "$Linux"] + Build.Sub_Directories;


  procedure Set_Confirmation (Message : String := "") is
  begin
    Promotion.Set_Message (Server.Confirmation & "%b" & Message);
  end Set_Confirmation;


  function Confirmation_Message return String is
  begin
    if Promotion.Message_Ready then
      return Promotion.Message;
    end if;
    return "";
  end Confirmation_Message;


  The_Actual_Project     : Text.String;
  The_Project_Name       : Text.String;
  The_Project_Directory  : Text.String;
  The_Language_Directory : Text.String;
  The_Source_Root        : Text.String;
  The_Binary_Root        : Text.String;
  The_Product_Directory  : Text.String;
  The_Product_Sub_Path   : Text.String;
  The_Promotion_Areas    : Text.String;
  The_Library_Path       : Text.String;

  The_Ada_Version : Text.String;

  The_Modifier_Tool       : Text.String;
  The_Modifier_Parameters : Text.String;
  The_Modifier_Message    : Text.String;

  The_Installation_Map : Installations.Map;

  The_Source_Directories : Text.List;
  The_Ignore_Areas       : Text.List;
  The_Implied_Areas      : Text.List;
  The_Reference_Areas    : Text.List;
  The_Library_Areas      : Text.List;
  The_Base_Path          : Text.List;
  The_Promotion_List     : Text.List;

  Project_Is_Defined      : Boolean := False;
  The_Case_Handling_Style : Case_Modification;

  The_Actual_Tools_Directory : Text.String;

  Ada_Project_Path : constant String := "ADA_PROJECT_PATH";


  procedure Set_Project_Undefined is
  begin
    Text.Clear (The_Actual_Project);
    Text.Clear (The_Project_Name);
    Text.Clear (The_Project_Directory);
    Text.Clear (The_Language_Directory);
    Text.Clear (The_Source_Root);
    Text.Clear (The_Binary_Root);
    Text.Clear (The_Product_Directory);
    Text.Clear (The_Product_Sub_Path);
    Text.Clear (The_Promotion_Areas);
    Text.Clear (The_Library_Path);
    Text.Clear (The_Ada_Version);
    Text.Clear (The_Modifier_Tool);
    Text.Clear (The_Modifier_Parameters);
    Text.Clear (The_Modifier_Message);
    Text.Clear (The_Actual_Tools_Directory);
    The_Ignore_Areas.Clear;
    The_Implied_Areas.Clear;
    The_Library_Areas.Clear;
    The_Reference_Areas.Clear;
    The_Base_Path.Clear;
    The_Source_Directories.Clear;
    The_Promotion_List.Clear;
    The_Library_Names.Clear;
    The_Library_Sources.Clear;
    The_Installation_Map.Clear;
    Project_Is_Defined := False;
  end Set_Project_Undefined;

  function Tools_Defined return Boolean renames Build.Tools_Defined;

  function Tools_Directory return String renames Build.Tools_Directory;

  function Tools_Folder return String is (Tools_Directory & Files.Separator);

  function Tools_Kind return String renames Build.Tools_Kind_Image;

  function Has_Second_Compiler return Boolean renames Build.Has_Second_Tools_Directory;

  procedure Set_Back_To_First_Compiler renames Build.Set_Back_To_First;

  function Is_Maching (Filename : String) return Boolean renames Build.Is_Maching;

  function Compiler_Area return String renames Build.Compiler_Area;

  function Language_Directory return String is (The_Language_Directory.S);

  function Language_Folder return String is (Language_Directory & Files.Separator);

  function Definition_File return String is (Language_Folder & Server.Project_File);

  function Directory_Path return String is (The_Project_Directory.S);

  function Folder return String is (Directory_Path & Files.Separator);

  function Gpr_Source_Folder return String is (Directory_Path & Compiler_Area & Files.Separator);

  function Actual return String is (The_Actual_Project.S);

  function Name return String is (The_Project_Name.S);

  function Binary_Folder return String is (The_Binary_Root & [Files.Separator]);

  function Source_Folder return String is (The_Source_Root & [Files.Separator]);

  function Product_Directory return String is (The_Product_Directory & Compiler_Area);

  function Product_Sub_Path return String is (The_Product_Sub_Path.S);

  function Product_Extension return String is (if Is_Dll then ".dll" else ".exe");

  function Product return String is (Product_Directory & Product_Sub_Path & Files.Separator & Name & Product_Extension);

  function Is_Legacy_Compiler return Boolean is (Product_Version = Legacy_Product_Version);

  function Modifier_Tool return String is (The_Modifier_Tool.S);


  function Modifier_Parameters return String is

    Product_Id : constant String := "%Product%";

    The_Parameters : Text.String := The_Modifier_Parameters;

  begin
    The_Parameters.Replace (Product_Id, By => Product);
    return The_Parameters.S;
  end Modifier_Parameters;


  function Installation_Destinations return Text.List is
    Cursor : constant Installations.Cursor := The_Installation_Map.Find (Name);
  begin
    if Installations.Has_Element (Cursor) then
      return Installations.Element (Cursor);
    else
      return [];
    end if;
  end Installation_Destinations;


  function Modifier_Success return String is
  begin
    return The_Modifier_Message.S;
  end Modifier_Success;


  function Ada_Version return String is
  begin
    if Is_Legacy_Compiler then
      return Legacy_Ada_Version;
    else
      return The_Ada_Version.S;
    end if;
  end Ada_Version;


  function Product_Name return String is
    Tools_Directory_Parts : constant Text.Strings := Text.Strings_Of (Tools_Directory, Files.Separator);
  begin
    if Tools_Directory_Parts.Count >= 2 then
      return Tools_Directory_Parts(Tools_Directory_Parts.Count - 2);
    else
      return "";
    end if;
  end Product_Name;


  function Product_Version return String is
    Tools_Directory_Parts : constant Text.Strings := Text.Strings_Of (Tools_Directory, Files.Separator);
  begin
    if Tools_Directory_Parts.Count >= 2 then
      return Tools_Directory_Parts(Tools_Directory_Parts.Count - 1);
    else
      return "";
    end if;
  end Product_Version;


  function Is_Dll return Boolean is
  begin
    pragma Assert (Build.Is_Defined);
    return Build.Is_Dll;
  end Is_Dll;


  function Has_Style return Boolean is
  begin
    pragma Assert (Build.Is_Defined);
    return Build.Has_Style;
  end Has_Style;


  function Is_Defined return Boolean is
  begin
    Log.Write ("||| Project.Is_Defined: " & Project_Is_Defined'img);
    return Project_Is_Defined;
  end Is_Defined;


  The_Configuration_Handle : access Configuration.File_Handle;

  function Element_For (Key         : String;
                        Application : String) return String is

    Handle  : constant Configuration.Section_Handle := Configuration.Handle_For (The_Configuration_Handle.all,
                                                                                 Application);
    Element : constant String := Configuration.Value_Of (Handle, Key);

  begin
    return Element;
  end Element_For;


  function Is_Base_Area (Item : String) return Boolean is
  begin
    for Area of The_Implied_Areas loop
      if Item = Area then
        return True;
      end if;
    end loop;
    for Area of The_Reference_Areas loop
      if Item = Area then
        return True;
      end if;
    end loop;
    for Area of The_Library_Areas loop
      if Item = Area then
        return True;
      end if;
    end loop;
    return False;
  end Is_Base_Area;


  function Is_To_Ignore (The_Directory : String) return Boolean is
  begin
    for Area of The_Ignore_Areas loop
      if Text.Matches (The_Directory, Source_Folder & Area) then
        return True;
      end if;
    end loop;
    return False;
  end Is_To_Ignore;


  function Part_Of (Filename : String;
                    After    : String) return String is
  begin
    if After'length > Filename'length then
      return "???";
    elsif Filename(Filename'first .. Filename'first + After'length - 1) = After then
      return Filename(Filename'first + After'length .. Filename'last);
    end if;
    return "";
  end Part_Of;


  function Project_Subdirectory return String is
  begin
    return Folder & Build.Directories_Area;
  end Project_Subdirectory;


  function Source_Directories return Text.List is
    use type Text.List;
  begin
    if Directory.Exists (Project_Subdirectory) then
      return Project_Subdirectory + The_Source_Directories;
    else
      return The_Source_Directories;
    end if;
  end Source_Directories;


  function Target_Directory return String is
    Project_Area : constant String := Part_Of (Directory_Path, After => Source_Folder);
  begin
    return Binary_Folder & Project_Area & Compiler_Area;
  end Target_Directory;


  function Target_Folder return String is (Target_Directory & Files.Separator);


  function Created_Target_Folder return String is
    The_Folder : constant String := Target_Folder;
  begin
    Files.Create_Folder (The_Folder);
    return The_Folder;
  end Created_Target_Folder;


  function Object_Directory return String is
  begin
    return Target_Folder & Object_Area;
  end Object_Directory;


  function Object_Folder return String is
  begin
    return Object_Directory & Files.Separator;
  end Object_Folder;


  function Ali_Directory return String is
  begin
    return Object_Folder & Ali_Area;
  end Ali_Directory;


  type Phase is (Unknown, Initializing, Promoting);

  The_Phase : Phase := Unknown;

  Error_Set : exception;

  procedure Set_Error (Message : String) is
  begin
    case The_Phase is
    when Initializing =>
      Promotion.Set_Message (Server.Not_Confirmed & Message);
      raise Error_Set;
    when Promoting =>
      Promotion.Set_Error (Message);
    when Unknown =>
      raise Program_Error;
    end case;
  end Set_Error;


  function Library_Check (Item : String) return Build.Library_Check_Completion is
  begin
    if Gpr.Contains_For (Build.Tools_Directory, Item) then
      return Build.Library_Ok;
    end if;
    return Build.Library_Not_Found;
  end Library_Check;


  procedure Check_Icon is
    Icon_File : constant String := Folder & Name & ".ico";
  begin
    pragma Assert (Build.Is_Defined);
    if Build.Has_Icon and then not File.Exists (Icon_File) then
      Set_Error ("Icon file <" & Icon_File & "> not found");
    end if;
  end Check_Icon;


  procedure Create_Object_Directories is
  begin
    Files.Create_Folder (Ali_Directory);
  exception
  when others =>
    null;
  end Create_Object_Directories;


  function Environment_Defined (Second_Compilation : Boolean := False) return Boolean is
  begin
    if Second_Compilation or else Build_Parser.Evaluated then
      Create_Object_Directories;
      Gpr.Define_Information_For (Build.Tools_Directory, Build.Libraries);
      The_Library_Path := Gpr.Library_Path;
      return True;
    else
      return False;
    end if;
  end Environment_Defined;


  procedure Generate_New_Resource is

    procedure Delete_Target_Directory is
    begin
      Directory.Delete (Target_Directory);
    exception
    when others =>
      Set_Error ("Directory locked: " & Target_Directory);
    end Delete_Target_Directory;

  begin -- Has_New_Resource
    The_Phase := Promoting;
    Check_Icon;
    declare
      Resource_Filename : constant String := Resource.Filename;
    begin
      if not File.Exists (Resource_Filename) or else File.Is_Newer (Program_Unit, Resource_Filename) then
        if Tools_Defined then
          Delete_Target_Directory;
          Create_Object_Directories;
          Resource.Generate;
        end if;
      end if;
      The_Phase := Unknown;
    end;
  end Generate_New_Resource;


  procedure Create_Work_Area_For (Project_Parts :     Text.Vector;
                                  The_Work_Path : out Text.List) is

    function Part_Of (Index : Positive) return String is
    begin
      return Project_Parts(Index);
    end Part_Of;

  begin -- Create_Work_Area_For
    The_Work_Path := The_Base_Path;
    Text.Clear (The_Source_Directories);
    The_Project_Directory := The_Language_Directory;
    for Index in Text.First_Index .. Project_Parts.Count - 1 loop
      declare
        Path : constant String := Ada_95.File.Normalized (Folder & Part_Of (Index));
      begin
        for System_Path of reverse The_Implied_Areas loop
          if Path = Source_Folder & System_Path then
            return;
          end if;
        end loop;
        for System_Path of reverse The_Reference_Areas loop
          if Path = Source_Folder & System_Path then
            return;
          end if;
        end loop;
        The_Project_Directory := [Path];
        The_Source_Directories.Prepend (Path);
        The_Work_Path.Prepend (Folder);
      end;
    end loop;
    for Area of The_Implied_Areas loop
      The_Source_Directories.Append (Source_Folder & Area);
    end loop;
    The_Project_Name := [Part_Of (Project_Parts.Count - 1)];
    Text.Clear (The_Product_Sub_Path);
    for Index in Text.First_Index .. Project_Parts.Count - 2 loop
      declare
        The_Part : constant String := Part_Of (Index);
      begin
        if Text.Matches (The_Part, The_Project_Name) then
          declare
            The_Items : constant Text.Vector := Project_Parts.Part (From => Index,
                                                                    To   => Project_Parts.Count - 2);

            The_Text : constant String := The_Items.To_List.To_Data (Separator => "\");
          begin
            if The_Text /= "" then
              The_Product_Sub_Path := [Ada_95.File.Normalized_Folder ("\" & The_Text)];
            end if;
          end;
        end if;
      end;
    end loop;

    if Environment_Defined then
      Check_Icon;
      if Directory.Exists (Project_Subdirectory) then
        The_Work_Path.Prepend (Project_Subdirectory & Files.Separator);
      end if;
      The_Library_Areas.Clear;
      for Source_Directory of The_Library_Sources loop
        The_Library_Areas.Append (Source_Directory);
        The_Work_Path.Append (Source_Directory & Files.Separator);
      end loop;
      for Area of The_Library_Areas loop
        Log.Write ("||| Library: " & Area);
      end loop;
    end if;
  end Create_Work_Area_For;


  procedure Initialize_Project (The_Work_Path : Text.List) is
  begin
    Ada_95.Project.Initialize (The_Work_Path);
  end Initialize_Project;


  function Application_From (The_Directory : String) return String is
    Package_Name : constant String := Files.Module_Of (The_Directory);
  begin
    return The_Directory & Files.Separator & Package_Name & Body_Extension;
  end Application_From;


  function Initialized (Filename : String) return Boolean is

    procedure Ini_Error (Message : String) with No_Return is
    begin
      Set_Error ("Error in '" & Definition_File & "' - " & Message);
    end Ini_Error;


    function Element_Of (Key         : String;
                         Application : String;
                         Must_Exist  : Boolean := True) return String is

      Element : constant String := Element_For (Key, Application);

    begin
      if Must_Exist and Element = "" then
        Ini_Error ("<[" & Application & "] " & Key & "> undefined");
      end if;
      return Element;
    end Element_Of;


    procedure Define_Location (The_Item    : out Text.String;
                               Key         :     String;
                               Application :     String;
                               Root        :     String := "") is

      Location      : constant String := Element_Of (Application => Application, Key => Key);
      The_Directory : constant String := Root & Location;

    begin
      if not Directory.Exists (The_Directory) then
        Ini_Error (Application & " " & Key & " <" & The_Directory & "> unknown");
      end if;
      The_Item := [Ada_95.File.Normalized (Location)];
    end Define_Location;


    procedure Define_Ada_Version is

      The_Version : constant String := Text.Trimmed (Element_For (Application => "Ada", Key => "Version"));

    begin
     if The_Version = "" then
       The_Ada_Version := [Default_Ada_Version];
     else
       The_Ada_Version := [Text.Lowercase_Of (The_Version)];
       if not (Ada_Version in "gnat83" | "gnat95" | "gnat05" | "gnat2005" | "gnat12" | "gnat2012" | "gnat2022") then
         Ini_Error ("Ada version " & The_Version & " unknown - latest supported version: gnat2022");
       end if;
     end if;
    end Define_Ada_Version;


    procedure Define_Modifier is

      Modifier  : constant String := Element_Of (Application => "Product", Key => "Modifier", Must_Exist => False);
      Items     : constant Text.Strings := Text.Strings_Of (Modifier, Separator => '|');
      Tool      : constant String := (if Items.Count > 0 then Items(Text.First_Index) else "");
      Parameter : constant String := (if Items.Count > 1 then Items(Text.First_Index + 1) else "");
      Message   : constant String := (if Items.Count = 3 then Items(Text.First_Index + 2) else "");

    begin
      if Modifier = "" then
        return;
      elsif Items.Count > 3 then
        Ini_Error ("Product syntax: Modifier = 'tool' ['parameters' ['success message']]");
      elsif Tool = "" then
        Ini_Error ("Product Modifier <tool> not defined");
      elsif not File.Exists (Tool) then
        Ini_Error ("Product Modifier <" & Tool & "> unknown");
      end if;
      The_Modifier_Tool := [Tool];
      Log.Write ("||| Modifier - Tool       : " & Modifier_Tool);
      if Items.Count >= 2 then
        The_Modifier_Parameters := [Parameter];
        Log.Write ("|||          - Parameters : " & The_Modifier_Parameters);
      end if;
      if Items.Count = 3 then
        The_Modifier_Message := [Message];
        Log.Write ("|||          - Message : " & The_Modifier_Message);
      end if;
    end Define_Modifier;


    function Is_Product_Name (Base_Name : String) return Boolean is

      Product_Exists : Boolean := False;

      procedure Iterator (The_Directory : String) is
      begin
         if File.Base_Name_Of (The_Directory) = Base_Name then
           Product_Exists := True;
         end if;
      end Iterator;

    begin -- Is_Product_Name
      Directory.Iterate_Over_Leafs (Language_Directory, Iterator'access);
      return Product_Exists;
    end Is_Product_Name;


    procedure Define_Installations is

      procedure Install_Error (Message : String) is
      begin
        Ini_Error ("Product Install: " &  Message & '.');
      end Install_Error;


      procedure Add_Installation (Item : Text.Strings) is

        Syntax : constant String
          := Item'image & Ascii.Cr &
             "Syntax: Install => [Item {'|' Item}]" & Ascii.Cr &
             "                Item => 'project' > [Dest [';' Dest]" & Ascii.Cr &
             "                Dest => ['destination file' ['*' ('32' | '64')]";

        Install_Name : constant String := (if Item.Count > 0 then Item(Text.First_Index) else "");


        procedure Add_Destinations (Destinations : Text.Strings) is

          The_Destinations : Text.List;

          type State is (Get_Destination, Destination_Defined, Get_Constraint);

          The_State : State := Get_Destination;

          The_Destination : Text.String;

          procedure Append_Destination (Constraint : String := "") is
            Destination : constant String := The_Destination.S;
          begin
            if not File.Is_Legal (Destination) then
              Install_Error ("Filename '" & Destination & "' is illegal");
            end if;
            declare
              Destination_Folder : constant String := File.Containing_Directory_Of (Destination);
            begin
              if not Directory.Exists (Destination_Folder) then
                Install_Error ("destination directory '" & Destination_Folder & "' is unknown");
              end if;
            end;
            if Directory.Exists (Destination) then
              Install_Error ("destination '" & Destination & "' is not a file");
            end if;
            The_Destinations.Append (Destination & Constraint);
            Log.Write ("||| Install - " & Install_Name & " -> " & Destination & Constraint);
          end Append_Destination;

        begin -- Add_Destinations
          if Item.Count = 0 then
            Install_Error (Syntax);
          else
            for Token of Destinations loop
              if Token = "" then
                null; -- skip spaces between symbols
              elsif Token = "," then
                Install_Error ("illegal symbol ','");
              elsif Token = "*" then
                if The_State = Destination_Defined then
                  The_State := Get_Constraint;
                else
                  Install_Error (Syntax);
                end if;
              elsif Token in "32" | "64" then
                if The_State = Get_Constraint then
                  Append_Destination (Constraint => '*' & Token);
                  The_State := Get_Destination;
                else
                  Install_Error (Syntax);
                end if;
              else
                case The_State  is
                when Get_Destination =>
                  The_Destination := [Token];
                  The_State := Destination_Defined;
                when Destination_Defined =>
                  Append_Destination;
                  The_Destination := [Token];
                when Get_Constraint =>
                  Install_Error (Syntax);
                end case;
              end if;
            end loop;
            if The_State = Destination_Defined then
              Append_Destination;
            end if;
            The_Installation_Map.Include (Install_Name, The_Destinations);
          end if;
        exception
        when Error_Set =>
          raise;
        when others =>
          Install_Error (Syntax);
        end Add_Destinations;

        Destinations : constant String := (if Item.Count > 1 then Item(Text.First_Index + 1) else "");

      begin -- Add_Installation
        if Item.Count /= 2 then
          Install_Error (Syntax);
        elsif not Is_Product_Name (Install_Name) then
          Install_Error ("project name '" & Install_Name & "' not found");
        else
          Add_Destinations (Text.Strings_Of (Destinations, Separator => ';', Symbols => "*,"));
        end if;
      end Add_Installation;

      Install_List : constant String := Element_Of (Application => "Product", Key => "Install", Must_Exist => False);
      Items        : constant Text.Strings := Text.Strings_Of (Install_List, Separator => '|');

    begin -- Define_Installations
      if Items.Count = 0 then
        return;
      end if;
      for Installation of Items loop
        Add_Installation (Text.Strings_Of (Installation, Separator => '>'));
      end loop;
    end Define_Installations;


    procedure Define_Source_Path (Path_Name  :        String;
                                  The_Areas  : out    Text.List;
                                  The_Path   : in out Text.List;
                                  Must_Exist :        Boolean := True) is

      Source_Path : constant String := Element_Of (Key         => Path_Name,
                                                   Application => "Source",
                                                   Must_Exist  => Must_Exist);
      Areas : constant Text.Strings := Text.Strings_Of (Source_Path, Separator => ';');

    begin
      for Index in Text.First_Index .. Areas.Count loop
        declare
          Area          : constant String := Text.Trimmed (Areas(Index));
          The_Directory : constant String := Source_Folder & Area;
        begin
          if not Directory.Exists (The_Directory) then
            Ini_Error ("Unknown source directory <" & The_Directory & ">");
          end if;
          The_Areas.Append (Ada_95.File.Normalized (Area));
          The_Path.Append (Ada_95.File.Normalized_Folder (The_Directory));
        end;
      end loop;
    end Define_Source_Path;


    function Project_Filename_Of (Actual_Name : String) return String is
    begin
      declare
        Actual_Directory : constant String := File.Containing_Directory_Of (Actual_Name);
        Directory_Name   : constant String := File.Base_Name_Of (Actual_Directory);
      begin
        if Text.Contains (Build.Sub_Directories, Directory_Name) then
          declare
            Application_Name : constant String := Application_From (File.Containing_Directory_Of (Actual_Directory));
          begin
            if File.Exists (Application_Name) then
              Log.Write ("||| Project Name: " & Application_Name);
              return Application_Name;
            end if;
          end;
        end if;
      end;
      return Actual_Name;
    exception
    when others =>
      return Actual_Name;
    end Project_Filename_Of;

    Project_Name : constant String :=  Project_Filename_Of (Filename);

    Project_Parts : constant Text.Vector := Files.Project_Parts_Of (Project_Name, Language, The_Language_Directory);

    The_Work_Path : Text.List;

  begin -- Initialized
    Log.Write ("||| Project.Initialize: " & Filename);
    The_Actual_Project := [Project_Name];
    The_Phase := Initializing;
    Build.Initialize (Project_Name, Library_Check'access);
    if The_Configuration_Handle = null then -- only first time because language directory does not change
      The_Configuration_Handle := new Configuration.File_Handle'(Configuration.Handle_For (Definition_File));
    end if;
    if Text.Is_Null (The_Language_Directory) then
      Set_Error ("Unknown " & Language & " Project for " & Filename);
    elsif not File.Exists (Definition_File) then
      Set_Error ("Project definition file missing: " & Definition_File);
    elsif Project_Parts.Count < 2 then
      Set_Error ("Unknown project for " & Filename);
    else
      declare
        Leaf_Directory : constant String := Files.Directory_Of (Project_Name);
      begin
        if not Directory.Is_Leaf (Leaf_Directory, Leaf_Directory_Exceptions) then
          Set_Error ("Not in project area " & Filename);
        else
          if not File.Exists (Application_From (Leaf_Directory)) then
            Set_Error ("No application in " & Leaf_Directory);
          end if;
        end if;
      end;
    end if;
    Define_Location (The_Source_Root, Key => "Root", Application => "Source");
    Define_Source_Path ("Ignore", The_Ignore_Areas, The_Path => The_Base_Path, Must_Exist => False);
    Define_Source_Path ("Path", The_Implied_Areas, The_Path => The_Base_Path);
    Define_Source_Path ("Reference", The_Reference_Areas, The_Path => The_Base_Path);
    Create_Work_Area_For (Project_Parts, The_Work_Path);
    Define_Location (The_Binary_Root, Key => "Root", Application => "Binary");
    Define_Location (The_Product_Directory, Key => "Location", Application => "Product");
    Define_Ada_Version;
    Define_Modifier;
    Define_Installations;
    declare
      Case_Update : constant String := Element_Of (Key => "Case_Update", Application => "Style", Must_Exist => False);
      Token_Kind  : constant String := Text.Legible_Of (Case_Update);
    begin
      if Token_Kind = "None" then
        The_Case_Handling_Style := No_Change;
      elsif Token_Kind = "Keywords" then
        The_Case_Handling_Style := Keywords;
      else
        The_Case_Handling_Style := Change_All;
      end if;
    end;
    ----------------------------------------
    for The_Folder of The_Work_Path loop
      Log.Write ("||| Folder: " & The_Folder);
    end loop;
    for Area of The_Implied_Areas loop
      Log.Write ("||| Implied: " & Area);
    end loop;
    for Area of The_Reference_Areas loop
      Log.Write ("||| Reference: " & Area);
    end loop;
    for Area of The_Ignore_Areas loop
      Log.Write ("||| Ignore: " & Area);
    end loop;
    ----------------------------------------
    Initialize_Project (The_Work_Path);
    Set_Confirmation (Language & " project " & Name & " opened");
    Project_Is_Defined := True;
    The_Phase := Unknown;
    return True;
  exception
  when Error_Set =>
    Set_Project_Undefined;
    The_Phase := Unknown;
    return False;
  end Initialized;


  procedure Change_To (Filename : String) is

    The_Directory : Text.String;

    Project_Parts : constant Text.Vector := Files.Project_Parts_Of (Filename, Language, The_Directory);

    The_Work_Path : Text.List;

  begin
    if not Text.Matches (The_Directory, The_Language_Directory) then
      raise Program_Error;
    end if;
    The_Phase := Promoting;
    Build.Initialize (Filename, Library_Check'access);
    Create_Work_Area_For (Project_Parts, The_Work_Path);
    -----------------------------------------------
    Log.Write ("||| Project.Change_To: " & Filename);
    for The_Folder of The_Work_Path loop
      Log.Write ("||| Folder: " & The_Folder);
    end loop;
    -----------------------------------------------
    Ada_95.Project.Finalize;
    Initialize_Project (The_Work_Path);
    Promotion.Set_Message ("Project changed to " & Filename);
    The_Phase := Unknown;
  end Change_To;


  function Is_Source (Filename : String) return Boolean is
    Is_In_Project : constant Boolean := Ada_95.File.Is_In_Project (Filename);
  begin
    Log.Write ("||| Project.Is_Source " & Filename & " => " & Is_In_Project'img);
    return Is_In_Project;
  end Is_Source;


  function Is_In_Library_Area (Filename : String) return Boolean is
    The_Directory : constant String := Files.Directory_Of (Filename);
  begin
    for Area of The_Reference_Areas loop
      if Text.Matches (The_Directory, Source_Folder & Area) then
        return True;
      end if;
    end loop;
    for Area of The_Library_Areas loop
      if Text.Matches (The_Directory, Area) then
        return True;
      end if;
    end loop;
    return False;
  end Is_In_Library_Area;


  function Program_Unit_Name return String is (Name & Body_Extension);


  function Program_Unit return String is (Folder & Program_Unit_Name);


  function Is_Program_Unit (Filename : String) return Boolean is
  begin
    return Text.Matches (Filename, Program_Unit);
  end Is_Program_Unit;


  function Case_Handling_Style return Case_Modification is (The_Case_Handling_Style);


  function Environment_Variables return String is

    The_Environment : Text.String;

    function New_Path return String is
    begin
      return Tools_Directory & ";" & Ada.Environment_Variables.Value ("Path");
    end New_Path;

    procedure Add (Id    : String;
                   Value : String) is
    begin
      if Text.Matches (Id, "path") then
        Text.Append (The_Environment, Id & '=' & New_Path & Ascii.Nul);
      elsif Text.Is_Null (The_Library_Path) then
        Text.Append (The_Environment, Id & '=' & Value & Ascii.Nul);
      elsif Text.Matches (Id, "gpr_project_path") then
        Log.Write ("||| " & Id & " ignored");
      elsif Text.Matches (Id, Ada_Project_Path) then
        null;
      else
        Text.Append (The_Environment, Id & '=' & Value & Ascii.Nul);
      end if;
    end Add;

  begin -- Environment_Variables
    Ada.Environment_Variables.Iterate (Add'access);
    if not Text.Is_Null (The_Library_Path) then
      declare
        Ada_Path : constant String := Ada_Project_Path & "=" & The_Library_Path;
      begin
        Log.Write ("||| Environment Variables: " & Ada_Path);
        Text.Append (The_Environment, Ada_Path & Ascii.Nul);
      end;
    end if;
    return The_Environment.S;
  end Environment_Variables;


  function Full_Name_Of (Filename : String) return String  is
  begin
    return Ada_95.File.Full_Name_Of (Filename);
  end Full_Name_Of;


  function Found_In_Directory (The_Directory : String;
                               Area          : String) return String is

    package FS renames Ada.Directories;

    Not_Found : exception;

    The_Handle : FS.Search_Type;
    The_Entry  : FS.Directory_Entry_Type;

  begin
    FS.Start_Search (Search    => The_Handle,
                     Directory => The_Directory,
                     Pattern   => "",
                     Filter    => [FS.Directory => True, others => False]);
    while FS.More_Entries (The_Handle) loop
      FS.Get_Next_Entry (The_Handle, The_Entry);
      declare
        Entry_Name     : constant String := FS.Simple_Name (The_Entry);
        Directory_Name : constant String := FS.Full_Name (The_Entry);
      begin
        if Text.Matches (Entry_Name, Area) then
          return Directory_Name;
        end if;
      end;
    end loop;
    FS.Start_Search (Search    => The_Handle,
                     Directory => The_Directory,
                     Pattern   => "",
                     Filter    => [FS.Directory => True, others => False]);
    while FS.More_Entries (The_Handle) loop
      FS.Get_Next_Entry (The_Handle, The_Entry);
      declare
        Entry_Name     : constant String := FS.Simple_Name (The_Entry);
        Directory_Name : constant String := FS.Full_Name (The_Entry);
      begin
        if Entry_Name(Entry_Name'first) = '.' then
          null;
        else
          begin
            return Found_In_Directory (The_Directory => Directory_Name,
                                       Area          => Area);
          exception
          when others =>
            null;
          end;
        end if;
      end;
    end loop;
    raise Not_Found;
  end Found_In_Directory;


  function Promotion_Areas return String is (The_Promotion_Areas.S);

  function Promotion_List return Text.List is

    use type Text.List;

    Area_List : constant String := Element_For (Key         => "All",
                                                Application => "Promote");

    Areas : constant Text.Strings := Text.Strings_Of (Area_List, Separator => '+', Symbols => "-");

    Remove_Area : Boolean := False;

    Header : constant String := "[Promote] All = " & Area_List & " -> ";

  begin -- Define_Promotion_List
    Text.Clear (The_Promotion_Areas);
    The_Promotion_List.Clear;
    for Index in Text.First_Index .. Areas.Count loop
      declare
        Item : constant String := Text.Trimmed (Areas(Index));
      begin
        if Item = "-" then
          Remove_Area := True;
        else
          declare
            Area : constant String := Item;

            function Location return String is
            begin
              return Found_In_Directory (Language_Directory, Area);
            end Location;

            procedure Append_Project (The_Directory : String) is
            begin
              if not Is_To_Ignore (The_Directory) then
                declare
                  Application : constant String := Application_From (The_Directory);
                begin
                  if Application /= "" and then File.Exists (Application) then
                    if Remove_Area then
                      begin
                        The_Promotion_List := The_Promotion_List - Application;
                      exception
                      when others =>
                        Promotion.Set_Error (Header & Application & " NOT removable!");
                      end;
                    else
                      The_Promotion_List.Append (Application);
                    end if;
                  end if;
                end;
              end if;
            end Append_Project;

          begin
            if Is_Base_Area (Area) then
              Promotion.Set_Error (Header & Area & " NOT allowed!");
            end if;
            begin
              Directory.Iterate_Over_Leafs (Location, Append_Project'access, Leaf_Directory_Exceptions);
            exception
            when others =>
              Promotion.Set_Error (Header & Area & " unknown!");
            end;
          end;
          Remove_Area := False;
        end if;
      end;
    end loop;
    for The_Project of The_Promotion_List loop
      Log.Write ("||| Project: " & The_Project);
    end loop;
    The_Promotion_Areas := [Ada_95.File.Normalized (Area_List)];
    return The_Promotion_List;
  end Promotion_List;


  function Must_Be_Build_First (Filename : String) return Boolean is
    The_Filename : constant String := Files.Folder_Of (Filename) & Files.Module_Of (Filename) & Gpr.File_Extension;
  begin
    return File.Exists (The_Filename);
  end Must_Be_Build_First;


  function Run return Boolean is
  begin
    return Target.Run (Product_Directory & "\" & Name & ".exe", "");
  end Run;


  procedure Finalize is
  begin
    Log.Write ("||| Project.Finalize");
    Ada_95.Project.Finalize;
    Set_Project_Undefined;
  end Finalize;

end Project;
