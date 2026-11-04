-- *********************************************************************************************************************
-- *                   (c) 2021 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Environment_Variables;
with Ada_95.File;
with Files;
with Log;

package body Ada_95.Build is

  type Tools_Kind is (Size_32, Size_64);

  type Directories is array (Tools_Kind) of Text.String;

  The_Project_Folder    : Text.String;
  Build_Defined         : Boolean;
  Use_Icon              : Boolean;
  The_Version           : Version;
  The_Kind              : Kind;
  The_Company           : Text.String;
  The_Description       : Text.String;
  The_Tools_Directory   : Text.String;
  The_Tools_Kind        : Tools_Kind;
  The_Tools_Directories : Directories;
  Has_Second_Tools      : Boolean;
  The_Libraries         : Text.List;
  The_Resource          : Text.String;
  The_Interface         : Text.List;
  Library_Check         : Library_Check_Function;


  procedure Clear is
  begin
    Build_Defined := False;
    Use_Icon := True;
    The_Kind := Windows_Application;
    The_Version := (others => <>);
    Text.Clear (The_Tools_Directory);
    Text.Clear (The_Tools_Directories(Size_32));
    Text.Clear (The_Tools_Directories(Size_64));
    Has_Second_Tools := False;
    Text.Clear (The_Company);
    Text.Clear (The_Description);
    Text.Clear (The_Resource);
    Text.Clear (The_Interface);
    Text.Clear (The_Libraries);
  end Clear;


  procedure Initialize (Filename : String;
                        Check    : Library_Check_Function) is
  begin
    Library_Check := Check;
    The_Project_Folder := [Files.Normalized_Folder(Files.Directory_Of (Filename))];
    Clear;
  end Initialize;


  function Project_Folder return String is
  begin
    return The_Project_Folder.S;
  end Project_Folder;


  procedure Define is
  begin
    Build_Defined := True;
  end Define;

  function Is_Defined return Boolean is (Build_Defined);


  procedure Set_Console_Application is
  begin
    The_Kind := Console_Application;
  end Set_Console_Application;


  function Defined_Kind (Item : String) return Boolean is
  begin
    if Item = "Console" then
      The_Kind := Console_Application;
    elsif Item = "Windows" then
      The_Kind := Windows_Application;
    elsif Item = "Dll" then
      The_Kind := Dll;
      Use_Icon := False;
    else
      return False;
    end if;
    return True;
  end Defined_Kind;

  function Is_Dll return Boolean is (The_Kind = Dll);


  function Application_Kind_Image return String is
  begin
    case Application_Kind(The_Kind) is
    when Windows_Application =>
      return "windows";
    when Console_Application =>
      return "console";
    end case;
  end Application_Kind_Image;


  procedure Define_Interface (Item : Text.List) is
  begin
    The_Interface := Item;
  end Define_Interface;

  function Actual_Interface return Text.List is (The_Interface);


  procedure Define (Item : Version) is
  begin
    The_Version := Item;
  end Define;

  function Actual_Version return Version is (The_Version);


  procedure Define_Company (Item : String) is
  begin
    The_Company := [Item];
  end Define_Company;

  function Actual_Company return String is (The_Company.S);


  function Has_Style return Boolean is (Actual_Company /= "");


  procedure Define_Description (Item : String) is
  begin
    The_Description := [Item];
  end Define_Description;

  function Actual_Description return String is (The_Description.S);


  function System_Drive return String is
    System_Drive_Variable : constant String := "SYSTEMDRIVE";
  begin
    if Ada.Environment_Variables.Exists (System_Drive_Variable) then
      return Ada.Environment_Variables.Value (System_Drive_Variable);
    else
      return "C:";
    end if;
  end System_Drive;


  function Is_Size_32 (Location : String) return Boolean is
    Gcc_32 : constant String := "i686-pc-mingw32-gcc.exe";
  begin
    return Files.Exists (Location & Files.Separator & Gcc_32);
  end Is_Size_32;


  function Tools_Location_Of (Compiler : String) return String is
    Actual_Tools : constant String := Files.Original_Name_Of
                                        (System_Drive & Files.Separator & Compiler & Files.Separator  & "bin");
  begin
    return Actual_Tools;
  end Tools_Location_Of;


  function Exists (Compiler : String) return Boolean is
  begin
    return Files.Directory_Exists (Tools_Location_Of (Compiler));
  end Exists;


  function Defined_Compiler (Item : String) return Boolean is
  begin
    declare
      The_Directory : constant String := Tools_Location_Of (Item);
    begin
      if Files.Directory_Exists (The_Directory) then
        Has_Second_Tools := False;
        Text.Clear (The_Tools_Directories(Size_32));
        Text.Clear (The_Tools_Directories(Size_64));
        if Is_Size_32 (The_Directory) then
          The_Tools_Kind := Size_32;
        else
          The_Tools_Kind := Size_64;
        end if;
        Define_Tools_Directory (The_Directory);
        return True;
      end if;
    end;
    return False;
  end Defined_Compiler;


  function Directories_Area return String is
  begin
    case The_Tools_Kind is
    when Size_32 =>
      return Sub_Directory_32;
    when Size_64 =>
      return Sub_Directory_64;
    end case;
  end Directories_Area;


  function Set_Tools (Location : String) return Boolean is
  begin
    if Is_Size_32 (Location) then
      The_Tools_Kind := Size_32;
    else
      The_Tools_Kind := Size_64;
    end if;
    if Text.Is_Null (The_Tools_Directories(The_Tools_Kind)) then
      Log.Write ("||| Tools Directory " & Directories_Area & " : " & Location);
      The_Tools_Directories(The_Tools_Kind) := [Location];
      return True;
    else
      Log.Write ("!!! Tools Directory " & Directories_Area & " already set");
      return False;
    end if;
  end Set_Tools;


  function Defined_Compilers (First  : String;
                              Second : String) return Boolean is
  begin
    declare
      First_Location : constant String := Tools_Location_Of (First);
    begin
      Text.Clear (The_Tools_Directory);
      Text.Clear (The_Tools_Directories(Size_32));
      Text.Clear (The_Tools_Directories(Size_64));
      Has_Second_Tools := False;
      if Files.Directory_Exists (First_Location) then
        if Second = "" then
          if Set_Tools (First_Location) then
            return True;
          end if;
        end if;
        declare
          Second_Location : constant String := Tools_Location_Of (Second);
        begin
          if Files.Directory_Exists (Second_Location) then
            if Set_Tools (First_Location) and then Set_Tools (Second_Location) then
              Has_Second_Tools := True;
              The_Tools_Kind := Size_64;
              return True;
            end if;
          end if;
        end;
      end if;
    end;
    return False;
  end Defined_Compilers;


  procedure Define_Tools_Directory (Item : String) is
  begin
    Log.Write ("||| Tools Directory: " & Item);
    The_Tools_Directory := [File.Normalized (Item)];
  end Define_Tools_Directory;


  function Has_Tools_Directory return Boolean is (not Text.Is_Null (The_Tools_Directory));

  function Has_Tools_Directories return Boolean is (not Text.Is_Null (The_Tools_Directories(The_Tools_Kind)));

  function Compiler_Area return String is (if Has_Tools_Directories then Files.Separator & Directories_Area else "");

  function Tools_Kind_Image return String is (if Has_Tools_Directories then Directories_Area & " " else "");


  function Tools_Directory return String is
  begin
    if Has_Tools_Directory then
      return The_Tools_Directory.S;
    elsif Has_Tools_Directories then
      return The_Tools_Directories(The_Tools_Kind).S;
    else
      return "";
    end if;
  end Tools_Directory;


  function Tools_Defined return Boolean is
  begin
    return Has_Tools_Directory or Has_Tools_Directories;
  end Tools_Defined;


  function Has_Second_Tools_Directory return Boolean is
  begin
    if Has_Second_Tools then
      The_Tools_Kind := Size_32;
      return True;
    end if;
    return False;
  end Has_Second_Tools_Directory;


  procedure Set_Back_To_First is
  begin
    The_Tools_Kind := Size_64;
  end Set_Back_To_First;


  function Is_Maching (Filename : String) return Boolean is

    Directory : constant String := Files.Directory_Of (Filename);

    function Direcory_Ends_With (Postfix : String) return Boolean is
    begin
      return Postfix = Directory(Directory'last - Postfix'length + 1 .. Directory'last);
    exception
    when others =>
      return False;
    end Direcory_Ends_With;

  begin -- Is_Maching
    if Direcory_Ends_With (Sub_Directory_32) then
       return The_Tools_Kind = Size_32;
    elsif Direcory_Ends_With (Sub_Directory_64) then
      return The_Tools_Kind = Size_64;
    else
      return True;
    end if;
  end Is_Maching;


  function Check_Of (Library : String) return Library_Check_Completion is
  begin
    return Library_Check (Library);
  end Check_Of;


  procedure Define_Libraries (Item : Text.List) is
  begin
    The_Libraries := Item;
  end Define_Libraries;


  function Libraries return Text.List is
  begin
    return The_Libraries;
  end Libraries;


  function Defined_Resource (Item : String) return Boolean is
    Filename : constant String := Project_Folder & Item & ".rc";
  begin
    if Files.Exists (Filename) then
      The_Resource := [Filename];
      return True;
    end if;
    return False;
  end Defined_Resource;

  function Has_Resource return Boolean is (not Text.Is_Null (The_Resource));

  function Actual_Resource return String is (The_Resource.S);


  function Defined_Icon (Item : String) return Boolean is
  begin
    if Item = "False" then
      Use_Icon := False;
    elsif Item = "True" then
      Use_Icon := True;
    else
      return False;
    end if;
    return True;
  end Defined_Icon;

  function Has_Icon return Boolean is (Use_Icon);

end Ada_95.Build;
