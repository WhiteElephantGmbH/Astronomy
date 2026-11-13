-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

with Ada.Characters.Handling;
with Ada_95.Source.File;
with Files;
with Log;

package body Ada_95.File is

  Unit_Separator : constant Character := '-';

  The_Work_Path : Text.List;


  function Normalized (Item : String) return String is
    The_Item : String := Text.Trimmed (Item);
  begin
    for Index in The_Item'range loop
      if The_Item(Index) = Files.Other_Separator then
        The_Item(Index) := Files.Separator;
      end if;
    end loop;
    return The_Item;
  end Normalized;


  function Normalized_Folder (Folder : String) return String is
    The_Folder : constant String := Normalized (Folder);
  begin
    if The_Folder'length = 0 then
      return "";
    elsif The_Folder(The_Folder'last) = Files.Separator then
      return The_Folder;
    else
      return The_Folder & Files.Separator;
    end if;
  end Normalized_Folder;


  procedure Define_Work_Path (List : Text.List) is
  begin
    The_Work_Path := List;
  end Define_Work_Path;


  function Information_For (Filename : String) return Information is

    Path_And_Name : constant String := Normalized (Filename);

    The_Extension : Kind  := Unknown;

  begin
    if Path_And_Name'length > 4 then
      declare
        Name_First :          Natural := Path_And_Name'first;
        Name_Last  : constant Natural := Path_And_Name'last - 4;
      begin
        if Path_And_Name(Path_And_Name'last - 3 .. Path_And_Name'last - 1) = ".ad" then
          case Path_And_Name(Path_And_Name'last) is
          when 's' =>
            The_Extension := Specification;
          when 'b' =>
            The_Extension := Implementation;
          when others =>
            return No_Information;
          end case;
        else
          return No_Information;
        end if;
        for Index in reverse Name_First .. Name_Last loop
          case Path_And_Name(Index) is
          when Files.Separator | Files.Other_Separator =>
            Name_First := Index + 1;
            exit;
          when others =>
            null;
          end case;
        end loop;
        declare
          Id : constant Unit_Name := Name.Id_Of (Path_And_Name(Name_First .. Name_Last), Separator => Unit_Separator);
        begin
          return Information'(Length    => Id'length,
                              Id        => Id,
                              Extension => The_Extension);

        end;
      end;
    end if;
    return No_Information;
  exception
  when others =>
    return No_Information;
  end Information_For;


  function Image_Of (Extension : Kind) return String is
  begin
    case Extension is
    when Specification =>
      return ".ads";
    when Implementation =>
      return ".adb";
    when Unknown =>
      return "";
    end case;
  end Image_Of;


  function Path_Of (Filename : String) return String is
  begin
    for Index in reverse Filename'range loop
      case Filename(Index) is
      when Files.Separator | Files.Other_Separator =>
        return Filename(Filename'first .. Index - 1);
      when others =>
        null;
      end case;
    end loop;
    return "";
  end Path_Of;


  function Name_And_Extension_Of (Filename : String) return String is
  begin
    for Index in reverse Filename'range loop
      case Filename(Index) is
      when Files.Separator | Files.Other_Separator =>
        return Filename(Index + 1 ..  Filename'last);
      when others =>
        null;
      end case;
    end loop;
    return Filename;
  end Name_And_Extension_Of;


  function Full_Name_Of (Name_And_Extension : String) return String is
  begin
    for Folder of The_Work_Path loop
      declare
        Full_Name : constant String := Folder & Name_And_Extension;
      begin
        if Source.File.Exists (Full_Name) then
          return Files.Original_Name_Of (Full_Name);
        end if;
      end;
    end loop;
    return "";
  end Full_Name_Of;


  function Attributes_Of (Id        : Unit_Name;
                          Extension : Kind;
                          Item      : Source.Handle) return Attributes is

    The_Attributes : Attributes;

    Filename : constant String := Text.Trimmed (Name.Image_Of (Id, Separator => Unit_Separator));

    function Found return Boolean is
      use type Source.Handle;
    begin
      for Folder of The_Work_Path loop
        declare
          Path_And_Name : constant String := Folder & Filename;

          function Found_With (The_Extension : String) return Boolean is
            The_Id : constant String := Path_And_Name & The_Extension;
          begin
            if Source.File.Exists (The_Id) then
              if Item = null or else
                Ada.Characters.Handling.To_Lower (The_Id) /= Ada.Characters.Handling.To_Lower (Normalized (Item.Id))
              then
                The_Attributes.Handle := new Source.File.Item'(Source.File.New_With (The_Id));
              else
                The_Attributes.Handle := Item;
              end if;
              return True;
            else
              return False;
            end if;
          end Found_With;

        begin
          case The_Attributes.Extension is
          when Specification =>
            if Found_With (".ads") then
              return True;
            end if;
          when Implementation =>
            if Found_With (".adb") then
              return True;
            end if;
          when Unknown =>
            return False;
          end case;
        end;
      end loop;
      return False;
    end Found;

  begin
    if Extension = Unknown then
      The_Attributes.Extension := Specification;
    else
      The_Attributes.Extension := Extension;
    end if;
    if Found then
      The_Attributes.Location := Workspace;
    elsif (Extension = Unknown) and (The_Attributes.Extension = Specification) then
      The_Attributes.Extension := Implementation;
      if Found then
        The_Attributes.Location := Workspace;
      else
        Log.Write ("!!! Attributes_Of - extension unknown ? - (Filename: <" & Filename & ">)");
        return No_Attributes;
      end if;
    else
      return No_Attributes;
    end if;
    The_Attributes.Update_Time := Source.Update_Time (The_Attributes.Handle.all);
    return The_Attributes;
  end Attributes_Of;


  function Attributes_Of (Id        : Unit_Name;
                          Extension : Kind := Unknown) return Attributes is
  begin
    return Attributes_Of (Id, Extension, null);
  end Attributes_Of;


  function Attributes_Of (Object : Source.Object'class) return Attributes is
    Item : constant Information := Information_For (Object.Id);
  begin
    return Attributes_Of (Item.Id, Item.Extension, new Source.Object'class'(Object));
  end Attributes_Of;


  function "=" (Left, Right : Attributes) return Boolean is
    use type Source.Time;
  begin
    return (Left.Update_Time = Right.Update_Time) and then
           (Normalized(Left.Handle.Id) = Normalized(Right.Handle.Id));
  end "=";


  function Unit_Name_Of (Unit : Unit_Name) return String is
  begin
    return Name.Image_Of (Unit, Unit_Separator);
  end Unit_Name_Of;


  function Subunit_Name_Of (Parent : Unit_Name;
                            Unit   : Unit_Name) return String is
  begin
    return Unit_Name_Of (Parent) & "__" & Unit_Name_Of (Unit);
  end Subunit_Name_Of;


  function Is_Standard (Unit : Unit_Name) return Boolean is
    use type Unit_Name;
  begin
    return Unit = Name.Id_Of (Standard_Name);
  end Is_Standard;


  function Is_Standard_Or_System (Unit : Name.Handle) return Boolean is
    The_Unit_Name : constant String := Name.Image_Of (Unit);
  begin
    return (The_Unit_Name = System_Name) or (The_Unit_Name = Standard_Name);
  end Is_Standard_Or_System;


  function Is_In_Project (Filename : String) return Boolean is

    Folder : constant String := Normalized_Folder (Path_Of (Filename));

  begin
    for Work_Folder of The_Work_Path loop
      if Folder = Work_Folder then
        return True;
      end if;
    end loop;
    return False;
  end Is_In_Project;

end Ada_95.File;
