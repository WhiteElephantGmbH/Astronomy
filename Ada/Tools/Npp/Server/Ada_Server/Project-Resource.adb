-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Calendar;
with Ada.Text_IO;
with Build;
with File;
with Files;
with Log;

package body Project.Resource is

  Copyright : constant String := [Character'val(16#A9#), ' '];

  Common_Object_Extension : constant String := ".coff";


  function Filename return String is
    Legacy_Source    : constant String := Folder & Name & Extension;
    Generated_Source : constant String := Target_Folder & Name & Extension;
  begin
    pragma Assert (Build.Is_Defined);
    if File.Exists (Legacy_Source) then
      Set_Error ("Resource file <" & Legacy_Source & "> is obsolescent");
    end if;
    return Generated_Source;
  end Filename;


  function Common_Object_File return String is (Object_Folder & Name & Common_Object_Extension);


  procedure Generate is

    Resource_Filename : constant String := Filename;

    The_File : Ada.Text_IO.File_Type;

    procedure Put (Line : String) is
    begin
      Ada.Text_IO.Put_Line (The_File, Line);
    end Put;

    function File_Version_Image return String is
      function Image_Of is new Text.Image_Of (Build.Version_Number);
      Version : constant Build.Version := Build.Actual_Version;
    begin
      return Image_Of (Version.Major) & ','
           & Image_Of (Version.Minor) & ','
           & Image_Of (Version.Variant) & ','
           & Image_Of (Version.Revision);
    end File_Version_Image;

    function Unix_Style_Of (Item : String) return String is
      The_Item : String := Item;
    begin
      for The_Character of The_Item loop
        if The_Character = Files.Separator then
          The_Character := Files.Other_Separator;
        end if;
      end loop;
      return The_Item;
    end Unix_Style_Of;

    function Icon_Name return String is
    begin
      return Unix_Style_Of (Folder & Name & ".ico");
    end Icon_Name;

    Actual_Year : constant Ada.Calendar.Year_Number := Ada.Calendar.Year (Ada.Calendar.Clock);

  begin -- Generate
    if File.Exists (Resource_Filename) then
      return;
    end if;
    Log.Write ("||| Generate Resource " & Resource_Filename);
    begin
      Ada.Text_IO.Create (The_File, Mode => Ada.Text_IO.Out_File, Name => Resource_Filename);
    exception
    when others =>
      Set_Error ("Can't create Resource " & Resource_Filename);
    end;
    if Build.Has_Icon then
      Put ("1 ICON """ & Icon_Name & """");
    end if;
    Put ("1 VERSIONINFO");
    Put ("  FILEVERSION " & File_Version_Image);
    Put ("  FILEFLAGSMASK 0x3FL");
    Put ("  FILEFLAGS 0x0L");
    Put ("  FILEOS 0x4L");
    Put ("  FILETYPE 0x1L");
    Put ("  FILESUBTYPE 0x0L");
    Put ("BEGIN");
    Put ("  BLOCK ""StringFileInfo""");
    Put ("  BEGIN");
    Put ("    BLOCK ""040904E4""");
    Put ("    BEGIN");
    Put ("      VALUE ""LegalCopyright"", """ & Copyright & Build.Actual_Company & Actual_Year'img & "\0""");
    Put ("      VALUE ""FileDescription"", """ & Build.Actual_Description & "\0""");
    Put ("      VALUE ""OriginalFilename"", """ & Name & (if Build.Is_Dll then ".dll" else ".exe") & "\0""");
    Put ("      VALUE ""InternalName"", """ & Name & "\0""");
    Put ("      VALUE ""ProductName"", """ & Product_Name & "\0""");
    Put ("      VALUE ""ProductVersion"", """ & Product_Version & "\0""");
    Put ("    END");
    Put ("  END");
    Put ("  BLOCK ""VarFileInfo""");
    Put ("  BEGIN");
    Put ("    VALUE ""Translation"", 0x409, 1252");
    Put ("  END");
    Put ("END");
    if Build.Has_Resource then
      Put ("#include """ & Unix_Style_Of(Build.Actual_Resource) & """");
    end if;
    Ada.Text_IO.Close (The_File);
  end Generate;

end Project.Resource;
