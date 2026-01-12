-- *********************************************************************************************************************
-- *                       (c) 2025 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Text_IO;
with Application;
with Configuration;
with Directory;
with Exceptions;
with File;
with Gui;
with Log;
with Text;

package body Picture is

  type Detection_Timeout is delta 0.1 range 0.0 .. 20.0;

  Detection_Delay : constant := 1.0;

  Default_Detection_Timeout : constant Detection_Timeout := 3.0;

  Default_Dirctory : constant String := "D:\SharpCap\";

  function Picture_Filename return String is

    function Tracker_Ini_Filename return String is

      Tracker   : constant String := "SkyTracker";
      Extension : constant String := "ini";

      Tracker_Folder : constant String := Application.Main_Directory & File.Folder_Separator & Tracker;

      Tracker_Config : constant String := File.Composure (Tracker_Folder, Tracker, Extension);

    begin
      if File.Exists (Tracker_Config) then
        return Tracker_Config;
      end if;
      for Folder of Directory.Iterator_For (Tracker_Folder) loop
        declare
          Filename : constant String := File.Composure (Folder, Tracker, Extension);
        begin
          if File.Exists (Filename) then
            return Filename;
          end if;
        end;
      end loop;
      return "";
    end Tracker_Ini_Filename;

    Data_Config     : constant Configuration.File_Handle := Configuration.Handle_For (Tracker_Ini_Filename);
    Picture_Section : constant Configuration.Section_Handle := Configuration.Handle_For (Data_Config, "Picture");

  begin -- Picture_Filename
    return Configuration.Value_Of (Picture_Section, "Filename");
  end Picture_Filename;

  Destination_Filename : constant String := Picture_Filename;


  Error_Detected  : exception;

  procedure Error (Item : String) is
  begin
    Gui.Message_Box (Item);
    raise Error_Detected;
  end Error;


  procedure Copy_Completed (The_Filename     : String;
                            Destination      : String;
                            Size_Check_Delay : Duration := 0.2) is

    Target_Folder      : constant String := File.Containing_Directory_Of (Destination);
    Temporary_Filename : constant String := File.Composure (Target_Folder, File.Base_Name_Of (The_Filename), "tmp");

    New_Filesize : File.Size;
    use type File.Size;

    procedure Await_Stable (The_Filename : String) is
      The_Filesize : File.Size := File.Size_Of (The_Filename);
      The_File : Ada.Text_IO.File_Type;
    begin
      Log.Write ("Await access to " & The_Filename);
      loop
        begin
          Ada.Text_IO.Open (The_File, Ada.Text_IO.Append_File, The_Filename);
          exit;
        exception
        when Ada.Text_IO.Use_Error =>
          delay 0.5;
        end;
      end loop;
      Ada.Text_IO.Close (The_File);
      Log.Write ("Await stable filesize for " & The_Filename);
      loop
        delay Size_Check_Delay;
        New_Filesize := File.Size_Of (The_Filename);
        Log.Write ("New file size:" & New_Filesize'image);
        if New_Filesize = 0 then
          Error ("File " & The_Filename & " is empty");
        end if;
        exit when New_Filesize = The_Filesize;
        The_Filesize := New_Filesize;
        delay 0.2;
      end loop;
      Log.Write ("Filesize is stable");
    end Await_Stable;

  begin
    if not Directory.Exists (Target_Folder) then
      Error ("Destination directory " & Target_Folder & " not found");
    end if;
    Await_Stable (The_Filename);
    Log.Write ("Copy to temporary file");
    File.Copy (The_Filename, Temporary_Filename);
    Await_Stable (Temporary_Filename);
    Log.Write ("Rename temporary file");
    File.Rename (Temporary_Filename, Destination);
    Log.Write ("Delete temporary file");
    File.Delete (Temporary_Filename);
    Log.Write ("Copy complete");
  exception
  when File.Use_Error =>
    Error ("Can't write file " & Destination);
  end Copy_Completed;


  package Cmd  renames Ada.Command_Line;

  procedure Get is
    Argument_Count         : constant Natural := Cmd.Argument_Count;
    File_Detection_Timeout : Detection_Timeout := Default_Detection_Timeout;
  begin
    if Argument_Count > 2 then
      Error ("Parameters syntax: [<source directory name> [<file detection timeout>]");
    elsif Argument_Count = 2 then
      begin
        File_Detection_Timeout := Detection_Timeout'value(Cmd.Argument(2));
      exception
      when others =>
        Error ("Size check delay not in range" & Detection_Timeout'first'image & " .." &  Detection_Timeout'last'image);
      end;
    end if;
    declare
      Picture_Directory  : constant String := (if Argument_Count = 0 then Default_Dirctory else Cmd.Argument(1));
      The_Filename       : Text.String;
      The_Detection_Time : Duration := 0.01; -- must be > 0.0
    begin
      if not Directory.Exists (Picture_Directory) then
        Error ("Source directory " & Picture_Directory & " not found");
      end if;
      if File.Exists (Destination_Filename) then
        Error ("Destionation filename " & Destination_Filename & " already exists");
      end if;
      loop
        for Filename of File.Iterator_For (Picture_Directory) loop
          if Text.Lowercase_Of (File.Extension_Of (Filename)) in "cr2" | "fits" | "jpeg" | "jpg" then
            if The_Filename.Is_Empty or else File.Is_Newer (Filename, The_Filename.S) then
              The_Filename := [Filename];
            end if;
          end if;
        end loop;
        exit when not The_Filename.Is_Empty;
        if The_Detection_Time > Duration(File_Detection_Timeout) then
          Error ("No picture file found in " & Picture_Directory);
        end if;
        The_Detection_Time := @ + Detection_Delay;
        delay Detection_Delay;
      end loop;
      Copy_Completed (The_Filename.S, Destination_Filename);
    end;
  exception
  when Item: Ada.IO_Exceptions.Use_Error =>
    Log.Write (Item);
    Gui.Message_Box (Exceptions.Name_Of (Item));
  when Error_Detected =>
    null;
  when Occurrence: others =>
    Log.Write (Occurrence);
    Gui.Message_Box ("Fatal Error");
  end Get;

end Picture;
