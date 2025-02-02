-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Application;
with Configuration;
with Directory;
with File;
with Gui;
with Log;
with Text;

package body Picture is

  subtype Check_Duration is Duration range 0.01 .. 2.0;

  Default_Check_Delay : constant Check_Duration := 0.02; -- default 200 ms

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
                            Size_Check_Delay : Check_Duration) is

    Target_Folder      : constant String := File.Containing_Directory_Of (Destination);
    Temporary_Filename : constant String := File.Composure (Target_Folder, File.Base_Name_Of (The_Filename), "tmp");

    The_Filesize : File.Size;
    New_Filesize : File.Size;
    use type File.Size;

    Delay_Time  : constant Duration := 0.1;
    The_Timeout : Duration := 5.0;

  begin
    if not Directory.Exists (Target_Folder) then
      Error ("Destination directory " & Target_Folder & " not found");
    end if;
    The_Filesize := File.Size_Of (The_Filename);
    loop
      delay Size_Check_Delay;
      New_Filesize := File.Size_Of (The_Filename);
      if New_Filesize = 0 then
        Error ("File " & The_Filename & " is empty");
      end if;
      exit when New_Filesize = The_Filesize;
      The_Filesize := New_Filesize;
    end loop;
    loop
      begin
        File.Copy (The_Filename, Temporary_Filename);
        exit;
      exception
      when File.Use_Error =>
        The_Timeout := The_Timeout - Delay_Time;
        if The_Timeout <= 0.0 then
          Error ("Cant't write file " & Temporary_Filename);
        end if;
        delay Delay_Time;
      end;
    end loop;
    begin
      File.Rename (Temporary_Filename, Destination);
      File.Delete (Temporary_Filename);
    exception
    when File.Use_Error =>
      Error ("Can't write file " & Destination);
    end;
  end Copy_Completed;


  package Cmd  renames Ada.Command_Line;

  procedure Get is
    Argument_Count   : constant Natural := Cmd.Argument_Count;
    Size_Check_Delay : Check_Duration := Default_Check_Delay;
  begin
    if Argument_Count > 2 then
      Error ("Parameters syntax: [<source directory name> [<size check delay>]");
    elsif Argument_Count = 2 then
      begin
        Size_Check_Delay := Check_Duration'value(Cmd.Argument(2));
      exception
      when others =>
        Error ("Size check delay not in range 0.01 .. 2.0");
      end;
    end if;
    declare
      Picture_Directory : constant String := (if Argument_Count = 0 then Default_Dirctory else Cmd.Argument(1));
      The_Filename      : Text.String;
      use type Text.String;
    begin
      if not Directory.Exists (Picture_Directory) then
        Error ("Source directory " & Picture_Directory & " not found");
      end if;
      if File.Exists (Destination_Filename) then
        Error ("Destionation filename " & Destination_Filename & " already exists");
      end if;
      for Filename of File.Iterator_For (Picture_Directory) loop
        Gui.Message_Box (Filename);
        if Text.Lowercase_Of (File.Extension_Of (Filename)) in "cr2" | "fits" | "jpeg" | "jpg" then
          if The_Filename.Is_Empty or else File.Is_Newer (Filename, +The_Filename) then
            The_Filename := [Filename];
          end if;
        end if;
      end loop;
      if The_Filename.Is_Empty then
        Error ("No picture file in source directory");
      end if;
      Copy_Completed (+The_Filename, Destination_Filename, Size_Check_Delay);
    end;
  exception
  when Error_Detected =>
    null;
  when Occurrence: others =>
    Log.Write (Occurrence);
    Gui.Message_Box ("Fatal Error");
  end Get;

end Picture;
