-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Directory;
with File;
with Gui;
with Log;
with Text;

package body Picture is

  package Cmd  renames Ada.Command_Line;

  Error_Detected  : exception;

  procedure Error (Item : String) is
  begin
    Gui.Message_Box (Item);
    raise Error_Detected;
  end Error;

  subtype Check_Duration is Duration range 0.01 .. 2.0;

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


  procedure Get is
    Argument_Count   : constant Natural := Cmd.Argument_Count;
    Size_Check_Delay : Check_Duration := 0.2; -- default 200 ms
  begin
    if Argument_Count = 0 then
      Error ("Parameters missing: [<source directory name> <destination filename> [<size check delay>]");
    elsif Argument_Count = 1 then
      Error ("Destination filename missing");
    elsif Argument_Count = 3 then
      begin
        Size_Check_Delay := Check_Duration'value(Cmd.Argument(3));
      exception
      when others =>
        Error ("Size check delay not in range 0.01 .. 2.0");
      end;
    elsif Argument_Count > 3 then
      Error ("Too many parameters");
    end if;
    declare
      Picture_Directory    : constant String := Cmd.Argument(1);
      Destination_Filename : constant String := Cmd.Argument(2);
      The_Filename         : Text.String;
      use type Text.String;
    begin
      if not Directory.Exists (Picture_Directory) then
        Error ("Source directory " & Picture_Directory & " not found");
      end if;
      if File.Exists (Destination_Filename) then
        Error ("Destionation filename " & Destination_Filename & " already exists");
      end if;
      for Filename of File.Iterator_For (Picture_Directory) loop
        if The_Filename.Is_Empty or else File.Is_Newer (Filename, +The_Filename) then
          The_Filename := [Filename];
        end if;
      end loop;
      if The_Filename.Is_Empty then
        Error ("No file in source directory");
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
