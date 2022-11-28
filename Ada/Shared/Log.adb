-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Calendar;
with Ada.Text_IO;
with Application;
with Configuration;
with Date_Time;
with Exceptions;
with File;
with Os;
with Strings;
with System;

package body Log is

  package Io renames Ada.Text_IO;

  subtype List is Strings.List;

  File_Section_Name   : constant String := "File";
  Name_Item           : constant String := "Name";
  Multiple_Item       : constant String := "Multiple";
  Maximum_Size_Item   : constant String := "Maximum_Size";
  Flush_Item          : constant String := "Flush";
  Filter_Section_Name : constant String := "Filter";
  Categories_Item     : constant String := "Categories";

  Default_Maximum_Size      : constant Natural := 100_000;
  Default_Flush_After_Write : constant Boolean := False;
  Default_Multiple_Files    : constant Boolean := False;

  First_Category   : constant Category := 1;
  Other_Category   : constant Category := 2 ** Max_Number_Of_Categories;
  No_Categories    : constant Category := 0;
  All_Categories   : constant Category := Category'last;

  All_Categories_Id : constant String := "All";
  No_Categories_Id  : constant String := "None";
  Debug_Category_Id : constant String := "Debug";


  function Configuration_Filename return String is

    Filename : constant String := Application.Composure ("Log", "ini");

    procedure Create_Default_Configuration is

      Default_Log_Filename : constant String := Application.Composure (Application.Name, "log");

      The_File : Io.File_Type;

      procedure Put (Line : String) is
      begin
        Io.Put_Line (The_File, Line);
      end Put;

    begin -- Create_Default_Configuration
      Io.Create (The_File, Name => Filename);
      Put (Strings.Bom_8 & "[" & File_Section_Name & "]");
      Put (";" & Name_Item & "        = " & Default_Log_Filename);
      Put (Multiple_Item & "     = " & Strings.Legible_Of (Default_Multiple_Files'img));
      Put (Maximum_Size_Item & " =" & Default_Maximum_Size'img);
      Put (Flush_Item & "        = " & Strings.Legible_Of (Default_Flush_After_Write'img));
      Put ("");
      Put ("[" & Filter_Section_Name & "]");
      Put (Categories_Item & " = Data");
      Io.Close (The_File);
    exception
    when others =>
      null;
    end Create_Default_Configuration;

  begin -- Configuration_Filename
    if not File.Exists (Filename) then
      Create_Default_Configuration;
    end if;
    return Filename;
  end Configuration_Filename;


  Log_Handle     : constant Configuration.File_Handle    := Configuration.Handle_For (Configuration_Filename);
  File_Section   : constant Configuration.Section_Handle := Configuration.Handle_For (Log_Handle, File_Section_Name);
  Filter_Section : constant Configuration.Section_Handle := Configuration.Handle_For (Log_Handle, Filter_Section_Name);


  procedure Rename_File (Name     : String;
                         New_Name : String) is
  begin
    File.Rename (Name, New_Name);
  exception
  when others =>
    null;
  end Rename_File;


  Log_Time_Delta : constant := 0.0001;

  type Log_Time is delta Log_Time_Delta range -131072.0 .. +131072.0 - Log_Time_Delta;

  The_File         : Io.File_Type;
  The_Maximum_Size : Natural; -- Maximum number of lines
  The_Current_Size : Natural := 0;

  type String_Access is access String;

  Primary_Filename   : String_Access;
  Secondary_Filename : String_Access;

  Categories : Category := No_Categories;

  Unlimited : constant Natural := 0;


  protected Guarded is

    procedure Open (Is_Started : out Boolean);

    procedure Write_Line (The_String : String);

    function Lookup_Category (The_Category : String) return Category;

    procedure Flush;

  private
    Flush_After_Write : Boolean := Default_Flush_After_Write;
    Multiple_Files    : Boolean := Default_Multiple_Files;
    Category_Names    : Strings.List;
  end Guarded;


  protected body Guarded is

    procedure Open (Is_Started : out Boolean) is

      procedure Determine_Maximum_File_Size is
      begin
        The_Maximum_Size := Configuration.Value_Of (File_Section, Maximum_Size_Item);
      exception
      when others =>
        The_Maximum_Size := Default_Maximum_Size;
      end Determine_Maximum_File_Size;

      function Derived_Filename_From (The_Filename : String;
                                      Suffix       : String) return String is
        Dot_Position : constant Natural := Strings.Location_Of ('.', The_Filename, The_Direction => Strings.Backward);
      begin
        if Dot_Position = Strings.Not_Found then -- No file extension
          return The_Filename & "_" & Suffix;
        else
          return The_Filename (The_Filename'first .. Dot_Position - 1) & "_" & Suffix &
                               The_Filename(Dot_Position .. The_Filename'last);
        end if;
      end Derived_Filename_From;

      function Reduced_List_Of (The_List : List) return List is
        Reduced_List : List;
      begin
        for The_Name of The_List loop
          declare
            Name : constant String := Strings.Legible_Of (The_Name);
          begin
            if Name /= No_Categories_Id then
              if Name = All_Categories_Id then
                return [Name];
              elsif not Reduced_List.Contains (Name) then
                if Natural(Reduced_List.Length) < Max_Number_Of_Categories then
                  Reduced_List.Append (Name);
                end if;
              end if;
            end if;
          end;
        end loop;
        return Reduced_List;
      end Reduced_List_Of;

      Date_And_Time : constant String := Date_Time.Image;

      function Filename return String is
        Name : constant String := Configuration.Value_Of (File_Section, Name_Item);
      begin
        if Multiple_Files then
          declare
            The_Date_And_Time : String := Date_And_Time;
          begin
            for The_Character of The_Date_And_Time loop
              if The_Character = ':' then
                The_Character := '-';
              end if;
            end loop;
            return Derived_Filename_From (Name, Suffix => The_Date_And_Time);
          end;
        else
          return Name;
        end if;
      end Filename;

      function Address_Size return String is
      begin
        return Strings.Trimmed (System.Address'size'img) & " bit ";
      end Address_Size;

    begin -- Open
      Multiple_Files := Configuration.Value_Of (File_Section, Multiple_Item);
      Flush_After_Write := Configuration.Value_Of (File_Section, Flush_Item);
      Primary_Filename := new String'(Filename);
      if Multiple_Files then
        Io.Create (The_File, Name => Primary_Filename.all);
      else
        Io.Open (The_File, Io.Out_File, Primary_Filename.all);
      end if;
      begin
        The_Current_Size := 0;
        Determine_Maximum_File_Size;
        if The_Maximum_Size /= Unlimited then
          Secondary_Filename := new String'(Derived_Filename_From (Primary_Filename.all, Suffix => "2"));
          File.Delete (Secondary_Filename.all);
        end if;
      exception
      when others =>
        null;
      end;
      Category_Names := Reduced_List_Of (Configuration.Value_Of (Filter_Section, Categories_Item));
      declare
        Enabled_Categories : constant Natural := Natural(Category_Names.Length);
      begin
        if Category_Names.Contains (All_Categories_Id) then
          Categories := All_Categories;
        else
          Categories := 2 ** Enabled_Categories - 1;
        end if;
      end;
      Io.Put_Line (The_File, Strings.Bom_8 & Address_Size & Application.Name &" version " & Application.Version);
      Io.Put_Line (The_File, "Log created " & Date_And_Time);
      if Categories = All_Categories then
        Io.Put_Line (The_File, "Logging all categories");
      elsif Categories = No_Categories then
        Io.Put_Line (The_File, "Logging disabled");
      else
        Io.Put_Line (The_File, "Logging categories = " & Strings.Data_Of (Category_Names,","));
      end if;
      Is_Started := True;
    exception
    when others =>
      Is_Started := False;
    end Open;


    procedure Write_Line (The_String : String) is
    begin
      if (The_Maximum_Size /= Unlimited) and then (The_Current_Size >= The_Maximum_Size) then
        Io.Close (The_File);
        Rename_File (Primary_Filename.all, New_Name => Secondary_Filename.all);
        Io.Create (The_File, Name => Primary_Filename.all);
        Io.Put_Line (The_File, Strings.Bom_8 &
                               "The log file reached it's maximum size of" & The_Maximum_Size'img & " records");
        Io.Put_Line (The_File, "These records are now stored in the file " & Secondary_Filename.all);
        The_Current_Size := 2;
      end if;
      declare
        Time : constant String := Log_Time'image(Log_Time(Ada.Calendar.Seconds(Ada.Calendar.Clock)));
      begin
        Io.Put_Line (The_File, Time & " (" & Os.Thread_Id & ") => " & The_String);
        if Flush_After_Write then
          Io.Flush (The_File);
        end if;
      end;
      The_Current_Size := The_Current_Size + 1;
    exception
    when others =>
      null;
    end Write_Line;


    function Lookup_Category (The_Category : String) return Category is
      Next_Category : Category := First_Category;
    begin
      for Name of Category_Names loop
        if The_Category = Name then
          return Next_Category;
        end if;
        Next_Category := Next_Category + 1;
      end loop;
      return Other_Category;
    end Lookup_Category;


    procedure Flush is
    begin
      Io.Flush (The_File);
    end Flush;

  end Guarded;


  function Logging_Started return Boolean is
    Is_Started : Boolean;
  begin
    Guarded.Open (Is_Started);
    return Is_Started;
  end Logging_Started;


  Logging_Is_Active : constant Boolean := Logging_Started;


  procedure Last_Chance_Handler (Occurrence : Ada.Exceptions.Exception_Occurrence)
  with
    No_Return, Unreferenced, Export,
    Convention    => C,
    External_Name => "__gnat_last_chance_handler";

  procedure Last_Chance_Handler (Occurrence : Ada.Exceptions.Exception_Occurrence) is

    procedure Unhandled_Terminate
    with
      No_Return, Import,
      Convention    => C,
      External_Name => "__gnat_unhandled_terminate";

  begin
    begin
      if Logging_Is_Active then
        Write ("LAST CHANCE HANDLER", Occurrence);
      else
        Io.Put_Line ("Last chance handler called before logging was elaborated");
        Io.Put_Line (Exceptions.Information_Of (Occurrence));
      end if;
    exception
    when others =>
      null;
    end;
    Unhandled_Terminate;
  end Last_Chance_Handler;


  Debug_Category : constant Category := Guarded.Lookup_Category (Debug_Category_Id);


  function Lookup (The_Category : String) return Category is
    Name : constant String := Strings.Legible_Of (The_Category);
  begin
    Write ("Log.Lookup: Category = " & Name);
    return Guarded.Lookup_Category (Name);
  end Lookup;


  function "&" (Left, Right : Category) return Category is
  begin
    return Left or Right;
  end "&";


  function Debug return Category is
  begin
    return Debug_Category;
  end Debug;


  function Is_Enabled (The_Category : Category) return Boolean is
  begin
    return (The_Category and Categories) /= 0;
  end Is_Enabled;


  procedure Write (The_String : String) is
  begin
    Write (The_String, Debug_Category);
  end Write;


  procedure Write (The_String   : String;
                   The_Category : Category) is
  begin
    if Logging_Is_Active and then Is_Enabled (The_Category) then
      Guarded.Write_Line (The_String);
    end if;
  exception
  when others =>
    null;
  end Write;


  procedure Write (Occurrence : Ada.Exceptions.Exception_Occurrence) is
  begin
    Write ("", Occurrence);
  end Write;


  procedure Write (Reason     : String;
                   Occurrence : Ada.Exceptions.Exception_Occurrence) is

    function Prefix return String is
    begin
      if Reason = "" then
        return "";
      end if;
      return "Exception: " & Reason & Ascii.Lf;
    end Prefix;

  begin
    begin
      Write (Prefix & Exceptions.Information_Of (Occurrence));
    exception
    when Item: others =>
      Write (Prefix & "Log.Write failed for " & Exceptions.Name_Of (Occurrence));
      Write (Exceptions.Information_Of (Item));
      Guarded.Flush;
    end;
  exception
  when others =>
    Write (Prefix & " Exception handling failed");
  end Write;

end Log;
