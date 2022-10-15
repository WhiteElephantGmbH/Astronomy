-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.UTF_Encoding.Strings;
with Os.Application;

package body Applications is

  package FS  renames Ada.Directories;
  package Utf renames Ada.Strings.UTF_Encoding.Strings;

  function To_Utf8 (Item       : String;
                    Output_BOM : Boolean := False) return String renames Utf.Encode;


  function Name return String is
  begin
    if Product = "" then
      return Os.Application.Name;
    else
      return Product;
    end if;
  end Name;


  function Main_Version return String is
  begin
    return Os.Application.Main_Version;
  exception
  when others =>
    return "no information";
  end Main_Version;


  function Version return String is
  begin
    return Os.Application.Version;
  exception
  when others =>
    return "no information";
  end Version;


  function Created_Application_Data_Directory return String is

    Is_Hidden : Boolean := False;

    function Main_Directory return String is
    begin
      case Os.Family is
      when Os.Windows =>
        declare
          App_Directory : constant String := To_Utf8 (Ada.Environment_Variables.Value ("AppData"));
        begin
          if Company = "" then
            return App_Directory;
          else
            return FS.Compose (App_Directory, Company);
          end if;
        end;
      when Os.Osx =>
        declare
          Home_Directory        : constant String := Ada.Environment_Variables.Value ("HOME");
          Library_Directory     : constant String := FS.Compose (Home_Directory, "Library");
          Application_Directory : constant String := FS.Compose (Library_Directory, "Application Support");
        begin
          if Company = "" then
            return Application_Directory;
          else
            return FS.Compose (Application_Directory, Company);
          end if;
        end;
      when Os.Linux =>
        declare
          Home_Directory : constant String := Ada.Environment_Variables.Value ("HOME");
        begin
          if Company = "" then
            Is_Hidden := True;
            return Home_Directory;
          else
            return FS.Compose (Home_Directory, "." & Company);
          end if;
        end;
      end case;
    end Main_Directory;

    function Prefix return String is (if Is_Hidden then "." else "");

  begin -- Created_Application_Data_Directory
    declare
      Main_Data_Directory        : constant String := Main_Directory;
      Application_Data_Directory : constant String := FS.Compose (Main_Data_Directory, Prefix & Name);
    begin
      FS.Create_Path (Application_Data_Directory);
      return Application_Data_Directory;
    end;
  exception
  when others =>
    return FS.Current_Directory;
  end Created_Application_Data_Directory;

  Created_Data_Directory : constant String := Created_Application_Data_Directory;


  function Data_Directory return String is
  begin
    return Created_Data_Directory;
  end Data_Directory;


  function Composure (Filename  : String;
                      Extension : String) return String is
  begin
    return FS.Compose (Data_Directory, Filename, Extension);
  end Composure;


  function Composure (Directory : String) return String is
  begin
    return FS.Compose (Data_Directory, Directory);
  end Composure;

end Applications;
