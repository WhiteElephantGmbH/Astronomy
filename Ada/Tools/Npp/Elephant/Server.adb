-- *********************************************************************************************************************
-- *                       (c) 2008 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Unchecked_Conversion;
with File;
with Log;
with System;
with Os.Application;
with Os.Pipe;
with Os.Process;

package body Server is

  The_Pipe : Os.Pipe.Handle;

  The_Length : Natural;
  The_Data   : aliased Source_Buffer;
  for The_Data'alignment use 4;

  The_Message : Text.String;


  procedure Read_Data is
  begin
    Os.Pipe.Read (The_Pipe, The_Data'address, The_Length);
  end Read_Data;


  function Data_String return String is
  begin
    return The_Data(The_Data'first .. The_Data'first + The_Length - 1);
  end Data_String;


  procedure Send (Service : Command;
                  Item    : String := "") is
    Id   :         constant Character := Character'val(Command'pos(Service));
    Data : aliased constant String := Id & Item;
  begin
    Os.Pipe.Write (The_Pipe, Data'address, Data'length);
    Read_Data;
  end Send;


  procedure Send (Item : String) is
  begin
    Os.Pipe.Write (The_Pipe, Item'address, Item'length);
    Read_Data;
  end Send;


  procedure Send (Item : Natural) is
  begin
    Send (Natural'image(Item));
  end Send;


  function Is_In_Project (Name : String) return Boolean is
  begin
    Send (Is_In_Project, Name);
    return Data_String = "" & Confirmation;
  exception
  when others =>
    return False;
  end Is_In_Project;


  procedure Set_Message (Item : String) is
  begin
    The_Message := [Item];
  end Set_Message;


  procedure Set_Error (Item : String) is
  begin
    Log.Write ("!!! " & Item);
    Set_Message (Item);
  end Set_Error;


  function Language_Of (Name : String) return String is
    Last : Natural := Natural'first;
  begin
    for Index in reverse Name'range loop
      case Name(Index) is
      when '\' | '/' =>
        if Last = Natural'first then
          if File.Exists (Name(Name'first .. Index) & Project_File) then
            Last := Index - 1;
          end if;
        else
          return Name(Index + 1 .. Last);
        end if;
      when others =>
        null;
      end case;
    end loop;
    return "";
  end Language_Of;


  function Project_Opened (Name : String) return Boolean is

    Language        : constant String := Language_Of (Name);
    Pipe_Name       : constant String := "Npp_" & Language & "_Pipe_V_1.0";
    Language_Server : constant String := Language & "_Server.exe";

    Project_Not_Opened : constant String := "Project containing " & Name & " not opened - ";
    Startup_Timeout    : constant Os.Pipe.Timer := 3.0;

  begin
    if Language = "" then
      Set_Message (Project_Not_Opened & "unknown project! ( no " & Project_File & ")");
      return False;
    end if;
    begin
      Os.Process.Create (Os.Application.Origin_Folder & '\' & Language_Server, Pipe_Name);
      begin
        Os.Pipe.Open (The_Pipe  => The_Pipe,
                      Name      => Pipe_Name,
                      Kind      => Os.Pipe.Client,
                      Mode      => Os.Pipe.Duplex,
                      Size      => Server.Source_Buffer'length,
                      Wait_Time => Startup_Timeout);
        Send (Open_Project, Name);
        if The_Length = 0 then
          Set_Message (Project_Not_Opened & "protocol error (no Confirmation)");
        else
          Set_Message (The_Data(The_Data'first + 1 .. The_Data'first + The_Length - 1));
          if The_Data(The_Data'first) = Confirmation then
            Log.Write ("+ Project Opened using: " & Name);
            Send (Get_Extensions);
            return True;
          elsif The_Data(The_Data'first) = Not_Confirmed then
            Log.Write ("- Project not Opened using: " & Name);
            return False;
          else
            Set_Error (Project_Not_Opened & "protocol error (wrong Confirmation)");
          end if;
        end if;
      exception
      when Os.Pipe.No_Server =>
        Set_Error (Project_Not_Opened & Language_Server & " not connected!");
      when Os.Pipe.Name_In_Use =>
        Set_Error (Project_Not_Opened & Language_Server & " in use!");
      when Os.Pipe.Broken =>
        Set_Error (Project_Not_Opened & Language_Server & " broken!");
      end;
    exception
    when Os.Process.Creation_Failure =>
      Set_Error (Project_Not_Opened & Language_Server & " missing!");
    when Occurence: others =>
      Set_Message (Project_Not_Opened & "internal error");
      Log.Write ("! " & Project_Not_Opened, Occurence);
    end;
    Os.Pipe.Close (The_Pipe);
    return False;
  end Project_Opened;


  procedure Close_Project is
  begin
    Log.Write ("+ Project Close");
    Send (Close_Project);
  exception
  when others =>
    null;
  end Close_Project;


  function Known_Extensions return String is
  begin
    Send (Get_Extensions);
    return Data_String;
  end Known_Extensions;


  function Edge_Column return Server.Column_Position is
    The_Edge_Column : Server.Column_Position;
    for The_Edge_Column'address use The_Data'address;
  begin
    Send (Get_Edge_Column);
    return The_Edge_Column;
  end Edge_Column;


  function Case_Updates return Case_Data is
  begin
    Send (Case_Updates);
    declare
      subtype Actual_Results is Case_Data(1 .. The_Length * 8 / Case_Data'component_size);
      Updates : Actual_Results;
      for Updates'address use The_Data'address;
    begin
      return Updates;
    end;
  end Case_Updates;


  function Updates_For (The_Filename : String;
                        First_Line   : Line_Number;
                        Last_Line    : Line_Number;
                        Content      : String) return Tokens is
  begin
    Send (Updates_For, The_Filename);
    Send (Natural(First_Line));
    Send (Natural(Last_Line));
    Send (Content);
    declare
      subtype Actual_Results is Tokens(1 .. The_Length * 8 / Tokens'component_size);
      Updates : Actual_Results;
      for Updates'address use The_Data'address;
    begin
      return Updates;
    end;
  end Updates_For;


  The_Actual_Column   : Column_Range;
  The_Actual_Line     : Line_Number;
  The_Actual_Filename : Text.String;


  procedure Read_Filename is
  begin
    Send (Get_Filename);
    The_Actual_Filename := [Data_String];
  end Read_Filename;


  procedure Read_Message is
  begin
    Send (Get_Message);
    Set_Message (Data_String);
  end Read_Message;


  function Has_Reference return Boolean is
    Reference : Location;
    for Reference'address use The_Data'address;
  begin
    if Reference = Not_Referenced then
      return False;
    else
      The_Actual_Column := Reference.Column;
      The_Actual_Line := Reference.Line;
      Read_Filename;
      return True;
    end if;
  end Has_Reference;


  function Has_Message return Boolean is
  begin
    if The_Length /= 0 then
      Set_Message (Data_String);
      return True;
    else
      return False;
    end if;
  end Has_Message;


  function Referenced (The_Filename : String;
                       At_Column    : Column_Range;
                       At_Line      : Line_Number;
                       Content  : String) return Boolean is
  begin
    Send (Referenced, The_Filename);
    Send (Natural(At_Column));
    Send (Natural(At_Line));
    Send (Content);
    return Has_Reference;
  exception
  when Item: others =>
    Log.Write ("Referenced", Item);
    return False;
  end Referenced;


  function Usage (The_Filename : String;
                  At_Column    : Column_Range;
                  At_Line      : Line_Number;
                  Content      : String) return Reference_Data is
  begin
    raise Program_Error;
    return No_Reference_Data;
  end Usage;


  function Usage (The_Filename : String;
                  At_Column    : Column_Range;
                  At_Line      : Line_Number;
                  Content      : String) return References is
    use type System.Address;
  begin
    Send (Usage, The_Filename);
    Send (Natural(At_Column));
    Send (Natural(At_Line));
    Send (Content);
    declare
      function References_Of is new Ada.Unchecked_Conversion (System.Address, References);
    begin
      return References_Of (The_Data(The_Data'first)'address);
    end;
  exception
  when Item: others =>
    Log.Write ("Usage", Item);
    return null;
  end Usage;


  function Unused return Reference_Data is
  begin
    raise Program_Error;
    return No_Reference_Data;
  end Unused;


  function Unused return References is
  begin
    Send (Command'(Unused));
    declare
      function References_Of is new Ada.Unchecked_Conversion (System.Address, References);
    begin
      return References_Of (The_Data(The_Data'first)'address);
    end;
  exception
  when Item: others =>
    Log.Write ("Unused", Item);
    return null;
  end Unused;


  procedure Promote (Name : String;
                     Kind : Promotion_Kind := Normal) is
  begin
    case Kind is
    when Normal =>
      Send (Promote, Name);
    when All_Projects =>
      Send (Promote_All, Name);
    when Run =>
      Send (Run, Name);
    end case;
  end Promote;


  function Has_Promotion_Message return Boolean is
  begin
    Send (Has_Promotion_Message);
    return Has_Message;
  end Has_Promotion_Message;


  function Has_Promotion_Error return Boolean is
  begin
    Send (Has_Promotion_Error);
    if Has_Reference then
      Read_Message;
      return True;
    else
      return False;
    end if;
  end Has_Promotion_Error;


  function Message return String is
  begin
    return The_Message.S;
  end Message;


  function Filename return String is
  begin
    return The_Actual_Filename.S;
  end Filename;


  function Column return Column_Range is
  begin
    return The_Actual_Column;
  end Column;


  function Line return Line_Number is
  begin
    return The_Actual_Line;
  end Line;


  function Names_Of (Item : Text.List) return String is
  begin
    raise Program_Error;
    return "";
  end Names_Of;

end Server;
