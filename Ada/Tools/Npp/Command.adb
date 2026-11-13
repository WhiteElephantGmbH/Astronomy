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
pragma Style_Astronomy;

with Ada.Command_Line;
with Log;
with Project;
with Server;
with Text;
with Unsigned;
with Os.Pipe;

package body Command is

  The_Data : Server.Source_Buffer;


  procedure Error (Message: String) is
  begin
    Log.Write ("!!! Command: " & Message);
  end Error;


  procedure Serve (Pipe_Name : String) is

    The_Pipe     : Os.Pipe.Handle;
    The_Length   : Natural;
    The_Filename : Text.String;
    The_Message  : Text.String;

    procedure Write_Confirmation is
    begin
      Os.Pipe.Write (The_Pipe, Server.Confirmation'address, Server.Confirmation'size / Unsigned.Byte'size);
    end Write_Confirmation;

    procedure Read_Data is
    begin
      Os.Pipe.Read (The_Pipe, The_Data'address, The_Length);
    exception
    when Os.Pipe.More_Data =>
      Log.Write ("!!! File length > 2 ** 20");
      loop
        begin
          Os.Pipe.Read (The_Pipe, The_Data'address, The_Length); -- flush all data
          The_Length := 0;
          exit;
        exception
        when Os.Pipe.More_Data =>
          null;
        when others =>
          The_Length := 0;
          exit;
        end;
      end loop;
    end Read_Data;

    function Read_Buffer return String is
    begin
      Read_Data;
      return The_Data(The_Data'first .. The_Data'first + The_Length - 1);
    end Read_Buffer;

    function Read_String return String is
      Data : constant String := Read_Buffer;
    begin
      Write_Confirmation;
      return Data;
    end Read_String;

    function Read_Command return Server.Command is
    begin
      Read_Data;
      return Server.Command'val(Character'pos(The_Data(The_Data'first)));
    end Read_Command;

    function Name_Parameter return String is
    begin
      return The_Data(The_Data'first + 1 .. The_Data'first + The_Length - 1);
    end Name_Parameter;

    function Confirmed_Name return String is
      Item : constant String := Name_Parameter;
    begin
      Write_Confirmation;
      return Item;
    end Confirmed_Name;

    function Read_Natural return Natural is
    begin
      return Natural'value(Read_String);
    end Read_Natural;

    function Read_Line_Number return Server.Line_Number is
    begin
      return Server.Line_Number(Read_Natural);
    exception
    when others =>
      return Server.Line_Number'last;
    end Read_Line_Number;

    function Read_Column_Position return Server.Column_Range is
    begin
      return Server.Column_Range(Read_Natural);
    end Read_Column_Position;

    procedure Write (Item : String) is
    begin
      Os.Pipe.Write (The_Pipe, Item'address, Item'length);
    end Write;

    procedure Write (Item : Text.String) is
    begin
      Write (Item.S);
    end Write;

    procedure Write (Item : Server.Column_Position) is
    begin
      Os.Pipe.Write (The_Pipe, Item'address, Item'size / Unsigned.Byte'size);
    end Write;

    procedure Write_Reference is
      Reference : aliased constant Server.Location := (Column => Server.Column,
                                                       Line   => Server.Line);
    begin
      The_Filename := [Server.Filename];
      Os.Pipe.Write (The_Pipe, Reference'address, Server.Location'size / Unsigned.Byte'size);
    end Write_Reference;

    procedure Write_No_Reference is
    begin
      Os.Pipe.Write (The_Pipe, Server.Not_Referenced'address, Server.Location'size / Unsigned.Byte'size);
    end Write_No_Reference;

    Is_Open : Boolean;

  begin -- Serve
    Os.Pipe.Open (The_Pipe => The_Pipe,
                  Name     => Pipe_Name,
                  Kind     => Os.Pipe.Server,
                  Mode     => Os.Pipe.Duplex,
                  Size     => The_Data'length);
    begin
      Log.Write ("&&& Server Open");
      case Read_Command is
      when Server.Open_Project =>
        Is_Open := Server.Project_Opened (Name_Parameter);
        Write (Project.Confirmation_Message);
      when others =>
        raise Program_Error;
      end case;
      while Is_Open loop
        case Read_Command is
        when Server.Open_Project =>
          raise Program_Error;
        when Server.Get_Edge_Column =>
          Write (Server.Edge_Column);
        when Server.Get_Extensions =>
          Write (Server.Known_Extensions);
        when Server.Is_In_Project =>
          declare
            Filename : constant String := Name_Parameter;
          begin
            if Server.Is_In_Project (Filename) then
              Write_Confirmation;
            else
              Write ("");
            end if;
          end;
        when Server.Case_Updates =>
          declare
            Results : constant Server.Case_Data := Server.Case_Updates;
          begin
            Os.Pipe.Write (The_Pipe, Results'address, Results'size / Unsigned.Byte'size);
          end;
        when Server.Updates_For =>
          declare
            Filename : constant String := Confirmed_Name;
            From     : constant Server.Line_Number := Read_Line_Number;
            To       : constant Server.Line_Number := Read_Line_Number;
            Results  : constant Server.Tokens := Server.Updates_For (Filename, From, To, Read_Buffer);
          begin
            Os.Pipe.Write (The_Pipe, Results'address, Results'size / Unsigned.Byte'size);
          end;
        when Server.Referenced =>
          declare
            Filename : constant String := Confirmed_Name;
            Column   : constant Server.Column_Range := Read_Column_Position;
            Line     : constant Server.Line_Number := Read_Line_Number;
          begin
            if Server.Referenced (Filename, Column, Line, Read_Buffer) then
              Write_Reference;
            else
              Write_No_Reference;
            end if;
          end;
        when Server.Usage =>
          declare
            Filename   : constant String := Confirmed_Name;
            Column     : constant Server.Column_Range := Read_Column_Position;
            Line       : constant Server.Line_Number := Read_Line_Number;
            References : constant Server.Reference_Data := Server.Usage (Filename, Column, Line, Read_Buffer);
          begin
            Os.Pipe.Write (The_Pipe, References'address, References'size / Unsigned.Byte'size);
          end;
        when Server.Unused =>
          declare
            References : constant Server.Reference_Data := Server.Unused;
          begin
            Os.Pipe.Write (The_Pipe, References'address, References'size / Unsigned.Byte'size);
          end;
        when Server.Promote =>
          Server.Promote (Name => Confirmed_Name);
        when Server.Promote_All =>
          Server.Promote (Name => Confirmed_Name,
                          Kind => Server.All_Projects);
        when Server.Run =>
          Server.Promote (Name => Confirmed_Name,
                          Kind => Server.Run);
        when Server.Has_Promotion_Message =>
          if Server.Has_Promotion_Message then
            The_Message := [Server.Message];
          else
            The_Message := [];
          end if;
          Write (The_Message);
        when Server.Has_Promotion_Error =>
          if Server.Has_Promotion_Error then
            The_Message := [Server.Message];
            Write_Reference;
          else
            The_Message := [];
            Write_No_Reference;
          end if;
        when Server.Get_Filename =>
          Write (The_Filename);
        when Server.Get_Message =>
          Write (The_Message);
        when Server.Close_Project =>
          Server.Close_Project;
          exit;
        end case;
      end loop;
    exception
    when Item: others =>
      Log.Write ("!!! Command.Serve", Item);
    end;
    Os.Pipe.Close (The_Pipe);
    Log.Write ("&&& Server Closed");
  exception
  when Os.Pipe.Broken =>
    Os.Pipe.Close (The_Pipe);
    Log.Write ("&&& Client Terminated");
  when Item: others =>
    Os.Pipe.Close (The_Pipe);
    Log.Write ("!!! Command.Serve", Item);
  end Serve;


  procedure Execute is
  begin
    if Ada.Command_Line.Argument_Count < 1 then
      Error ("Pipe name missing");
    elsif Ada.Command_Line.Argument_Count = 1 then
      begin
        Serve (Pipe_Name => Ada.Command_Line.Argument(1));
      exception
      when others =>
        Error ("Invalid pipe name");
      end;
    else
      Error ("Too many parameters");
    end if;
  exception
  when Item: others =>
    Log.Write ("!!! Command.Execute", Item);
  end Execute;

end Command;
