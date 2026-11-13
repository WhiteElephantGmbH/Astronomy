-- *********************************************************************************************************************
-- *                               (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Streams.Stream_IO;
with AWS.Client;
with AWS.Messages;
with AWS.Response;
with Application;
with Collector;
with Error;
with File;
with Gui;
with Image;
with Os.Application;
with Os.Process;
with Siril;
with Traces;
with User;

package body Control is

  package Log is new Traces ("Control");

  package SIO renames Ada.Streams.Stream_IO;

  task type Manager is
  end Manager;

  type Command is (Close, New_Picture, View);

  protected Action_Handler is

    procedure Put (The_Action : User.Action);

    procedure Put (The_Action : Collector.Action);

    entry Get (The_Command : out Command);

    procedure Enable_Termination;

    entry Wait_For_Termination;

  private
    Next_Command           : Command := View; -- not Close
    Command_Is_Pending     : Boolean := False;
    Termination_Is_Enabled : Boolean := False;
  end Action_Handler;


  procedure User_Action_Handler (The_Action : User.Action) is
  begin
    Action_Handler.Put (The_Action);
  end User_Action_Handler;


  procedure Collector_Handler (The_Action : Collector.Action) is
  begin
    Action_Handler.Put (The_Action);
  end Collector_Handler;


  protected body Action_Handler is

    procedure Put (The_Action : Collector.Action) is
    begin
      if Next_Command = Close then
        return;
      end if;
      case The_Action is
      when Collector.New_Picture =>
        Next_Command := New_Picture;
      end case;
      Command_Is_Pending := True;
    end Put;


    procedure Put (The_Action : User.Action) is
      use type User.Action;
    begin
      if Next_Command = Close then
        return;
      end if;
      case The_Action is
      when User.View =>
        Next_Command := View;
      when User.Close =>
        Next_Command := Close;
      end case;
      Command_Is_Pending := True;
    end Put;


    entry Get (The_Command : out Command) when Command_Is_Pending is
    begin
      The_Command := Next_Command;
      Command_Is_Pending := False;
    end Get;


    procedure Enable_Termination is
    begin
      Termination_Is_Enabled := True;
    end Enable_Termination;


    entry Wait_For_Termination when Termination_Is_Enabled is
    begin
      null;
    end Wait_For_Termination;

  end Action_Handler;


  procedure Progress (Command  : String;
                      Progress : Siril.Percent) is
    use type Siril.Percent;
  begin
    if Progress = Siril.Start then
      Log.Write (Command & " Started");
    elsif Progress = Siril.Complete then
      Log.Write (Command & " complete");
    else
      null;
    end if;
  end Progress;


  task body Manager is

    procedure Generate_Jpec is
    begin
      File.Delete (Image.Jpec_Filename);
      Siril.Open (Progress'unrestricted_access);
      Siril.Change_Directory (Image.Work_Directory);
      Siril.Load (Image.Fits_Filename);
      Siril.Transfer (User.Tones);
      Siril.Set (User.Saturation);
      Siril.Save_Jpeg (Image.Picture_Name);
      Siril.Close;
      Log.Write ("Generated new picture");
    exception
    when others =>
      Log.Error ("Generate picture failed");
      Siril.Close;
      raise;
    end Generate_Jpec;

    procedure Send_Jpec is

      procedure Check (Result : AWS.Response.Data) is
        Status : constant AWS.Messages.Status_Code := AWS.Response.Status_Code (Result);
      begin
        if not (Status in  AWS.Messages.Success) then
          Log.Error ("Status:" & AWS.Response.Status_Code (Result)'image);
        end if;
      end Check;


      function Read_File return Ada.Streams.Stream_Element_Array is
        The_File : SIO.File_Type;
      begin
        SIO.Open (The_File, SIO.In_File, Image.Jpec_Filename);
        declare
          Size : constant Ada.Streams.Stream_Element_Offset := Ada.Streams.Stream_Element_Offset (SIO.Size (The_File));
          Data : Ada.Streams.Stream_Element_Array (1 .. Size);
          Last : Ada.Streams.Stream_Element_Offset;
        begin
          SIO.Read (The_File, Data, Last);
          SIO.Close (The_File);
          return Data;
        end;
      end Read_File;

    begin -- Send_Jpec
      Check (AWS.Client.Post (URL          => "http://192.168.178.20:8000/image",
                              Data         => Read_File,
                              Content_Type => "image/jpeg"));
    end Send_Jpec;

    The_Command : Command;

  begin -- Manager
    Log.Write ("manager start");
    Collector.Start (Collector_Handler'access);
    loop
      Action_Handler.Get (The_Command);
      case The_Command is
      when View =>
        Generate_Jpec;
        Send_Jpec;
      when New_Picture =>
        Generate_Jpec;
        Send_Jpec;
      when Close =>
        exit;
      end case;
    end loop;
    Log.Write ("manager end");
    Collector.Stop;
    Action_Handler.Enable_Termination;
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
    Collector.Stop;
    Gui.Close;
    Action_Handler.Enable_Termination;
  end Manager;


  procedure Start is

    The_Manager : access Manager with Unreferenced;

    procedure Startup is
    begin
      The_Manager := new Manager;
    end Startup;

    procedure Termination is
    begin
      Action_Handler.Wait_For_Termination;
    end Termination;

  begin -- Start
    if (not Os.Is_Osx) and then (not Os.Application.Is_First_Instance) then
      --
      -- Note: This test is to prevent this application from being run more than once concurrently.
      --       If we mandate that this application is always run from within an OSX .app bundle then
      --       OSX will enforce this and therefore this test is not required.
      --       In this case it is better not to attempt detecting first instance because if the application
      --       is terminated by force quit the mutex is not released but remains until the host is rebooted.
      --
      Error.Raise_With (Application.Name & " already running");
    end if;
    Os.Process.Set_Priority_Class (Os.Process.Realtime);
    User.Execute (Startup'access,
                  User_Action_Handler'access,
                  Termination'access);
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Start;

end Control;
