-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
with AWS.Messages;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with Image;
with Traces;
with Url_Names;

package body Collector is

  package Log is new Traces ("Collector");

  package SIO renames Ada.Streams.Stream_IO;


  task Controller is
    entry Start;
    entry Stopped;
  end Controller;


  The_Action_Handler : Action_Handler;

  procedure Start (Handler : Action_Handler) is
  begin
    The_Action_Handler := Handler;
    Controller.Start;
  end Start;


  WS : AWS.Server.HTTP;

  procedure Stop is
  begin
    AWS.Server.Shutdown (WS);
    Controller.Stopped;
  end Stop;


  subtype Data_Array  is Ada.Streams.Stream_Element_Array;
  subtype Data_Offset is Ada.Streams.Stream_Element_Offset;

  use type Data_Offset;

  Buffer_Size : constant Data_Offset := 2**16;

  function Callback (Request : AWS.Status.Data) return AWS.Response.Data is

    procedure Get_Picture is
    begin
      AWS.Server.Get_Message_Body;
      declare
        Buffer   : Data_Array (1 .. Buffer_Size);
        Last     : Data_Offset;
        File     : SIO.File_Type;
        Filename : constant String := Image.Fits_Filename;
      begin
        SIO.Create (File => File,
                    Mode => SIO.Out_File,
                    Name => Filename);
        while not AWS.Status.End_Of_Body (Request) loop
          AWS.Status.Read_Body (Request, Buffer, Last);
          if Last >= Buffer'first then
            SIO.Write (File, Buffer(Buffer'first .. Last));
          end if;
        end loop;
        SIO.Close (File);
        Log.Write ("Received " & Filename);
        The_Action_Handler (New_Picture);
      exception
      when others =>
        if SIO.Is_Open (File) then
          SIO.Close (File);
        end if;
        raise;
      end;
    end Get_Picture;

    URI : constant String := AWS.Status.URI (Request);

  begin -- Callback
    Log.Write ("Request " & URI);
    if URI = Url_Names.Picture then
      Get_Picture;
      return AWS.Response.Build (Content_Type => "text/plain",
                                 Message_Body => "OK");
    else
      raise Constraint_Error;
    end if;
  exception
  when Item: others =>
    Log.Termination (Item);
    return AWS.Response.Acknowledge (AWS.Messages.S400, "ERROR");
  end Callback;


  task body Controller is
  begin
    accept Start;
    Log.Write ("started");
    AWS.Server.Start (WS,
                      Name     => "Sky_Processor",
                      Callback => Callback'access,
                      Port     => 11001);
    AWS.Server.Wait (AWS.Server.No_Server);
    accept Stopped;
    Log.Write ("finalized");
  exception
  when Item: others =>
    Log.Termination (Item);
    AWS.Server.Shutdown (WS);
  end Controller;

end Collector;
