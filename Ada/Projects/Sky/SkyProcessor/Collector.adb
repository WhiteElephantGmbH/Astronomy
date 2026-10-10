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
with AWS.Parameters;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with Image;
with Processor;
with Traces;
with Url_Names;

package body Collector is

  package Log is new Traces ("Collector");

  package SIO renames Ada.Streams.Stream_IO;

  task Controller is
    entry Start;
  end Controller;

  procedure Start is
  begin
    Controller.Start;
  end Start;


  subtype Data_Array  is Ada.Streams.Stream_Element_Array;
  subtype Data_Offset is Ada.Streams.Stream_Element_Offset;

  use type Data_Offset;

  Buffer_Size : constant Data_Offset := 2**16;

  WS : AWS.Server.HTTP;

  The_Index : Image.Count := 0;

  function Callback (Request : AWS.Status.Data) return AWS.Response.Data is
    URI : constant String := AWS.Status.URI (Request);
  begin
    Log.Write ("Request " & URI);
    if URI = Url_Names.Light_Parameters then
      declare
        Ra  : constant String := Url_Names.Ra;
        Dec : constant String := Url_Names.Dec;
        P   : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      begin
        Log.Write ("Ra  = " & P.Get (Ra));
        Log.Write ("Dec = " & P.Get (Dec));
        Image.Clear;
        The_Index := Image.Number'first;
        return AWS.Response.Acknowledge (AWS.Messages.S200, "OK");
      end;
    end if;
    AWS.Server.Get_Message_Body;
    declare
      Buffer   : Data_Array (1 .. Buffer_Size);
      Last     : Data_Offset;
      File     : SIO.File_Type;
      Filename : constant String := Image.Collector_Filename (The_Index);
      use type Image.Number;
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
      Processor.Set (The_Index);
      The_Index := @ + 1;
    exception
    when others =>
      if SIO.Is_Open (File) then
        SIO.Close (File);
      end if;
      raise;
    end;
    return AWS.Response.Build (Content_Type => "text/plain",
                               Message_Body => "OK");
  exception
  when Item: others =>
    Log.Termination (Item);
    return AWS.Response.Build (Content_Type => "text/plain",
                               Message_Body => "ERROR");
  end Callback;


  task body Controller is
  begin
    accept Start;
    Log.Write ("Start");
    Processor.Start;
    AWS.Server.Start (WS,
                      Name     => "Sky_Processor",
                      Callback => Callback'access,
                      Port     => 12000);
    AWS.Server.Wait (AWS.Server.Q_Key_Pressed);
    AWS.Server.Shutdown (WS);
    Processor.Shutdown;
    Log.Write ("Shutdown");
  exception
  when Item: others =>
    Log.Termination (Item);
    Processor.Shutdown;
  end Controller;


end Collector;
