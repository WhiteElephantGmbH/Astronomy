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
with Space;
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

  The_Dark_Parameters : Processor.Dark_Parameters;

  type Processing is (Stopped, Darks, Lights);

  The_Processing : Processing := Stopped;

  procedure Check (Item : Processing) is
  begin
    if The_Processing /= Item then
      raise Program_Error;
    end if;
  end Check;


  function Callback (Request : AWS.Status.Data) return AWS.Response.Data is

    procedure Get_Dark_Parameters is
      P : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
    begin
      The_Dark_Parameters := (Id    => P.Get (Url_Names.Id),
                              Count => Image.Count'value(P.Get (Url_Names.Count)));
      Log.Write ("Dark - Id   : " & The_Dark_Parameters.Id);
      Log.Write ("     - Count:"  & The_Dark_Parameters.Count'image);
      Image.Clear;
      The_Index := 0;
    end Get_Dark_Parameters;

    The_Light_Parameters : Processor.Light_Parameters;

    procedure Get_Light_Parameters is
      P : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
    begin
      The_Light_Parameters := (Target => Space.Direction_Of (Ra_Image  => P.Get (Url_Names.Ra),
                                                             Dec_Image => P.Get (Url_Names.Dec)),
                               Darks_Id => P.Get (Url_Names.Id));
      Log.Write ("Light - Target  : " & Space.Image_Of (The_Light_Parameters.Target));
      Log.Write ("      - Darks_Id: " & The_Light_Parameters.Darks_Id);
      Image.Clear;
      The_Index := 0;
    end Get_Light_Parameters;

    procedure Get_Picture is
    begin
      AWS.Server.Get_Message_Body;
      declare
        Buffer   : Data_Array (1 .. Buffer_Size);
        Last     : Data_Offset;
        File     : SIO.File_Type;
        Filename : constant String := Image.Collector_Filename (The_Index);
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
      exception
      when others =>
        if SIO.Is_Open (File) then
          SIO.Close (File);
        end if;
        raise;
      end;
    end Get_Picture;

    URI : constant String := AWS.Status.URI (Request);

    use type Image.Count;

  begin -- Callback
    Log.Write ("Request " & URI);
    if URI = Url_Names.Dark_Parameters then
      Check (Stopped);
      Get_Dark_Parameters;
      The_Processing := Darks;
      return AWS.Response.Acknowledge (AWS.Messages.S200, "OK");
    elsif URI = Url_Names.Light_Parameters then
      Check (Stopped);
      Get_Light_Parameters;
      Processor.New_Sequence (The_Light_Parameters);
      The_Processing := Lights;
      return AWS.Response.Acknowledge (AWS.Messages.S200, "OK");
    elsif URI = Url_Names.Picture then
      The_Index := @ + 1;
      Get_Picture;
      case The_Processing is
      when Darks =>
        if The_Index = The_Dark_Parameters.Count then
          Processor.Evaluate_Darks (The_Dark_Parameters);
          Image.Clear;
          The_Processing := Stopped;
        end if;
      when Lights =>
        Processor.Set (The_Index);
      when Stopped =>
        raise Program_Error;
      end case;
      return AWS.Response.Build (Content_Type => "text/plain",
                                 Message_Body => "OK");
    elsif URI = Url_Names.Stop then
      Processor.Stop;
      The_Processing := Stopped;
      return AWS.Response.Acknowledge (AWS.Messages.S200, "OK");
    else
      raise Constraint_Error;
    end if;
  exception
  when Item: others =>
    The_Processing := Stopped;
    Log.Termination (Item);
    return AWS.Response.Acknowledge (AWS.Messages.S400, "ERROR");
  end Callback;


  task body Controller is
  begin
    accept Start;
    Log.Write ("started");
    Processor.Start;
    AWS.Server.Start (WS,
                      Name     => "Sky_Processor",
                      Callback => Callback'access,
                      Port     => 12000);
    AWS.Server.Wait (AWS.Server.Q_Key_Pressed);
    AWS.Server.Shutdown (WS);
    Processor.Shutdown;
    Log.Write ("finalized");
  exception
  when Item: others =>
    Log.Termination (Item);
    AWS.Server.Shutdown (WS);
    Processor.Shutdown;
  end Controller;

end Collector;
