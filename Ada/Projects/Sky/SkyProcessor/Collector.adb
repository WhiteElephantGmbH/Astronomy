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
with Units;

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

  type Processing is (Stopped, Darks, Lights);

  The_Processing : Processing := Stopped;

  procedure Check (Item : Processing) is
  begin
    if The_Processing /= Item then
      raise Program_Error;
    end if;
  end Check;


  function Callback (Request : AWS.Status.Data) return AWS.Response.Data is

    function Darks_Filename return String is
      P : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      Filename : constant String := P.Get (Url_Names.Id);
    begin
      Log.Write ("Darks - Filename : " & Filename);
      return Filename;
    end Darks_Filename;

    The_Light_Parameters : Processor.Light_Parameters;

    procedure Get_Light_Parameters is
      P : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
    begin
      The_Light_Parameters := (Target => Space.Direction_Of (Ra_Image  => P.Get (Url_Names.Ra),
                                                             Dec_Image => P.Get (Url_Names.Dec)),
                               Darks_Id     => [P.Get (Url_Names.Id)],
                               Camera_Model => Units.Camera_Model'value(P.Get (Url_Names.Camera_Model)),
                               Pixel_Size   => Units.Pixel_Size'value(P.Get (Url_Names.Pixel_Size)),
                               Focal_Length => Units.Focal_Length'value(P.Get (Url_Names.Focal_Length)));
      Log.Write ("Light - Target      : " & Space.Image_Of (The_Light_Parameters.Target));
      Log.Write ("      - Darks_Id    : " & The_Light_Parameters.Darks_Id.S);
      Log.Write ("      - Camera_Model: " & The_Light_Parameters.Camera_Model'image);
      Log.Write ("      - Pixel_Size  :"  & The_Light_Parameters.Pixel_Size'image);
      Log.Write ("      - Focal_Length:"  & The_Light_Parameters.Focal_Length'image);
      Image.Set (The_Light_Parameters.Camera_Model);
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
    if URI = Url_Names.New_Darks then
      Check (Stopped);
      if Image.Cleared then
        The_Processing := Darks;
        The_Index := 0;
        return AWS.Response.Acknowledge (AWS.Messages.S200, "OK");
      else
        Log.Warning ("Dark work area not cleared.");
        return AWS.Response.Acknowledge (AWS.Messages.S300, "WARNING");
      end if;
    elsif URI = Url_Names.Light_Parameters then
      Check (Stopped);
      if Image.Cleared then
        Get_Light_Parameters;
        Processor.New_Sequence (The_Light_Parameters);
        The_Processing := Lights;
        return AWS.Response.Acknowledge (AWS.Messages.S200, "OK");
      else
        Log.Warning ("Light images not cleared.");
        return AWS.Response.Acknowledge (AWS.Messages.S300, "WARNING");
      end if;
    elsif URI = Url_Names.Picture then
      The_Index := @ + 1;
      Get_Picture;
      case The_Processing is
      when Darks =>
        null;
      when Lights =>
        Processor.Set (The_Index);
      when Stopped =>
        raise Program_Error;
      end case;
      return AWS.Response.Build (Content_Type => "text/plain",
                                 Message_Body => "OK");
    elsif URI = Url_Names.Store_Darks then
      Processor.Evaluate_Darks (Darks_Filename);
      The_Processing := Stopped;
      return AWS.Response.Acknowledge (AWS.Messages.S200, "OK");
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
                      Port     => 11000);
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
