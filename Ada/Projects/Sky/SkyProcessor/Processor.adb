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
with Ada.Text_IO;
with AWS.Client;
with AWS.Messages;
with AWS.Response;
with File;
with Siril;
with Traces;
with Units;

package body Processor is

  package Log is new Traces ("Processor");

  package IO  renames Ada.Text_IO;
  package SIO renames Ada.Streams.Stream_IO;

  type Action is (Start_Light_Sequence, Processing, Shutdown);


  protected Action_Handler is

    procedure New_Light_Sequence;

    procedure Set_Collected (The_Index : Image.Count);

    procedure Get_Collected (The_Index : out Image.Count);

    procedure Set_Shutdown;

    entry Get (Item : out Action);

  private
    Is_Waiting          : Boolean := True;
    The_Action          : Action  := Processing;
    The_Collected_Count : Image.Count := 0;
    The_Processed_Count : Image.Count := 0;
  end Action_Handler;


  task Control is
    entry Start;
  end Control;


  procedure Start is
  begin
    IO.Put_Line ("Start");
    Control.Start;
  end Start;


  The_Light_Parameters : Light_Parameters;

  procedure New_Sequence (Parameters : Light_Parameters) is
  begin
    Log.Write ("New Light Sequence");
    The_Light_Parameters := Parameters;
    Action_Handler.New_Light_Sequence;
  end New_Sequence;


  procedure Set (Index : Image.Number) is
  begin
    Log.Write ("Set Collected" & Index'image);
    Action_Handler.Set_Collected (Index);
  end Set;


  procedure Stop is
  begin
    Log.Write ("Processor Stop");
    Siril.Close_Immediate;
  end Stop;


  procedure Shutdown is
  begin
    Log.Write ("Shutdown");
    Siril.Close_Immediate;
    Action_Handler.Set_Shutdown;
  end Shutdown;


  Work_Directory   : constant String := Image.Siril_Work_Directory;

  Picture_Name     : constant String := "Picture";
  Picture_Filename : constant String := Work_Directory & '\' & Picture_Name & ".jpg";


  function Generated_Jpec return Boolean is

    Source_Directory : constant String := Image.Siril_LIGHT_Directory;
    Lights_Directory : constant String := Work_Directory & "\lights";

    Celestron_Focal_Length : constant Units.Focal_Length := 2669.6;

    EOS6D_Pixel_Size : constant Units.Pixel_Size := 6.55;

    M13_Tones : constant Units.Tones := (Shadows    => 0.00016,
                                         Midtones   => 0.00018,
                                         Highlights => 1.0);

    M13_Saturation : constant Units.Saturation := (Amount     => 0.9,
                                                   Multiplier => 1.0);
    procedure Progress (Command  : String;
                        Progress : Siril.Percent) is
      use type Siril.Percent;
    begin
      if Progress = Siril.Start then
        IO.Put (Command & " started");
      elsif Progress = Siril.Complete then
        IO.Put_Line (Ascii.Cr & Command & " Complete");
      else
        IO.Put (Ascii.Cr & Command & Progress'image & "%       ");
      end if;
    end Progress;

  begin
    File.Delete (Picture_Filename);
    Siril.Open (Progress'unrestricted_access);
    Siril.Change_Directory (Source_Directory);

    Siril.Convert_Light (Destination => Lights_Directory);
    Siril.Change_Directory (Lights_Directory);

    Siril.Calibrate_Light (Dark => Work_Directory & "\darks_stacked.fit",
                           Flat => Work_Directory & "\flats_stacked.fit");
    Siril.Register_Light;
    Siril.Stack_Light;
    Siril.Load_Stacked_Light;

    Siril.Plate_Solve (The_Light_Parameters.Target, Celestron_Focal_Length, EOS6D_Pixel_Size);

    Siril.Remove_Background;
    Siril.Calibrate_Color;
    Siril.Transfer (M13_Tones);
    Siril.Set (M13_Saturation);

    Siril.Change_Directory ("..");
    Siril.Save_Jpeg (Picture_Name);
    Siril.Close;
    IO.Put_Line ("New Jpec");
    return True;
  exception
  when Siril.Aborted =>
    IO.Put_Line ("Stopped");
    return False;
  end Generated_Jpec;


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
      SIO.Open (The_File, SIO.In_File, Picture_Filename);
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


  task body Control is
    The_Action      : Action;
    Collected_Count : Image.Count := 0;
    Processed_Count : Image.Count;
    use type Image.Number;
  begin
    accept Start;
    Log.Write ("started");
    loop
      Action_Handler.Get (The_Action);
      case The_Action is
      when Start_Light_Sequence =>
        IO.Put_Line ("new light sequence");
        Collected_Count := 0;
      when Processing =>
        Processed_Count := Collected_Count + 1;
        Action_Handler.Get_Collected (Collected_Count);
        Image.Move (Processed_Count, Collected_Count);
        IO.Put_Line ("process 1 .." & Collected_Count'image);
        if Generated_Jpec then
          Send_Jpec;
        end if;
      when Shutdown =>
        exit;
      end case;
    end loop;
    Log.Write ("finalized");
  exception
  when Item: others =>
    Log.Termination (Item);
  end Control;


  protected body Action_Handler is

    procedure New_Light_Sequence is
    begin
      if The_Action /= Shutdown then
        The_Action := Start_Light_Sequence;
        Is_Waiting := False;
      end if;
    end New_Light_Sequence;


    procedure Set_Collected (The_Index : Image.Count) is
      use type Image.Count;
    begin
      if The_Action /= Shutdown then
        The_Action := Processing;
        The_Collected_Count := The_Index;
        Is_Waiting := The_Collected_Count < 2 or else The_Collected_Count <= The_Processed_Count;
      end if;
    end Set_Collected;


    procedure Get_Collected (The_Index : out Image.Count) is
    begin
      The_Processed_Count := The_Collected_Count;
      The_Index := The_Collected_Count;
    end Get_Collected;


    procedure Set_Shutdown is
    begin
      Is_Waiting := False;
      The_Action := Shutdown;
    end Set_Shutdown;


    entry Get (Item : out Action) when not Is_Waiting is
    begin
      Item := The_Action;
      Is_Waiting := True;
    end Get;

  end Action_Handler;

end Processor;
