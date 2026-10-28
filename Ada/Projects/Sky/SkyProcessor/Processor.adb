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

  type Action is (Process_Darks, Start_Light_Sequence, Show_First, Processing, Shutdown);


  protected Action_Handler is

    procedure Evaluate_Darks;

    procedure New_Light_Sequence;

    procedure Set_Collected (The_Index : Image.Count);

    procedure Get_Collected (The_Index : out Image.Count);

    procedure Set_Shutdown;

    entry Get (Item : out Action);

    procedure Set_Complete;

    entry Await_Completion;

  private
    Is_Waiting          : Boolean := True;
    Is_Complete         : Boolean := False;
    The_Action          : Action  := Processing;
    The_Collected_Count : Image.Count := 0;
    The_Processed_Count : Image.Count := 0;
  end Action_Handler;


  task Control is
    entry Start;
  end Control;


  type Log_Kind is (Normal, Warning, Error);

  procedure Show (Message : String;
                  Kind    : Log_Kind := Normal) is
  begin
    case Kind is
    when Normal =>
      Log.Write (Message);
      IO.Put_Line (Message);
    when Warning =>
      Log.Warning (Message);
      IO.Put_Line ("Warning: " & Message);
    when Error =>
      Log.Error (Message);
      IO.Put_Line ("Error: " & Message & '!');
    end case;
  end Show;


  procedure Start is
  begin
    Show ("Start");
    Control.Start;
  end Start;


  The_Darks_Filename : Text.String;

  procedure Evaluate_Darks (Filename : String) is
  begin
    Show ("Evaluate Darks");
    The_Darks_Filename := [Filename];
    Action_Handler.Evaluate_Darks;
    Log.Write ("Await Completion");
    Action_Handler.Await_Completion;
    Show ("Darks Evaluated");
    if not Image.Cleared then
      Show ("Image area not cleared !", Warning);
    end if;
  end Evaluate_Darks;


  The_Light_Parameters : Light_Parameters;

  procedure New_Sequence (Parameters : Light_Parameters) is
  begin
    Show ("New light sequence");
    The_Light_Parameters := Parameters;
    Action_Handler.New_Light_Sequence;
  end New_Sequence;


  procedure Set (Index : Image.Number) is
  begin
    Show ("Set Collected" & Index'image);
    Action_Handler.Set_Collected (Index);
  end Set;


  procedure Stop is
  begin
    Show ("Stop");
    Siril.Close_Immediate;
  end Stop;


  procedure Shutdown is
  begin
    Show ("Shutdown");
    Siril.Close_Immediate;
    Action_Handler.Set_Shutdown;
  end Shutdown;


  procedure Progress (Command  : String;
                      Progress : Siril.Percent) is
    use type Siril.Percent;
  begin
    if Progress = Siril.Start then
      IO.Put (Command & " Started");
    elsif Progress = Siril.Complete then
      IO.Put_Line (Ascii.Cr & Command & " complete                        ");
    else
      IO.Put (Ascii.Cr & Command & Progress'image & "%       ");
    end if;
  end Progress;


  Stacked_Fit : constant String := "stacked.fit";

  Darks_Directory : constant String := Image.Darks_Directory;
  Flats_Directory : constant String := Image.Flats_Directory;
  Work_Directory  : constant String := Image.Siril_Work_Directory;


  procedure Generate_Darks is
    Darks_Work_Directory : constant String := Image.Darks_Work_Directory;
    Darks_Fit_Filename   : constant String := Darks_Directory & "\" & The_Darks_Filename.S & "_" & Stacked_Fit;
  begin
    IO.Put_Line ("Generate " & Darks_Fit_Filename);
    Siril.Open (Progress'unrestricted_access);
    Siril.Change_Directory (Image.Collector_Directory);
    Siril.Convert_Dark (Destination => Darks_Work_Directory);
    Siril.Change_Directory (Darks_Work_Directory);
    Siril.Stack_Dark (Darks_Fit_Filename);
    Siril.Close;
    Action_Handler.Set_Complete;
    Log.Write ("Generated " & Darks_Fit_Filename);
  end Generate_Darks;


  Picture_Name     : constant String := "Picture";
  Picture_Filename : constant String := Work_Directory & '\' & Picture_Name & ".jpg";


  procedure Generate_First_Jpec is
  begin
    File.Delete (Picture_Filename);
    Siril.Open;
    Siril.Load (Image.First_LIGHT_Filename);
    Siril.Auto_Stretch;
    Siril.Change_Directory (Work_Directory);
    Siril.Save_Jpeg (Picture_Name);
    Siril.Close;
    IO.Put_Line ("Generated first picture");
  exception
  when others =>
    Siril.Close;
    raise;
  end Generate_First_Jpec;


  procedure Generate_Jpec is

    Source_Directory : constant String := Image.Siril_LIGHT_Directory;
    Lights_Directory : constant String := Work_Directory & "\lights";

    Flats_Stacked_Fit : constant String := Flats_Directory & "\" & Stacked_Fit;
    Darks_Stacked_Fit : constant String := Darks_Directory & "\" & The_Light_Parameters.Darks_Id.S & "_" & Stacked_Fit;

    Celestron_Focal_Length : constant Units.Focal_Length := 2669.6;

    EOS6D_Pixel_Size : constant Units.Pixel_Size := 6.55;

    M13_Tones : constant Units.Tones := (Shadows    => 0.00016,
                                         Midtones   => 0.00018,
                                         Highlights => 1.0);

    M13_Saturation : constant Units.Saturation := (Amount     => 0.9,
                                                   Multiplier => 1.0);
  begin
    File.Delete (Picture_Filename);
    Siril.Open (Progress'unrestricted_access);
    Siril.Change_Directory (Source_Directory);

    Siril.Convert_Light (Destination => Lights_Directory);
    Siril.Change_Directory (Lights_Directory);

    Siril.Calibrate_Light (Dark => Darks_Stacked_Fit,
                           Flat => Flats_Stacked_Fit);
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
    IO.Put_Line ("Generated new picture");
  exception
  when others =>
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
      begin
        Action_Handler.Get (The_Action);
        case The_Action is
        when Process_Darks =>
          Generate_Darks;
        when Start_Light_Sequence =>
          Collected_Count := 0;
        when Show_First =>
          Generate_First_Jpec;
          Send_Jpec;
        when Processing =>
          Processed_Count := Collected_Count + 1;
          Action_Handler.Get_Collected (Collected_Count);
          Image.Move (Processed_Count, Collected_Count);
          IO.Put_Line ("Process 1 .." & Collected_Count'image);
          Generate_Jpec;
          Send_Jpec;
        when Shutdown =>
          exit;
        end case;
      exception
      when Siril.Aborted =>
        IO.Put_Line ("Stopped");
      when Siril.Not_Enough_Stars =>
        Show ("Not enough stars detected!", Warning);
      when Siril.Plate_Solving_Failed =>
        Show ("Plate solving failed!", Warning);
      end;
    end loop;
    Log.Write ("finalized");
  exception
  when Item: others =>
    IO.Put_Line ("Error: Unexpected Termination!");
    Log.Termination (Item);
  end Control;


  protected body Action_Handler is

    procedure Evaluate_Darks is
    begin
      if The_Action /= Shutdown then
        The_Action := Process_Darks;
        Is_Waiting := False;
        Is_Complete := False;
      end if;
    end Evaluate_Darks;


    procedure New_Light_Sequence is
    begin
      if The_Action /= Shutdown then
        The_Action := Start_Light_Sequence;
        The_Collected_Count := 0;
        The_Processed_Count := 0;
        Is_Waiting := False;
      end if;
    end New_Light_Sequence;


    procedure Set_Collected (The_Index : Image.Count) is
      use type Image.Count;
    begin
      if The_Action /= Shutdown then
        The_Collected_Count := The_Index;
        if The_Collected_Count = 1 then
          The_Action := Show_First;
        else
          The_Action := Processing;
        end if;
        Is_Waiting := The_Collected_Count = 0 or else The_Collected_Count <= The_Processed_Count;
      end if;
    end Set_Collected;


    procedure Get_Collected (The_Index : out Image.Count) is
    begin
      The_Processed_Count := The_Collected_Count;
      The_Index := The_Collected_Count;
    end Get_Collected;


    procedure Set_Shutdown is
    begin
      Is_Complete := True;
      Is_Waiting := False;
      The_Action := Shutdown;
    end Set_Shutdown;


    entry Get (Item : out Action) when not Is_Waiting is
    begin
      Item := The_Action;
      Is_Waiting := True;
    end Get;


    procedure Set_Complete is
    begin
      Is_Complete := True;
    end Set_Complete;


    entry Await_Completion when Is_Complete is
    begin
      Is_Complete := False;
    end Await_Completion;

  end Action_Handler;

end Processor;
