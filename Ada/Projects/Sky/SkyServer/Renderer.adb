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

with Distributor;
with File;
with Image;
with Rt;
with Siril;
with Siril_Interface;
with Traces;

package body Renderer is

  Id : constant String := "Renderer";

  package Log is new Traces (Id);

  package SI is new Siril_Interface (Id);
  package Session is new Siril (SI);

  task type Manager is
    entry Set (Tones : Units.Tones);
    entry Set (Saturation : Units.Saturation);
    entry New_Sequence;
    entry New_Picture;
    entry Close;
  end Manager;

  The_Manager : access Manager;


  procedure Start is
  begin
    The_Manager := new Manager;
  end Start;


  procedure Set (Tones : Units.Tones) is
  begin
    The_Manager.Set (Tones);
  end Set;


  procedure Set (Saturation : Units.Saturation) is
  begin
    The_Manager.Set (Saturation);
  end Set;


  procedure New_Sequence is
  begin
    The_Manager.New_Sequence;
  end New_Sequence;


  procedure New_Picture is
  begin
    The_Manager.New_Picture;
  end New_Picture;


  procedure Close is
  begin
    The_Manager.Close;
  end Close;


  task body Manager is

    procedure Delete (Filename : String) is
      Max_Retries : constant := 10;
    begin
      for Attempt in 1 .. Max_Retries loop
        begin
          File.Delete (Filename);
          return;
        exception
        when File.In_Use =>
          Log.Warning ("JPEG delete retry" & Attempt'image & " of" & Max_Retries'image);
          if Attempt = Max_Retries then
            raise;
          end if;
          Rt.Wait (0.1);
        end;
      end loop;
    end Delete;

    The_Tones : Units.Tones := (Shadows    => 0.00017,
                                Midtones   => 0.00019,
                                Highlights => 0.85650);

    The_Saturation : Units.Saturation := (Amount     => 0.9,
                                          Multiplier => 5.0);

    Picture_Available : Boolean := False;

    procedure Generate_Jpeg is
    begin
      if Picture_Available then
        Delete (Image.Renderer_Jpeg_Filename);
        Session.Open;
        Session.Change_Directory (Image.Renderer_Work_Directory);
        Session.Load (Image.Renderer_Fits_Filename);
        Session.Transfer (The_Tones);
        Session.Set (The_Saturation);
        Session.Save_Jpeg (Image.Picture_Name);
        Session.Close;
        Log.Write ("Generated new picture");
      else
        Log.Warning ("Picture not available");
      end if;
    exception
    when others =>
      Session.Close;
      raise;
    end Generate_Jpeg;

    procedure Send_Jpeg is
    begin
      if Picture_Available then
        Distributor.Send_Jpeg (Image.Renderer_Jpeg_Filename);
      end if;
    end Send_Jpeg;

  begin -- Manager
    Log.Write ("manager start");
    loop
      select
        accept Set (Tones : Units.Tones) do
          The_Tones := Tones;
          Generate_Jpeg;
        end Set;
        Send_Jpeg;
      or
        accept Set (Saturation : Units.Saturation) do
          The_Saturation := Saturation;
          Generate_Jpeg;
        end Set;
        Send_Jpeg;
      or
        accept New_Sequence do
          Picture_Available := False;
        end New_Sequence;
      or
        accept New_Picture do
          Picture_Available := Image.Copied_Fits;
        end New_Picture;
        Generate_Jpeg;
        Send_Jpeg;
      or
        accept Close;
        exit;
      end select;
    end loop;
    Log.Write ("manager end");
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Manager;

end Renderer;
