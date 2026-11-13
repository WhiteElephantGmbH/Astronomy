-- *********************************************************************************************************************
-- *                            (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                             *
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

with Application;
with Gui.Registered;
with Text;
with Traces;

package body User is

  package Log is new Traces ("User");

  Shadows_Slider    : Gui.Track_Bar;
  Midtones_Slider   : Gui.Track_Bar;
  Highlights_Slider : Gui.Track_Bar;
  Amount_Slider     : Gui.Track_Bar;
  Multiplier_Slider : Gui.Track_Bar;

  Shadows_Display    : Gui.Plain_Edit_Box;
  Midtones_Display   : Gui.Plain_Edit_Box;
  Highlights_Display : Gui.Plain_Edit_Box;
  Amount_Display     : Gui.Plain_Edit_Box;
  Multiplier_Display : Gui.Plain_Edit_Box;

  Control_Page   : Gui.Page;
  Action_Routine : Action_Handler;

  -- Slider Maximum Positions
  Shadows_Maximum    : constant := 1000; -- 0 .. 1000 correspond to values 0.00000 .. 0.01000
  Midtones_Maximum   : constant := 999;  -- 0 .. 999  correspond to values 0.00001 .. 0.01000
  Highlights_Maximum : constant := 9999; -- 0 .. 9999 correspond to values 0.00010 .. 1.00000
  Amount_Maximum     : constant := 100;  -- 0 .. 100  correspond to values 0.0 .. 1.00
  Multiplier_Maximum : constant := 499;  -- 0 .. 499  correspond to values 0.01 .. 5.00

  -- Slider Frequencies
  Shadows_Frequency    : constant := 100;  -- 0 .. 1000 correspond to values 0.00000 .. 0.01000
  Midtones_Frequency   : constant := 100;  -- 0 .. 999  correspond to values 0.00001 .. 0.01000
  Highlights_Frequency : constant := 1000; -- 0 .. 9999 correspond to values 0.00010 .. 1.00000
  Amount_Frequency     : constant := 25;   -- 0 .. 100  correspond to values 0.0 .. 1.00
  Multiplier_Frequency : constant := 50;   -- 0 .. 499  correspond to values 0.01 .. 5.00

  function Tone_At (Position : Natural) return Units.Tone is
    use type Units.Tone;
  begin
    return Units.Tone(Units.Tone'delta) * Position;
  end Tone_At;

  function Tone_Image_At (Position : Natural) return String is
  begin
    return Text.Trimmed (Tone_At (Position)'image);
  end Tone_Image_At;


  function Midtone_At (Position : Natural) return Units.Tone is
  begin
    return Tone_At (Position + 1);
  end Midtone_At;

  function Midtone_Image_At (Position : Natural) return String is
  begin
    return Text.Trimmed (Midtone_At (Position)'image);
  end Midtone_Image_At;


  function Highlights_At (Position : Natural) return Units.Tone is
  begin
    return Tone_At ((Position + 1) * 10);
  end Highlights_At;

  function Highlights_Image_At (Position : Natural) return String is
  begin
    return Text.Trimmed (Highlights_At (Position)'image);
  end Highlights_Image_At;


  function Amount_At (Position : Natural) return Units.Saturation_Amount is
    use type Units.Saturation_Amount;
  begin
    return Units.Saturation_Amount(Units.Saturation_Amount'delta) * Position;
  end Amount_At;

  function Amount_Image_At (Position : Natural) return String is
  begin
    return Text.Trimmed (Amount_At (Position)'image);
  end Amount_Image_At;


  function Multiplier_At (Position : Natural) return Units.Saturation_Multiplier is
    use type Units.Saturation_Multiplier;
  begin
    return Units.Saturation_Multiplier(Units.Saturation_Multiplier'delta) * (Position + 1);
  end Multiplier_At;

  function Multiplier_Image_At (Position : Natural) return String is
  begin
    return Text.Trimmed (Multiplier_At (Position)'image);
  end Multiplier_Image_At;


  function Tone_Position (Value : Units.Tone) return Natural is
  begin
    return Natural (Value / Units.Tone'delta);
  end Tone_Position;

  function Midtones_Position (Value : Units.Tone) return Natural is
  begin
    return Tone_Position (Value) - 1;
  end Midtones_Position;


  function Highlights_Position (Value : Units.Tone) return Natural is
  begin
    return Tone_Position (Value) / 10 - 1;
  end Highlights_Position;

  function Amount_Position (Value : Units.Saturation_Amount) return Natural is
  begin
    return Natural (Value / Units.Saturation_Amount'delta);
  end Amount_Position;


  function Multiplier_Position (Value : Units.Saturation_Multiplier) return Natural is
  begin
    return Natural (Value / Units.Saturation_Multiplier'delta) - 1;
  end Multiplier_Position;


  procedure View is
  begin
    Action_Routine.all (View);
  end View;


  procedure Display_Shadow is
  begin
    Gui.Set_Text (Shadows_Display, Tone_Image_At (Gui.Get_Position (Shadows_Slider)));
  end Display_Shadow;


  procedure Update_Tone_Limits is
    Shadow_Position    : constant Natural := Gui.Get_Position (Shadows_Slider);
    Highlight_Position : constant Natural := Gui.Get_Position (Highlights_Slider);
  begin
    -- Highlights_At (P) = Tone_At ((P + 1) * 10).
    -- The strict inequality Shadows < Highlights must always hold.
    Gui.Define_Limits (Shadows_Slider,
                       Minimum => 0,
                       Maximum => Natural'min (Shadows_Maximum, (Highlight_Position + 1) * 10 - 1));
    Gui.Define_Limits (Highlights_Slider,
                       Minimum => Shadow_Position / 10,
                       Maximum => Highlights_Maximum);
  end Update_Tone_Limits;


  procedure Shadows_Changed is
  begin
    Update_Tone_Limits;
    Display_Shadow;
    View;
  end Shadows_Changed;


  procedure Display_Midtones is
  begin
    Gui.Set_Text (Midtones_Display, Midtone_Image_At (Gui.Get_Position (Midtones_Slider)));
  end Display_Midtones;

  procedure Midtones_Changed is
  begin
    Display_Midtones;
    View;
  end Midtones_Changed;


  procedure Display_Highlights is
  begin
    Gui.Set_Text (Highlights_Display, Highlights_Image_At (Gui.Get_Position (Highlights_Slider)));
  end Display_Highlights;

  procedure Highlights_Changed is
  begin
    Update_Tone_Limits;
    Display_Highlights;
    View;
  end Highlights_Changed;


  procedure Set (Tones : Units.Tones) is
  begin
    -- Allow either slider to be positioned before reapplying coupled limits.
    Gui.Define_Limits (Shadows_Slider, Minimum => 0, Maximum => Shadows_Maximum);
    Gui.Define_Limits (Highlights_Slider, Minimum => 0, Maximum => Highlights_Maximum);
    Gui.Set_Position (Shadows_Slider, Tone_Position (Tones.Shadows));
    Display_Shadow;
    Gui.Set_Position (Midtones_Slider, Midtones_Position (Tones.Midtones));
    Display_Midtones;
    Gui.Set_Position (Highlights_Slider, Highlights_Position (Tones.Highlights));
    Update_Tone_Limits;
    Display_Highlights;
  end Set;


  function Tones return Units.Tones is
  begin
    return (Shadows    => Tone_At (Gui.Get_Position (Shadows_Slider)),
            Midtones   => Midtone_At (Gui.Get_Position (Midtones_Slider)),
            Highlights => Highlights_At (Gui.Get_Position (Highlights_Slider)));
  end Tones;


  procedure Display_Amount is
  begin
    Gui.Set_Text (Amount_Display, Amount_Image_At (Gui.Get_Position (Amount_Slider)));
  end Display_Amount;

  procedure Amount_Changed is
  begin
    Display_Amount;
    View;
  end Amount_Changed;


  procedure Display_Multiplier is
  begin
    Gui.Set_Text (Multiplier_Display, Multiplier_Image_At (Gui.Get_Position (Multiplier_Slider)));
  end Display_Multiplier;

  procedure Multiplier_Changed is
  begin
    Display_Multiplier;
    View;
  end Multiplier_Changed;


  procedure Set (Saturation : Units.Saturation) is
  begin
    Gui.Set_Position (Amount_Slider, Amount_Position (Saturation.Amount));
    Display_Amount;
    Gui.Set_Position (Multiplier_Slider, Multiplier_Position (Saturation.Multiplier));
    Display_Multiplier;
  end Set;


  function Saturation return Units.Saturation is
  begin
    return (Amount     => Amount_At (Gui.Get_Position (Amount_Slider)),
            Multiplier => Multiplier_At (Gui.Get_Position (Multiplier_Slider)));
  end Saturation;


  procedure Execute (The_Startup_Handler     : not null access procedure;
                     The_Action_Handler      : Action_Handler;
                     The_Termination_Handler : not null access procedure) is

    Windows_Width  : constant Natural := 440;
    Windows_Height : constant Natural := 800;

    procedure Create_Interface is

      procedure Define_Control_Page is

        procedure Create (Slider    : out Gui.Track_Bar;
                          Display   : out Gui.Plain_Edit_Box;
                          Name      : String;
                          Changed   : Gui.Action_Routine;
                          Positions : Positive;
                          Frequency : Positive) is

          Id         : constant String := [Name(Name'first)];
          Id_Size    : constant := 10;
          Name_Size  : constant := 100;
          Value_Size : constant := 85;

        begin
          Slider := Gui.Create (Control_Page, Id, Changed, Id_Size);
          Gui.Define_Range (Slider, Positions);
          Gui.Define_Tic_Frequency (Slider, Frequency);
          Gui.Set_Position (Slider, 0);
          Display := Gui.Create (Control_Page, Name,
                                 Initial_Text   => "",
                                 Is_Modifiable  => False,
                                 The_Size       => Value_Size,
                                 The_Title_Size => Name_Size);
        end Create;

      begin
        Control_Page := Gui.Add_Page (The_Title  => "Control",
                                      The_Style  => [Gui.Buttons_Fill_Horizontally => True,
                                                     Gui.Buttons_Fill_Vertically   => False]);

        Create (Shadows_Slider, Shadows_Display, "Shadows", Shadows_Changed'access,
                Positions => Shadows_Maximum,
                Frequency => Shadows_Frequency);
        Create (Midtones_Slider, Midtones_Display, "Midtones", Midtones_Changed'access,
                Positions => Midtones_Maximum,
                Frequency => Midtones_Frequency);
        Create (Highlights_Slider, Highlights_Display, "Highlights", Highlights_Changed'access,
                Positions => Highlights_Maximum,
                Frequency => Highlights_Frequency);
        Create (Amount_Slider, Amount_Display, "Amount", Amount_Changed'access,
                Positions => Amount_Maximum,
                Frequency => Amount_Frequency);
        Create (Multiplier_Slider, Multiplier_Display, "Multiplier", Multiplier_Changed'access,
                Positions => Multiplier_Maximum,
                Frequency => Multiplier_Frequency);

        Set (Tones => (Shadows    => 0.00016,
                       Midtones   => 0.00018,
                       Highlights => 1.0));

        Set (Saturation => (Amount     => 0.9,
                            Multiplier => 1.0));

      end Define_Control_Page;

    begin -- Create_Interface
      Define_Control_Page;
      The_Startup_Handler.all;
    exception
    when Item: others =>
      Log.Termination (Item);
    end Create_Interface;


    procedure Termination is
    begin
      Action_Routine.all (Close);
      Log.Write ("terminating");
      The_Termination_Handler.all;
    exception
    when others =>
      Log.Error ("Termination failed");
    end Termination;

  begin -- Execute
    Action_Routine := The_Action_Handler;
    Gui.Registered.Execute (The_Application_Name    => Application.Title,
                            The_Startup_Routine     => Create_Interface'access,
                            The_Termination_Routine => Termination'access,
                            Initial_Metrics         => (Width  => Windows_Width,
                                                        Height => Windows_Height,
                                                        others => <>));
    Log.Write ("terminated");
  end Execute;

end User;
