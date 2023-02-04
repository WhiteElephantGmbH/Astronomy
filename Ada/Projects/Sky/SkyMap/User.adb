-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
pragma Style_White_Elephant;

with Ada.Calendar;
with Angle;
with Application;
with Error;
with Gui.Registered;
with Map;
with Site;
with Star;
with Traces;

package body User is

  package Log is new Traces ("User");

  Application_Name : constant String := Application.Name;
  Version          : constant String := Application.Main_Version;

  Generator_Page  : Gui.Page;
  Generate_Button : Gui.Button;
  Longitude       : Gui.Plain_Edit_Box;
  Latitude        : Gui.Plain_Edit_Box;

  Default_Latitude  : constant String := "+47" & Angle.Degree & "40'0.06""";
  Default_Longitude : constant String := "+008" & Angle.Degree & "44'30.28""";


  procedure Generate is

    use type Star.Magnitude;

    function Value_Of (Box     : in out Gui.Plain_Edit_Box;
                       Default :        String) return Angle.Value is
    begin
      return Angle.Value_Of (Gui.Contents_Of (Box));
    exception
    when others =>
      Gui.Set_Text (Longitude, Default);
      Error.Raise_With ("Value Error -> Default " & Default & " set");
    end Value_Of;

  begin
    Set_Status ("");
    Generate_Button.Disable;
    Site.Define (Site.Data'(Latitude  => Value_Of (Latitude, Default_Latitude),
                            Longitude => Value_Of (Longitude, Default_Longitude),
                            Elevation => 0));

    Star.Read (Ada.Calendar.Clock);
    Map.Draw (Size          => 1000.0,
              Margin        => 10.0,
              Star_Min      => 1.0,
              Star_Max      => 5.0,
              Magnitude_Min => -1.0,
              Magnitude_Max => 6.0);
    Generate_Button.Enable;
  exception
  when Error.Occurred =>
    Set_Status (Error.Message);
    Generate_Button.Enable;
  end Generate;


  procedure Execute is

    Windows_Width  : constant Natural := 400;
    Windows_Height : constant Natural := 400;

    procedure Create_Interface is

      Separation     : constant := 6;
      Longitude_Text : constant String := "Longitude"; -- largest text

      Title_Size : constant Natural := Gui.Text_Size_Of (Longitude_Text) + Separation;
      Text_Size  : constant Natural := Gui.Text_Size_Of (Default_Longitude) + Separation;

    begin -- Create_Interface
      Generator_Page := Gui.Add_Page (The_Title  => "Main",
                                      The_Style  => [Gui.Buttons_Fill_Horizontally => True,
                                                     Gui.Buttons_Fill_Vertically   => False]);
      Generate_Button := Gui.Create (Generator_Page, "Generate", Generate'access);
      Longitude := Gui.Create (Generator_Page, Longitude_Text, "",
                               Is_Modifiable  => True,
                               The_Size       => Text_Size,
                               The_Title_Size => Title_Size);

      Latitude := Gui.Create (Generator_Page, "Latitude", "",
                              Is_Modifiable  => True,
                              The_Size       => Text_Size,
                              The_Title_Size => Title_Size);
      if Site.Is_Defined then
        Gui.Set_Text (Longitude, Angle.Image_Of (Site.Longitude, Decimals => 2, Show_Signed => True));
        Gui.Set_Text (Latitude, Angle.Image_Of (Site.Latitude, Decimals => 2, Show_Signed => True));
      else
        Gui.Set_Text (Longitude, Default_Longitude);
        Gui.Set_Text (Latitude, Default_Latitude);
      end if;
    exception
    when Item: others =>
      Log.Termination (Item);
    end Create_Interface;


    function Title return String is
    begin
      return Application_Name;
    end Title;


    procedure Termination is
    begin
      Log.Write ("Teminating");
    exception
    when others =>
      Log.Error ("Termination failed");
    end Termination;

  begin -- Execute
    Gui.Registered.Execute (The_Application_Name    => Title,
                            The_Version             => Version,
                            The_Startup_Routine     => Create_Interface'access,
                            The_Termination_Routine => Termination'access,
                            Initial_Metrics         => (Width  => Windows_Width,
                                                        Height => Windows_Height,
                                                        others => <>));
    Log.Write ("Teminated");
  end Execute;


  procedure Set_Status (Image : String) is
  begin
    Gui.Set_Status_Line (Image);
  end Set_Status;


  procedure Show_Error (Image : String) is
  begin
    Gui.Message_Box (Image);
    Gui.Beep;
  end Show_Error;

end User;
