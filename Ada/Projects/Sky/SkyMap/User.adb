-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Angle;
with Application;
with Error;
with Gui.Registered;
with Map;
with Parameter;
with Site;
with Strings;
with Time;
with Traces;

package body User is

  package Log is new Traces ("User");

  Application_Name : constant String := Application.Name;
  Version          : constant String := Application.Main_Version;

  Generator_Page  : Gui.Page;
  Generate_Button : Gui.Button;
  Longitude       : Gui.Plain_Edit_Box;
  Latitude        : Gui.Plain_Edit_Box;
  Date_Time       : Gui.Plain_Edit_Box;
  Paper_Format    : Gui.Plain_Combo_Box;

  Default_Latitude  : constant String := "+47" & Angle.Degree & "42'43.09""";
  Default_Longitude : constant String := "+008" & Angle.Degree & "38'07.94""";


  function Image_Of (Item : Angle.Value) return String is
  begin
    return Angle.Image_Of (Item, Decimals => 2, Show_Signed => True);
  end Image_Of;


  procedure Generate is

    function Value_Of (Box     : in out Gui.Plain_Edit_Box;
                       Default :        String) return Angle.Value is
    begin
      declare
        Value : constant Angle.Value := Angle.Value_Of (Gui.Contents_Of (Box));
      begin
        Gui.Set_Text (Box, Image_Of (Value));
        return Value;
      end;
    exception
    when others =>
      Gui.Set_Text (Box, Default);
      Error.Raise_With ("Eingabefehler -> " & Default & " gesetzt", Clear_Rest => True);
    end Value_Of;

    function Universal_Time return Time.Ut is
      Image      : constant String := Strings.Trimmed (Gui.Contents_Of (Date_Time));
      Local_Time : constant String := Time.Image_Of (Time.Universal);
      Time_Value : Time.Calendar_Value;
    begin
      if Image = "" then
        Time_Value := Time.Calendar_Now;
      else
        Time_Value := Time.Calendar_Value_Of (Image);
      end if;
      return Time.Universal_Of (Time_Value);
    exception
    when Time.Illegal =>
      Gui.Set_Text (Date_Time, Local_Time);
      Error.Raise_With ("Falsche Datumeingabe -> Lokalzeit gesetzt");
    when Time.Out_Of_Range =>
      Gui.Set_Text (Date_Time, Local_Time);
      Error.Raise_With ("Jahr nicht" & Time.Year'first'image & " .." & Time.Year'last'image & " -> Localzeit gesetzt");
    when Time.Ut_Range_Error =>
      Gui.Set_Text (Date_Time, Local_Time);
      Error.Raise_With ("Weltzeit nicht im erlaubten Bereich -> Lokalzeit gesetzt");
    end Universal_Time;

  begin -- Generate
    Set_Status ("");
    Parameter.Read;
    Generate_Button.Disable;
    Site.Define (Site.Data'(Latitude  => Value_Of (Latitude, Default_Latitude),
                            Longitude => Value_Of (Longitude, Default_Longitude),
                            Elevation => 0));
    Map.Draw (Ut     => Universal_Time,
              Format => Map.Paper_Format'value(Gui.Contents_Of (Paper_Format)));
    Generate_Button.Enable;
  exception
  when Error.Occurred =>
    Gui.Beep;
    Set_Status (Error.Message);
    Generate_Button.Enable;
  when Item: others =>
    Log.Termination (Item);
    Set_Status ("### Internal Error ###");
  end Generate;


  procedure Execute is

    Windows_Width  : constant Natural := 400;
    Windows_Height : constant Natural := 400;

    procedure Create_Interface is

      Separation    : constant := 12;
      Date_And_Time : constant String := "Datum und Zeit"; -- largest text

      Title_Size : constant Natural := Gui.Text_Size_Of (Date_And_Time) + Separation;
      Text_Size  : constant Natural := Gui.Text_Size_Of ("31.12.2399 23:59:59.9") + Separation;

    begin -- Create_Interface
      Generator_Page := Gui.Add_Page (The_Title  => "Main",
                                      The_Style  => [Gui.Buttons_Fill_Horizontally => True,
                                                     Gui.Buttons_Fill_Vertically   => False]);
      Generate_Button := Gui.Create (Generator_Page, "Generiere Sternkarte", Generate'access);
      Longitude := Gui.Create (Generator_Page, "Längengrad", "",
                               Is_Modifiable  => True,
                               The_Size       => Text_Size,
                               The_Title_Size => Title_Size);

      Latitude := Gui.Create (Generator_Page, "Breitengrad", "",
                              Is_Modifiable  => True,
                              The_Size       => Text_Size,
                              The_Title_Size => Title_Size);
      if Site.Is_Defined then
        Gui.Set_Text (Longitude, Image_Of (Site.Longitude));
        Gui.Set_Text (Latitude, Image_Of (Site.Latitude));
      else
        Gui.Set_Text (Longitude, Default_Longitude);
        Gui.Set_Text (Latitude, Default_Latitude);
      end if;
      Date_Time := Gui.Create (Generator_Page, Date_And_Time, "",
                               Is_Modifiable  => True,
                               The_Size       => Text_Size,
                               The_Title_Size => Title_Size);
      Paper_Format := Gui.Create (Generator_Page, "Papier Format",
                                  The_Size       => Text_Size,
                                  The_Title_Size => Title_Size);
      for Value in Map.Paper_Format'range loop
        Gui.Add_Text (Paper_Format, Value'image);
      end loop;
      Gui.Select_Text (Paper_Format, "A3");
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
