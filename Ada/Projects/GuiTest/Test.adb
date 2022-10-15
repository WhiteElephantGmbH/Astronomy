-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Gui;
with Gui.Registered;
with Application;
with Log;

package body Test is

  Application_Name    : constant String := Application.Name;
  Application_Version : constant String := "v" & Application.Version;

  First_Page  : Gui.Page;
  Button_One  : Gui.Button with Unreferenced;
  The_Display : Gui.Text_View;

  Text_Colour : constant Gui.Color := Gui.Red;

  procedure Do_One is
  begin
    Gui.Append_Line_To (The_Display, "Do_One", Text_Colour);
  end Do_One;


  procedure Create_User_Interface is
  begin
    First_Page := Gui.Add_Page ("Main");
    Button_One := Gui.Create (First_Page, "One", Do_One'access);
    The_Display := Gui.Create (First_Page, Word_Wrapping => True);
    Gui.Append_Line_To (The_Display, "This is a very long line to demonstrate text wrapping or the lack of it." &
                                     " So it has to go and on. Not quite forever but long enough for a test");
    Gui.Append_Line_To (The_Display, "Size of Gui.Information is" & Gui.Information'size'img);
    Gui.Append_Line_To (The_Display, "Name of Application is " & Gui.Name_Of_Application);
    Gui.Show;
  end Create_User_Interface;


  procedure Termination is
  begin
    Log.Write ("Termination");
  end Termination;


  procedure Work is
  begin
    Gui.Message_Box ("Hello World");
    Gui.Registered.Execute (Application_Name,
                            Application_Version,
                            Create_User_Interface'access,
                            Termination'access,
                            Initial_Metrics => (Width  => 650,
                                                Height => 400,
                                                others => <>));
  exception
  when Item: others =>
    Log.Write (Item);
  end Work;

end Test;
