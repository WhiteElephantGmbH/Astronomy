-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

with Ada.Calendar;
with Ada.Command_Line;
with Ada_95.Rename;
with Directory;
with Display;
with File;
with Log;
with Text;

package body Work is

  Number_Of_Files : Natural := 0;


  procedure Process_File (Filename : String) is
  begin
    Number_Of_Files := Number_Of_Files + 1;
    Ada_95.Rename.Add (Filename);
  end Process_File;


  procedure Find_Bugs_In (The_Directory : String) is

  begin
    for The_File of File.Iterator_For (The_Directory) loop
      Process_File (The_File);
    end loop;
  exception
  when Occurrence: others =>
    Log.Write ("Control.Start", Occurrence);
  end Find_Bugs_In;


  function Rename (From : String) return Boolean is
  begin
    if not Directory.Exists (From) then
      return False; -- Directory not found
    end if;
    Number_Of_Files := 0;
    Find_Bugs_In (From);
    return True;
  end Rename;


  procedure Display_Help_Text is
  begin
    Display.Show ("Rename <Directory>");
    Display.Show ("  <Directory> = Mandatory parameter specifying the directory of the ada source files.");
    Display.Show ("This program renames ada source files to the standard file names.");
    Display.Show ("  Example: a-chahan.ads -> to Ada-Characters-Handling.ads");
  end Display_Help_Text;


  procedure Start is
    use type Ada.Calendar.Time;
    Nr_Of_Arguments : constant Natural := Ada.Command_Line.Argument_Count;
    The_Start_Time  : Ada.Calendar.Time;
  begin
    if Nr_Of_Arguments = 0 then
      Display_Help_Text;
    elsif Nr_Of_Arguments > 1 then
      Display.Show ("Incorrect number of parameters!");
      Display_Help_Text;
    else
      declare
        Folder : constant String := Ada.Command_Line.Argument(1);
      begin
        The_Start_Time := Ada.Calendar.Clock;
        Ada_95.Rename.Start;
        if Rename (Text.Trimmed (Folder)) then
          Ada_95.Rename.Finalize;
          Display.Show ("Processed" & Natural'image (Number_Of_Files) & " files in" &
                         Duration'image(Ada.Calendar.Clock - The_Start_Time) & " seconds");
        else
          Display.Show ("Directory " & Folder & " not found!");
        end if;
      end;
    end if;
  exception
  when Occurrence: others =>
    Log.Write ("Start", Occurrence);
  end Start;

end Work;
