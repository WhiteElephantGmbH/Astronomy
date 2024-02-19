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

with File;
with Traces;
with Os.Process;
with Strings;

package body Camera is

  package Log is new Traces (Id);

  The_Id : Os.Process.Id;

  The_Command    : Strings.Element;
  The_Work_Area  : Strings.Element;
  The_Parameters : Strings.Element;
  Is_Defined     : Boolean := False;

  procedure Define (Command    : String;
                    Parameters : String;
                    Picture    : String) is
    Picture_Id : constant String := "%picture%";
    The_Index  : Natural;
    use all type Strings.Element;
  begin
    Log.Write ("Command: " & Command);
    The_Command := [Command];
    The_Work_Area := [File.Containing_Directory_Of (Command)];
    The_Parameters := [Parameters];
    The_Index := Index (The_Parameters, Pattern => Picture_Id);
    if The_Index >= Parameters'first then
      Replace_Slice (The_Parameters, The_Index, The_Index + Picture_Id'length - 1, Picture);
    end if;
    Log.Write ("Parameters: " & The_Parameters);
    Is_Defined := True;
  end Define;


  procedure Capture is
    use type Strings.Element;
  begin
    if Is_Defined then
      The_Id := Os.Process.Created (Executable     => +The_Command,
                                    Parameters     => +The_Parameters,
                                    Current_Folder => +The_Work_Area);
      Log.Write ("capture started");
    else
      Log.Warning ("parameters undefined");
    end if;
  exception
  when others =>
    Log.Error ("capture failed");
  end Capture;


  procedure Stop is
  begin
    Os.Process.Terminate_With (The_Id);
    Log.Write ("Capture stopped");
  end Stop;

end Camera;
