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

with Directory;
with File;
with Text;

package body Image is

  Raw_File_Extension : constant String := ".cr2";

  Processor_Directory : constant String := "D:\Pictures\Processor";
  Collector_Directory : constant String := Processor_Directory & "\Collector";
  Work_Directory      : constant String := Processor_Directory & "\Siril";
  LIGHT_Directory     : constant String := Work_Directory & "\LIGHT";
  Lights_Directory    : constant String := Work_Directory & "\Lights";


  function Name_Of (Filename : String;
                    Index    : Number) return String is
    Index_Image : constant String := "00" & Text.Trimmed (Index'image);
  begin
    return Filename & '_' & Index_Image(Index_Image'last - 2 .. Index_Image'last) & Raw_File_Extension;
  end Name_Of;


  function Collector_Filename (Index : Number) return String is (Name_Of (Collector_Directory & "\Image", Index));

  function LIGHT_Filename (Index : Number) return String is (Name_Of (LIGHT_Directory & "\Image", Index));


  procedure Clear is
  begin
    Directory.Delete (Collector_Directory);
    Directory.Create (Collector_Directory);
    Directory.Delete (LIGHT_Directory);
    Directory.Create (LIGHT_Directory);
    Directory.Delete (Lights_Directory);
  end Clear;


  procedure Move (From : Number;
                  To   : Number) is
  begin
    for The_Index in From .. To loop
      File.Rename (Old_Name => Collector_Filename (The_Index),
                   New_Name => LIGHT_Filename (The_Index));
    end loop;
  end Move;

  function Siril_Work_Directory return String is (Work_Directory);

  function Siril_LIGHT_Directory return String is (LIGHT_Directory);

end Image;
