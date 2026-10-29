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

  Processor_Directory : constant String := "D:\Pictures\Processor";
  Work_Directory      : constant String := Processor_Directory & "\Siril";
  LIGHT_Directory     : constant String := Work_Directory & "\LIGHT";

  Source : constant String := "\Image";


  The_Camera_Model : Units.Camera_Model := Units.Unknown_Camera;

  procedure Set (Camera_Model : Units.Camera_Model) is
  begin
    The_Camera_Model := Camera_Model;
  end Set;

  function Raw_File_Extension return String is ('.' & Units.Extension (The_Camera_Model));


  function Name_Of (Filename : String;
                    Index    : Number) return String is
    pragma Assert (Number'last < 10000);
    Leeding_Zeros : constant String := "000";
    Name_Index    : constant String := Leeding_Zeros & Text.Trimmed (Index'image);
  begin
    return Filename & '_' & Name_Index(Name_Index'last - Leeding_Zeros'length .. Name_Index'last) & Raw_File_Extension;
  end Name_Of;


  function Collector_Directory return String is (Processor_Directory & "\Collector");

  function Collector_Filename (Index : Number) return String is (Name_Of (Collector_Directory & Source, Index));

  function LIGHT_Filename (Index : Number) return String is (Name_Of (LIGHT_Directory & Source, Index));

  function First_LIGHT_Filename return String is (Collector_Filename (Number'first));


  function Darks_Work_Directory return String is
    Work_Directory_For_Darks : constant String := Collector_Directory & "/darks";
  begin
    Directory.Create (Work_Directory_For_Darks);
    return Work_Directory_For_Darks;
  end Darks_Work_Directory;


  function Cleared return Boolean is
  begin
    Directory.Delete (Collector_Directory);
    Directory.Create (Collector_Directory);
    Directory.Delete (Work_Directory);
    Directory.Create (LIGHT_Directory);
    return True;
  exception
  when others =>
    return False;
  end Cleared;


  procedure Move (From : Number;
                  To   : Number) is
  begin
    for The_Index in From .. To loop
      File.Rename (Old_Name => Collector_Filename (The_Index),
                   New_Name => LIGHT_Filename (The_Index));
    end loop;
  end Move;


  function Darks_Directory return String is (Processor_Directory & "\Darks");

  function Flats_Directory return String is (Processor_Directory & "\Flats");

  function Siril_Work_Directory return String is (Work_Directory);

  function Siril_LIGHT_Directory return String is (LIGHT_Directory);

end Image;
