-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Text;

package Files is

  Separator : constant Character := '\';

  Other_Separator : constant Character := '/';

  function Directory_Exists (The_Filename : String) return Boolean; -- case sensitive

  function Exists (The_Filename : String) return Boolean; -- case sensitive

  function Is_On_Network (Drive : Character) return Boolean;

  function Unc_Of (Name : String) return String;

  function Original_Name_Of (Filename : String) return String;

  function Normalized (Item : String) return String;

  function Normalized_Folder (Folder : String) return String;

  function Directory_Of (Full_Name : String) return String;

  function Folder_Of (Full_Name : String) return String;

  function Name_Of (Full_Name : String) return String;

  function Module_Of (Filename : String) return String;

  function Target_Of (Filename : String) return String;

  function Folder_Exists (Folder : String) return Boolean; -- case sensitive

  procedure Create_Folder (Folder : String);

  procedure Delete_Folder (Folder : String);

  function Found_Directory_In (Folder : String;
                               Area   : String) return String;

  function Is_Uptodate (Generated_Filename : String;
                        From_Filename      : String) return Boolean;

  function Project_Parts_Of (Name          :     String;
                             Area          :     String;
                             The_Directory : out Text.String) return Text.Vector;
end Files;
