-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Name;
with Ada_95.Source;
with Text;

package Ada_95.File is

  Ada_Exceptions_Name : constant String := "Ada.Exceptions";

  Ada_Tags_Name : constant String := "Ada.Tags";

  Standard_Name : constant String := "Standard";

  System_Name : constant String := "System";

  System_Machine_Code_Name : constant String := "System.Machine_Code";

  subtype Unit_Name is Name.List;
  subtype Unit_Name_Handle is Name.List_Handle;

  type Kind is (Unknown, Specification, Implementation);

  type Area is (Unknown, Workspace, Library);

  type Information (Length : Natural) is record
    Id        : Unit_Name(1..Length);
    Extension : Kind;
  end record;

  No_Information : constant Information := (Length    => 0,
                                            Id        => [others => null],
                                            Extension => Unknown);

  type Attributes is record
    Handle      : Source.Handle;
    Extension   : Kind        := Unknown;
    Location    : Area        := Unknown;
    Update_Time : Source.Time := Source.Undefined_Time;
  end record;

  function "=" (Left, Right : Attributes) return Boolean;

  No_Attributes : constant Attributes := (Handle      => null,
                                          Extension   => Unknown,
                                          Location    => Unknown,
                                          Update_Time => Source.Undefined_Time);

  procedure Define_Work_Path (List : Text.List);

  function Name_And_Extension_Of (Filename : String) return String;

  function Full_Name_Of (Name_And_Extension : String) return String;

  function Information_For (Filename : String) return Information;

  function Image_Of (Extension : Kind) return String;

  function Path_Of (Filename : String) return String;

  function Attributes_Of (Id        : Unit_Name;
                          Extension : Kind := Unknown) return Attributes;

  function Attributes_Of (Object : Source.Object'class) return Attributes;

  function Unit_Name_Of (Unit : Unit_Name) return String;

  function Subunit_Name_Of (Parent : Unit_Name;
                            Unit   : Unit_Name) return String;

  function Is_Standard (Unit : Unit_Name) return Boolean;

  function Is_Standard_Or_System (Unit : Name.Handle) return Boolean;

  function Normalized (Item : String) return String;

  function Normalized_Folder (Folder : String) return String;

  function Is_In_Project (Filename : String) return Boolean;

end Ada_95.File;
