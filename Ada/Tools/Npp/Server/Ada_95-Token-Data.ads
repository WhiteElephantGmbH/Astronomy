-- *********************************************************************************************************************
-- *                      (c) 2007 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                           *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.File;
with Container.List;
with Container.Tree;

package Ada_95.Token.Data is

  Unknown_Specification : exception;

  type Unit_Type is tagged;

  type Unit_Handle is access all Unit_Type'class;
  for Unit_Handle'storage_pool use Memory.Pool.all;

  ----------------------------------------------------------------------------------------------------------------------

  type Library_Element is record -- must be stored in global pool
    Id          : File.Unit_Name_Handle;
    Is_Standard : Boolean := False;
    Is_Marked   : Boolean := False;
    Is_Visited  : Boolean := False;
    Unit        : Unit_Handle;
  end record;

  function Comparison (Left, Right : Library_Element) return Result;

  function Id_Comparison (Id         : File.Unit_Name;
                          In_Element : Library_Element) return Result;

  function Element_With (Id : File.Unit_Name) return Library_Element;

  package Library_Tree is new Container.Tree (Element        => Library_Element,
                                              Key            => File.Unit_Name,
                                              Comparison     => Comparison,
                                              Key_Comparison => Id_Comparison,
                                              Element_With   => Element_With);

  ----------------------------------------------------------------------------------------------------------------------

  function "=" (Left, Right : Data_Handle) return Boolean;

  function Comparison (Left, Right : Data_Handle) return Result;

  function Id_Comparison (Id         : Name.Handle;
                          In_Element : Data_Handle) return Result;

  function Element_With (Id : Name.Handle) return Data_Handle;

  package Tree is new Container.Tree (Element        => Data_Handle,
                                      Key            => Name.Handle,
                                      Comparison     => Comparison,
                                      Key_Comparison => Id_Comparison,
                                      Element_With   => Element_With);


  function Is_Equal (Left, Right : Data_Handle) return Boolean;

  function Is_Less_Than (Left, Right : Data_Handle) return Boolean;

  package List is new Container.List (Element => Data_Handle,
                                      "="     => Is_Equal,
                                      "<"     => Is_Less_Than);

  function Full_Name_Of (Item : Data_Handle) return String;

  ----------------------------------------------------------------------------------------------------------------------

  type Generations;

  type Generation_Access is access Generations;

  type Generation is record
    Unit     : Unit_Handle;
    Children : Tree.Item;
  end record;

  type Generations is array (Positive range <>) of Generation;

  type Resources is record
    Element     : Library_Tree.Cursor;
    Attributes  : File.Attributes;
    Bucket      : Memory.Bucket;
    Tokens      : Token.Sequence;
    Imports     : Library_Tree.Item;
    Generations : Generation_Access;
  end record;

  type Resource_Handle is access Resources;
  for Resource_Handle'storage_pool use Memory.Pool.all;


  ----------------------------------------------------------------------------------------------------------------------
  -- Objects
  ----------------------------------------------------------------------------------------------------------------------

  type End_Identifier is new Data_Type with null record;

  overriding
  function Data_Kind_Of (Item : End_Identifier) return Data_Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Use_List_Handle is access List.Item;
  for Use_List_Handle'storage_pool use Memory.Pool.all;

  type Declaration_Type is abstract new Data_Type with record
    Is_Used : Boolean;
    Parent  : Unit_Handle;
  end record;

  type Declaration_Handle is access all Declaration_Type'class;
  for Declaration_Handle'storage_pool use Memory.Pool.all;


  type Unit_Type is abstract new Declaration_Type with record
    Used_Packages : Use_List_Handle;
  end record;

  function Resource (Item : Unit_Type) return Resource_Handle is (null);

  procedure Declare_Item (To   : in out Unit_Type;
                          Item :        Data_Handle) is null;

  procedure Declare_Item (To         : in out Unit_Type;
                          Item       :        Data_Handle;
                          The_Cursor : out    Tree.Cursor) is null;

  function Declaration_Of (Unused : Identifier_Handle;
                           From   : Unit_Type) return Data_Handle is (null);

  function Inner_Declaration_Of (Unused : Identifier_Handle;
                                 From   : Unit_Type) return Data_Handle is (null);

  procedure Set_Used (Item : Data_Handle) with Inline;

  procedure Set_Used (Item : List.Item);

  ----------------------------------------------------------------------------------------------------------------------

  type Unit_Body is tagged;

  type Unit_Body_Handle is access all Unit_Body'class;
  for Unit_Body_Handle'storage_pool use Memory.Pool.all;

  type Unit_Declaration is abstract new Unit_Type with record
    Implementation : Unit_Body_Handle;
  end record;

  type Unit_Declaration_Handle is access all Unit_Declaration'class;
  for Unit_Declaration_Handle'storage_pool use Memory.Pool.all;

  type Block_Type is abstract new Unit_Type with record
    Declarations : Tree.Item;
  end record;

  overriding
  procedure Declare_Item (To   : in out Block_Type;
                          Item :        Data_Handle);
  overriding
  procedure Declare_Item (To         : in out Block_Type;
                          Item       :        Data_Handle;
                          The_Cursor : out    Tree.Cursor);
  overriding
  function Declaration_Of (Id   : Identifier_Handle;
                           From : Block_Type) return Data_Handle;
  overriding
  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Block_Type) return Data_Handle;

  type Block_Handle is access all Block_Type'class;
  for Block_Handle'storage_pool use Memory.Pool.all;


  type Unit_Body is abstract new Block_Type with record
    Specification : Unit_Declaration_Handle;
  end record;


  type Block is new Block_Type with null record;

  overriding
  function Data_Kind_Of (Item : Block) return Data_Kind;


  type Private_Block is new Block_Type with null record;

  overriding
  function Data_Kind_Of (Item : Private_Block) return Data_Kind;

  overriding
  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Private_Block) return Data_Handle;


  type Formal_Block is new Block_Type with record
    Last_Position : Natural := 0;
  end record;

  overriding
  function Data_Kind_Of (Item : Formal_Block) return Data_Kind;

  type Formal_Block_Handle is access all Formal_Block'class;
  for Formal_Block_Handle'storage_pool use Memory.Pool.all;

  ----------------------------------------------------------------------------------------------------------------------

  type Array_Definition (Dimension : Natural) is record
    Index_Subtypes : List.Elements (1..Dimension);
    Component_Type : Data_Handle;
  end record;

  type Array_Definition_Handle is access Array_Definition;
  for Array_Definition_Handle'storage_pool use Memory.Pool.all;

  type Subprogram_Profile is record
    Parameters  : List.Elements_Access;
    Result_Type : Data_Handle;
  end record;

  function "=" (Left, Right : Subprogram_Profile) return Boolean with Inline;

  ----------------------------------------------------------------------------------------------------------------------

  type Package_Specification is new Unit_Declaration with record
    Declarations : Tree.Item;
    Private_Part : Block_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Package_Specification) return Data_Kind;

  overriding
  procedure Declare_Item (To   : in out Package_Specification;
                          Item :        Data_Handle);
  overriding
  procedure Declare_Item (To         : in out Package_Specification;
                          Item       :        Data_Handle;
                          The_Cursor : out    Tree.Cursor);
  overriding
  function Declaration_Of (Id   : Identifier_Handle;
                           From : Package_Specification) return Data_Handle;
  overriding
  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Package_Specification) return Data_Handle;

  type Package_Specification_Handle is access all Package_Specification'class;
  for Package_Specification_Handle'storage_pool use Memory.Pool.all;


  type Generic_Package_Declaration is new Package_Specification with record
    Generic_Parameters : Formal_Block_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Generic_Package_Declaration) return Data_Kind;

  overriding
  function Declaration_Of (Id   : Identifier_Handle;
                           From : Generic_Package_Declaration) return Data_Handle;
  overriding
  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Generic_Package_Declaration) return Data_Handle;

  type Generic_Package_Handle is access all Generic_Package_Declaration'class;
  for Generic_Package_Handle'storage_pool use Memory.Pool.all;


  type Package_Body is new Unit_Body with null record;

  overriding
  function Data_Kind_Of (Item : Package_Body) return Data_Kind;

  overriding
  function Declaration_Of (Id   : Identifier_Handle;
                           From : Package_Body) return Data_Handle;
  overriding
  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Package_Body) return Data_Handle;

  type Separate_Package_Body is new Package_Body with null record;

  ----------------------------------------------------------------------------------------------------------------------

  type Package_Instantiation is tagged;

  type Instantiation_Handle is access all Package_Instantiation'class;
  for Instantiation_Handle'storage_pool use Memory.Pool.all;

  type Package_Instantiation is new Unit_Type with record
    Generic_Package      : Unit_Handle;
    Parent_Instantiation : Instantiation_Handle;
    Actual_Part          : List.Elements_Access;
  end record;

  function Data_Kind_Of (Item : Package_Instantiation) return Data_Kind;


  type Package_Renaming is new Unit_Type with record
    Renamed_Item : Data_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Package_Renaming) return Data_Kind;

  overriding
  function Declaration_Of (Id   : Identifier_Handle;
                           From : Package_Renaming) return Data_Handle;
  overriding
  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Package_Renaming) return Data_Handle;

  type Package_Renaming_Handle is access all Package_Renaming'class;
  for Package_Renaming_Handle'storage_pool use Memory.Pool.all;


  type Generic_Package_Renaming is new Package_Renaming with null record;

  overriding
  function Data_Kind_Of (Item : Generic_Package_Renaming) return Data_Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Subprogram_Declaration is new Unit_Declaration with record
    Profile  : Subprogram_Profile;
    Overload : Declaration_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Subprogram_Declaration) return Data_Kind;

  overriding
  function Declaration_Of (Id   : Identifier_Handle;
                           From : Subprogram_Declaration) return Data_Handle;
  overriding
  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Subprogram_Declaration) return Data_Handle;

  type Subprogram_Declaration_Handle is access all Subprogram_Declaration'class;
  for Subprogram_Declaration_Handle'storage_pool use Memory.Pool.all;

  type Predefined_Equality_Operation is new Subprogram_Declaration with null record;


  type Generic_Subprogram_Declaration is new Subprogram_Declaration with record
    Generic_Parameters : Formal_Block_Handle;
  end record;

  function Data_Kind_Of (Item : Generic_Subprogram_Declaration) return Data_Kind;

  type Generic_Subprogram_Handle is access all Generic_Subprogram_Declaration'class;
  for Generic_Subprogram_Handle'storage_pool use Memory.Pool.all;


  type Used_Subprogram is new Unit_Type with record
    Profile  : Subprogram_Profile;
    Overload : Declaration_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Used_Subprogram) return Data_Kind;

  type Used_Subprogram_Handle is access all Used_Subprogram'class;
  for Used_Subprogram_Handle'storage_pool use Memory.Pool.all;


  type Used_Generic_Subprogram is new Used_Subprogram with record
    Generic_Parameters : Formal_Block_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Used_Generic_Subprogram) return Data_Kind;

  type Used_Generic_Subprogram_Handle is access all Used_Generic_Subprogram'class;
  for Used_Generic_Subprogram_Handle'storage_pool use Memory.Pool.all;


  type Subprogram_Body is new Unit_Body with record
    Profile  : Subprogram_Profile;
    Overload : Declaration_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Subprogram_Body) return Data_Kind;

  overriding
  function Declaration_Of (Id   : Identifier_Handle;
                           From : Subprogram_Body) return Data_Handle;
  overriding
  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Subprogram_Body) return Data_Handle;

  type Subprogram_Body_Handle is access all Subprogram_Body'class;
  for Subprogram_Body_Handle'storage_pool use Memory.Pool.all;

  type Separate_Subprogram_Body is new Subprogram_Body with null record;

  ----------------------------------------------------------------------------------------------------------------------

  type Subprogram_Renaming is new Subprogram_Body with record
    Renamed_Unit : Unit_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Subprogram_Renaming) return Data_Kind;

  type Subprogram_Renaming_Handle is access all Subprogram_Renaming'class;
  for Subprogram_Renaming_Handle'storage_pool use Memory.Pool.all;


  type Generic_Subprogram_Renaming is new Subprogram_Renaming with null record;

  overriding
  function Data_Kind_Of (Item : Generic_Subprogram_Renaming) return Data_Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Unknown_Library is new Unit_Type with record
    Resource : Resource_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Unknown_Library) return Data_Kind;

  overriding
  function Resource (Item : Unknown_Library) return Resource_Handle;

  ----------------------------------------------------------------------------------------------------------------------

  type Library_Package_Specification is new Package_Specification with record
    Resource   : Resource_Handle;
    Is_Private : Boolean;
  end record;

  overriding
  function Resource (Item : Library_Package_Specification) return Resource_Handle;

  type Library_Specification_Handle is access all Library_Package_Specification'class;
  for Library_Specification_Handle'storage_pool use Memory.Pool.all;


  type Generic_Library_Package_Declaration is new Generic_Package_Declaration with record
    Resource : Resource_Handle;
  end record;

  overriding
  function Resource (Item : Generic_Library_Package_Declaration) return Resource_Handle;


  type Library_Package_Body is new Package_Body with record
    Resource : Resource_Handle;
  end record;

  type Library_Body_Handle is access all Library_Package_Body'class;
  for Library_Body_Handle'storage_pool use Memory.Pool.all;

  overriding
  function Resource (Item : Library_Package_Body) return Resource_Handle;


  type Package_Subunit is new Package_Body with record
    Resource : Resource_Handle;
  end record;

  overriding
  function Resource (Item : Package_Subunit) return Resource_Handle;

  ----------------------------------------------------------------------------------------------------------------------

  type Library_Package_Instantiation is new Package_Instantiation with record
    Resource : Resource_Handle;
  end record;

  overriding
  function Resource (Item : Library_Package_Instantiation) return Resource_Handle;


  type Library_Package_Renaming is new Package_Renaming with record
    Resource : Resource_Handle;
  end record;

  overriding
  function Resource (Item : Library_Package_Renaming) return Resource_Handle;


  type Generic_Library_Package_Renaming is new Generic_Package_Renaming with record
    Resource : Resource_Handle;
  end record;

  overriding
  function Resource (Item : Generic_Library_Package_Renaming) return Resource_Handle;

  ----------------------------------------------------------------------------------------------------------------------

  type Library_Subprogram_Declaration is new Subprogram_Declaration with record
    Resource : Resource_Handle;
  end record;

  overriding
  function Resource (Item : Library_Subprogram_Declaration) return Resource_Handle;


  type Generic_Library_Subprogram_Declaration is new Generic_Subprogram_Declaration with record
    Resource : Resource_Handle;
  end record;

  overriding
  function Resource (Item : Generic_Library_Subprogram_Declaration) return Resource_Handle;


  type Library_Subprogram_Instantiation is new Library_Subprogram_Declaration with record
    null;
  end record;

  type Library_Subprogram_Instantiation_Handle is access all Library_Subprogram_Instantiation'class;
  for Library_Subprogram_Instantiation_Handle'storage_pool use Memory.Pool.all;


  type Library_Subprogram_Body is new Subprogram_Body with record
    Resource : Resource_Handle;
  end record;

  overriding
  function Resource (Item : Library_Subprogram_Body) return Resource_Handle;


  type Subprogram_Subunit is new Subprogram_Body with record
    Resource : Resource_Handle;
  end record;

  overriding
  function Resource (Item : Subprogram_Subunit) return Resource_Handle;

  ----------------------------------------------------------------------------------------------------------------------

  type Library_Subprogram_Renaming is new Subprogram_Renaming with record
    Resource : Resource_Handle;
  end record;

  overriding
  function Resource (Item : Library_Subprogram_Renaming) return Resource_Handle;


  type Generic_Library_Subprogram_Renaming is new Generic_Subprogram_Renaming with record
    Resource : Resource_Handle;
  end record;

  overriding
  function Resource (Item : Generic_Library_Subprogram_Renaming) return Resource_Handle;

  ----------------------------------------------------------------------------------------------------------------------

  type With_Declaration is tagged;

  type With_Handle is access all With_Declaration'class;
  for With_Handle'storage_pool use Memory.Pool.all;

  type With_Declaration is new Declaration_Type with record
    Unit       : Unit_Handle;
    Children   : Tree.Item;
    Copy_Of    : With_Handle;
    Is_Private : Boolean;
  end record;

  overriding
  function Data_Kind_Of (Item : With_Declaration) return Data_Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Active_Declaration is abstract new Unit_Declaration with record
    Declarations : Tree.Item;
  end record;

  overriding
  procedure Declare_Item (To   : in out Active_Declaration;
                          Item :        Data_Handle);
  overriding
  procedure Declare_Item (To         : in out Active_Declaration;
                          Item       :        Data_Handle;
                          The_Cursor : out    Tree.Cursor);
  overriding
  function Declaration_Of (Id   : Identifier_Handle;
                           From : Active_Declaration) return Data_Handle;
  overriding
  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Active_Declaration) return Data_Handle;

  type Active_Declaration_Handle is access all Active_Declaration'class;
  for Active_Declaration_Handle'storage_pool use Memory.Pool.all;


  type Active_Body is abstract new Unit_Body with record
    Type_Link : Data_Handle;
  end record;

  overriding
  function Declaration_Of (Id   : Identifier_Handle;
                           From : Active_Body) return Data_Handle;
  overriding
  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Active_Body) return Data_Handle;

  type Active_Body_Handle is access all Active_Body'class;
  for Active_Body_Handle'storage_pool use Memory.Pool.all;


  type Protected_Declaration is new Active_Declaration with null record;

  overriding
  function Data_Kind_Of (Item : Protected_Declaration) return Data_Kind;


  type Protected_Body is new Active_Body with null record;

  overriding
  function Data_Kind_Of (Item : Protected_Body) return Data_Kind;


  type Task_Declaration is new Active_Declaration with null record;

  overriding
  function Data_Kind_Of (Item : Task_Declaration) return Data_Kind;


  type Task_Body is new Active_Body with null record;

  overriding
  function Data_Kind_Of (Item : Task_Body) return Data_Kind;


  type Entry_Declaration is new Subprogram_Declaration with record
    Index_Type : Data_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Entry_Declaration) return Data_Kind;

  type Entry_Declaration_Handle is access all Entry_Declaration'class;
  for Entry_Declaration_Handle'storage_pool use Memory.Pool.all;


  type Entry_Body is new Subprogram_Body with record
    Index_Type : Data_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Entry_Body) return Data_Kind;

  type Entry_Body_Handle is access all Entry_Body'class;
  for Entry_Body_Handle'storage_pool use Memory.Pool.all;


  type Accept_Declaration is new Block_Type with record
    Profile : Subprogram_Profile;
  end record;

  overriding
  function Data_Kind_Of (Item : Accept_Declaration) return Data_Kind;

  overriding
  function Declaration_Of (Id   : Identifier_Handle;
                           From : Accept_Declaration) return Data_Handle;
  overriding
  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Accept_Declaration) return Data_Handle;

  type Accept_Handle is access all Accept_Declaration'class;
  for Accept_Handle'storage_pool use Memory.Pool.all;

  ----------------------------------------------------------------------------------------------------------------------

  type Predefined_Operator_Type is new Data_Type with null record; -- no location

  overriding
  function Data_Kind_Of (Item : Predefined_Operator_Type) return Data_Kind;

  type Predefined_Name_Id_Type is new Data_Type with null record; -- no location

  overriding
  function Data_Kind_Of (Item : Predefined_Name_Id_Type) return Data_Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Aspect_Element is record
    Mark       : Lexical.Aspect_Id;
    Definition : Identifier_Handle;
  end record;

  type Aspect_Specification is array (Positive range <>) of Aspect_Element;

  No_Aspects : constant Aspect_Specification(1..0) := [others => (others => <>)];

  ----------------------------------------------------------------------------------------------------------------------

  type Type_Declaration is abstract new Declaration_Type with record
    Parent_Type : Data_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Type_Declaration) return Data_Kind is abstract;

  type Type_Handle is access all Type_Declaration'class;
  for Type_Handle'storage_pool use Memory.Pool.all;

  type Any_Type_Declaration is new Type_Declaration with null record;

  overriding
  function Data_Kind_Of (Item : Any_Type_Declaration) return Data_Kind;

  type Scalar_Type is abstract new Type_Declaration with null record;

  type Root_Type is abstract new Scalar_Type with null record;

  type Root_Integer is new Root_Type with null record;

  overriding
  function Data_Kind_Of (Item : Root_Integer) return Data_Kind;

  type Root_Real is new Root_Type with null record;

  overriding
  function Data_Kind_Of (Item : Root_Real) return Data_Kind;

  type Root_Access is new Root_Type with null record;

  overriding
  function Data_Kind_Of (Item : Root_Access) return Data_Kind;

  type Sub_Type is new Type_Declaration with record
    Is_Class_Wide : Boolean;
  end record;

  overriding
  function Data_Kind_Of (Item : Sub_Type) return Data_Kind;

  type Sub_Type_Handle is access all Sub_Type'class;
  for Sub_Type_Handle'storage_pool use Memory.Pool.all;

  type Discrete_Type is new Scalar_Type with null record;

  overriding
  function Data_Kind_Of (Item : Discrete_Type) return Data_Kind;

  type Enumeration_Type is new Discrete_Type with record
    Values        : Tree.Item;
    Last_Position : Integer := 0;
  end record;

  overriding
  function Data_Kind_Of (Item : Enumeration_Type) return Data_Kind;

  type Enumeration_Handle is access all Enumeration_Type'class;
  for Enumeration_Handle'storage_pool use Memory.Pool.all;

  type Integer_Type is abstract new Discrete_Type with null record;

  overriding
  function Data_Kind_Of (Item : Integer_Type) return Data_Kind;

  type Signed_Integer_Type is new Integer_Type with null record;

  type Modular_Integer_Type is new Integer_Type with null record;

  type Real_Type is abstract new Scalar_Type with null record;

  overriding
  function Data_Kind_Of (Item : Real_Type) return Data_Kind;

  type Floating_Point_Type is new Real_Type with null record;

  type Fixed_Point_Type is new Real_Type with null record;

  type Interface_Type is new Type_Declaration with record
    Progenitors : List.Item;
    Methods     : List.Item;
  end record;

  overriding
  function Data_Kind_Of (Item : Interface_Type) return Data_Kind;

  type Interface_Handle is access all Interface_Type'class;
  for Interface_Handle'storage_pool use Memory.Pool.all;

  type Iterable_Aspects is record
    First       : Identifier_Handle;
    Next        : Identifier_Handle;
    Has_Element : Identifier_Handle;
    Element     : Identifier_Handle;
  end record;

  type Iterable_Aspect_Handle is access all Iterable_Aspects;
  for Iterable_Aspect_Handle'storage_pool use Memory.Pool.all;

  type Record_Type is new Type_Declaration with record
    Discriminants : List.Item;
    Components    : List.Item;
    Aspects       : Iterable_Aspect_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Record_Type) return Data_Kind;

  type Record_Handle is access all Record_Type'class;
  for Record_Handle'storage_pool use Memory.Pool.all;

  type Tagged_Record_Type is new Record_Type with record
    Methods    : List.Item;
    Interfaces : List.Item;
  end record;

  type Tagged_Record_Handle is access all Tagged_Record_Type'class;
  for Tagged_Record_Handle'storage_pool use Memory.Pool.all;

  type Abstract_Record_Type is new Record_Type with null record;

  type Abstract_Tagged_Record_Type is new Tagged_Record_Type with null record;

  type Array_Type is new Type_Declaration with record
    Definition : Array_Definition_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Array_Type) return Data_Kind;

  type Array_Handle is access all Array_Type'class;
  for Array_Handle'storage_pool use Memory.Pool.all;

  type Root_String is new Array_Type with null record;

  type Access_Type is new Type_Declaration with null record;

  overriding
  function Data_Kind_Of (Item : Access_Type) return Data_Kind;

  type Class_Access_Type is new Access_Type with null record;

  type Subprogram_Access_Type is new Access_Type with record
    Profile : Subprogram_Profile;
  end record;

  overriding
  function Data_Kind_Of (Item : Subprogram_Access_Type) return Data_Kind;

  type Subprogram_Access_Handle is access all Subprogram_Access_Type'class;
  for Subprogram_Access_Handle'storage_pool use Memory.Pool.all;

  type Protected_Subprogram_Access_Type is new Subprogram_Access_Type with null record;

  type Aggregate_Aspects is record
    Empty          : Identifier_Handle;
    Add_Named      : Identifier_Handle;
    Add_Unnamed    : Identifier_Handle;
    New_Indexed    : Identifier_Handle;
    Assign_Indexed : Identifier_Handle;
  end record;

  type Aggregate_Aspect_Handle is access all Aggregate_Aspects;
  for Aggregate_Aspect_Handle'storage_pool use Memory.Pool.all;

  type Derived_Type is new Type_Declaration with record
    Aggregate : Aggregate_Aspect_Handle;
  end record;

  type Derived_Type_Handle is access all Derived_Type'class;
  for Derived_Type_Handle'storage_pool use Memory.Pool.all;

  overriding
  function Data_Kind_Of (Item : Derived_Type) return Data_Kind;

  type Incomplete_Type is new Type_Declaration with null record;

  overriding
  function Data_Kind_Of (Item : Incomplete_Type) return Data_Kind;

  type Iterator_Aspects is record
    Constant_Indexing : Identifier_Handle;
    Variable_Indexing : Identifier_Handle;
    Default_Iterator  : Identifier_Handle;
    Element           : Identifier_Handle;
  end record;

  type Private_Aspects is record
    Iterable  : Iterable_Aspect_Handle;
    Iterator  : Iterator_Aspects;
    Aggregate : Aggregate_Aspect_Handle;
  end record;

  type Private_Aspect_Handle is access all Private_Aspects;
  for Private_Aspect_Handle'storage_pool use Memory.Pool.all;

  type Private_Type is new Type_Declaration with record
    Discriminants : List.Item;
    Aspects       : Private_Aspect_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Private_Type) return Data_Kind;

  type Private_Type_Handle is access all Private_Type'class;
  for Private_Type_Handle'storage_pool use Memory.Pool.all;

  type Tagged_Private_Type is new Private_Type with record
    Methods    : List.Item;
    Interfaces : List.Item;
  end record;

  type Tagged_Private_Handle is access all Tagged_Private_Type'class;
  for Tagged_Private_Handle'storage_pool use Memory.Pool.all;

  type Private_Extension_Type is new Tagged_Private_Type with null record;

  overriding
  function Data_Kind_Of (Item : Private_Extension_Type) return Data_Kind;

  type Abstract_Private_Extension_Type is new Private_Extension_Type with null record;

  type Active_Type is abstract new Type_Declaration with record
    Discriminants : List.Item;
    Object        : Unit_Handle;
  end record;

  type Active_Type_Handle is access all Active_Type'class;
  for Active_Type_Handle'storage_pool use Memory.Pool.all;

  type Task_Type is new Active_Type with null record;

  overriding
  function Data_Kind_Of (Item : Task_Type) return Data_Kind;

  type Protected_Type is new Active_Type with null record;

  overriding
  function Data_Kind_Of (Item : Protected_Type) return Data_Kind;

  type Synchronized_Interface_Type is new Interface_Type with record
    Active_Object : Active_Type_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Synchronized_Interface_Type) return Data_Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Exception_Object is new Declaration_Type with null record;

  overriding
  function Data_Kind_Of (Item : Exception_Object) return Data_Kind;


  type Enumeration_Value is new Declaration_Type with record
    Position : Integer;
    Profile  : Subprogram_Profile;
    Overload : Declaration_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Enumeration_Value) return Data_Kind;

  type Enumeration_Value_Handle is access all Enumeration_Value'class;
  for Enumeration_Value_Handle'storage_pool use Memory.Pool.all;


  type Label is new Declaration_Type with null record;

  overriding
  function Data_Kind_Of (Item : Label) return Data_Kind;


  type Data_Object is new Declaration_Type with record
    Object_Type   : Data_Handle;
    Is_Class_Wide : Boolean;
    Has_Default   : Boolean;
  end record;

  overriding
  function Data_Kind_Of (Item : Data_Object) return Data_Kind;

  type Object_Handle is access all Data_Object'class;
  for Object_Handle'storage_pool use Memory.Pool.all;

  type Parameter is new Data_Object with null record;

  type Incomplete_Object is tagged;

  type Incomplete_Object_Handle is access all Incomplete_Object'class;
  for Incomplete_Object_Handle'storage_pool use Memory.Pool.all;

  type Complete_Object is new Data_Object with record
    Incomplete_Data : Incomplete_Object_Handle;
  end record;

  type Complete_Object_Handle is access all Complete_Object'class;
  for Complete_Object_Handle'storage_pool use Memory.Pool.all;

  type Incomplete_Object is new Data_Object with record
    Complete_Data : Complete_Object_Handle;
  end record;

  ----------------------------------------------------------------------------------------------------------------------

  type Formal_Declaration is abstract new Declaration_Type with record
    Declaration : Data_Handle;
    Position    : Positive;
  end record;

  type Formal_Handle is access all Formal_Declaration'class;
  for Formal_Handle'storage_pool use Memory.Pool.all;


  type Formal_Object is new Formal_Declaration with record
    Has_Default : Boolean;
  end record;

  overriding
  function Data_Kind_Of (Item : Formal_Object) return Data_Kind;

  type Formal_Object_Handle is access all Formal_Object'class;
  for Formal_Object_Handle'storage_pool use Memory.Pool.all;


  type Formal_Type is new Formal_Declaration with null record;

  overriding
  function Data_Kind_Of (Item : Formal_Type) return Data_Kind;


  type Formal_Package is new Formal_Declaration with record
    Generic_Package : Unit_Handle;
    Actual_Part     : List.Elements_Access;
  end record;

  overriding
  function Data_Kind_Of (Item : Formal_Package) return Data_Kind;

  type Formal_Package_Handle is access all Formal_Package'class;
  for Formal_Package_Handle'storage_pool use Memory.Pool.all;


  type Formal_Subprogram is tagged;

  type Formal_Subprogram_Handle is access all Formal_Subprogram'class;
  for Formal_Subprogram_Handle'storage_pool use Memory.Pool.all;

  type Formal_Subprogram is new Formal_Declaration with record
    Default_Name : Identifier_Handle;
    Profile      : Subprogram_Profile;
    Overload     : Formal_Subprogram_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Formal_Subprogram) return Data_Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Instantiated_Item is new Data_Type with record
    Item          : Data_Handle;
    Instantiation : Instantiation_Handle;
  end record;

  overriding
  function Data_Kind_Of (Item : Instantiated_Item) return Data_Kind;

  type Item_Instantiation is access all Instantiated_Item'class;
  for Item_Instantiation'storage_pool use Memory.Pool.all;

  type Instantiated_Type is new Instantiated_Item with null record;

  ----------------------------------------------------------------------------------------------------------------------

  type Context is record
    Scope    : Unit_Handle;
    Sub_Type : Data_Handle;
  end record;


  ----------------------------------------------------------------------------------------------------------------------
  -- Constructors
  ----------------------------------------------------------------------------------------------------------------------

  procedure Initialize;

  procedure Add_Standard (Unit : Unit_Handle);

  procedure Add_Unit (Self : Unit_Handle);

  procedure Import (Id         : Identifiers;
                    Self       : Unit_Handle;
                    Is_Private : Boolean);

  procedure Use_Package (Item  : Data_Handle;
                         Scope : Unit_Handle);

  procedure Use_Type (Item    : Data_Handle;
                      Scope   : Unit_Handle;
                      Use_All : Boolean);

  ----------------------------------------------------------------------------------------------------------------------

  function Used (Profile : Subprogram_Profile) return Subprogram_Profile;

  procedure Set_Profile_Used (Unit : Unit_Handle);

  ----------------------------------------------------------------------------------------------------------------------

  function New_Unknown_Library (Resource : Resource_Handle) return Unit_Handle;

  ----------------------------------------------------------------------------------------------------------------------

  function New_Block (Id     : Identifier_Handle;
                      Parent : Unit_Handle) return Unit_Handle;

  function New_Exception_Block (Id     : Identifier_Handle;
                                Parent : Unit_Handle) return Unit_Handle;

  function New_Private_Part (Visible_Part : Unit_Handle) return Unit_Handle;


  function New_Formal_Part (Parent : Unit_Handle) return Formal_Block_Handle;

  ----------------------------------------------------------------------------------------------------------------------

  function New_Generic_Library_Package_Declaration (Id       : Identifier_List;
                                                    Resource : Resource_Handle) return Unit_Handle;

  procedure Add_Generic_Library_Package_Declaration (Self               : Unit_Handle;
                                                     Generic_Parameters : Formal_Block_Handle);


  function New_Generic_Library_Subprogram_Declaration (Id       : Identifier_List;
                                                       Resource : Resource_Handle) return Unit_Handle;

  procedure Add_Generic_Library_Subprogram_Declaration (Self               : Unit_Handle;
                                                        Generic_Parameters : Formal_Block_Handle;
                                                        Profile            : Subprogram_Profile);


  function New_Library_Package_Specification (Id         : Identifier_List;
                                              Resource   : Resource_Handle;
                                              Is_Private : Boolean) return Unit_Handle;

  procedure Add_Library_Package_Specification (Self : Unit_Handle);


  function New_Library_Subprogram_Declaration (Id       : Identifier_List;
                                               Resource : Resource_Handle) return Unit_Handle;

  procedure Add_Library_Subprogram_Declaration (Self    : Unit_Handle;
                                                Profile : Subprogram_Profile);


  function New_Library_Package_Body (Id       : Identifier_List;
                                     Resource : Resource_Handle) return Unit_Handle;

  function New_Package_Subunit (Id       : Identifier_List;
                                Resource : Resource_Handle) return Unit_Handle;

  procedure Add_Library_Package_Body (Self : Unit_Handle);


  function New_Library_Subprogram_Body (Id       : Identifier_List;
                                        Resource : Resource_Handle) return Unit_Handle;

  function New_Subprogram_Subunit (Id       : Identifier_List;
                                   Resource : Resource_Handle) return Unit_Handle;

  procedure Add_Library_Subprogram_Body (Self    : Unit_Handle;
                                         Profile : Subprogram_Profile);

  ----------------------------------------------------------------------------------------------------------------------

  function New_Library_Package_Renaming (Id       : Identifier_List;
                                         Resource : Resource_Handle) return Unit_Handle;

  procedure Add_Library_Package_Renaming (Self            : Unit_Handle;
                                          Renamed_Package : Data_Handle);


  function New_Library_Subprogram_Renaming (Id       : Identifier_List;
                                            Resource : Resource_Handle) return Unit_Handle;

  procedure Add_Library_Subprogram_Renaming (Self               : Unit_Handle;
                                             Renamed_Subprogram : Unit_Handle);


  function New_Library_Package_Instantiation (Id       : Identifier_List;
                                              Resource : Resource_Handle) return Unit_Handle;

  procedure Add_Library_Package_Instantiation (Self                 : Unit_Handle;
                                               Generic_Package      : Unit_Handle;
                                               Parent_Instantiation : Instantiation_Handle;
                                               Actual_Part          : List.Elements);

  function New_Library_Subprogram_Instantiation (Id       : Identifier_List;
                                                 Resource : Resource_Handle) return Unit_Handle;

  procedure Add_Library_Subprogram_Instantiation (Self    : Unit_Handle;
                                                  Profile : Subprogram_Profile);

  function New_Generic_Library_Package_Renaming (Id       : Identifier_List;
                                                 Resource : Resource_Handle) return Unit_Handle;

  procedure Add_Generic_Library_Package_Renaming (Self            : Unit_Handle;
                                                  Renamed_Package : Unit_Handle);


  function New_Generic_Library_Subprogram_Renaming (Id       : Identifier_List;
                                                    Resource : Resource_Handle) return Unit_Handle;

  procedure Add_Generic_Library_Subprogram_Renaming (Self               : Unit_Handle;
                                                     Renamed_Subprogram : Unit_Handle);

  ----------------------------------------------------------------------------------------------------------------------

  function New_Generic_Package_Declaration (Id                 : Identifier_Handle;
                                            Parent             : Unit_Handle;
                                            Generic_Parameters : Formal_Block_Handle) return Unit_Handle;

  procedure New_Generic_Subprogram_Declaration (Id                 : Identifier_Handle;
                                                Parent             : Unit_Handle;
                                                Generic_Parameters : Formal_Block_Handle;
                                                Profile            : Subprogram_Profile);

  function New_Package_Specification (Id     : Identifier_Handle;
                                      Parent : Unit_Handle) return Unit_Handle;

  function New_Function_Expression_Declaration (Id      : Identifier_Handle;
                                                Parent  : Unit_Handle;
                                                Profile : Subprogram_Profile) return Unit_Handle;

  function New_Subprogram_Declaration (Id      : Identifier_Handle;
                                       Parent  : Unit_Handle;
                                       Profile : Subprogram_Profile) return Unit_Handle;

  function New_Package_Body (Id          : Identifier_Handle;
                             Parent      : Unit_Handle;
                             Is_Separate : Boolean := False) return Unit_Handle;

  function New_Subprogram_Body (Id          : Identifier_Handle;
                                Parent      : Unit_Handle;
                                Profile     : Subprogram_Profile;
                                Is_Separate : Boolean := False) return Unit_Handle;

  ----------------------------------------------------------------------------------------------------------------------

  procedure New_Package_Renaming (Id              : Identifier_Handle;
                                  Parent          : Unit_Handle;
                                  Renamed_Package : Data_Handle);

  procedure New_Subprogram_Renaming (Id                 : Identifier_Handle;
                                     Parent             : Unit_Handle;
                                     Profile            : Subprogram_Profile;
                                     Renamed_Subprogram : Unit_Handle;
                                     Renamed_Id         : Identifier_Handle);

  procedure New_Generic_Subprogram_Renaming (Id                 : Identifier_Handle;
                                             Parent             : Unit_Handle;
                                             Renamed_Subprogram : Unit_Handle);

  ----------------------------------------------------------------------------------------------------------------------

  procedure New_Package_Instantiation (Id                   : Identifier_Handle;
                                       Parent               : Unit_Handle;
                                       Generic_Package      : Unit_Handle;
                                       Parent_Instantiation : Instantiation_Handle;
                                       Actual_Part          : List.Elements);

  ----------------------------------------------------------------------------------------------------------------------

  function New_Protected_Declaration (Id     : Identifier_Handle;
                                      Parent : Unit_Handle) return Unit_Handle;

  function New_Protected_Body (Id     : Identifier_Handle;
                               Parent : Unit_Handle) return Unit_Handle;

  function New_Task_Declaration (Id     : Identifier_Handle;
                                 Parent : Unit_Handle) return Unit_Handle;

  function New_Task_Body (Id     : Identifier_Handle;
                          Parent : Unit_Handle) return Unit_Handle;

  procedure New_Entry_Declaration (Id         : Identifier_Handle;
                                   Parent     : Unit_Handle;
                                   Profile    : Subprogram_Profile;
                                   Index_Type : Data_Handle);

  function New_Entry_Body (Id         : Identifier_Handle;
                           Parent     : Unit_Handle;
                           Profile    : Subprogram_Profile;
                           Index_Type : Data_Handle) return Unit_Handle;

  function New_Accept_Declaration (Id      : Identifier_Handle;
                                   Parent  : Unit_Handle;
                                   Profile : Subprogram_Profile) return Unit_Handle;

  ----------------------------------------------------------------------------------------------------------------------

  procedure New_Subtype (Id            : Identifier_Handle;
                         Parent        : Unit_Handle;
                         From_Type     : Data_Handle;
                         Is_Class_Wide : Boolean);

  function New_Enumeration_Type (Id     : Identifier_Handle;
                                 Parent : Unit_Handle) return Data_Handle;

  function New_Signed_Integer_Type (Id     : Identifier_Handle;
                                    Parent : Unit_Handle) return Data_Handle;

  function New_Modular_Integer_Type (Id     : Identifier_Handle;
                                     Parent : Unit_Handle) return Data_Handle;

  function New_Floating_Point_Type (Id     : Identifier_Handle;
                                    Parent : Unit_Handle) return Data_Handle;

  function New_Fixed_Point_Type (Id     : Identifier_Handle;
                                 Parent : Unit_Handle) return Data_Handle;

  function New_Record_Type (Id            : Identifier_Handle;
                            Is_Abstract   : Boolean;
                            Is_Tagged     : Boolean;
                            Parent        : Unit_Handle;
                            Ancestor      : Data_Handle;
                            Discriminants : List.Item;
                            Interfaces    : List.Item) return Data_Handle;

  function New_Array_Type (Id         : Identifier_Handle;
                           Parent     : Unit_Handle;
                           Definition : Array_Definition_Handle) return Data_Handle;

  function New_Subprogram_Access_Type (Id      : Identifier_Handle;
                                       Parent  : Unit_Handle;
                                       Profile : Subprogram_Profile) return Data_Handle;

  function New_Protected_Subprogram_Access_Type (Id      : Identifier_Handle;
                                                 Parent  : Unit_Handle;
                                                 Profile : Subprogram_Profile) return Data_Handle;

  function New_Access_Type (Id      : Identifier_Handle;
                            Parent  : Unit_Handle;
                            To_Type : Data_Handle) return Data_Handle;

  function New_Class_Access_Type (Id      : Identifier_Handle;
                                  Parent  : Unit_Handle;
                                  To_Type : Data_Handle) return Data_Handle;

  function New_Derived_Type (Id        : Identifier_Handle;
                             Parent    : Unit_Handle;
                             From_Type : Data_Handle) return Data_Handle;

  function New_Private_Extension_Type (Id            : Identifier_Handle;
                                       Is_Abstract   : Boolean;
                                       Parent        : Unit_Handle;
                                       From_Type     : Data_Handle;
                                       Discriminants : List.Item;
                                       Interfaces    : List.Item) return Data_Handle;

  function New_Incomplete_Type (Id     : Identifier_Handle;
                                Parent : Unit_Handle) return Data_Handle;

  function New_Private_Type (Id            : Identifier_Handle;
                             Is_Tagged     : Boolean;
                             Parent        : Unit_Handle;
                             Discriminants : List.Item) return Data_Handle;

  procedure Add_Iterator_Aspects (To      : Data_Handle;
                                  Aspects : Aspect_Specification);

  function New_Protected_Type (Id            : Identifier_Handle;
                               Parent        : Unit_Handle;
                               Discriminants : List.Item) return Unit_Handle;

  function New_Task_Type (Id            : Identifier_Handle;
                          Parent        : Unit_Handle;
                          Discriminants : List.Item) return Unit_Handle;

  function New_Interface_Type (Id         : Identifier_Handle;
                               Parent     : Unit_Handle;
                               Interfaces : List.Item) return Data_Handle;

  ----------------------------------------------------------------------------------------------------------------------

  function New_Instantiation (Item          : Data_Handle;
                              Instantiation : Instantiation_Handle) return Data_Handle;

  ----------------------------------------------------------------------------------------------------------------------

  procedure New_End_Identifier (Id   : Identifier_Handle;
                                Self : Data_Handle);

  ----------------------------------------------------------------------------------------------------------------------

  procedure New_Enumeration_Value (Id           : Identifier_Handle;
                                   Parent       : Unit_Handle;
                                   Subtype_Mark : Data_Handle);

  procedure New_Exceptions (Names  : Identifiers;
                            Parent : Unit_Handle);

  procedure New_Label (Id     : Identifier_Handle;
                       Parent : Unit_Handle);

  procedure New_Object (Id            : Identifier_Handle;
                        Subtype_Mark  : Data_Handle;
                        Is_Class_Wide : Boolean;
                        Has_Default   : Boolean;
                        Parent        : Unit_Handle);

  procedure New_Objects (Names         : Identifiers;
                         Subtype_Mark  : Data_Handle;
                         Is_Class_Wide : Boolean;
                         Has_Default   : Boolean;
                         Parent        : Unit_Handle);

  procedure New_Constants (Names         : Identifiers;
                           Subtype_Mark  : Data_Handle;
                           Is_Class_Wide : Boolean;
                           Has_Default   : Boolean;
                           Parent        : Unit_Handle);

  ----------------------------------------------------------------------------------------------------------------------

  function New_Component_List (Component_Names : Identifiers;
                               Subtype_Mark    : Data_Handle;
                               Is_Class_Wide   : Boolean;
                               Has_Default     : Boolean;
                               Parent          : Unit_Handle) return List.Item;

  function New_Discriminant_List (Component_Names : Identifiers;
                                  Subtype_Mark    : Data_Handle;
                                  Is_Class_Wide   : Boolean;
                                  Has_Default     : Boolean;
                                  Parent          : Unit_Handle) return List.Item renames New_Component_List;

  function New_Parameter_List (Parameter_Names : Identifiers;
                               Subtype_Mark    : Data_Handle;
                               Is_Class_Wide   : Boolean;
                               Has_Default     : Boolean;
                               Parent          : Unit_Handle) return List.Item;

  ----------------------------------------------------------------------------------------------------------------------

  procedure New_Formal_Decimal_Fixed_Point_Type (Id         : Identifier_Handle;
                                                 Parameters : Formal_Block_Handle);

  procedure New_Formal_Discrete_Type (Id         : Identifier_Handle;
                                      Parameters : Formal_Block_Handle);

  procedure New_Formal_Floating_Point_Type (Id         : Identifier_Handle;
                                            Parameters : Formal_Block_Handle);

  procedure New_Formal_Modular_Type (Id         : Identifier_Handle;
                                     Parameters : Formal_Block_Handle);

  procedure New_Formal_Ordinary_Fixed_Point_Type (Id         : Identifier_Handle;
                                                  Parameters : Formal_Block_Handle);

  procedure New_Formal_Signed_Integer_Type (Id         : Identifier_Handle;
                                            Parameters : Formal_Block_Handle);

  procedure New_Formal_Private_Type (Id         : Identifier_Handle;
                                     Parameters : Formal_Block_Handle);

  procedure New_Formal_Incomplete_Type (Id         : Identifier_Handle;
                                        Parameters : Formal_Block_Handle);

  procedure New_Formal_Derived_Type (Id         : Identifier_Handle;
                                     From_Type  : Data_Handle;
                                     Parameters : Formal_Block_Handle);

  procedure New_Formal_Abstract_Private_Extension_Type (Id         : Identifier_Handle;
                                                        From_Type  : Data_Handle;
                                                        Parameters : Formal_Block_Handle);

  procedure New_Formal_Private_Extension_Type (Id         : Identifier_Handle;
                                               From_Type  : Data_Handle;
                                               Parameters : Formal_Block_Handle);

  procedure New_Formal_Limited_Private_Type (Id         : Identifier_Handle;
                                             Parameters : Formal_Block_Handle);

  procedure New_Formal_Access_Type (Id         : Identifier_Handle;
                                    To_Type    : Data_Handle;
                                    Parameters : Formal_Block_Handle);

  procedure New_Formal_Array_Type (Id         : Identifier_Handle;
                                   Definition : Array_Definition_Handle;
                                   Parameters : Formal_Block_Handle);

  procedure New_Formal_Objects (Formal_Names : Identifiers;
                                Subtype_Mark : Data_Handle;
                                Has_Default  : Boolean;
                                Parameters   : Formal_Block_Handle);

  procedure New_Formal_Subprogram_Declaration (Id           : Identifier_Handle;
                                               Default_Name : Identifier_Handle;
                                               Profile      : Subprogram_Profile;
                                               Parameters   : Formal_Block_Handle);

  procedure New_Formal_Package_Declaration (Id              : Identifier_Handle;
                                            Generic_Package : Unit_Handle;
                                            Actual_Part     : List.Elements_Access;
                                            Parameters      : Formal_Block_Handle);

  ----------------------------------------------------------------------------------------------------------------------

  function New_Profile return Subprogram_Profile;

  function New_Profile (Parameters : List.Item) return Subprogram_Profile;

  function New_Array_Definition (Index_Subtypes : List.Item;
                                 Component_Type : Data_Handle) return Array_Definition_Handle;


  ----------------------------------------------------------------------------------------------------------------------
  -- Overload association
  ----------------------------------------------------------------------------------------------------------------------

  function Is_Subprogram (Item : Data_Handle) return Boolean;

  procedure Chain_In (Item : Declaration_Handle);

  procedure Overload (Item : Declaration_Handle;
                      To   : Declaration_Handle) with Inline;

  function Is_Overloaded (Item : Declaration_Handle) return Boolean with Inline;

  function Overload_Of (Item : Declaration_Handle) return Declaration_Handle with Inline;

  function Profile_Of (Item : Declaration_Handle) return Subprogram_Profile with Inline;

  function Parameter_Index_Of (Profile  : Subprogram_Profile;
                               Selector : Identifier_Handle) return Natural;
  Parameter_Not_Found : constant := 0;

  function Has_Default_Parameter (Profile      : Subprogram_Profile;
                                  The_Position : Positive) return Boolean;

  function Parameter_Object_Of (Profile      : Subprogram_Profile;
                                The_Position : Positive) return Object_Handle;

  function Actual_Type_Of (Formal_Parameter : Formal_Handle;
                           Instantiation    : Instantiation_Handle) return Data_Handle;

  function Matches (Left, Right   : Data_Handle;
                    Instantiation : Instantiation_Handle) return Boolean;

  function Matches (Left, Right   : Subprogram_Profile;
                    Instantiation : Instantiation_Handle) return Boolean;

  function Matches (The_Parameter     : Data_Handle;
                    Profile_Parameter : Object_Handle;
                    Instantiation     : Instantiation_Handle) return Boolean;

  procedure Associate_Entry (Id         : Identifier_Handle;
                             With_Entry : Unit_Handle;
                             Profile    : Subprogram_Profile);

  function Class_Of (Profile : Subprogram_Profile) return Type_Handle;


  ----------------------------------------------------------------------------------------------------------------------
  -- Selectors
  ----------------------------------------------------------------------------------------------------------------------

  type Declaration_Kind is (Is_Type, Is_Known, Is_Unused_Declaration, Is_Unused_Type_Declaration, Is_Unknown);

  function Is_Unused (Id : Handle) return Boolean;

  function Kind_Of (Id : Handle) return Declaration_Kind;

  function Root_Type_Of (Item : Data_Handle) return Data_Handle with Inline;

  function Base_Type_Of (Item : Data_Handle) return Data_Handle with Inline;

  function Completion_Of (The_Type : Type_Handle) return Data_Handle with Inline;

  function Type_Of (Item : Data_Handle) return Data_Handle with Inline;

  function Object_Type_Of (Item : Data_Handle) return Data_Handle with Inline;

  function Range_Type_Of (Item : Data_Handle) return Data_Handle with Inline;

  function Iterable_Type_Of (Item : Data_Handle) return Data_Handle;

  function Parent_Type_Of (Item              :     Data_Handle;
                           The_Instantiation : out Instantiation_Handle) return Type_Handle with Inline;

  function Parent_Type_Of (Item : Data_Handle) return Type_Handle;

  function Is_Tagged_Type (Item : Data_Handle) return Boolean with Inline;

  function Is_Subtype (Item : Data_Handle) return Boolean with Inline;

  function Is_In (Parent_Class : Data_Handle;
                  Item         : Data_Handle) return Boolean;

  function Component_Choice_Of (Item     : Identifier_Handle;
                                The_Type : Record_Handle) return Data_Handle;

  function Component_Choice_Of (The_Position : Positive;
                                The_Type     : Record_Handle) return Data_Handle;

  function Formal_Position_Of (Item  : Identifier_Handle;
                               Scope : Formal_Block_Handle) return Natural;

  function Formal_Type_At (The_Position : Positive;
                           Scope        : Formal_Block_Handle) return Data_Handle;

  function Generic_Parameters_Of (Unit : Unit_Handle) return Formal_Block_Handle;

  procedure Get_Inner_Generic_Parameters_Of (The_Unit       : in out Unit_Handle;
                                             The_Parameters :    out Formal_Block_Handle);

  function Actual_Declaration_Of (Formal        : Formal_Handle;
                                  Instantiation : Instantiation_Handle) return Data_Handle;

  function Index_Subtype_Of (Definition    : Array_Definition_Handle;
                             Index         : Positive;
                             Instantiation : Instantiation_Handle) return Data_Handle with Inline;

  function Actual_Profile_For (Generic_Unit : Unit_Handle;
                               Subprogram   : Identifier_Handle;
                               Actual_Part  : List.Elements) return Subprogram_Profile;

  function Discriminant_Of (Item     : Identifier_Handle;
                            The_Type : Data_Handle) return Data_Handle;

  function Discriminant_Type_Of (The_Position : Positive;
                                 The_Type     : Data_Handle) return Data_Handle;

  function Has_Discriminats (The_Type : Data_Handle) return Boolean;

  function Entry_Of (Item  : Identifier_Handle;
                     Scope : Unit_Handle) return Unit_Handle;

  function Index_Type_Of (Item : Data_Handle) return Data_Handle;

  function Declaration_From (Scope         : Unit_Handle;
                             Item          : Identifier_Handle;
                             Suppress_Used : Boolean := False) return Data_Handle with Inline;

  function Inner_Declaration_From (Scope         : Unit_Handle;
                                   Item          : Identifier_Handle;
                                   Suppress_Used : Boolean := False) return Data_Handle with Inline;

  function Declaration_From (The_Generations : Generation_Access;
                             Index           : Positive;
                             Item            : Identifier_Handle) return With_Handle;

  function Declaration_From (The_Generations : Generation_Access;
                             Unit            : Unit_Handle;
                             Item            : Identifier_Handle) return With_Handle;

  function Declaration_From (The_Import : With_Handle;
                             Item       : Identifier_Handle) return With_Handle;

  function Declaration_From (Used_Packages     : Use_List_Handle;
                             The_Generations   : Generation_Access;
                             Item              : Identifier_Handle;
                             The_Instantiation : out Instantiation_Handle) return Data_Handle;

  function Is_Library (Item : Unit_Handle) return Boolean;

  function Library_Id (Item : Unit_Handle) return Identifiers;

  function Is_Used (Unit : Unit_Handle;
                    By   : Unit_Handle) return Boolean;

  function Any_Type (Id : Identifier_Handle) return Data_Handle;

  function Ada_Tag return Data_Handle;

  function Asm_Input return Data_Handle;

  function Asm_Output return Data_Handle;

  function System_Address return Data_Handle;

  function Predefined_Boolean return Data_Handle with Inline;

  function Predefined_Root_Integer return Data_Handle with Inline;

  function Predefined_Root_Real return Data_Handle with Inline;

  function Predefined_Root_String return Data_Handle with Inline;

  function Predefined_Root_Access return Data_Handle with Inline;

  function Predefined_Operator return Data_Handle with Inline;

  function Predefined_Name return Data_Handle with Inline;

end Ada_95.Token.Data;
