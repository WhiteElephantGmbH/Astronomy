-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Comment;
with Ada_95.Error;
with Ada_95.Lexical;
with Ada_95.Name;
with Ada_95.Number;
with Ada_95.Source;
with Ada_95.Texts;
with Memory;
with Server;

package Ada_95.Token is

  ----------------------------------------------------------------------------------------------------------------------
  -- Logging
  ----------------------------------------------------------------------------------------------------------------------

  procedure Increment_Log_Indent;

  procedure Decrement_Log_Indent;

  procedure Write_Log (Message : String);

  ----------------------------------------------------------------------------------------------------------------------
  -- Types
  ----------------------------------------------------------------------------------------------------------------------

  subtype Column_Position is Source.Column_Position;
  subtype Column_Range is Source.Column_Range;
  subtype Source_Line is Source.Line;

  subtype Line_Number is Server.Line_Number;

  subtype Counter is Server.Line_Counter;

  type Comment_List is array (Counter range <>) of Comment.Handle;

  type Area is record
    First_Column            : Column_Position;
    Last_Column             : Column_Position;
    Last_Column_Line_Offset : Counter;
  end record;

  type Kind is
    (Is_Aspect,
     Is_Attribute,
     Is_Comment,
     Is_Special_Comment,
     Is_Character_Literal,
     Is_Delimiter,
     Is_Error,
     Is_Identifier,
     Is_Invisible,
     Is_Pragma_Identifier,
     Is_Reserved_Word,
     Is_Numeric_Literal,
     Is_String_Literal);

  type Object is tagged;

  type Line_Object is tagged;

  type Lexical_Object is tagged;

  type String_Literal;

  type Name_Object is tagged;

  type Aspect is tagged;

  type Attribute is tagged;

  type Pragma_Identifier is tagged;

  type Identifier is tagged;

  type Line_End_Comment is tagged;

  type Comment_Lines is tagged;

  type Special_Comment is tagged;

  type Error_Object is tagged;

  type Handle is access all Object'class;
  for Handle'storage_pool use Memory.Pool.all;

  type Line_Handle is access all Line_Object'class;
  for Line_Handle'storage_pool use Memory.Pool.all;

  type Lexical_Handle is access all Lexical_Object'class;
  for Lexical_Handle'storage_pool use Memory.Pool.all;

  type String_Handle is access all String_Literal;
  for String_Handle'storage_pool use Memory.Pool.all;

  type Name_Handle is access all Name_Object'class;
  for Name_Handle'storage_pool use Memory.Pool.all;

  type Aspect_Handle is access all Aspect'class;
  for Aspect_Handle'storage_pool use Memory.Pool.all;

  type Attribute_Handle is access all Attribute'class;
  for Attribute_Handle'storage_pool use Memory.Pool.all;

  type Pragma_Identifier_Handle is access all Pragma_Identifier'class;
  for Pragma_Identifier_Handle'storage_pool use Memory.Pool.all;

  type Identifier_Handle is access all Identifier'class;
  for Identifier_Handle'storage_pool use Memory.Pool.all;

  type Line_End_Comment_Handle is access all Line_End_Comment'class;
  for Line_End_Comment_Handle'storage_pool use Memory.Pool.all;

  type Comment_Lines_Handle is access all Comment_Lines'class;
  for Comment_Lines_Handle'storage_pool use Memory.Pool.all;

  type Special_Comment_Handle is access all Special_Comment'class;
  for Special_Comment_Handle'storage_pool use Memory.Pool.all;

  type Error_Handle is access all Error_Object'class;
  for Error_Handle'storage_pool use Memory.Pool.all;

  function "<" (Left, Right : Handle) return Boolean;

  function Is_Null (Item : Identifier_Handle) return Boolean with Inline;

  function "=" (Left, Right : Identifier_Handle) return Boolean with Inline;

  function "<" (Left, Right : Identifier_Handle) return Boolean with Inline;

  type Identifiers is array (Positive range <>) of Identifier_Handle;

  No_Identifiers : constant Identifiers(1..0) := [others => null];

  function Comparison (Left, Right : Identifiers) return Result;

  function "=" (Left, Right : Identifiers) return Boolean;

  function "<" (Left, Right : Identifiers) return Boolean;

  type Identifier_List is access all Identifiers;
  for Identifier_List'storage_pool use Memory.Pool.all;

  function Comparison (Left, Right : Identifier_List) return Result;

  function "=" (Left, Right : Identifier_List) return Boolean;

  function "<" (Left, Right : Identifier_List) return Boolean;

  type Sequence is record
    First       : Handle;
    Last        : Handle;
    Last_Line   : Line_Handle;
    Error_Token : Error_Handle;
  end record;


  ----------------------------------------------------------------------------------------------------------------------
  -- Constructors
  ----------------------------------------------------------------------------------------------------------------------

  procedure Create_List (Filename : String);

  procedure End_List;

  procedure Set_Position (Column : Column_Position) with Inline;

  function Position return Column_Position with Inline;

  procedure New_Line with Inline;

  procedure Append_Comment (Item : Comment.Handle);

  procedure Append_Name (Item : Name.Handle) with Inline;

  procedure Append_Literal (Item : Character) with Inline;

  procedure Append_Literal (Item           : Number.Handle;
                            Is_Real_Number : Boolean) with Inline;

  procedure Append_Literal (Item         : Texts.Handle;
                            First_Column : Column_Position;
                            Last_Column  : Column_Position) with Inline;

  procedure Append_Symbol (Item : Lexical.Delimiter) with Inline;

  procedure Append_Error (Message    : Error.Kind;
                          Item       : Error.Handle;
                          The_Column : Column_Range);

  procedure Mark_Error (Message : Error.Kind;
                        Item    : Lexical_Handle;
                        Tokens  : in out Sequence);


  ----------------------------------------------------------------------------------------------------------------------
  -- Selectors
  ----------------------------------------------------------------------------------------------------------------------

  function Actual_List return Sequence;

  function Handle_Of (Item : Sequence;
                      Line : Line_Number) return Handle;

  function Handle_Of (Item   : Sequence;
                      Line   : Line_Number;
                      Column : Column_Position) return Handle;

  function Is_Declaration (Item : Handle) return Boolean;

  function Declaration_Of (Item : Handle) return Handle;

  function Is_Used (Item : Handle) return Boolean;

  function Is_Unused_Declaration (Item : Handle) return Boolean;

  function Reference_Of (Item : Handle) return Handle;

  function Line_Count_Of (Item : Sequence) return Counter;

  function Filename_Of (Item : Handle) return String;

  function Line_Image_Of (Item : Handle) return String;

  function Line_Number_Of (Item : Handle) return Line_Number;

  function Column_Position_Of (Item : Handle) return Column_Position;

  function Lexical_After (Item : Handle) return Lexical_Handle with Inline;

  function Lexical_After (Item : Lexical_Handle) return Lexical_Handle with Inline;

  function Lexical_Before (Item : Handle) return Lexical_Handle with Inline;

  function Lexical_Before (Item : Lexical_Handle) return Lexical_Handle with Inline;

  function Next_Special_Comment (Item : Handle) return Special_Comment_Handle;

  function Line_End_Special_Comment (Item : Handle) return Special_Comment_Handle;

  function Name_List_Of (Item : Identifiers) return Name.List;

  function Image_Of (Item      : Identifiers;
                     Separator : Character := '.') return String;

  function Next_Style (Item          :     Handle;
                       Style_Defined : out Boolean) return Lexical.Style_Pragma;

  function Is_Marked (Mark       : String;
                      Annotation : Special_Comment_Handle) return Boolean;


  ----------------------------------------------------------------------------------------------------------------------
  -- Objects
  ----------------------------------------------------------------------------------------------------------------------

  type Object is abstract tagged record
    Previous : Handle;
    Next     : Handle;
  end record;

  function Area_Of (Item : Object) return Area is abstract;

  function Image_Of (Item : Object) return String; -- default empty string

  function Kind_Of (Item : Object) return Kind is abstract;

  ----------------------------------------------------------------------------------------------------------------------

  type File_Begin (Length : Natural) is new Object with record
    Name : String (1 .. Length);
  end record;

  function Area_Of (Item : File_Begin) return Area;

  function Kind_Of (Item : File_Begin) return Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type File_End is new Object with null record;

  function Area_Of (Item : File_End) return Area;

  function Kind_Of (Item : File_End) return Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Line_Object is abstract new Object with record
    Previous_Line : Line_Handle;
    Count         : Counter := 1;
    Column        : Column_Position;
  end record
  with Pack;

  ----------------------------------------------------------------------------------------------------------------------

  type Line_End is new Line_Object with null record;

  function Area_Of (Item : Line_End) return Area;

  function Kind_Of (Item : Line_End) return Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Lines is new Line_Object with null record;

  function Area_Of (Item : Lines) return Area;

  function Kind_Of (Item : Lines) return Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Comment_Object is abstract new Line_Object with null record;

  ----------------------------------------------------------------------------------------------------------------------

  type Line_End_Comment is new Comment_Object with record
    Handle : Comment.Handle;
  end record;

  function Area_Of (Item : Line_End_Comment) return Area;

  function Image_Of (Item : Line_End_Comment) return String;

  function Kind_Of (Item : Line_End_Comment) return Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Comment_Lines (Length : Counter) is new Comment_Object with record
    List : Comment_List(1 .. Length);
  end record;

  function Area_Of (Item : Comment_Lines) return Area;

  function Kind_Of (Item : Comment_Lines) return Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Special_Comment is new Line_End_Comment with record
    Is_Used : Boolean;
  end record;

  function Area_Of (Item : Special_Comment) return Area;

  function Kind_Of (Item : Special_Comment) return Kind;

  type Special_Line_End_Comment is new Special_Comment with null record;

  ----------------------------------------------------------------------------------------------------------------------

  type Lexical_Object is abstract new Object with record
    Column  : Column_Position;
    Element : Lexical.Element;
  end record
  with Pack;

  function Area_Of (Item : Lexical_Object) return Area is abstract;

  function Kind_Of (Item : Lexical_Object) return Kind is abstract;

  ----------------------------------------------------------------------------------------------------------------------

  type Name_Object is abstract new Lexical_Object with record
    Id : Name.Handle;
  end record;

  function Area_Of (Item : Name_Object) return Area;

  function Image_Of (Item : Name_Object) return String;

  function Kind_Of (Item : Name_Object) return Kind is abstract;

  ----------------------------------------------------------------------------------------------------------------------

  type Reserved_Word is new Name_Object with null record;

  function Kind_Of (Item : Reserved_Word) return Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Aspect is new Name_Object with record
    Designator : Lexical.Aspect_Id;
  end record;

  function Kind_Of (Item : Aspect) return Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Attribute is new Name_Object with record
    Designator : Lexical.Attribute_Id;
  end record;

  function Kind_Of (Item : Attribute) return Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Pragma_Identifier is new Name_Object with record
    Designator : Lexical.Pragma_Id;
    Is_Used    : Boolean;
  end record;

  overriding
  function Kind_Of (Item : Pragma_Identifier) return Kind;

  ----------------------------------------------------------------------------------------------------------------------

  subtype Is_In_Package is Data_Kind range Is_Package_Body .. Is_Package_Specification;

  subtype Is_In_Package_Renaming is Data_Kind range Is_Package_Renaming .. Is_Generic_Package_Renaming;

  subtype Is_In_Subprogram is Data_Kind range Is_Subprogram_Body .. Is_Subprogram_Declaration;

  type Data_Type is abstract tagged record
    Location : Identifier_Handle;
  end record;

  function Data_Kind_Of (Item : Data_Type) return Data_Kind is abstract;

  type Data_Handle is access all Data_Type'class;
  for Data_Handle'storage_pool use Memory.Pool.all;

  type Identifier is new Name_Object with record
    Data : Data_Handle;
  end record;

  function Kind_Of (Item : Identifier) return Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Operator_Symbol is new Identifier with null record;

  function Area_Of (Item : Operator_Symbol) return Area;

  function Image_Of (Item : Operator_Symbol) return String;

  type Equality_Operator is new Operator_Symbol with null record;

  ----------------------------------------------------------------------------------------------------------------------

  type Character_Literal is new Identifier with null record;

  function Area_Of (Item : Character_Literal) return Area;

  function Image_Of (Item : Character_Literal) return String;

  function Kind_Of (Item : Character_Literal) return Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type String_Literal is new Lexical_Object with record
    Handle       : Texts.Handle;
    First_Column : Column_Position; -- screen position
    Last_Column  : Column_Position; -- screen position
  end record;

  function Area_Of (Item : String_Literal) return Area;

  function Image_Of (Item : String_Literal) return String;

  function Kind_Of (Item : String_Literal) return Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Numeric_Literal is abstract new Lexical_Object with record
    Handle : Number.Handle;
  end record;

  function Area_Of (Item : Numeric_Literal) return Area;

  function Image_Of (Item : Numeric_Literal) return String;

  function Kind_Of (Item : Numeric_Literal) return Kind;

  type Integer_Literal is new Numeric_Literal with null record;

  type Real_Literal is new Numeric_Literal with null record;

  ----------------------------------------------------------------------------------------------------------------------

  type Delimiter is new Lexical_Object with null record;

  function Area_Of (Item : Delimiter) return Area;

  function Image_Of (Item : Delimiter) return String;

  function Kind_Of (Item : Delimiter) return Kind;

  ----------------------------------------------------------------------------------------------------------------------

  type Error_Object is new Lexical_Object with record
    Message    : Error.Kind;
    Handle     : Error.Handle;
  end record;

  function Area_Of (Item : Error_Object) return Area;

  function Image_Of (Item : Error_Object) return String;

  function Kind_Of (Item : Error_Object) return Kind;

end Ada_95.Token;
