-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Container;

package Ada_95 with Preelaborate is

  Max_Line_Length : constant := 120;

  type Format_Kind is (
    Is_End_Of_File,

    Is_Comment,
    Is_Special_Comment,
    Is_New_Line,

    Is_Interface,
    Is_Overriding,
    Is_Synchronized,

    Is_In,
    Is_End,
    Is_Is,
    Is_If,
    Is_Return,
    Is_When,
    Is_Out,
    Is_Procedure,
    Is_Then,
    Is_Begin,
    Is_With,
    Is_Function,
    Is_Raise,
    Is_Renames,
    Is_Others,
    Is_Package,
    Is_Exception,
    Is_Constant,
    Is_Loop,
    Is_Type,
    Is_New,
    Is_Record,
    Is_Else,
    Is_Null,
    Is_Case,
    Is_Or,
    Is_For,
    Is_Not,
    Is_And,
    Is_All,
    Is_Private,
    Is_Subtype,
    Is_Elsif,
    Is_Body,
    Is_Declare,
    Is_Use,
    Is_While,
    Is_Array,
    Is_Of,
    Is_Exit,
    Is_Accept,
    Is_At,
    Is_Pragma,
    Is_Entry,
    Is_Do,
    Is_Select,
    Is_Abstract,
    Is_Generic,
    Is_Task,
    Is_Delay,
    Is_Aliased,
    Is_Limited,
    Is_Mod,
    Is_Xor,
    Is_Terminate,
    Is_Reverse,
    Is_Protected,
    Is_Abs,
    Is_Rem,
    Is_Separate,
    Is_Tagged,
    Is_Abort,
    Is_Requeue,
    Is_Until,
    Is_Goto,

    Is_Range,
    Is_Access,
    Is_Delta,
    Is_Digits,

    Is_Semicolon,
    Is_Period,
    Is_Comma,
    Is_Assignment,
    Is_Association,
    Is_Equal,
    Is_Not_Equal,
    Is_Greater,
    Is_Greater_Or_Equal,
    Is_Less,
    Is_Less_Or_Equal,
    Is_Ampersand,
    Is_Plus,
    Is_Minus,
    Is_Asterisk,
    Is_Slash,
    Is_Left_Parenthesis,
    Is_Right_Parenthesis,
    Is_Exponentiation,
    Is_Colon,
    Is_Apostrophe,
    Is_Vertical_Line,
    Is_Quote,
    Is_Range_Delimiter,
    Is_Unconstrained,
    Is_Start_Label,
    Is_End_Label,
    Is_Special_Id, -- only allowed in Standard

    Is_Attribute,
    Is_Aspect,
    Is_Declaring_Identifier,
    Is_Identifier,
    Is_Character_Literal,
    Is_Integer_Literal,
    Is_Real_Literal,
    Is_String_Literal,

    Is_Error,
    Is_Unknown);

  type Data_Kind is
   (Is_End_Identifier,

    Is_Package_Body,
    Is_Package_Specification,

    Is_Generic_Package_Declaration,
    Is_Package_Instantiation,

    Is_Package_Renaming,
    Is_Generic_Package_Renaming,

    Is_Subprogram_Body,
    Is_Subprogram_Declaration,

    Is_Generic_Subprogram_Declaration,

    Is_Subprogram_Renaming,
    Is_Generic_Subprogram_Renaming,

    Is_Used_Subprogram,
    Is_Used_Generic_Subprogram,

    Is_Accept_Declaration,
    Is_Access_Type,
    Is_Array_Type,
    Is_Block,
    Is_Derived_Type,
    Is_Discrete_Type,
    Is_Formal_Block,
    Is_Formal_Object,
    Is_Formal_Package,
    Is_Formal_Subprogram,
    Is_Formal_Type,
    Is_Entry_Declaration,
    Is_Entry_Body,
    Is_Enumeration_Value,
    Is_Enumeration_Type,
    Is_Exception,
    Is_Label,
    Is_Incomplete_Type,
    Is_Instantiation,
    Is_Integer_Type,
    Is_Interface_Type,
    Is_Object,
    Is_Predefined_Operator_Type,
    Is_Predefined_Pragma_Argument_Id_Type,
    Is_Private_Extension_Type,
    Is_Private_Block,
    Is_Private_Type,
    Is_Protected_Body,
    Is_Protected_Declaration,
    Is_Protected_Type,
    Is_Real_Type,
    Is_Record_Type,
    Is_Subprogram_Access_Type,
    Is_Subtype,
    Is_Task_Body,
    Is_Task_Declaration,
    Is_Task_Type,
    Is_Unknown,
    Is_With_Declaration);

  subtype Result is Container.Result;

  pragma Warnings (Off);
  use all type Result;

end Ada_95;
