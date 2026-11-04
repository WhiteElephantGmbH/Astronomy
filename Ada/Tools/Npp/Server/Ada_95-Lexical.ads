-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package Ada_95.Lexical is

  type Element is (
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
    Is_Some,
    Is_Goto,

    Is_Range,
    Is_Access,
    Is_Delta,
    Is_Digits,
    Is_Mod, -- used as Reserved_Word'last

    Semicolon,
    Period,
    Comma,
    Assignment,
    Association,
    Equal,
    Not_Equal,
    Greater,
    Greater_Or_Equal,
    Less,
    Less_Or_Equal,
    Ampersand,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Left_Bracket,
    Right_Bracket,
    Left_Parenthesis,
    Right_Parenthesis,
    Exponentiation,
    Colon,
    Apostrophe,
    Vertical_Line,
    Quote,
    Range_Delimiter,
    Unconstrained,
    Start_Label,
    End_Label,
    Target_Name,
    Special_Id, -- only allowed in Standard

    Aspect,
    Attribute,
    Pragma_Identifier,
    Identifier,
    Integer_Literal,
    Real_Literal,
    String_Literal,
    Error);
  for Element'size use 8;

  subtype Reserved_Word is Element range Element'first .. Is_Mod;

  subtype Delimiter is Element range Semicolon .. Special_Id;


  type Attribute_Id is (

    -- reserved words

    Is_Range,
    Is_Access,
    Is_Delta,
    Is_Digits,
    Is_Mod,

    -- single attributes

    Is_Image, -- used as Single_Attribute'first
    Is_Initialized,
    Is_Last,
    Is_Length,
    Is_Value,
    Is_Pos,
    Is_Val,
    Is_Position,
    Is_Class,
    Is_Tag,
    Is_Version,
    Is_Width,
    Is_Valid,
    Is_Succ,
    Is_Pred,
    Is_Identity,
    Is_Max,
    Is_Aft,
    Is_Base,
    Is_Caller,
    Is_Count,
    Is_Fore,
    Is_Min,
    Is_Compose,
    Is_Fraction,
    Is_Callable,
    Is_Machine,
    Is_Truncation,
    Is_Model,
    Is_Terminated,
    Is_Ceiling,
    Is_Rounding,
    Is_Adjacent,
    Is_Constrained,
    Is_Definite,
    Is_Denorm,
    Is_Exponent,
    Is_Floor,
    Is_Modulus,
    Is_Remainder,
    Is_Round,
    Is_Scale,
    Is_Scaling,
    Is_Bit,
    Is_Elaborated,
    Is_Emax,
    Is_Enabled,
    Is_Epsilon,
    Is_Img,
    Is_Tick,
    Is_Large,
    Is_Mantissa,
    Is_Old,
    Is_Reduce,
    Is_Result,

    -- single attributes shared with aspects (must match Aspect_Id enumeration)

    Is_Address,
    Is_Alignment,
    Is_First,
    Is_Input,
    Is_Output,
    Is_Read,
    Is_Size,
    Is_Small,
    Is_Write, -- used as Single_Attribute'last

    -- compound attributes

    Is_Abort_Signal, -- used as Compound_Attribute'first
    Is_Address_Size,
    Is_Asm_Input,
    Is_Asm_Output,
    Is_Ast_Entry,
    Is_Bit_Position,
    Is_Compiler_Version,
    Is_Code_Address,
    Is_Default_Bit_Order,
    Is_Descriptor_Size,
    Is_Elab_Body,
    Is_Elab_Spec,
    Is_Enum_Rep,
    Is_Enum_Val,
    Is_Finalization_Size,
    Is_Fixed_Value,
    Is_Has_Access_Values,
    Is_Has_Discriminants,
    Is_Has_Tagged_Values,
    Is_Integer_Value,
    Is_Loop_Entry,
    Is_Machine_Size,
    Is_Max_Integer_Size,
    Is_Max_Interrupt_Priority,
    Is_Max_Priority,
    Is_Maximum_Alignment,
    Is_Mechanism_Code,
    Is_Null_Parameter,
    Is_Passed_By_Reference,
    Is_Pool_Address,
    Is_Range_Length,
    Is_Safe_Emax,
    Is_Safe_Large,
    Is_Storage_Unit,
    Is_Target_Name,
    Is_To_Address,
    Is_Type_Class,
    Is_Uet_Address,
    Is_Unconstrained_Array,
    Is_Universal_Literal_String,
    Is_Unrestricted_Access,
    Is_Wchar_T_Size,
    Is_Word_Size,
    Is_First_Bit,
    Is_Unchecked_Access,
    Is_Last_Bit,
    Is_Body_Version,
    Is_Copy_Sign,
    Is_Leading_Part,
    Is_Machine_Emax,
    Is_Machine_Emin,
    Is_Machine_Mantissa,
    Is_Machine_Overflows,
    Is_Machine_Rounds,
    Is_Max_Size_In_Storage_Elements,
    Is_Model_Emin,
    Is_Model_Epsilon,
    Is_Model_Mantissa,
    Is_Model_Small,
    Is_Partition_Id,
    Is_Safe_First,
    Is_Safe_Last,
    Is_Signed_Zeros,
    Is_System_Allocator_Alignment,
    Is_Unbiased_Rounding,
    Is_Valid_Value,
    Is_Wide_Image,
    Is_Wide_Value,
    Is_Wide_Wide_Image,
    Is_Wide_Wide_Value,

    -- compound attributes shared with aspects (must match Aspect_Id enumeration)

    Is_Bit_Order,
    Is_Component_Size,
    Is_External_Tag,
    Is_Machine_Radix,
    Is_Object_Size,
    Is_Preelaborable_Initialization,
    Is_Put_Image,
    Is_Scalar_Storage_Order,
    Is_Storage_Pool,
    Is_Storage_Size,
    Is_Value_Size);

  for Attribute_Id'size use 8;

  subtype Single_Attribute is Attribute_Id range Is_Image .. Is_Write;

  subtype Compound_Attribute is Attribute_Id range Is_Abort_Signal .. Attribute_Id'last;


  type Aspect_Id is (

    -- single aspects shared with attributes (must match Attribute_Id enumeration)

    Is_Address,
    Is_Alignment,
    Is_First,
    Is_Input,
    Is_Output,
    Is_Read,
    Is_Size,
    Is_Small,
    Is_Write,

    -- single aspects

    Is_Aggregate, -- used as Single_Aspect'first
    Is_Code,
    Is_Coding,
    Is_Depends,
    Is_Dimension,
    Is_Element,
    Is_Empty,
    Is_Ghost,
    Is_Global,
    Is_Initializes,
    Is_Invariant,
    Is_Iterable,
    Is_Layout,
    Is_Mechanism,
    Is_Next,
    Is_Post,
    Is_Pre,
    Is_Predicate,
    Is_Radix,
    Is_Remote,
    Is_Synchronization,
    Is_Unmodified,

    -- single aspects shared with pragmas (must match Pragma_Id enumeration)

    Is_Annotate,
    Is_Cpu,
    Is_Independent,
    Is_Obsolescent,
    Is_Preelaborate,
    Is_Pure,
    Is_Unreferenced,
    Is_Volatile,
    Is_Warnings,
    Is_Asynchronous,
    Is_Atomic,
    Is_Convention,
    Is_Export,
    Is_Import,
    Is_Inline,
    Is_Pack,
    Is_Priority,
    Is_Shared,
    Is_Static, -- used as Single_Aspect'last

    -- compound aspects shared with attributes (must match Attribute_Id enumeration)

    Is_Bit_Order,
    Is_Component_Size,
    Is_External_Tag,
    Is_Machine_Radix,
    Is_Object_Size,
    Is_Preelaborable_Initialization,
    Is_Put_Image,
    Is_Scalar_Storage_Order,
    Is_Storage_Pool,
    Is_Storage_Size,
    Is_Value_Size,

    -- compound aspects

    Is_Abstract_State, -- use as Compound_Aspect'first
    Is_Add_Named,
    Is_Add_Unnamed,
    Is_Always_Terminates,
    Is_Assign_Indexed,
    Is_Async_Readers,
    Is_Async_Writers,
    Is_Constant_After_Elaboration,
    Is_Constant_Indexing,
    Is_Contract_Cases,
    Is_Default_Component_Value,
    Is_Default_Initial_Condition,
    Is_Default_Iterator,
    Is_Default_Value,
    Is_Disable_Controlled,
    Is_Dimension_System,
    Is_Dynamic_Predicate,
    Is_Effective_Reads,
    Is_Effective_Writes,
    Is_Exceptional_Cases,
    Is_Exclusive_Functions,
    Is_Extensions_Visible,
    Is_External_Name,
    Is_Ghost_Predicate,
    Is_Has_Element,
    Is_Implicit_Dereference,
    Is_Initial_Condition,
    Is_Integer_Literal,
    Is_Iterator_Element,
    Is_Link_Name,
    Is_Linker_Section,
    Is_Lock_Free,
    Is_New_Indexed,
    Is_No_Tagged_Streams,
    Is_No_Task_Parts,
    Is_Part_Of,
    Is_Persistent_Bss,
    Is_Predicate_Failure,
    Is_Real_Literal,
    Is_Refined_Depends,
    Is_Refined_Global,
    Is_Refined_Post,
    Is_Refined_State,
    Is_Relaxed_Initialization,
    Is_Remote_Access_Type,
    Is_Simple_Storage_Pool,
    Is_Simple_Storage_Pool_Type,
    Is_Static_Predicate,
    Is_String_Literal,
    Is_Suppress_Debug_Info,
    Is_Test_Case,
    Is_Thread_Local_Storage,
    Is_Type_Invariant,
    Is_Universal_Data,
    Is_Unreferenced_Objects,
    Is_Variable_Indexing,
    Is_Volatile_Full_Access,
    Is_Volatile_Function,

    -- compound aspects shared with pragmas (must match Pragma_Id enumeration)

    Is_All_Calls_Remote,
    Is_Default_Storage_Pool,
    Is_Discard_Names,
    Is_Dispatching_Domain,
    Is_Elaborate_Body,
    Is_Favor_Top_Level,
    Is_Independent_Components,
    Is_Inline_Always,
    Is_No_Elaboration_Code_All,
    Is_Pure_Function,
    Is_Relative_Deadline,
    Is_Remote_Call_Interface,
    Is_Remote_Types,
    Is_Shared_Passive,
    Is_Spark_Mode,
    Is_Suppress_Initialization,
    Is_Unchecked_Union,
    Is_Universal_Aliasing,
    Is_Volatile_Components,
    Is_Atomic_Components,
    Is_Attach_Handler,
    Is_Interrupt_Handler,
    Is_Interrupt_Priority,
    Is_No_Return);

  for Aspect_Id'size use 8;

  subtype Single_Aspect is Aspect_Id range Is_Aggregate .. Is_Static;

  subtype Compound_Aspect is Aspect_Id range Is_Abstract_State .. Aspect_Id'last;

  type Pragma_Id is (

    -- single pragma shared with aspects (must match Aspect_Id enumeration)

    Is_Annotate,
    Is_Cpu,
    Is_Independent,
    Is_Obsolescent,
    Is_Preelaborate,
    Is_Pure,
    Is_Unreferenced,
    Is_Volatile,
    Is_Warnings,
    Is_Asynchronous, -- Obsolescent_Single_Pragma'first
    Is_Atomic,
    Is_Convention,
    Is_Export,
    Is_Import,
    Is_Inline,
    Is_Pack,
    Is_Priority,
    Is_Shared,
    Is_Static, -- Obsolescent_Single_Pragma'last

    -- single pragma

    Is_Assert, -- used as Single_Pragma'first
    Is_Assume,
    Is_Build, -- WE
    Is_Elaborate,
    Is_External,
    Is_Implemented,
    Is_List,
    Is_Ordered,
    Is_Optimize,
    Is_Page,
    Is_Precondition,
    Is_Profile,
    Is_Polling,
    Is_Restrictions,
    Is_Reviewable,
    Is_Suppress,
    Is_Undefined,
    Is_Unsuppress, -- used as Single_Pragma'last

    -- compound pragma shared with aspects (must match Aspect_Id enumeration)

    Is_All_Calls_Remote,
    Is_Default_Storage_Pool,
    Is_Discard_Names,
    Is_Dispatching_Domain,
    Is_Elaborate_Body,
    Is_Favor_Top_Level,
    Is_Independent_Components,
    Is_Inline_Always,
    Is_No_Elaboration_Code_All,
    Is_Pure_Function,
    Is_Relative_Deadline,
    Is_Remote_Call_Interface,
    Is_Remote_Types,
    Is_Shared_Passive,
    Is_Spark_Mode,
    Is_Suppress_Initialization,
    Is_Unchecked_Union,
    Is_Universal_Aliasing,
    Is_Volatile_Components,
    Is_Atomic_Components, -- Obsolescent_Compound_Pragma'first
    Is_Attach_Handler,
    Is_Interrupt_Handler,
    Is_Interrupt_Priority,
    Is_No_Return, -- Obsolescent_Compound_Pragma'last

    -- compound pragma

    Is_Ada_05, -- used as Compound_Pragma'first
    Is_Ada_12,
    Is_Ada_20,
    Is_Ada_2005,
    Is_Ada_2012,
    Is_Ada_2022,
    Is_Assertion_Policy,
    Is_Compile_Time_Error,
    Is_Compile_Time_Warning,
    Is_Compiler_Unit_Warning,
    Is_Complete_Representation,
    Is_Complex_Representation,
    Is_Console_Application, -- WE
    Is_Convention_Identifier,
    Is_Detect_Blocking,
    Is_Elaborate_All,
    Is_Export_Procedure,
    Is_Finalize_Storage_Only,
    Is_Implementation_Defined,
    Is_Inspection_Point,
    Is_Linker_Options,
    Is_Locking_Policy,
    Is_Loop_Invariant,
    Is_Machine_Attribute,
    Is_Memory_Size,
    Is_No_Component_Reordering,
    Is_No_Strict_Aliasing,
    Is_Normalize_Scalars,
    Is_Partition_Elaboration_Policy,
    Is_Priority_Specific_Dispatching,
    Is_Provide_Shift_Operators,
    Is_Queueing_Policy,
    Is_Stream_Convert,
    Is_Style_Checks,
    Is_Style_Astronomy, -- use as first in Style_Pragma
    Is_Style_None,
    Is_Style_Soudronic,
    Is_Style_Unrestricted,
    Is_Style_White_Elephant, -- use as last in Style_Pragma
    Is_System_Name,
    Is_Task_Dispatching_Policy,
    Is_Unevaluated_Use_Of_Old,
    Is_Unimplemented_Unit,
    Is_Weak_External, -- used as Compound_Pragma'last

    Is_Preelaborable_Initialization); -- shared with attributes and aspects

  for Pragma_Id'size use 8;

  subtype Single_Pragma is Pragma_Id range Is_Assert .. Is_Unsuppress;

  subtype Obsolescent_Single_Pragma is Pragma_Id range Is_Asynchronous .. Is_Shared;

  subtype Obsolescent_Compound_Pragma is Pragma_Id range Is_Atomic_Components .. Is_No_Return;

  subtype Compound_Pragma is Pragma_Id range Is_Ada_05 .. Is_Weak_External;

  subtype Style_Pragma is Pragma_Id range Is_Style_Astronomy .. Is_Style_White_Elephant;

  subtype User_Pragma is Style_Pragma;


  type Name_Key is new Natural;

  Null_Key : constant Name_Key := 0;


  procedure Define;


  function Next_Name_Key return Name_Key with Inline;


  function Is_Reserved_Word (Key : Name_Key) return Boolean with Inline;

  function Reserved_Word_Of (Key : Name_Key) return Reserved_Word with Inline;


  function Is_Attribute (Key : Name_Key) return Boolean with Inline;

  function Attribute_Of (Key : Name_Key) return Attribute_Id with Inline;


  function Is_Aspect (Key : Name_Key) return Boolean with Inline;

  function Aspect_Of (Key : Name_Key) return Aspect_Id with Inline;


  function Is_Pragma_Id (Key : Name_Key) return Boolean with Inline;

  function Pragma_Id_Of (Key : Name_Key) return Pragma_Id with Inline;

end Ada_95.Lexical;
