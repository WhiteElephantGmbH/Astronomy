-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Strings.Equal_Case_Insensitive;
with Ada.Tags;
with Ada_95.Build;
with Ada_95.Token.Checker;
with Indefinite_Doubly_Linked_Lists;
with Log;
with Text;

package body Ada_95.Token.Parser is

  --====================================================================================================================
  -- State Machine Constant Tables
  --====================================================================================================================

  -- simple_expression ::= S_0
  --   [u] S_1 (P F_111 [e F_112 P] | a F_112 P) T_11 {m T_12 (P F_121 [e F_112 P] | a F_112 P)}
  --    {b S_2 (P F_211 [e F_212 P] | a F_212 P) T_21 {m T_22 (P F_221 [e F_212 P] | a F_212 P)}}

  type Simple_Expression_State is
    (S_0, S_1, S_2,
     T_11, T_12,
     T_21, T_22,
     F_111, F_112, F_121,
     F_211, F_212, F_221);

  type Simple_Expression_Element is
    (Allocator,
     Aggregate,
     Aggregate_Or_Expression, -- Expression in parenthesis
     Is_Identifier,
     Operator,
     Is_Character_Literal,
     Is_Integer_Literal,
     Is_Real_Literal,
     Is_String_Literal,
     Is_Null,
     Unexpected,
     Unknown);

  type Simple_Expression_Handle is record
     State   : Simple_Expression_State;
     Element : Simple_Expression_Element;
  end record;

  type Simple_Expression_Data is array (Simple_Expression_State, Lexical.Element) of Simple_Expression_Handle;

  Next_Simple_Expression_Data : constant Simple_Expression_Data :=
    [S_0    => [Lexical.Plus | Lexical.Minus                                       => (S_1,   Operator),
                Lexical.Left_Bracket                                               => (F_111, Aggregate),
                Lexical.Left_Parenthesis                                           => (F_111, Aggregate_Or_Expression),
                Lexical.Identifier | Lexical.Target_Name                           => (F_111, Is_Identifier),
                Lexical.Integer_Literal                                            => (F_111, Is_Integer_Literal),
                Lexical.Real_Literal                                               => (F_111, Is_Real_Literal),
                Lexical.String_Literal                                             => (F_111, Is_String_Literal),
                Lexical.Is_New                                                     => (F_111, Allocator),
                Lexical.Is_Null                                                    => (F_111, Is_Null),
                Lexical.Is_Abs | Lexical.Is_Not                                    => (F_112, Operator),
                others                                                             => (S_0,   Unexpected)],

     S_1    => [Lexical.Left_Bracket                                               => (F_111, Aggregate),
                Lexical.Left_Parenthesis                                           => (F_111, Aggregate_Or_Expression),
                Lexical.Identifier | Lexical.Target_Name                           => (F_111, Is_Identifier),
                Lexical.Integer_Literal                                            => (F_111, Is_Integer_Literal),
                Lexical.Real_Literal                                               => (F_111, Is_Real_Literal),
                Lexical.String_Literal                                             => (F_111, Is_String_Literal),
                Lexical.Is_New                                                     => (F_111, Allocator),
                Lexical.Is_Null                                                    => (F_111, Is_Null),
                Lexical.Is_Abs | Lexical.Is_Not                                    => (F_112, Operator),
                others                                                             => (S_0,   Unexpected)],

     F_111  => [Lexical.Exponentiation                                             => (F_112, Operator),
                Lexical.Asterisk | Lexical.Slash | Lexical.Is_Mod | Lexical.Is_Rem => (T_12,  Operator),
                Lexical.Plus | Lexical.Minus | Lexical.Ampersand                   => (S_2,   Operator),
                others                                                             => (S_0,   Unknown)],

     F_112  => [Lexical.Left_Bracket                                               => (T_11,  Aggregate),
                Lexical.Left_Parenthesis                                           => (T_11,  Aggregate_Or_Expression),
                Lexical.Identifier | Lexical.Target_Name                           => (T_11,  Is_Identifier),
                Lexical.Integer_Literal                                            => (T_11,  Is_Integer_Literal),
                Lexical.Real_Literal                                               => (T_11,  Is_Real_Literal),
                Lexical.String_Literal                                             => (T_11,  Is_String_Literal),
                Lexical.Is_New                                                     => (T_11,  Allocator),
                Lexical.Is_Null                                                    => (T_11,  Is_Null),
                others                                                             => (S_0,   Unexpected)],

     T_11   => [Lexical.Asterisk | Lexical.Slash | Lexical.Is_Mod | Lexical.Is_Rem => (T_12,  Operator),
                Lexical.Plus | Lexical.Minus | Lexical.Ampersand                   => (S_2,   Operator),
                others                                                             => (S_0,   Unknown)],

     T_12   => [Lexical.Left_Bracket                                               => (F_121, Aggregate),
                Lexical.Left_Parenthesis                                           => (F_121, Aggregate_Or_Expression),
                Lexical.Identifier | Lexical.Target_Name                           => (F_121, Is_Identifier),
                Lexical.Integer_Literal                                            => (F_121, Is_Integer_Literal),
                Lexical.Real_Literal                                               => (F_121, Is_Real_Literal),
                Lexical.String_Literal                                             => (F_121, Is_String_Literal),
                Lexical.Is_New                                                     => (F_121, Allocator),
                Lexical.Is_Null                                                    => (F_121, Is_Null),
                Lexical.Is_Abs | Lexical.Is_Not                                    => (F_112, Operator),
                others                                                             => (S_0,   Unexpected)],

     F_121  => [Lexical.Exponentiation                                             => (F_112, Operator),
                Lexical.Asterisk | Lexical.Slash | Lexical.Is_Mod | Lexical.Is_Rem => (T_12,  Operator),
                Lexical.Plus | Lexical.Minus | Lexical.Ampersand                   => (S_2,   Operator),
                others                                                             => (S_0,   Unknown)],

     S_2    => [Lexical.Left_Bracket                                               => (F_211, Aggregate),
                Lexical.Left_Parenthesis                                           => (F_211, Aggregate_Or_Expression),
                Lexical.Identifier | Lexical.Target_Name                           => (F_211, Is_Identifier),
                Lexical.Integer_Literal                                            => (F_211, Is_Integer_Literal),
                Lexical.Real_Literal                                               => (F_211, Is_Real_Literal),
                Lexical.String_Literal                                             => (F_211, Is_String_Literal),
                Lexical.Is_New                                                     => (F_211, Allocator),
                Lexical.Is_Null                                                    => (F_211, Is_Null),
                Lexical.Is_Abs | Lexical.Is_Not                                    => (F_212, Operator),
                others                                                             => (S_0,   Unexpected)],

     F_211  => [Lexical.Exponentiation                                             => (F_212, Operator),
                Lexical.Asterisk | Lexical.Slash | Lexical.Is_Mod | Lexical.Is_Rem => (T_22,  Operator),
                Lexical.Plus | Lexical.Minus | Lexical.Ampersand                   => (S_2,   Operator),
                others                                                             => (S_0,   Unknown)],

     F_212  => [Lexical.Left_Bracket                                               => (T_21,  Aggregate),
                Lexical.Left_Parenthesis                                           => (T_21,  Aggregate_Or_Expression),
                Lexical.Identifier | Lexical.Target_Name                           => (T_21,  Is_Identifier),
                Lexical.Integer_Literal                                            => (T_21,  Is_Integer_Literal),
                Lexical.Real_Literal                                               => (T_21,  Is_Real_Literal),
                Lexical.String_Literal                                             => (T_21,  Is_String_Literal),
                Lexical.Is_New                                                     => (T_21,  Allocator),
                Lexical.Is_Null                                                    => (T_21,  Is_Null),
                others                                                             => (S_0,   Unexpected)],

     T_21   => [Lexical.Asterisk | Lexical.Slash | Lexical.Is_Mod | Lexical.Is_Rem => (T_22,  Operator),
                Lexical.Plus | Lexical.Minus | Lexical.Ampersand                   => (S_2,   Operator),
                others                                                             => (S_0,   Unknown)],

     T_22   => [Lexical.Left_Bracket                                               => (F_221, Aggregate),
                Lexical.Left_Parenthesis                                           => (F_221, Aggregate_Or_Expression),
                Lexical.Identifier | Lexical.Target_Name                           => (F_221, Is_Identifier),
                Lexical.Integer_Literal                                            => (F_221, Is_Integer_Literal),
                Lexical.Real_Literal                                               => (F_221, Is_Real_Literal),
                Lexical.String_Literal                                             => (F_221, Is_String_Literal),
                Lexical.Is_New                                                     => (F_221, Allocator),
                Lexical.Is_Null                                                    => (F_221, Is_Null),
                Lexical.Is_Abs | Lexical.Is_Not                                    => (F_212, Operator),
                others                                                             => (S_0,   Unexpected)],

     F_221  => [Lexical.Exponentiation                                             => (F_212, Operator),
                Lexical.Asterisk | Lexical.Slash | Lexical.Is_Mod | Lexical.Is_Rem => (T_22,  Operator),
                Lexical.Plus | Lexical.Minus | Lexical.Ampersand                   => (S_2,   Operator),
                others                                                             => (S_0,   Unknown)]];


  --====================================================================================================================
  -- Syntax Checks and Type Evaluation
  --====================================================================================================================

  Dummy : Data_Handle;

  procedure Process (Unit : Data.Unit_Handle) is


  ----------------------------------------------------------------------------------------------------------------------

    -- abort_statement ::=
    --      abort task_name {, task_name};
    --
    procedure Abort_Statement (Scope : Data.Unit_Handle);


    -- abortable_part ::=
    --      sequence_of_statements
    --
    -- coded in Select_Statement


    -- abstract_subprogram_declaration ::=
    --      [overriding_indicator]
    --      subprogram_specification is abstract
    --      [aspect_specification] ;
    --
    -- coded in Declarative_Part


    -- accept_alternative ::=
    --      accept_statement [sequence_of_statements]
    --
    -- coded in Select_Statement


    -- accept_statement ::=
    --      accept entry_direct_name [ ( entry_index ) ] parameter_profile [do
    --        handled_sequence_of_statements
    --      end [entry_identifier]] ;
    --
    procedure Accept_Statement (Scope : Data.Unit_Handle);


    -- access_definition ::=
    --      [not null] access access_definition_part

    -- access_definition_part ::=
    --      [constant] subtype_mark
    --    | [protected] procedure parameter_profile
    --    | [protected] function parameter_and_result_profile
    --
    function Access_Definition_Part (Scope : Data.Unit_Handle) return Data_Handle;


    --    access_to_object_definition ::=
    --         access [general_access_modifier] subtype_indication
    --
    -- coded in Access_Type_Definition


    --    access_to_subprogram_definition ::=
    --         access [protected] procedure parameter_profile
    --       | access [protected] function  parameter_and_result_profile
    --
    -- coded in Access_Type_Definition


    -- access_type_definition ::=
    --      [null_exclusion] access_to_object_definition
    --    | [null_exclusion] access_to_subprogram_definition
    --
    function Access_Type_Definition (Id       : Identifier_Handle;
                                     Within   : Data.Context) return Data_Handle;


    -- [actual_parameter_part] ::=
    --      [ ( parameter_association {, parameter_association} ) ]
    --
    function Found_Actual_Parameters (Scope         : Data.Unit_Handle;
                                      Profile       : Data.Subprogram_Profile;
                                      Instantiation : Data.Instantiation_Handle := null;
                                      Parent_Class  : Data.Type_Handle := null;
                                      Complete      : Boolean := False;
                                      Is_Equality   : Boolean := False) return Boolean;

    -- aggregate ::=
    --      record_aggregate
    --    | extension_aggregate
    --    | array_aggregate
    --
    function Aggregate (Within : Data.Context) return Data_Handle;


    -- allocator ::=
    --      new subtype_indication
    --    | new qualified_expression
    --
    function Allocator (Within : Data.Context) return Data_Handle;


    -- ancestor_part ::=
    --      expression
    --    | subtype_mark
    --
    -- coded in Aggregate


    -- array_aggregate ::=
    --      positional_array_aggregate
    --    | named_array_aggregate
    --
    -- coded in Aggregate


    -- array_component_association ::=
    --      discrete_choice_list => expression
    --
    -- coded in Aggregate


    -- array_type_definition ::=
    --      unconstrained_array_definition
    --    | constrained_array_definition
    --
    function Array_Type_Definition (Scope : Data.Unit_Handle) return Data.Array_Definition_Handle;


    -- aspect_clause ::=
    --      attribute_definition_clause
    --    | enumeration_representation_clause
    --    | record_representation_clause
    --    | at_clause
    --
    procedure Aspect_Clause (Scope : Data.Unit_Handle);


    -- assignment_statement ::=
    --      variable_name := expression ;
    --
    procedure Assignment_Call_Or_Code_Statement (Scope : Data.Unit_Handle);


    -- asynchronous_select ::=
    --      select
    --        triggering_alternative
    --      then abort
    --        abortable_part
    --      end select ;
    --
    -- coded in Select_Statement


    -- aspect_definition ::= name | expression | identifier | aggregate | global_aspect_definition
    --
    -- coded in Aspect_Specification


    -- aspect_mark :==
    --    Aspect_Identifier [ ' class]
    --
    function Aspect_Mark return Lexical.Aspect_Id;


    -- aspect_specification ::=
    --       with aspect_mark [=> aspect_definition] {,aspect_mark [=> aspect_definition] }
    --
    procedure Aspect_Specification (Within  : Data.Context;
                                    For_Ids : Identifiers := No_Identifiers);


    -- at_clause ::=
    --      for direct_name use at expression ;
    --
    -- coded in Aspect_Clause


    -- attribute_definition_clause ::=
    --      for local_name'attribute_designator use expression ;
    --    | for local_name'attribute_designator use name ;
    --
    -- coded in Aspect_Clause


    -- attribute_designator ::=
    --      identifier[(static_expression)]
    --    | Access
    --    | Delta
    --    | Digits
    --
    function Attribute_Designator (Within : Data.Context) return Lexical.Attribute_Id;


    -- attribute_reference ::=
    --      prefix ' attribute_designator
    --    | reduction_attribute_reference
    --

    -- basic_declaration ::=
    --      type_declaration
    --    | subtype_declaration
    --    | object_declaration
    --    | number_declaration
    --    | subprogram_declaration
    --    | abstract_subprogram_declaration
    --    | expression_function_declaration
    --    | package_declaration
    --    | renaming_declaration
    --    | exception_declaration
    --    | generic_declaration
    --    | generic_instantiation
    --
    -- coded in Declarative_Part


    -- basic_declarative_item ::=
    --      basic_declaration
    --    | aspect_clause
    --    | use_clause
    --
    -- coded in Declarative_Part


    -- binary_adding_operator ::=
    --      +
    --    | -
    --    | &
    --
    -- coded in Simple_Expression


    -- body ::=
    --      proper_body
    --    | body_stub
    --
    -- coded in Declarative_Part


    -- body_stub ::=
    --      subprogram_body_stub
    --    | package_body_stub
    --    | task_body_stub
    --    | protected_body_stub
    --
    -- coded in Declarative_Part


    -- block_statement ::=
    --      [block_statement_identifier:]
    --        [declare
    --           declarative_part]
    --        begin
    --          handled_sequence_of_statements
    --        end [block_identifier] ;
    --
    procedure Block_Statement (Parent : Data.Unit_Handle;
                               Id     : Identifier_Handle := null);


    -- case_expression ::=
    --      case selecting_expression is case_expression_alternative { , case_expression_alternative}
    --
    --         case_expression_alternative ::= when discrete_choice_list => dependent_expression
    --
    function Case_Expression (Within : Data.Context) return Data_Handle;


    -- case_statement ::=
    --      case expression is
    --         case_statement_alternative {case_statement_alternative}
    --      end case;
    --
    procedure Case_Statement (Scope : Data.Unit_Handle);


    -- case_statement_alternative ::=
    --      when discrete_choice_list => sequence_of_statements
    --
    -- coded in Case_Statement


    -- code_statement ::=
    --      qualified_expression;
    --
    -- coded in Assignment_Block_Call_Or_Code_Statement


    -- choice_parameter_specification ::=
    --      defining_identifier
    --
    -- coded in Handled_Sequence_Of_Statements


    -- compilation ::=
    --      {compilation_unit} <-ONLY ONE !->
    --
    -- compilation_unit ::=
    --      context_clause library_item
    --    | context_clause subunit
    --
    -- coded in Compilation_Unit


    -- component_clause ::=
    --      component_local_name at position range first_bit .. last_bit ;
    --
    -- coded in Declarative_Part


    -- component_choice_list ::=
    --      component_selector_name { | component_selector_name}
    --    | others
    --
    -- coded in Aggregate


    -- component_declaration ::=
    --      defining_identifier_list : component_definition [:= default_expression] [aspect_specification] ;
    --
    --function Component_Declaration (Within : Data.Context) return Data.List.Item;
    --procedure Component_Declaration (Scope : Data.Unit_Handle);


    -- component_definition ::=
    --      [aliased] [not null] subtype_indication_part
    --      [aliased] [not null] access access_definition_part
    --
    function Component_Definition (Within : Data.Context) return Data_Handle;


    -- component_item ::=
    --      component_declaration
    --    | aspect_clause
    --
    -- coded in Declarative_Part


    -- component_list ::=
    --       component_item {component_item}
    --    | {component_item} variant_part
    --    |  null ;
    --
    -- coded in Declarative_Part


    -- composite_constraint ::=
    --      index_constraint
    --    | discriminant_constraint
    --
    -- coded in Subtype_Indication


    -- compound_statement ::=
    --      if_statement
    --    | case_statement
    --    | loop_statement
    --    | block_statement
    --    | accept_statement
    --    | select_statement
    --
    -- coded in Sequence_Of_Statements


    -- condition ::=
    --      boolean_expression
    --
    procedure Condition (Scope : Data.Unit_Handle) with Inline;


    -- conditional_entry_call ::=
    --      select
    --        entry_call_alternative
    --      else
    --        sequence_of_statements
    --      end select ;
    --
    -- coded in Select_Statement


    -- conditional_expression ::= if_expression | case_expression
    --
    -- coded in If_Expression and Case_Expression


    -- constraint ::=
    --      scalar_constraint
    --    | composite_constraint
    --
    -- coded in Subtype_Indication


    -- constrained_array_definition ::=
    --      array (discrete_subtype_definition {, discrete_subtype_definition}) of component_definition
    --
    -- coded in Array_Type_Definition


    -- context_clause ::=
    --      {context_item}
    --
    -- coded in Preprocessing_And_Context_Clause


    -- context_item ::=
    --      [private | limited [private]] with_clause
    --    | use_clause
    --
    -- coded in Preprocessing_And_Context_Clause


    -- decimal_fixed_point_definition ::=
    --      delta static_expression digits static_expression [real_range_specification]
    --
    --   coded in Declarative_Part


    -- declarative_item ::=
    --      basic_declarative_item
    --    | body
    -- coded in Declarative_Part


    -- declarative_part ::=
    --      {declarative_item}
    --
    procedure Declarative_Part (Scope    : Data.Unit_Handle;
                                Is_Basic : Boolean := False);


    -- declare_expression ::=
    --      declare {declare_item} begin expression
    --
    --    declare_item :== object_declaration | object_renaming_declaration
    --
    function Declare_Expression (Within : Data.Context) return Data_Handle;


    -- default_expression ::=
    --      expression
    --
    -- coded in Expression


    -- default_name ::=
    --      name
    --
    -- coded in Name_Of


    -- defining_character_literal ::=
    --      character_literal
    --
    -- coded in package Ada_95.Token


    -- defining_designator ::=
    --      defining_program_unit_name
    --    | defining_operator_symbol
    --
    -- coded in package Ada_95.Token (operator :== subtype of identifier)


    -- defining_identifier_list ::=
    --   defining_identifier {, defining_identifier}
    --
    function Defining_Identifier_List return Identifiers;
    function Next_Defining_Identifier_List return Identifiers;


    -- defining_operator_symbol ::=
    --      operator_symbol
    --
    -- coded in package Ada_95.Token


    -- defining_program_unit_name ::=
    --      [parent_unit_name . ]defining_identifier
    --
    -- coded in Defining_Identifier_List


    -- delay_alternative ::=
    --      delay_statement [sequence_of_statements]
    --
    -- coded in Select_Statement


    -- delay_relative_statement ::=
    --      delay delay_expression ;
    --
    -- coded in Delay_Statement;


    -- delay_statement ::=
    --      delay_until_statement
    --    | delay_relative_statement
    --
    procedure Delay_Statement (Scope : Data.Unit_Handle);


    -- delay_until_statement ::=
    --      delay until delay_expression ;
    --
    -- coded in Delay_Statement;


    -- delta_constraint ::=
    --      delta static_expression [range_constraint]
    --
    procedure Delta_Constraint (Within : Data.Context);


    -- derived_type_definition ::=
    --      [abstract] [limited] new parent_subtype_indication [[and interface_list] record_extension_part]
    --
    -- coded in Declarative_Part


    -- designator ::=
    --      [parent_unit_name . ]identifier
    --    | operator_symbol
    --
    -- coded in Library_Item_Or_Subunit


    -- digits_constraint ::=
    --      digits static_expression [range_constraint]
    --
    procedure Digits_Constraint (Within : Data.Context);


    -- direct_name ::=
    --      identifier
    --    | operator_symbol
    --
    -- coded in package Ada_95.Token (operator <=> subtype of identifier)


    -- discrete_choice ::=
    --      expression | discrete_range | others
    --
    -- coded in Discrete_Choice_List


    -- discrete_choice_list ::=
    --      discrete_choice { | discrete_choice}
    --
    function Discrete_Choice_List (Within : Data.Context) return Boolean;


    -- discrete_range ::=
    --      discrete_subtype_indication
    --    | range
    --
    function Discrete_Range (Within : Data.Context) return Data_Handle;

    function Discrete_Range_Or_Expression (Within : Data.Context) return Data_Handle;
    -- used in discrete_choice


    -- discrete_subtype_definition ::=
    --      discrete_subtype_indication
    --    | range
    --
    function Discrete_Subtype_Definition (Scope : Data.Unit_Handle) return Data_Handle;


    -- discriminant_association ::=
    --      [discriminant_selector_name { | discriminant_selector_name} => ] expression
    --
    -- coded in Subtype_Indication


    -- discriminant_constraint ::=
    --      ( discriminant_association { , discriminant_association} )
    --
    -- coded in Subtype_Indication


    -- discriminant_part ::=
    --      unknown_discriminant_part
    --    | known_discriminant_part
    --
    function Conditional_Discriminant_Part (Scope : Data.Unit_Handle) return Data.List.Item;


    -- discriminant_specification ::=
    --      defining_identifier_list : subtype_mark [ := default_expression]
    --    | defining_identifier_list : access_definition [ := default_expression]
    --
    -- coded in Known_Discriminant_Part


    -- entry_barrier ::=
    --      when condition
    --
    -- coded in Entry_Body


    -- entry_body ::=
    --      entry defining_identifier entry_body_formal_part entry_barrier is
    --        declarative_part
    --      begin
    --        handled_sequence_of_statements
    --      end [entry_identifier] ;
    --
    procedure Entry_Body (Scope : Data.Unit_Handle) with Inline;


    -- entry_body_formal_part ::=
    --      [ ( entry_index_specification ) ] parameter_profile
    --
    -- coded in Entry_Body


    -- entry_call_alternative ::=
    --      entry_call_statement [sequence_of_statements]
    --
    -- coded in Select_Statement


    -- entry_call_statement ::=
    --      entry_name [actual_parameter_part] ;
    --
    -- coded in Assignment_Call_Or_Code_Statement


    -- entry_declaration ::=
    --      [overriding_indicator]
    --      entry defining_identifier [(discrete_subtype_definition)] parameter_profile
    --      [aspect_specification] ;
    --
    -- coded in Declarative_Part


    -- entry_index ::=
    --      expression
    --
    -- coded in Accept_Statement


    -- entry_index_specification ::=
    --      for defining_identifier in discrete_subtype_definition
    --
    -- coded in Entry_Body


    -- enumeration_aggregate ::=
    --      array_aggregate
    --
    -- coded in Aggregate


    -- enumeration_representation_clause ::=
    --      for first_subtype_local_name use enumeration_aggregate ;
    --
    -- coded in Aspect_Clause


    -- enumeration_type_definition ::=
    --     ( enumeration_literal_specification { , enumeration_literal_specification} )
    --
    -- coded in Declarative_Part


    -- enumeration_literal_specification ::=
    --      defining_identifier
    --    | defining_character_literal
    --
    -- coded in Declarative_Part


    -- exception_choice ::=
    --      exception_name
    --    | others
    --
    -- coded in Handled_Sequence_Of_Statements


    -- exception_declaration ::=
    --      defining_identifier_list : exception;
    --
    -- coded in Declarative_Part


    -- exception_handler ::=
    --      when [choice_parameter_specification : ] exception_choice { | exception_choice} =>
    --        sequence_of_statements
    --
    -- coded in Handled_Sequence_Of_Statements


    -- exception_renaming_declaration ::=
    --     defining_identifier : exception renames exception_name ;
    --
    -- coded in Declarative_Part


    -- exit_statement ::=
    --      exit [loop_name] [when condition] ;
    --
    procedure Exit_Statement (Scope : Data.Unit_Handle);


    -- explicit_actual_parameter ::=
    --      expression
    --    | variable_name
    --
    -- coded in Actual_Parameter_Part


    -- explicit_dereference ::=
    --      name . all
    --
    -- coded in Name_Of


    -- explicit_generic_actual_parameter ::=
    --      expression
    --    | variable_name
    --    | subprogram_name
    --    | entry_name
    --    | subtype_mark
    --    | package_instance_name
    --
    -- coded in Conditional_Generic_Actual_Part


    -- expression ::=
    --      relation {and relation} | relation {and then relation}
    --    | relation {or relation}  | relation {or else relation}
    --    | relation {xor relation}
    --
    function Expression (Within : Data.Context) return Data_Handle;


    -- expression_function_declaration ::=
    --      [overriding_indicator]
    --      function_specification is
    --         ( expression )
    --         [aspect_specification] ;
    --
    -- coded in Declarative_Part


    -- extension_aggregate ::=
    --      ( ancestor_part with record_component_association_list )
    --
    -- coded in Aggregate


    -- factor ::=
    --      primary [** primary]
    --    | abs primary
    --    | not primary
    --
    -- coded in Simple_Expression


    -- first_bit ::=
    --      static_simple_expression
    --
    -- coded in Declarative_Part


    -- fixed_point_definition ::=
    --      ordinary_fixed_point_definition
    --    | decimal_fixed_point_definition
    --
    -- coded in Declarative_Part


    -- floating_point_definition ::=
    --      digits static_expression [real_range_specification]
    --
    -- coded in Declarative_Part


    -- for_loop_condition := loop_parameter_specification | iterator_specification
    --
    --    loop_parameter_specification ::=
    --         defining_identifier in [reverse] discrete_subtype_definition
    --
    --    iterator_specification ::=
    --         defining_identifier in [reverse] iterator_name
    --       | defining_identifier [ : subtype_indication] of [reverse] iterable_name
    --
    procedure For_Loop_Condition (Scope : Data.Unit_Handle);


    -- formal_access_type_definition ::=
    --      access_type_definition
    --
    -- coded in Generic_Formal_Part


    -- formal_array_type_definition ::=
    --      array_type_definition
    --
    -- coded in Generic_Formal_Part


    -- formal_decimal_fixed_point_definition ::=
    --      delta <> digits <>
    --
    -- coded in Generic_Formal_Part


    -- formal_derived_type_definition ::=
    --      [abstract] new subtype_mark [with private]
    --
    -- coded in Generic_Formal_Part


    -- formal_discrete_type_definition ::=
    --      (<>)
    --
    -- coded in Generic_Formal_Part


    -- formal_floating_point_definition ::=
    --      digits <>
    --
    -- coded in Generic_Formal_Part


    -- formal_modular_type_definition ::=
    --      mod <>
    --
    -- coded in Generic_Formal_Part


    -- formal_object_declaration ::=
    --      defining_identifier_list : mode subtype_mark [:= default_expression] ;
    --
    -- coded in Generic_Formal_Part


    -- formal_ordinary_fixed_point_definition ::=
    --      delta <>
    --
    -- coded in Generic_Formal_Part


    -- formal_package_actual_part ::=
    --      ( [others =>] <> )
    --    | [generic_actual_part]
    --    | (formal_package_association {, formal_package_association} [, others => <>])
    --
    -- coded in Generic_Formal_Part


    -- formal_package_association ::=
    --      generic_association | generic_formal_parameter_selector_name => <>
    --
    -- coded in Generic_Formal_Part


    -- formal_package_declaration ::=
    --      with package defining_identifier is new generic_package_name formal_package_actual_part ;
    --
    -- coded in Generic_Formal_Part


    -- formal_part ::=
    --      (parameter_specification { ; parameter_specification})
    --
    function Formal_Part (Scope : Data.Unit_Handle) return Data.List.Item with Inline;


    -- formal_private_type_definition ::=
    --      [[abstract] tagged] [limited] private
    --
    -- coded in Generic_Formal_Part


    -- formal_signed_integer_type_definition ::=
    --      range <>
    --
    -- coded in Generic_Formal_Part


    -- formal_subprogram_declaration ::=
    --      with subprogram_specification [is [abstract | [abstract] subprogram_default] [aspect_specification] ;
    --
    -- coded in Generic_Formal_Part


    -- formal_type_declaration ::=
    --      formal_complete_type_declaration
    --    | formal_incomplete_type_definition
    --
    -- coded in Generic_Formal_Part


    -- formal_complete_type_declaration ::=
    --      type defining_identifier [discriminant_part] is formal_type_definition [aspect_specification] ;
    --
    -- coded in Generic_Formal_Part


    -- formal_inomplete_type_declaration ::=
    --      type defining_identifier [discriminant_part] [is tagged] ;
    --
    -- coded in Generic_Formal_Part


    -- formal_type_definition ::=
    --      formal_private_type_definition
    --    | formal_derived_type_definition
    --    | formal_discrete_type_definition
    --    | formal_signed_integer_type_definition
    --    | formal_modular_type_definition
    --    | formal_floating_point_definition
    --    | formal_ordinary_fixed_point_definition
    --    | formal_decimal_fixed_point_definition
    --    | formal_array_type_definition
    --    | formal_access_type_definition
    --
    -- coded in Generic_Formal_Part


    -- full_type_declaration ::=
    --      type defining_identifier [known_discriminant_part] is type_definition [aspect_specification] ;
    --    | task_type_declaration
    --    | protected_type_declaration
    --
    -- coded in Declarative_Part


    -- function_call ::=
    --      name [actual_parameter_part]
    --
    -- coded in Name_Of


    -- general_access_modifier ::=
    --      all
    --    | constant
    --
    -- coded in Access_Type_Definition


    -- [generic_actual_part] ::=
    --      ( generic_association {, generic_association} )
    --
    function Conditional_Generic_Actual_Part (Generic_Unit      : Data.Unit_Handle;
                                              Scope             : Data.Unit_Handle;
                                              The_Instantiation : Data.Instantiation_Handle := null)
      return Data.List.Elements;


    -- generic_association ::=
    --      [generic_formal_parameter_selector_name =>] explicit_generic_actual_parameter
    --
    -- coded in Conditional_Generic_Actual_Part


    -- generic_declaration ::=
    --      generic_subprogram_declaration
    --    | generic_package_declaration
    --
    -- coded in Declarative_Part


    -- generic_formal_parameter_declaration ::=
    --      formal_object_declaration
    --    | formal_type_declaration
    --    | formal_subprogram_declaration
    --    | formal_package_declaration
    --
    -- coded in Generic_Formal_Part


    -- generic_formal_part ::=
    --      generic {generic_formal_parameter_declaration | use_clause}
    --
    function Generic_Formal_Part (Scope : Data.Unit_Handle) return Data.Formal_Block_Handle;


    -- generic_instantiation ::=
    --      package defining_program_unit_name is new generic_package_name [generic_actual_part] ;
    --    | procedure defining_program_unit_name is new generic_procedure_name [generic_actual_part] ;
    --    | function defining_designator is new generic_function_name [generic_actual_part] ;
    --
    -- coded in Library_Item_Or_Subunit


    -- generic_package_declaration ::=
    --      generic_formal_part  package_specification;
    --
    -- coded in Declarative_Part


    -- generic_renaming_declaration ::=
    --      generic package defining_program_unit_name renames generic_package_name ;
    --    | generic procedure defining_program_unit_name renames generic_procedure_name ;
    --    | generic function defining_program_unit_name renames generic_function_name ;
    --
    -- coded in Library_Item_Or_Subunit


    -- generic_subprogram_declaration ::=
    --      generic_formal_part  subprogram_specification [aspect_specification] ;
    --
    -- coded in Declarative_Part


    -- goto_statement ::=
    --      goto label_name ;
    --
    procedure Goto_Statement (Scope : Data.Unit_Handle);


    -- guard ::=
    --      when condition =>
    --
    -- coded in Select_Statement


    -- handled_sequence_of_statements ::=
    --        sequence_of_statements
    --      [exception
    --        exception_handler
    --      {exception_handler}]
    --
    procedure Handled_Sequence_Of_Statements (Scope           : Data.Unit_Handle;
                                              Is_In_Task_Body : Boolean := False);


    -- highest_precedence_operator ::=
    --      **
    --    | abs
    --    | not
    --
    -- coded in Simple_Expression


    -- if_expression ::=
    --      if condition then dependent_expression
    --        {elsif condition then dependent_expression}
    --        [else dependent_expression]
    --
    function If_Expression (Within : Data.Context) return Data_Handle;


    -- if_statement ::=
    --      if condition then
    --        sequence_of_statements
    --      [{elsif condition then
    --        sequence_of_statements}]
    --      end if;
    --
    procedure If_Statement (Scope : Data.Unit_Handle);


    -- incomplete_type_declaration ::=
    --      type defining_identifier [discriminant_part] ;
    --
    -- coded in Declarative_Part


    -- index_constraint ::=
    --      (discrete_range { , discrete_range})
    --
    -- coded in Subtype_Indication


    -- index_subtype_definition ::=
    --      subtype_mark range <>
    --
    -- coded in Array_Type_Definition


    -- indexed_component ::=
    --      name ( expression { , expression} )
    --
    -- coded in Name_Of


    -- interface_list ::= subtype_mark {and subtype_mark}
    --
    function Interface_List (Scope : Data.Unit_Handle) return Data.List.Item;


    -- interface_type_definition ::=
    --      [limited | task | protected | synchronized] interface [and interface_list]
    --
    -- coded in Declarative_Part


    -- integer_type_definition ::=
    --      signed_integer_type_definition
    --    | modular_type_definition
    --
    -- coded in Declarative_Part


    -- iteration_scheme ::=
    --      while condition
    --    | for iterator_specification
    --    | for loop_parameter_specification
    --
    -- coded in Loop_Statement


    -- iterator_filter ::=
    --      when condition
    --
    -- coded in Loop_Statement


    -- iterator_specification ::=
    --      defining_identifier in [reverse] iterator_name
    --      defining_identifier [ : subtype_indication] of [reverse] iterable_name
    --
    -- coded in Loop_Statement

    -- iterated_element_association ::=
    --      for loop_parameter_specification [use key_expression] => expression
    --    | for iterator_specification [use key_expression] => expression
    --
    function Iterated_Element_Association (Within : Data.Context) return Data_Handle;

    -- known_discriminant_part ::=
    --      ( discriminant_specification { ; discriminant_specification} )
    --
    function Known_Discriminant_Part (Scope : Data.Unit_Handle) return Data.List.Item with Inline;


    -- label ::=
    --      <<label_statement_identifier>>
    --
    procedure Label (Scope : Data.Unit_Handle);


    -- last_bit ::=
    --      static_simple_expression
    --
    -- coded in Declarative_Part


    -- library_item ::=
    --      [private] library_unit_declaration
    --    | library_unit_body
    --    | [private] library_unit_renaming_declaration
    -- coded in Library_Item_Or_Subunit


    -- library_unit_body ::=
    --      subprogram_body
    --    | package_body
    -- coded in Library_Item_Or_Subunit


    -- library_unit_declaration ::=
    --      subprogram_declaration
    --    | package_declaration
    --    | generic_declaration
    --    | generic_instantiation
    -- coded in Library_Item_Or_Subunit


    -- library_unit_renaming_declaration ::=
    --      package_renaming_declaration
    --    | generic_renaming_declaration
    --    | subprogram_renaming_declaration
    --
    -- coded in Library_Item_Or_Subunit


    -- local_name ::=
    --      direct_name
    --    | direct_name ' attribute_designator
    --    | library_unit_name
    --
    -- coded in Declarative_Part


    -- logical_operator ::=
    --      and
    --    | or
    --    | xor
    --
    -- coded in Simple_Expression


    -- loop_parameter_specification ::=
    --      defining_Identifier in [reverse] discrete_suptype_definition [iterator_filter]
    --
    -- coded in For_Loop_Condition


    -- loop_statement ::=
    --      [loop_statement_identifier : ]
    --        [iteration_scheme] loop
    --          sequence_of_statements
    --        end loop [loop_identifier] ;
    --
    procedure Loop_Statement (Scope  : Data.Unit_Handle;
                              Has_Id : Boolean := False);

    procedure While_Loop_Statement (Scope  : Data.Unit_Handle;
                                    Has_Id : Boolean := False);

    procedure For_Loop_Statement (Parent : Data.Unit_Handle;
                                  Id     : Identifier_Handle := null);

    -- mod_clause ::=
    --      at mod static_expression ;
    --
    -- coded in Declarative_Part


    -- mode ::=
    --      [in]
    --    | in out
    --    | out
    --
    procedure Mode with Inline;


    -- modular_type_definition ::=
    --      mod static_expression
    --
    -- coded in Type_Declaration


    -- multiplying_operator ::=
    --      *
    --    | /
    --    | mod
    --    | rem
    --
    -- coded in Simple_Expression


    -- name ::=
    --      direct_name
    --    | explicit_dereference
    --    | indexed_component
    --    | slice
    --    | selected_component
    --    | attribute_reference
    --    | type_conversion
    --    | function_call
    --    | character_literal
    --    | qualified_expression
    --    | target_name
    --
    function Name_Of (Within                : Data.Context;
                      Procedure_Allowed     : Boolean := False;
                      No_Association        : Boolean := False;
                      Is_Subtype_Mark       : Boolean := False;
                      Is_Subtype_Indication : Boolean := False) return Data_Handle;

    -- named_array_aggregate ::=
    --      ( array_component_association { , array_component_association} )
    --
    -- coded in Aggregate


    -- number_declaration ::=
    --      defining_identifier_list : constant := static_expression ;
    --
    -- coded in Declarative_Part


    -- [not null]
    --
    procedure Conditional_Null_Exclusion;


    -- null_statement ::=
    --      null ;
    --
    -- coded in package Ada_95.Token


    -- object_declaration ::=
    --     defining_identifier_list : [aliased] [constant]
    --       (subtype_indication | access_definition | array_type_definition) [ := expression] [aspect_specification] ;
    --   | single_task_declaration
    --   | single_protected_declaration
    --
    -- coded in Declarative_Part


    -- object_renaming_declaration ::=
    --      defining_identifier : [null_exclusion] subtype_mark renames object_name [aspect_specification] ;
    --   |  defining_identifier : access_definition renames object_name [aspect_specification] ;
    --
    -- coded in Declarative_Part


    -- ordinary_fixed_point_definition ::=
    --      delta static_expression  real_range_specification
    --
    -- coded in Declarative_Part


    -- operator_symbol ::=
    --      string_literal
    --
    -- coded in package Ada_95.Token


    -- Overriding_Indicator ::=
    --     [ not ] overriding
    function Has_Overriding_Indicator return Boolean;


    -- package_body ::=
    --      package body defining_program_unit_name [aspect_specification] is
    --        declarative_part
    --      [begin
    --        handled_sequence_of_statements]
    --      end [[parent_unit_name.]identifier] ;
    procedure Package_Body (Self : Data.Unit_Handle);


    -- package_body_stub ::=
    --      package body defining_identifier is separate ;
    --
    -- coded in package_body


    -- package_declaration ::=
    --      package_specification ;
    --
    -- coded in Library_Item_Or_Subunit


    -- package_renaming_declaration ::=
    --     package defining_program_unit_name renames package_name ;
    --
    -- coded in Library_Item_Or_Subunit


    -- package_specification ::=
    --      package defining_program_unit_name [aspect_specification] is
    --        {basic_declarative_item}
    --      [private
    --        {basic_declarative_item}]
    --      end [[parent_unit_name.]identifier]
    --
    procedure Package_Specification (Self : Data.Unit_Handle);


    -- parameter_association ::=
    --      [formal_parameter_selector_name =>] explicit_actual_parameter
    --
    -- coded in Actual_Parameter_Part


    -- parameter_and_result_profile ::=
    --      [formal_part] return ([null_exclusion] subtype_mark) | access_definition
    --
    function Subprogram_Profile (Scope       : Data.Unit_Handle;
                                 Is_Function : Boolean := False) return Data.Subprogram_Profile;


    -- parameter_profile ::=
    --      [formal_part]
    --
    -- coded in Subprogram_Profile


    -- parameter_specification ::=
    --      defining_identifier_list : [aliased] mode [null_exlusion] subtype_mark [ := default_expression]
    --    | defining_identifier_list : access_definition [ := default_expression]
    --
    function Parameter_Specification (Scope : Data.Unit_Handle) return Data.List.Item;


    -- position ::=
    --      static_expression
    --
    -- coded in Declarative_Part


    -- positional_array_aggregate ::=
    --      ( expression , expression { , expression} )
    --    | ( expression { , expression} , others => expression )
    --
    -- coded in Aggregate


    -- pragma ::=
    --    pragma identifier [ ( pragma_argument_association { , pragma_argument_association} ) ] ;
    --
    procedure Pragma_Call (Scope : Data.Unit_Handle);


    -- pragma_argument_association ::=
    --      [pragma_argument_identifier => ] name
    --    | [pragma_argument_identifier => ] expression
    --
    -- coded in Pragma_Call


    -- primary ::=
    --      numeric_literal
    --    | null
    --    | string_literal
    --    | aggregate
    --    | name
    --    | allocator
    --    | (expression)
    --    | (conditional_expression)
    --    | (quantified_expression)
    --    | (declare_expression)
    --
    -- coded in Simple_Expression


    -- private_extension_declaration ::=
    --      type defining_identifier [discriminant_part] is
    --        [abstract] [limited | synchronized] new ancestor_subtype_indication
    --        [and interface_List] with private
    --        [aspect_specification] ;
    --
    -- coded in Declarative_Part


    -- private_type_declaration ::=
    --      type defining_identifier [discriminant_part] is [[abstract] tagged] [limited] private
    --        [aspect_specification] ;
    --
    -- coded in Declarative_Part


    -- procedure_call_statement ::=
    --      name [actual_parameter_part] ;
    --
    -- coded in Assignment_Block_Call_Or_Code_Statement


    -- proper_body ::=
    --      subprogram_body
    --    | package_body
    --    | task_body
    --    | protected_body
    --
    -- coded in Declarative_Part & Subunit


    -- protected_body ::=
    --     protected body defining_identifier is
    --       { protected_operation_item }
    --     end [protected_identifier] ;
    --
    procedure Protected_Body (Scope : Data.Unit_Handle);


    -- protected_body_stub ::=
    --      protected body defining_identifier is separate ;
    --
    -- coded in protected_body


    -- protected_definition ::=
    --        { protected_operation_declaration }
    --      [ private
    --        { protected_element_declaration } ]
    --      end [protected_identifier]
    --
    -- coded in Declarative_Part


    -- protected_element_declaration ::=
    --      protected_operation_declaration
    --    | component_declaration
    --
    -- coded in Declarative_Part


    -- protected_operation_declaration ::=
    --      subprogram_declaration
    --    | entry_declaration
    --    | aspect_clause
    --
    -- coded in Declarative_Part


    -- protected_operation_item ::=
    --      subprogram_declaration
    --    | subprogram_body
    --    | null_procedure_declaration
    --    | expression_function_declaration
    --    | entry_body
    --    | aspect_clause
    --
    -- coded in Protected_Body


    -- protected_type_declaration ::=
    --      protected type defining_identifier [known_discriminant_part] [aspect_specification]
    --        is [new interface_list with] protected_definition ;
    --
    -- coded in Declarative_Part


    -- qualified_expression ::=
    --      subtype_mark ' ( expression )
    --    | subtype_mark ' aggregate
    --
    -- coded in Allocator or Name


    -- quantified_expression ::=
    --      quantifier for_loop_condition => predicate
    --
    --    quantifier :== all | some
    --
    function Quantified_Expression (Within : Data.Context) return Data_Handle;


    -- raise_expression ::=
    --      raise exception_name [with string_expression]
    --
    function Raise_Expression (Within : Data.Context) return Data_Handle;


    -- raise_statement ::=
    --      raise [exception_name [with string_expression]] ;
    --
    procedure Raise_Statement (Scope : Data.Unit_Handle);


    -- range ::=
    --      range_attribute_reference
    --    | simple_expression .. simple_expression
    function Range_Production (Within : Data.Context) return Data_Handle;


    -- range_attribute_designator ::=
    --      range [ ( static_expression ) ]
    --
    -- coded in Range_Production


    -- range_attribute_reference ::=
    --      name ' range_attribute_designator
    --
    -- coded in Range_Production


    -- range_constraint ::=
    --      range range
    --
    function Range_Constraint (Within : Data.Context) return Data_Handle;


    -- real_range_specification ::=
    --      range static_simple_expression .. static_simple_expression
    --
    -- coded in Declarative_Part


    -- real_type_definition ::=
    --      floating_point_definition
    --    | fixed_point_definition
    --
    -- coded in Declarative_Part


    -- record_aggregate ::=
    --      ( record_component_association_list )
    --
    -- coded in Aggregate


    -- record_component_association ::=
    --       [ component_choice_list => ] expression
    --     | component_choice_list => <>
    --
    -- coded in Aggregate


    -- record_component_association_list ::=
    --       record_component_association { , record_component_association}
    --     | null record
    --
    -- coded in Aggregate


    -- record_definition ::=
    --      record component_list end record
    --    | null record
    --
    -- coded in Declarative_Part


    -- record_extension_part ::=
    --      with record_definition
    --
    -- coded in Declarative_Part


    -- record_representation_clause ::=
    --      for first_subtype_local_name use record [mod_clause]
    --        {component_clause}
    --      end record ;
    --
    -- coded in Aspect_Clause


    -- record_type_definition ::=
    --      [[abstract] tagged] [limited] record_definition
    --
    -- coded in Declarative_Part


    -- relation ::=
    --      simple_expression [relational_operator simple_expression]
    --    | simple_expression [not] in membership_choice_list
    --    | raise_expression
    --
    -- coded in Expression


    -- relational_operator ::=
    --      =
    --    | /=
    --    | <
    --    | <=
    --    | >
    --    | >=
    --
    -- coded in Expression


    -- renaming_declaration ::=
    --      object_renaming_declaration
    --    | exception_renaming_declaration
    --    | package_renaming_declaration
    --    | subprogram_renaming_declaration
    --    | generic_renaming_declaration
    --
    -- coded in Declarative_Part


    -- reduction_attribute_reference ::=
    --      value_sequence ' reduction_attribute_designator
    --    | prefix ' reduction_attribute_designator
    --
    -- value_Sequence ::=
    --      [ [parallel [(chunk_specification)] [aspect_specification]] iterated_element_association ]
    --
    -- chunk_specification ::=
    --      integer_simple_expression
    --    | defining_identifier in descrete_subtype_definition
    --
    -- iterated_element_association ::=
    --      for loop_parameter_specification [use key_expression] => expression
    --    | for iterator_specification [use key_expression] => expression
    --
    function Reduction_Attribute_Reference (Within : Data.Context) return Data_Handle;


    -- requeue_statement ::=
    --      requeue entry_name [with abort] ;
    --
    procedure Requeue_Statement (Scope : Data.Unit_Handle);


    -- return_statement ::=
    --      simple_return_statement
    --    | extended_return_statement
    --
    --    simple_return_statement ::=
    --         return [expression] ;
    --
    --    extended_return_statement ::=
    --         return extended_return_object_declaration [do handled_sequence_of_statements end return] ;
    --
    --       extended_return_object_declaration ::=
    --            defining_identifier : [aliased] [constant] return_subtype_indication [ := expression]
    --
    procedure Return_Statement (Scope : Data.Unit_Handle);


    -- scalar_constraint ::=
    --      range_constraint
    --    | digits_constraint
    --    | delta_constraint
    --
    -- coded in Subtype_Indication


    -- select_alternative ::=
    --      accept_alternative
    --    | delay_alternative
    --    | terminate_alternative
    --
    -- coded in Select_Statement


    -- select_statement ::=
    --      selective_accept
    --    | timed_entry_call
    --    | conditional_entry_call
    --    | asynchronous_select
    --
    procedure Select_Statement (Scope : Data.Unit_Handle);


    -- selected_component ::=
    --      name . selector_name
    --
    -- coded in Name_Of


    -- selective_accept ::=
    --      select [guard]
    --        select_alternative
    --      { or [guard]
    --        select_alternative }
    --      [ else
    --        sequence_of_statements ]
    --      end select ;
    --
    -- coded in Select_Statement


    -- selector_name ::=
    --      identifier
    --    | character_literal
    --    | operator_symbol
    --
    function Selector_Name return Identifier_Handle;


    -- sequence_of_statements ::=
    --      statement {statement}
    --
    procedure Sequence_Of_Statements (Scope          : Data.Unit_Handle;
                                      Is_Conditional : Boolean := False);


    -- signed_integer_type_definition ::=
    --      range static_simple_expression .. static_simple_expression
    --
    -- coded in Type_Declaration


    -- single_protected_declaration ::=
    --      protected defining_identifier is protected_definition;
    --
    -- coded in Declarative_Part


    -- single_task_declaration ::=
    --      task defining_identifier [is task_definition] ;
    --
    -- coded in Declarative_Part


    -- simple_expression ::=
    --      [unary_adding_operator] term {binary_adding_operator term}
    function Simple_Expression (Within : Data.Context) return Data_Handle;


    -- simple_statement ::=
    --      null_Statement
    --    | assignment_statement
    --    | exit_statement
    --    | goto_statement
    --    | procedure_call_statement
    --    | return_statement
    --    | entry_call_statement
    --    | requeue_statement
    --    | delay_statement
    --    | abort_statement
    --    | raise_statement
    --    | code_statement
    --
    -- coded in Sequence_Of_Statements


    -- slice ::=
    --      name ( discrete_range )
    --
    -- coded in Name_Of


    -- statement ::=
    --      {label} simple_statement
    --    | {label} compound_statement
    --
    -- coded in Sequence_Of_Statements


    -- statement_identifier ::=
    --      direct_name
    --
    -- coded in package Ada_95.Token (operator <=> subtype of identifier)


    -- subprogram_body ::=
    --      subprogram_specification is
    --        declarative_part
    --      begin
    --        handled_sequence_of_statements
    --      end [designator];
    --
    procedure Subprogram_Body (Self          : Data.Unit_Handle;
                               Is_Overriding : Boolean := False);


    -- subprogram_body_stub ::=
    --      subprogram_specification is separate ;
    --
    -- coded in subprogram_body


    -- subprogram_declaration ::=
    --      [overriding_indicator]
    --      subprogram_specification
    --      [aspect_specification] ;
    --
    -- coded in Library_Item_Or_Subunit or Declarative_Part


    -- subprogram_default ::=
    --      default_name
    --    | <>
    --    | null
    --
    -- coded in Generic_Formal_Part


    -- subprogram_renaming_declaration ::=
    --     subprogram_specification renames callable_entity_name ;
    --
    -- coded in Library_Item_Or_Subunit


    -- subprogram_specification ::=
    --      procedure defining_program_unit_name  parameter_profile
    --    | function defining_designator  parameter_and_result_profile
    --
    -- coded in Library_Item_Or_Subunit


    -- subtype_declaration ::=
    --    subtype defining_identifier is subtype_indication [aspect_specification] ;
    --
    -- coded in basic_declaration


    -- subtype_indication ::=
    --      [null_exlusion] subtype_indication_part
    --
    function Subtype_Indication (Within : Data.Context) return Data_Handle with Inline;


    -- subtype_indication_part ::=
    --      subtype_mark [constraint]
    --
    function Subtype_Indication_Part (Within : Data.Context) return Data_Handle with Inline;


    -- subtype_mark ::=
    --      subtype_name
    --
    function Subtype_Mark (Scope                 : Data.Unit_Handle;
                           Is_Subtype_Indication : Boolean := False) return Data_Handle with Inline;


    -- subunit ::=
    --      separate (parent_unit_name) proper_body
    --
    --    proper_body ::=
    --         subprogram_body
    --       | package_body
    --       | task_body
    --       | protected_body
    --
    -- coded in compilation_unit


    -- task_body ::=
    --      task body defining_identifier is
    --        declarative_part
    --      begin
    --        handled_sequence_of_statements
    --      end [task_identifier] ;
    --
    procedure Task_Body (Scope : Data.Unit_Handle);


    -- task_body_stub ::=
    --      task body defining_identifier is separate ;
    --
    -- coded in task_body


    -- task_definition ::=
    --        {task_item}
    --      [ private
    --        {task_item}]
    --      end [task_identifier]
    --
    -- coded in Declarative_Part


    -- task_item ::=
    --      entry_declaration
    --    | aspect_clause
    --
    -- coded in Declarative_Part


    -- task_type_declaration ::=
    --      task type defining_identifier [known_discriminant_part] [aspect_specification] [is task_definition] ;
    --
    -- coded in Declarative_Part


    -- term ::=
    --      factor {multiplying_operator factor}
    --
    -- coded in Simple_Expression


    -- terminate_alternative ::=
    --      terminate ;
    --
    -- coded in Select_Statement


    -- timed_entry_call ::=
    --      select
    --        entry_call_alternative
    --      or
    --        delay_alternative
    --      end select ;
    --
    -- coded in Select_Statement


    -- triggering_alternative ::=
    --      triggering_statement [sequence_of_statements]
    --
    -- coded in Select_Statement


    -- triggering_statement ::=
    --       entry_call_statement
    --    | delay_statement
    --
    -- coded in Select_Statement


    -- type_conversion ::=
    --      subtype_mark ( expression )
    --    | subtype_mark ( name )
    --
    -- coded in Name_Of


    -- type_declaration ::=
    --      full_type_declaration
    --    | incomplete_type_declaration
    --    | private_type_declaration
    --    | private_extension_declaration
    --
    -- coded in Declarative_Part


    -- type_definition ::=
    --      enumeration_type_definition
    --    | integer_type_definition
    --    | real_type_definition
    --    | array_type_definition
    --    | record_type_definition
    --    | access_type_definition
    --    | derived_type_definition
    --    | interface_type_definition
    --
    -- coded in Declarative_Part


    -- unary_adding_operator ::=
    --      +
    --    | -
    --
    -- coded in Simple_Expression


    -- unconstrained_array_definition ::=
    --      array ( index_subtype_definition { , index_subtype_definition} ) of component_definition
    --
    -- coded in Array_Type_Definition


    -- unknown_discriminant_part ::=
    --      ( <> )
    --
    -- coded in Conditional_Discriminant_Part


    -- use_clause ::=
    --      use_package_clause
    --    | use_type_clause
    --
    procedure Use_Clause (Scope      : Data.Unit_Handle;
                          In_Context : Boolean := False);


    -- use_package_clause ::=
    --      use package_name { , package_name} ;
    --
    procedure Use_Package_Clause (Scope      : Data.Unit_Handle;
                                  In_Context : Boolean);


    -- use_type_clause ::=
    --      use [all] type subtype_mark { , subtype_mark} ;
    --
    procedure Use_Type_Clause (Scope   : Data.Unit_Handle;
                               Use_All : Boolean := False);


    -- variant ::=
    --      when discrete_choice_list => component_list
    --
    -- coded in Declarative_Part


    -- variant_part ::=
    --      case discriminant_direct_name is
    --          variant
    --         {variant}
    --      end case;
    --
    -- coded in Declarative_Part


    --  with_clause ::=
    --       with library_unit_name { , library_unit_name} ;
    procedure With_Clause (Is_Private : Boolean := False) with Inline;

  ----------------------------------------------------------------------------------------------------------------------

    use type Lexical.Element;

    Reported_Error : exception;

    Resource : constant Data.Resource_Handle := Data.Resource (Unit.all);

    Is_In_Standard : constant Boolean := Resource.Element.Is_Standard;

    The_Token : Lexical_Handle;
    The_Style : Lexical.Style_Pragma;
    Unused_Id : Identifier_Handle;

    procedure Report_Error (Item     : Error.Kind;
                            At_Token : Lexical_Handle) with No_Return is
    begin
      Token.Mark_Error (Item, At_Token, Resource.Tokens);
      raise Reported_Error;
    end Report_Error;

    procedure Style_Error (The_Error : Error.Incorrect_Style;
                           At_Token  : Lexical_Handle) is
    begin
      Report_Error (The_Error, At_Token);
    end Style_Error;

    procedure Conditional_Style_Error (The_Error       : Error.Incorrect_Style;
                                       The_Error_Token : Lexical_Handle := The_Token) is
    begin
      if Checker.Has_Style (The_Style) then
        Style_Error (The_Error, The_Error_Token);
      end if;
    end Conditional_Style_Error;

    procedure Style_Error_If_Real_Time (The_Error       : Error.Incorrect_Style;
                                        The_Error_Token : Lexical_Handle := The_Token) is
    begin
      if Checker.Is_Real_Time (The_Style) then
        Style_Error (The_Error, The_Error_Token);
      end if;
    end Style_Error_If_Real_Time;

    procedure Style_Error_If_Restricted (The_Error       : Error.Incorrect_Style;
                                         The_Error_Token : Lexical_Handle := The_Token) is
    begin
      if Checker.Is_Restricted (The_Style) then
        Style_Error (The_Error, The_Error_Token);
      end if;
    end Style_Error_If_Restricted;

    procedure Syntax_Error with No_Return is
    begin
      Report_Error (Error.Syntax_Error, The_Token);
    end Syntax_Error;

    procedure Not_Implemented (Message : String) is
    begin
      Log.Write ("!!! " & Message);
      Report_Error (Error.Not_Implemented, The_Token);
    end Not_Implemented;

    function End_Of_File return Boolean with Inline is
    begin
      return The_Token = null;
    end End_Of_File;

    procedure Check_No_End_Of_File with Inline is
    begin
      if End_Of_File then
        Report_Error (Error.End_Of_File_Error, null);
      end if;
    end Check_No_End_Of_File;

    procedure Get_Next_Token with Inline is
    begin
      Check_No_End_Of_File;
      The_Token := Lexical_After (The_Token);
      --TEST-----------------------------------------------
      --if End_Of_File then
      --  Write_Log ("TOKEN: NONE");
      --else
      --  Write_Log ("TOKEN: " & Image_Of (The_Token.all));
      --end if;
      -----------------------------------------------------
    end Get_Next_Token;

    function Next_Token return Lexical_Handle with Inline is
    begin
      Get_Next_Token;
      return The_Token;
    end Next_Token;

    function Token_Element return Lexical.Element with Inline is
    begin
      Check_No_End_Of_File;
      return The_Token.Element;
    end Token_Element;

    function Element_Is (Element : Lexical.Element) return Boolean with Inline is
    begin
      if not End_Of_File and then The_Token.Element = Element then
        Get_Next_Token;
        return True;
      else
        return False;
      end if;
    end Element_Is;

    function Next_Element_Is (Element : Lexical.Element) return Boolean with Inline is
    begin
      Get_Next_Token;
      return Element_Is (Element);
    end Next_Element_Is;

    function Parsed_Element_Is (Element : Lexical.Element) return Boolean with Inline is
    begin
      if Token_Element = Element then
        Get_Next_Token;
        return True;
      else
        Get_Next_Token;
        return False;
      end if;
    end Parsed_Element_Is;

    function Element_Ahead_Is (Element : Lexical.Element) return Boolean with Inline is
      Current_Token : constant Lexical_Handle := The_Token;
    begin
      Get_Next_Token;
      if not End_Of_File and then The_Token.Element = Element then
        The_Token := Current_Token;
        return True;
      else
        The_Token := Current_Token;
        return False;
      end if;
    end Element_Ahead_Is;

    function Next_Element_Ahead_Is (Element : Lexical.Element) return Boolean with Inline is
      Current_Token : constant Lexical_Handle := The_Token;
    begin
      Get_Next_Token;
      if not End_Of_File and then The_Token.Element = Element then
        return True;
      else
        The_Token := Current_Token;
        return False;
      end if;
    end Next_Element_Ahead_Is;

    procedure Check (Element : Lexical.Element) with Inline is
    begin
      if Token_Element /= Element then
        Syntax_Error;
      end if;
    end Check;


    The_Actual_Identifier : Identifier_Handle;

    procedure Check_Identifier with Inline is
    begin
      Check (Lexical.Identifier);
      The_Actual_Identifier := Identifier_Handle(The_Token);
    end Check_Identifier;

    function Style_Checked (Item : Identifier_Handle) return Identifier_Handle with Inline is
    begin
      if not Checker.Is_Ok (Item, The_Style) then
        Style_Error (Error.Incorrect_Identifier, Lexical_Handle(Item));
      end if;
      return Item;
    end Style_Checked;

    procedure Check_Declaring_Identifier with Inline is
    begin
      Check (Lexical.Identifier);
      The_Actual_Identifier := Style_Checked(Identifier_Handle(The_Token));
    end Check_Declaring_Identifier;


    procedure Get_Element (Item : Lexical.Element) with Inline is
    begin
      Check (Item);
      Get_Next_Token;
    end Get_Element;

    procedure Get_Next_Element (Item : Lexical.Element) with Inline is
    begin
      Get_Next_Token;
      Get_Element (Item);
    end Get_Next_Element;

    procedure Get_Conditional (Element : Lexical.Element) with Inline is
    begin
      if not End_Of_File and then The_Token.Element = Element then
        Get_Next_Token;
      end if;
    end Get_Conditional;

    procedure Get_Next_Conditional (Element : Lexical.Element) with Inline is
    begin
      Get_Next_Token;
      Get_Conditional (Element);
    end Get_Next_Conditional;

    function Actual_Identifier return Identifier_Handle with Inline is
    begin
      Check_Identifier;
      Get_Next_Token;
      return The_Actual_Identifier;
    end Actual_Identifier;

    function Declaring_Identifier return Identifier_Handle with Inline is
    begin
      Check_Declaring_Identifier;
      Get_Next_Token;
      return The_Actual_Identifier;
    end Declaring_Identifier;

    function Next_Identifier return Identifier_Handle with Inline is
    begin
      Get_Next_Token;
      return Actual_Identifier;
    end Next_Identifier;

    function Next_Declaring_Identifier return Identifier_Handle with Inline is
    begin
      Get_Next_Token;
      return Declaring_Identifier;
    end Next_Declaring_Identifier;

    function Token_Identifiers (Separator : Lexical.Element;
                                Declaring : Boolean := False) return Identifiers is
      Start_Token : constant Lexical_Handle := The_Token;
      The_Length  : Positive := 1;
    begin
      Get_Next_Token;
      loop
        exit when not Element_Is (Separator) or else not Element_Is (Lexical.Identifier);
        The_Length := The_Length + 1;
      end loop;
      declare
        The_List  : Identifiers(1..The_Length);
        The_Index : Positive := The_List'first;
      begin
        The_Token := Start_Token;
        loop
          if Declaring then
            The_List(The_Index) := Declaring_Identifier;
          else
            The_List(The_Index) := Actual_Identifier;
          end if;
          exit when The_Index = The_List'last;
          Get_Next_Token;
          The_Index := Positive'succ(The_Index);
        end loop;
        return The_List;
      end;
    end Token_Identifiers;

    function Name_Of (Scope : Data.Unit_Handle) return Data_Handle is
    begin
      return Name_Of (Within => (Scope, null));
    end Name_Of;

    function Next_Name_Of (Scope : Data.Unit_Handle) return Data_Handle is
    begin
      Get_Next_Token;
      return Name_Of (Scope);
    end Next_Name_Of;

    function Unit_Name return Identifiers with Inline is
    begin
      return Token_Identifiers (Lexical.Period);
    end Unit_Name;

    function Next_Unit_Name return Identifiers with Inline is
    begin
      Get_Next_Token;
      return Unit_Name;
    end Next_Unit_Name;

    procedure Get_Next_Unit_Name with Inline is
    begin
      Get_Next_Token;
      declare
        Unused : constant Identifiers := Unit_Name;
      begin
        null;
      end;
    end Get_Next_Unit_Name;

    procedure Set_Used (Item : Identifier_Handle;
                        To   : Boolean := True) is
      The_Handle : constant Data_Handle := Item.Data;
    begin
      if The_Handle /= null and then The_Handle.all in Data.Declaration_Type'class then
        Data.Declaration_Handle(The_Handle).Is_Used := To;
      end if;
    end Set_Used;

    procedure Set_Used (Items: Identifiers) is
    begin
      for Item of Items loop
        Set_Used (Item);
      end loop;
    end Set_Used;

    procedure Get_From (Scope             :     Data.Unit_Handle;
                        The_Unit          : out Data.Unit_Handle;
                        The_Instantiation : out Data.Instantiation_Handle) is

      The_Item : Data_Handle := Next_Name_Of (Scope);

    begin
      if The_Item /= null then
        if The_Item.all in Data.Instantiated_Item'class then
          The_Instantiation := Data.Item_Instantiation(The_Item).Instantiation;
          The_Item := Data.Item_Instantiation(The_Item).Item;
        end if;
        if not (The_Item.all in Data.Unit_Type'class) then
          The_Item := null;
        end if;
      end if;
      The_Unit := Data.Unit_Handle(The_Item);
    end Get_From;

  ----------------------------------------------------------------------------------------------------------------------

    procedure Check_Designator (Item : Data.Unit_Handle) is

      Location : constant Lexical_Handle := The_Token;
      The_Unit : constant Token.Identifier_Handle := Actual_Identifier;

    begin
      if The_Unit = Item.Location then
        Data.New_End_Identifier (The_Unit, Data_Handle(Item));
      else
        Report_Error (Error.Designator_Expected, Location);
      end if;
    end Check_Designator;


    -- end ( [designator] | [[parent_unit_name . ]identifier] ) ;
    --
    procedure Check_End (Item : Data.Unit_Handle) is
    begin
      Get_Element (Lexical.Is_End);
      case Token_Element is
      when Lexical.Identifier =>
        if Data.Is_Library (Item) then
          declare
            Id    : constant Identifiers := Data.Library_Id (Item);
            Units : constant Identifiers := Unit_Name;
          begin
            if Units'length <= Id'length then
              for The_Index in Units'range loop
                declare
                  The_Unit : constant Identifier_Handle := Id(Id'last - Units'last + The_Index);
                begin
                  if not Is_Null (The_Unit) and then not Is_Null (Units(The_Index)) and then
                    Units(The_Index) = The_Unit
                  then
                    Data.New_End_Identifier (Units(The_Index), The_Unit.Data);
                  else
                    Report_Error (Error.Designator_Expected, Lexical_Handle(Units(The_Index)));
                  end if;
                end;
              end loop;
            end if;
          end;
        else -- designator
          Check_Designator (Item);
        end if;
        Get_Element (Lexical.Semicolon);
      when Lexical.Semicolon =>
        Get_Next_Token;
      when others =>
        Syntax_Error;
      end case;
    end Check_End;


    --      end [identifier]
    --
    procedure Check_Declaration_End (Item : Data.Unit_Handle) is
    begin
      Get_Element (Lexical.Is_End);
      if not End_Of_File and then The_Token.Element = Lexical.Identifier then
        if Item.Location = Identifier_Handle(The_Token) then
          Data.New_End_Identifier (Identifier_Handle(The_Token), Item.Location.Data);
          Get_Next_Token;
        else
          Report_Error (Error.Designator_Expected, The_Token);
        end if;
      end if;
    end Check_Declaration_End;


    -- aspect_specification ::=
    --      with aspect_mark [=> aspect_definition] {,aspect_mark [=> aspect_definition] }
    --
    --    aspect_definition ::= name | expression | identifier | aggregate | global_aspect_definition
    --
    function Aspects_Elements (Within : Data.Context) return Data.Aspect_Specification is
      The_Aspects : Data.Aspect_Specification (1..16);
      The_Last    : Natural := The_Aspects'first - 1;

    begin
      loop
        The_Last := @ + 1;
        The_Aspects(The_Last).Mark := Aspect_Mark;
        if Element_Is (Lexical.Association) then
          case The_Aspects(The_Last).Mark is
          when Lexical.Is_Convention =>
            declare
              Id         : constant Identifier_Handle := Actual_Identifier;
              Convention : constant String := Image_Of (Id.all);
              function "=" (Left, Right : String) return Boolean renames Ada.Strings.Equal_Case_Insensitive;
            begin
              if Convention = "arm" or
                 Convention = "c" or
                 Convention = "c_pass_by_copy" or
                 Convention = "intrinsic" or
                 Convention = "stdcall"
              then
                Id.Data := Data.Predefined_Name;
              end if;
            end;
          when Lexical.Is_Iterable | Lexical.Is_Aggregate =>
            Get_Element (Lexical.Left_Parenthesis);
            loop
              The_Aspects(The_Last).Mark := Aspect_Mark;
              Get_Element (Lexical.Association);
              The_Aspects(The_Last).Definition := Actual_Identifier;
              exit when not Element_Is (Lexical.Comma);
              The_Last := @ + 1;
            end loop;
            Get_Element (Lexical.Right_Parenthesis);
          when Lexical.Is_Abstract_State
             | Lexical.Is_Annotate
             | Lexical.Is_Default_Initial_Condition
             | Lexical.Is_Exceptional_Cases
             | Lexical.Is_Global
             | Lexical.Is_Initializes
             | Lexical.Is_Initial_Condition
             | Lexical.Is_Integer_Literal
             | Lexical.Is_Part_Of
             | Lexical.Is_Post
             | Lexical.Is_Pre
             | Lexical.Is_Put_Image
             | Lexical.Is_Spark_Mode
          =>
            declare
              Start_Token : constant Lexical_Handle := The_Token;
              Last_Token  : Lexical_Handle;
            begin
              Dummy := Expression (Within);
              Last_Token := The_Token;
              The_Token := Start_Token;
              while The_Token /= Last_Token loop
                if The_Token.Element = Lexical.Identifier then
                  if Identifier_Handle(The_Token).Data = null then
                    Identifier_Handle(The_Token).Data := Data.Predefined_Name;
                  end if;
                end if;
                Get_Next_Token;
              end loop;
            end;
          when others =>
            Dummy := Expression (Within);
            if Dummy = null then
              The_Actual_Identifier.Data := Data.Discriminant_Of (The_Actual_Identifier, Within.Sub_Type);
            end if;
            The_Aspects(The_Last).Definition := The_Actual_Identifier;
          end case;
        end if;
        exit when not Element_Is (Lexical.Comma);
      end loop;
      return The_Aspects(The_Aspects'first .. The_Last);
    end Aspects_Elements;


    procedure Continue_Aspect_Specification (Within          :        Data.Context;
                                             Is_Unreferenced : in out Boolean) is
      Aspects : constant Data.Aspect_Specification := Aspects_Elements (Within);
      use type Lexical.Aspect_Id;
    begin
      if Aspects'length = 0 then
        raise Program_Error;
      end if;
      if Is_Unreferenced then
        for The_Aspect of Aspects loop
          Is_Unreferenced := (The_Aspect.Mark = Lexical.Is_Unreferenced) or
                             (The_Aspect.Mark = Lexical.Is_Attach_Handler);
          exit when Is_Unreferenced;
        end loop;
      end if;
    end Continue_Aspect_Specification;


    procedure Aspect_Specification (Within  : Data.Context;
                                    For_Ids : Identifiers := No_Identifiers) is
      Is_Unreferenced : Boolean := For_Ids /= No_Identifiers;
    begin
      Get_Element (Lexical.Is_With);
      Continue_Aspect_Specification (Within, Is_Unreferenced);
      if Is_Unreferenced then
        Set_Used (For_Ids);
      end if;
    end Aspect_Specification;


    procedure Conditional_Aspect_Specification (Within  : Data.Context;
                                                For_Ids : Identifiers := No_Identifiers) is
    begin
      if not End_Of_File and then The_Token.Element = Lexical.Is_With then
        Aspect_Specification (Within, For_Ids);
      end if;
    end Conditional_Aspect_Specification;


    function Aspect_Specification (Within : Data.Context) return Data.Aspect_Specification is
    begin
      if Element_Is (Lexical.Is_With) then
        return Aspects_Elements (Within);
      end if;
      return Data.No_Aspects;
    end Aspect_Specification;


    The_Statement_Count : Integer;
    Had_Raise_Statement : Boolean;
    The_Target_Type     : Data_Handle;


    -- subprogram_body ::=
    --      subprogram_specification [aspect_specification] is
    --        declarative_part
    --      begin
    --        handled_sequence_of_statements
    --      end [designator] ;
    --
    -- subprogram_body_stub ::=
    --      subprogram_specification is separate ;
    --
    procedure Subprogram_Body (Self          : Data.Unit_Handle;
                               Is_Overriding : Boolean := False) is
    begin
      if Element_Is (Lexical.Is_Separate) then
        Data.Set_Profile_Used (Self);
        Get_Element (Lexical.Semicolon);
      else
        if Is_Overriding then
          Self.Is_Used := True;
        end if;
        Conditional_Aspect_Specification ((Self, null));
        Get_Element (Lexical.Is_Is);
        Declarative_Part (Self);
        Get_Element (Lexical.Is_Begin);
        The_Statement_Count := 0;
        Had_Raise_Statement := False;
        Handled_Sequence_Of_Statements (Self);
        if The_Statement_Count < 1 then
          Data.Set_Profile_Used (Self);
        end if;
        Check_End (Self);
      end if;
    end Subprogram_Body;


    -- package_body ::=
    --      package body defining_program_unit_name [aspect_specification] is
    --        declarative_part
    --      [begin
    --        handled_sequence_of_statements]
    --      end [[parent_unit_name.]identifier] ;
    --
    -- package_body_stub ::=
    --      body defining_program_unit_name is separate ;
    --
    procedure Package_Body (Self : Data.Unit_Handle) is
    begin
      Conditional_Aspect_Specification ((Self, null));
      Get_Element (Lexical.Is_Is);
      if Element_Is (Lexical.Is_Separate) then
        Get_Element (Lexical.Semicolon);
      else
        Declarative_Part (Self);
        if Element_Is (Lexical.Is_Begin) then
          Handled_Sequence_Of_Statements (Self);
        end if;
        Check_End (Self);
      end if;
    end Package_Body;


    -- package_specification ::=
    --      package defining_program_unit_name [aspect_specification] is
    --        {basic_declarative_item}
    --      [private
    --        {basic_declarative_item}]
    --      end [[parent_unit_name.]identifier]
    --
    procedure Package_Specification (Self : Data.Unit_Handle) is
    begin
      Get_Element (Lexical.Is_Is);
      Declarative_Part (Self, Is_Basic => True);
      if Element_Is (Lexical.Is_Private) then
        Declarative_Part (Data.New_Private_Part(Self), Is_Basic => True);
      end if;
      Check_End (Self);
    end Package_Specification;


    -- mode ::=
    --      [in]
    --    | in out
    --    | out
    --
    procedure Mode is
    begin
      case Token_Element is
      when Lexical.Is_In =>
        Get_Next_Token;
        if Token_Element = Lexical.Is_Out then
          Get_Next_Token;
        end if;
      when Lexical.Is_Out =>
        Get_Next_Token;
      when others =>
        null;
      end case;
    end Mode;


    -- defining_identifier_list ::=
    --   defining_identifier { , defining_identifier}
    --
    function Defining_Identifier_List return Identifiers is
    begin
      return Token_Identifiers (Lexical.Comma, Declaring => True);
    end Defining_Identifier_List;

    function Next_Defining_Identifier_List return Identifiers is
    begin
      Get_Next_Token;
      return Defining_Identifier_List;
    end Next_Defining_Identifier_List;


    Is_Class_Wide_Type : Boolean;

    -- subtype_mark ::=
    --      subtype_name
    --
    function Subtype_Mark (Scope                 : Data.Unit_Handle;
                           Is_Subtype_Indication : Boolean := False) return Data_Handle is

      Type_Mark : Data_Handle;

    begin
      Is_Class_Wide_Type := False;
      Type_Mark := Name_Of ((Scope, null), Is_Subtype_Mark => True, Is_Subtype_Indication => Is_Subtype_Indication);
      if Type_Mark /= null and then not Data.Is_Subtype (Type_Mark) then
        if Type_Mark.all in Data.Active_Body'class then
          Type_Mark := Data.Active_Body_Handle(Type_Mark).Type_Link;
        else
          Type_Mark := null;
        end if;
      end if;
      if Type_Mark /= null then
        if Type_Mark.all in Data.Sub_Type'class then
          Is_Class_Wide_Type := Data.Sub_Type_Handle(Type_Mark).Is_Class_Wide;
        elsif Type_Mark.all in Data.Class_Access_Type'class then
          Is_Class_Wide_Type := True;
        end if;
      else
        Is_Class_Wide_Type := False;
      end if;
      return Type_Mark;
    end Subtype_Mark;


    function Access_Type_Of (Id       : Identifier_Handle;
                             Parent   : Data.Unit_Handle;
                             To_Type  : Data_Handle) return Data_Handle is
    begin
      if Is_Class_Wide_Type then
        return Data.New_Class_Access_Type (Id      => Id,
                                           Parent  => Parent,
                                           To_Type => To_Type);
      else
        return Data.New_Access_Type (Id      => Id,
                                     Parent  => Parent,
                                     To_Type => To_Type);
      end if;
    end Access_Type_Of;


    -- access_definition_part ::=
    --      [constant] subtype_mark
    --    | [protected] procedure parameter_profile
    --    | [protected] function  parameter_and_result_profile
    --
    function Access_Definition_Part (Scope : Data.Unit_Handle) return Data_Handle is
    begin
      case Token_Element is
      when Lexical.Identifier =>
        return Access_Type_Of (Id      => null,
                               Parent  => Scope,
                               To_Type => Subtype_Mark (Scope));
      when Lexical.Is_Constant =>
        Get_Next_Token;
        return Access_Type_Of (Id      => null,
                               Parent  => Scope,
                               To_Type => Subtype_Mark (Scope));

      when Lexical.Is_Protected =>
        case Next_Token.Element is
        when Lexical.Is_Function | Lexical.Is_Procedure =>
          return Data.New_Protected_Subprogram_Access_Type
                     (Id      => null,
                      Parent  => Scope,
                      Profile => Subprogram_Profile (Scope, Parsed_Element_Is (Lexical.Is_Function)));
        when others =>
          Syntax_Error;
        end case;
      when Lexical.Is_Function | Lexical.Is_Procedure =>
        return Data.New_Subprogram_Access_Type
                   (Id      => null,
                    Parent  => Scope,
                    Profile => Data.Used (Subprogram_Profile (Scope, Parsed_Element_Is (Lexical.Is_Function))));
      when others =>
        Syntax_Error;
      end case;
    end Access_Definition_Part;


    -- expression ::=
    --      relation {and relation} | relation {and then relation}
    --    | relation {or relation}  | relation {or else relation}
    --    | relation {xor relation}
    --
    --    relation ::=
    --         simple_expression [relational_operator simple_expression]
    --       | simple_expression [not] in membership_choice_list
    --       | raise_expression
    --
    --       membership_choice_list ::= membership_choice { | membership_choice}
    --
    --          membership_choice ::= choice_expression | range | subtype_mark
    --
    --             choice_expression ::=
    --                  choice_relation {and choice_relation}
    --                | choice_relation {or choice_relation}
    --                | choice_relation {xor choice_relation}
    --                | choice_relation {and then choice_relation}
    --                | choice_relation {or else choice_relation}
    --
    --                choice_relation ::=
    --                   simple_expression [relational_operator simple_expression]
    --
    --       relational_operator ::=
    --           =
    --         | /=
    --         | <
    --         | <=
    --         | >
    --         | >=
    --
    Was_Component : Boolean;

    function Expression (Within : Data.Context) return Data_Handle is

      The_Type : Data_Handle;

      procedure Relation with Inline is
        Left_Token : constant Lexical_Handle := The_Token;
        End_Token  : Lexical_Handle;
      begin
        --TEST-----------------------------------------------
        --Write_Log ("RELATION " & Image_Of (The_Token.all));
        -----------------------------------------------------
        if Element_Is (Lexical.Is_Raise) then
          The_Type := Raise_Expression (Within);
        else
          The_Type := Simple_Expression (Within);
          case Token_Element is
          when Lexical.Equal
             | Lexical.Not_Equal
             | Lexical.Less
             | Lexical.Less_Or_Equal
             | Lexical.Greater
             | Lexical.Greater_Or_Equal
          =>
            Get_Next_Token;
            if The_Type = null then
              The_Type := Simple_Expression (Within);
              if The_Type /= null then
                End_Token := The_Token;
                The_Token := Left_Token;
                The_Type := Simple_Expression ((Within.Scope, The_Type));
                The_Token := End_Token;
              end if;
            else
              Dummy := Simple_Expression ((Within.Scope, The_Type));
            end if;
            The_Type := Data.Predefined_Boolean;
            return;
          when Lexical.Is_Not =>
            Get_Next_Element (Lexical.Is_In);
          when Lexical.Is_In =>
            Get_Next_Token;
          when others =>
            return;
          end case;
          loop
            if The_Type = null then
              Dummy := Discrete_Range (Within);
            else
              Dummy := Discrete_Range ((Within.Scope, The_Type));
            end if;
            exit when not Element_Is (Lexical.Vertical_Line);
          end loop;
          The_Type := Data.Predefined_Boolean;
        end if;
      end Relation;

    begin --Expression;
      Was_Component := True;
      case Token_Element is
      when Lexical.Is_If =>
        --TEST------------------------
        --Write_Log ("if expression");
        ------------------------------
        return If_Expression (Within);
      when Lexical.Is_Case =>
        --TEST--------------------------
        --Write_Log ("case expression");
        --------------------------------
        return Case_Expression (Within);
      when Lexical.Is_For =>
        --TEST--------------------------------
        --Write_Log ("quantified expression");
        --------------------------------------
        return Quantified_Expression (Within);
      when Lexical.Is_Declare =>
        --TEST-----------------------------
        --Write_Log ("declare expression");
        -----------------------------------
        return Declare_Expression (Within);
      when others =>
        null;
      end case;
      Relation;
      case Token_Element is
      when Lexical.Is_And =>
        if Next_Element_Is (Lexical.Is_Then) then
          Relation;
          while Element_Is (Lexical.Is_And) loop
            Get_Element (Lexical.Is_Then);
            Relation;
          end loop;
        else
          Relation;
          while Element_Is (Lexical.Is_And) loop
            Relation;
          end loop;
        end if;
      when Lexical.Is_Or =>
        if Next_Element_Is (Lexical.Is_Else) then
          Relation;
          while Element_Is (Lexical.Is_Or) loop
            Get_Element (Lexical.Is_Else);
            Relation;
          end loop;
        else
          Relation;
          while Element_Is (Lexical.Is_Or) loop
            Relation;
          end loop;
        end if;
      when Lexical.Is_Xor =>
        Get_Next_Token;
        Relation;
        while Element_Is (Lexical.Is_Xor) loop
          Relation;
        end loop;
      when others =>
        null;
      end case;
      --TEST----------------------------------------------------------------
      --if The_Type = null then
      --  Write_Log ("Expression Result: null");
      --else
      --  Write_Log ("Expression Result: " & Data.Full_Name_Of (The_Type) &
      --             " - TYPE " & Ada.Tags.External_Tag (The_Type.all'tag));
      --end if;
      ----------------------------------------------------------------------
      return The_Type;
    end Expression;

    -- simple_expression ::=
    --      [unary_adding_operator] term {binary_adding_operator term}
    --
    --    unary_adding_operator ::=
    --          +
    --        | -
    --
    --    binary_adding_operator ::=
    --         +
    --       | -
    --       | &
    --
    --    term ::=
    --         factor {multiplying_operator factor}
    --
    --       multiplying_operator ::=
    --            *
    --          | /
    --          | mod
    --          | rem
    --
    --    factor ::=
    --         primary [ ** primary]
    --       | abs primary
    --       | not primary
    --
    --       primary ::=
    --            numeric_literal
    --          | null
    --          | string_literal
    --          | aggregate
    --          | name
    --          | allocator
    --          | ( expression )
    --          | ( conditional_expression )
    --          | ( quantified_expression )
    --          | ( declare_expression )
    --
    --          allocator ::=
    --               new subtype_indication
    --             | new qualified_expression
    --
    function Simple_Expression (Within : Data.Context) return Data_Handle is

      The_Type    : Data_Handle;
      The_Context : Data.Context := Within;
      The_Next    : Simple_Expression_Handle := (S_0, Unknown);

    begin -- Simple_Expression
      if Token_Element in Lexical.Plus | Lexical.Minus then
        The_Type := Within.Sub_Type; -- force result type if unary plus or minus
      end if;
      loop
        The_Next := Next_Simple_Expression_Data (The_Next.State, Token_Element);
        --TEST----------------------------------------------------------------------------
        --Write_Log ("State = " & Simple_Expression_State'image (The_Next.State) &
        --           ", Element = " & Simple_Expression_Element'image (The_Next.Element) &
        --           ", Type = " & Data.Full_Name_Of (The_Type));
        ----------------------------------------------------------------------------------
        case The_Next.Element is
        when Is_Identifier =>
          if The_Type = null then
            The_Type := Name_Of (The_Context);
          else
            The_Context.Sub_Type := The_Type;
            Dummy := Name_Of (The_Context);
          end if;
        when Is_Integer_Literal =>
          Get_Next_Token;
          The_Type := Data.Predefined_Root_Integer;
        when Is_Real_Literal =>
          Get_Next_Token;
          The_Type := Data.Predefined_Root_Real;
        when Is_Character_Literal =>
          Get_Next_Token;
          if The_Type /= null then
            The_Type := Data.Predefined_Root_String;
          end if;
        when Is_String_Literal =>
          Get_Next_Token;
          The_Type := Data.Predefined_Root_String;
        when Is_Null =>
          Get_Next_Token;
          The_Type := Data.Predefined_Root_Access;
        when Operator =>
          if The_Type /= null and then The_Type.all in Data.Enumeration_Type'class then
            The_Type := Data.Predefined_Root_String;
          end if;
          Get_Next_Token;
        when Aggregate =>
          The_Type := Aggregate (Within);
        when Aggregate_Or_Expression =>
          if Next_Element_Ahead_Is (Lexical.Is_Raise) then
            Get_Next_Token;
            The_Type := Raise_Expression (Within);
            Get_Element (Lexical.Right_Parenthesis);
            exit;
          else
            The_Type := Aggregate (Within);
          end if;
        when Allocator =>
          The_Type := Allocator (Within);
        when Unknown =>
          exit;
        when Unexpected =>
          Syntax_Error;
        end case;
      end loop;
      --TEST----------------------------------------------------------------------
      --if The_Type = null then
      --  Write_Log ("Simple Expression Result: null");
      --else
      --  Write_Log ("Simple Expression Result: " & Data.Full_Name_Of (The_Type) &
      --             " - TYPE " & Ada.Tags.External_Tag (The_Type.all'tag));
      --end if;
      ----------------------------------------------------------------------------
      return The_Type;
    end Simple_Expression;


    -- case_expression ::=
    --      case selecting_expression is case_expression_alternative { , case_expression_alternative}
    --
    --         case_expression_alternative ::= when discrete_choice_list => dependent_expression
    --
    function Case_Expression (Within : Data.Context) return Data_Handle is
      The_Selector : Data_Handle;
      The_Type     : Data_Handle;
      Is_Last      : Boolean;
    begin
      Get_Next_Token;
      The_Selector := Expression (Within);
      Get_Element (Lexical.Is_Is);
      loop
        Get_Element (Lexical.Is_When);
        Is_Last := Discrete_Choice_List ((Within.Scope, The_Selector));
        Get_Element (Lexical.Association);
        The_Type := Expression (Within);
        exit when Is_Last or else not Element_Is (Lexical.Comma);
      end loop;
      return The_Type;
    end Case_Expression;


    -- if_expression ::=
    --      if condition then dependent_expression
    --        {elsif condition then dependent_expression}
    --        [else dependent_expression]
    --
    function If_Expression (Within : Data.Context) return Data_Handle is
      The_Type : Data_Handle;
    begin
      Get_Next_Token;
      loop
        Condition (Within.Scope);
        Get_Element (Lexical.Is_Then);
        The_Type := Expression (Within);
        case Token_Element is
        when Lexical.Is_Elsif =>
          Get_Next_Token;
        when Lexical.Is_Else =>
          Get_Next_Token;
          The_Type := Expression (Within);
          exit;
        when others =>
          exit;
        end case;
      end loop;
      return The_Type;
    end If_Expression;


    -- quantified_expression ::=
    --      quantifier for_loop_condition => predicate
    --
    --    quantifier :== all | some
    --
    function Quantified_Expression (Within : Data.Context) return Data_Handle is
      Scope         : constant Data.Unit_Handle := Data.New_Block (null, Within.Scope);
      The_Predicate : Data_Handle;
    begin
      if not Next_Element_Is (Lexical.Is_All) then
        Get_Element (Lexical.Is_Some);
      end if;
      For_Loop_Condition (Scope);
      Get_Element (Lexical.Association);
      The_Predicate := Expression ((Scope, Within.Sub_Type));
      return The_Predicate;
    end Quantified_Expression;


    -- [constraint] ::=
    --      scalar_constraint
    --    | composite_constraint
    --
    --    scalar_constraint ::=
    --          range_constraint
    --        | digits_constraint
    --        | delta_constraint
    --
    --    composite_constraint ::=
    --         index_constraint
    --       | discriminant_constraint
    --
    --       index_constraint ::=
    --            ( discrete_range { , discrete_range} )
    --
    --       discriminant_constraint ::=
    --            ( discriminant_association { , discriminant_association} )
    --
    --          discriminant_association ::=
    --               [discriminant_selector_name { | discriminant_selector_name} => ] expression
    --
    procedure Conditional_Constraint (Within : Data.Context) is
      The_Position : Positive := Positive'first;
      The_Type     : Data_Handle;
    begin
      case Token_Element is
      when Lexical.Is_Range =>
        Dummy := Range_Constraint (Within);
      when Lexical.Is_Digits =>
        Digits_Constraint (Within);
      when Lexical.Is_Delta =>
        Delta_Constraint (Within);
      when Lexical.Left_Parenthesis =>
        loop
          Get_Next_Token;
          if Element_Ahead_Is (Lexical.Association) then
            declare
              Saved_Token : constant Token.Lexical_Handle := The_Token;
            begin
              The_Type := Data.Discriminant_Of (Actual_Identifier, Within.Sub_Type);
              if The_Type /= null then
                Data.Set_Used (The_Type);
                The_Type := Data.Object_Type_Of (The_Type);
              end if;
              if The_Type = null then
                The_Token := Saved_Token;
                Dummy := Discrete_Range (Within);
              end if;
            end;
          else
            The_Type := Data.Discriminant_Type_Of (The_Position, Within.Sub_Type);
            if The_Type = null then
              Dummy := Discrete_Range (Within);
            else
              Dummy := Discrete_Range ((Within.Scope, The_Type));
            end if;
          end if;
          case Token_Element is
          when Lexical.Association =>
            Get_Next_Token;
            if The_Type = null then
              Dummy := Expression (Within);
            else
              Dummy := Expression ((Within.Scope, The_Type));
            end if;
            exit when Token_Element /= Lexical.Comma;
          when Lexical.Vertical_Line =>
            loop
              Get_Next_Token;
              Dummy := Expression (Within);
              exit when Token_Element /= Lexical.Vertical_Line;
            end loop;
            Get_Element (Lexical.Association);
            if The_Type = null then
              Dummy := Expression (Within);
            else
              Dummy := Expression ((Within.Scope, The_Type));
            end if;
            exit when Token_Element /= Lexical.Comma;
          when Lexical.Comma =>
            The_Position := Positive'succ(The_Position);
          when Lexical.Right_Parenthesis =>
            exit;
          when others =>
            Not_Implemented ("Conditional_Constraint (unknown)");
          end case;
        end loop;
        Get_Element (Lexical.Right_Parenthesis);
      when others =>
        null;
      end case;
    end Conditional_Constraint;


    -- allocator ::=
    --      new subtype_indication
    --    | new qualified_expression
    --
    function Allocator (Within : Data.Context) return Data_Handle is
      The_Type : Data_Handle;
    begin
      Get_Next_Token;
      The_Type := Name_Of (Within);
      if Element_Is (Lexical.Apostrophe) then -- qualified_expression
        Dummy := Aggregate ((Within.Scope, The_Type));
      else -- subtype_indication
        if Within.Sub_Type = null or else
          (The_Type /= null and then (The_Type.all in Data.Record_Type'class or
                                      The_Type.all in Data.Active_Type'class or
                                      The_Type.all in Data.Active_Body'class))
        then
          Conditional_Constraint ((Within.Scope, The_Type));
        else
          Conditional_Constraint (Within);
        end if;
      end if;
      return Data.Predefined_Root_Access;
    end Allocator;


    -- aggregate ::=
    --      record_aggregate
    --    | extension_aggregate
    --    | array_aggregate
    --
    --    record_aggregate ::=
    --         ( record_component_association_list )
    --
    function Aggregate (Within : Data.Context) return Data_Handle is

      The_Result_Type     : Data_Handle := Within.Sub_Type;
      The_Instantiation   : Data.Instantiation_Handle := null;
      Termination_Element : Lexical.Element;


      -- array_aggregate ::=
      --       positional_array_aggregate
      --     | named_array_aggregate
      --
      --    positional_array_aggregate ::=
      --         ( expression , expression { , expression} )
      --       | ( expression { , expression} , others => expression )
      --
      --    named_array_aggregate ::=
      --         ( array_component_association { , array_component_association} )
      --
      --       array_component_association ::=
      --            discrete_choice_list => expression
      --
      procedure Array_Aggregate (The_Definition : Data.Array_Definition_Handle) is

        procedure Array_Aggregate (Definition : Data.Array_Definition_Handle;
                                   Index      : Positive) is

          procedure Component is
            The_Type : Data_Handle := Definition.Component_Type;
            use type Data.Instantiation_Handle;
          begin
            --TEST-------------------------------------------------------
            --Write_Log ("Array_Component: " & Image_Of (The_Token.all));
            -------------------------------------------------------------
            if Index = Definition.Dimension then
              if The_Instantiation /= null and then The_Type /= null and then The_Type.all in Data.Formal_Type'class
              then
                The_Type := Data.Actual_Declaration_Of (Data.Formal_Handle(The_Type), The_Instantiation);
              end if;
              if not Element_Is (Lexical.Unconstrained) then
                Dummy := Expression ((Within.Scope, The_Type));
              end if;
            elsif Element_Is (Lexical.Left_Parenthesis) then
              Array_Aggregate (Definition, Index + 1);
              Get_Element (Lexical.Right_Parenthesis);
            elsif Element_Is (Lexical.Left_Bracket) then
              Array_Aggregate (Definition, Index + 1);
              Get_Element (Lexical.Right_Bracket);
            end if;
          end Component;

        begin -- Array_Aggregate (inner)
          --TEST----------------------------------------------------------
          --Write_Log ("Array_Aggregate: INDEX=" & Positive'image(Index));
          ----------------------------------------------------------------
          case Token_Element is
          when Lexical.Is_Others =>
            Get_Next_Element (Lexical.Association);
            Component;
          when others =>
            declare
              Component_Token : constant Lexical_Handle := The_Token;
            begin
              loop
                declare
                  Index_Subtype : constant Data_Handle := Data.Index_Subtype_Of (Definition, Index, The_Instantiation);
                begin
                  --TEST-------------------------------------------------------------------------
                  --if Index_Subtype = null then
                  --  Write_Log ("Index_Subtype of UNKNOWN TYPE");
                  --else
                  --  Write_Log ("Index_Subtype of " & Ada.Tags.External_Tag (Index_Subtype.all'tag));
                  --end if;
                  -------------------------------------------------------------------------------
                  Dummy := Range_Production ((Within.Scope, Index_Subtype));
                  case Token_Element is
                  when Lexical.Association =>
                    Get_Next_Token;
                  when Lexical.Vertical_Line =>
                    loop
                      Get_Next_Token;
                      Dummy := Range_Production ((Within.Scope, Index_Subtype));
                      exit when Token_Element /= Lexical.Vertical_Line;
                    end loop;
                    Get_Element (Lexical.Association);
                  when others =>
                    exit;
                  end case;
                end;
                Component;
                case Token_Element is
                when Lexical.Comma =>
                  Get_Next_Token;
                when others =>
                  if Token_Element = Termination_Element then
                    return;
                  end if;
                  Not_Implemented ("associated array_aggregate (unknown)");
                end case;
                if Element_Is (Lexical.Is_Others) then
                  Get_Element (Lexical.Association);
                  Component;
                  return;
                end if;
              end loop;

              The_Token := Component_Token;
              --TEST-----------------------------------------------------------------------------
              --Write_Log ("Array_Aggregate - WITHOUT Association: " & Image_Of (The_Token.all));
              -----------------------------------------------------------------------------------
              loop
                Component;
                case Token_Element is
                when Lexical.Comma =>
                  Get_Next_Token;
                when others =>
                  if Token_Element = Termination_Element then
                    return;
                  end if;
                  Not_Implemented ("array_aggregate (unknown)");
                end case;
                if Element_Is (Lexical.Is_Others) then
                  Get_Element (Lexical.Association);
                  Component;
                  return;
                end if;
              end loop;
            end;
          end case;
        end Array_Aggregate;

        Extra_Parenthesis_Count : Natural := 0;

      begin -- Array_Aggregate
        if The_Definition.Component_Type = null or else The_Definition.Component_Type.all in Data.Scalar_Type'class then
          while Element_Is (Lexical.Left_Parenthesis) loop
            Extra_Parenthesis_Count := Extra_Parenthesis_Count + 1;
          end loop;
        end if;
        Array_Aggregate (The_Definition, 1);
        while Extra_Parenthesis_Count > 0 loop
          Get_Element (Lexical.Right_Parenthesis);
          Extra_Parenthesis_Count := Extra_Parenthesis_Count - 1;
        end loop;
      end Array_Aggregate;


      -- enumeration_aggregate ::=
      --      array_aggregate
      --
      procedure Enumeration_Aggregate (Component_Type : Data_Handle) is
      begin
        loop
          if not Element_Is (Lexical.Is_Others) then
            Dummy := Expression ((Within.Scope, Component_Type));
          end if;
          if Element_Is (Lexical.Association) then -- else not an aggregate
            Dummy := Expression (Within);
          end if;
          exit when not Element_Is (Lexical.Comma);
        end loop;
      end Enumeration_Aggregate;


      -- record_component_association_list ::=
      --       record_component_association { , record_component_association}
      --     | null record
      --
      --    record_component_association ::=
      --          [ component_choice_list => ] expression
      --        | component_choice_list => <>
      --
      --       component_choice_list ::=
      --             component_selector_name { | component_selector_name}
      --           | others
      --
      procedure Record_Component_Association_List (Record_Handle : Data.Record_Handle) is
        The_Type        : Data_Handle;
        The_Position    : Natural := Positive'first;
        Other_Position  : Natural := The_Position;
        Has_Choice_List : Boolean := False;
      begin
        if (Token_Element = Lexical.Is_Null) and then Next_Element_Ahead_Is (Lexical.Is_Record) then
          Get_Next_Token;
        else
          loop
            declare
              Component_Token : constant Lexical_Handle := The_Token;
            begin
              loop
                case Token_Element is
                when Lexical.Identifier =>
                  The_Type := Data.Component_Choice_Of (Actual_Identifier, Record_Handle);
                when Lexical.Is_Others =>
                  Get_Next_Element (Lexical.Association);
                  The_Type := Data.Component_Choice_Of (Other_Position, Record_Handle);
                  Data.Set_Used (Record_Handle.Discriminants);
                  Has_Choice_List := True;
                  exit;
                when Lexical.Association =>
                  The_Position := Data.List.Not_Found;
                  Other_Position := Positive'succ(Other_Position);
                  Get_Next_Token;
                  Has_Choice_List := True;
                  exit;
                when Lexical.Vertical_Line =>
                  Get_Next_Token;
                when others =>
                  The_Token := Component_Token;
                  if The_Position = Data.List.Not_Found then
                    Syntax_Error;
                  else
                    The_Type := Data.Component_Choice_Of (The_Position, Record_Handle);
                    The_Position := Positive'succ(The_Position);
                    Other_Position := The_Position;
                  end if;
                  exit;
                end case;
              end loop;
              Data.Set_Used (Record_Handle.Components);
            end;
            if The_Type = null then
              The_Result_Type := null;
            end if;
            --TEST----------------------------------------------------------------------------
            --if The_Type = null then
            --  Write_Log ("Record_Component of UNKNOWN TYPE");
            --else
            --  Write_Log ("Record_Component of " & Ada.Tags.External_Tag (The_Type.all'tag));
            --end if;
            ----------------------------------------------------------------------------------
            --Data.Set_In_Record;
            if (not Has_Choice_List) or else (not Element_Is (Lexical.Unconstrained)) then
              Dummy := Expression ((Within.Scope, The_Type));
            end if;
            --Data.Clear_In_Record;
            exit when not Element_Is (Lexical.Comma);
          end loop;
        end if;
      end Record_Component_Association_List;


      -- extension_aggregate ::=
      --       ( ancestor_part with record_component_association_list )
      --
      --    ancestor_part ::=
      --         expression
      --       | subtype_mark
      --
      function Is_Extension_Aggregate (Record_Handle : Data.Record_Handle) return Boolean is
        Saved_Token : constant Lexical_Handle := The_Token;
      begin
        if Token_Element /= Lexical.Identifier then
          return False;
        end if;
        Dummy := Expression (Within);
        if Element_Is (Lexical.Is_With) then
          Record_Component_Association_List (Record_Handle);
          return True;
        else
          The_Token := Saved_Token;
          return False;
        end if;
      end Is_Extension_Aggregate;


      procedure Unknown_Aggregate (The_Expected_Type : Data_Handle := Within.Sub_Type) is
        The_Within : Data.Context := Within;
      begin
        loop
          case Token_Element is
          when Lexical.Plus
             | Lexical.Minus
             | Lexical.Left_Parenthesis
             | Lexical.Identifier
             | Lexical.Target_Name
             | Lexical.Integer_Literal
             | Lexical.Real_Literal
             | Lexical.String_Literal
             | Lexical.Is_Null
             | Lexical.Is_And
             | Lexical.Is_Abs
             | Lexical.Is_Not
          =>
            if Token_Element = Lexical.Target_Name then
              The_Within.Sub_Type := The_Target_Type;
            end if;
            The_Result_Type := Expression (The_Within);
            if Termination_Element = Lexical.Right_Bracket then
              if The_Result_Type /= null and then
                Data.Base_Type_Of (The_Result_Type).all in Data.Discrete_Type'class
              then
                Log.Write ("%%% Root_String for " & Ada.Tags.External_Tag (The_Result_Type.all'tag));
                The_Result_Type := Data.Predefined_Root_String;
                exit;
              end if;
            end if;
            if Element_Is (Lexical.Range_Delimiter) then
              The_Result_Type := Expression ((Within.Scope, The_Result_Type));
            end if;
          when Lexical.Association =>
            Get_Next_Token;
            if not Element_Is (Lexical.Unconstrained) then
              The_Result_Type := Expression ((Within.Scope, The_Expected_Type));
            end if;
          when Lexical.Is_Others =>
            Get_Next_Element (Lexical.Association);
            if not Element_Is (Lexical.Unconstrained) then
              The_Result_Type := Expression ((Within.Scope, The_Expected_Type));
            end if;
            exit;
          when Lexical.Comma
             | Lexical.Equal
             | Lexical.Is_New
             | Lexical.Is_With
             | Lexical.Vertical_Line
          =>
            Get_Next_Token;
          when Lexical.Is_If =>
            --TEST----------------------------------------
            --Write_Log ("-> if Expression from unknown");
            ----------------------------------------------
            The_Result_Type := If_Expression (Within);
            exit;
          when Lexical.Is_Case =>
            --TEST------------------------------------------
            --Write_Log ("-> case Expression from unknown");
            ------------------------------------------------
            The_Result_Type := Case_Expression (Within);
            exit;
          when Lexical.Is_For =>
            --TEST------------------------------------------------
            --Write_Log ("-> quantified expression from unknown");
            ------------------------------------------------------
            The_Result_Type := Quantified_Expression (Within);
            exit;
          when Lexical.Is_Declare =>
            --TEST---------------------------------------------
            --Write_Log ("-> declare expression from unknown");
            ---------------------------------------------------
            The_Result_Type := Declare_Expression (Within);
            exit;
          when Lexical.Left_Bracket =>
            if Next_Element_Ahead_Is (Lexical.Is_For) then
              --TEST-------------------------------------------------------
              --Write_Log ("-> Iterated Element Association from unknown");
              -------------------------------------------------------------
              The_Result_Type := Iterated_Element_Association (Within);
              Get_Element (Lexical.Right_Bracket);
              exit;
            end if;
            Not_Implemented ("Aggregate Association: " & Image_Of (The_Token.all));
          when others =>
            exit when Token_Element = Termination_Element;
            Not_Implemented ("Aggregate: " & Image_Of (The_Token.all));
          end case;
        end loop;
      end Unknown_Aggregate;

      The_Base_Type    : Data_Handle;
      The_Element_Type : Data_Handle;

    begin --Aggregate
      case Token_Element is
      when Lexical.Left_Parenthesis =>
        Termination_Element := Lexical.Right_Parenthesis;
      when Lexical.Left_Bracket =>
        if Next_Element_Ahead_Is (Lexical.Right_Bracket) then
          Get_Next_Token;
          return The_Result_Type;
        end if;
        Termination_Element := Lexical.Right_Bracket;
      when others =>
        Syntax_Error;
      end case;
      Get_Next_Token;
      if Within.Sub_Type = null or else
        not ((Within.Sub_Type.all in Data.Type_Declaration'class) or
             (Within.Sub_Type.all in Data.Instantiated_Type'class))
      then
        --TEST------------------------------------------
        --Write_Log ("-> AGGREGATE OF UNKNOWN SUBTYPE");
        ------------------------------------------------
        Unknown_Aggregate;
      else
        --TEST-----------------------------------------------------------------------------
        --Write_Log ("Aggregate or Qualified Expression of " & Image_Of (The_Token.all));
        --Write_Log ("  Subtype Name: " & Data.Full_Name_Of (Within.Sub_Type));
        --Write_Log ("          Type: " & Ada.Tags.External_Tag (Within.Sub_Type.all'tag));
        -----------------------------------------------------------------------------------
        case Token_Element is
        when Lexical.Plus
           | Lexical.Minus
           | Lexical.Identifier
           | Lexical.Target_Name
           | Lexical.Integer_Literal
           | Lexical.Real_Literal
           | Lexical.String_Literal
           | Lexical.Is_Abs
           | Lexical.Is_Not
        => -- check for qualified expression
          declare
            Saved_Token : constant Lexical_Handle := The_Token;
            The_Type    : Data_Handle;
            The_Within  : Data.Context := Within;
          begin
            if Token_Element = Lexical.Target_Name then
              The_Within.Sub_Type := The_Target_Type;
            end if;
            The_Type := Expression (The_Within); -- qualified expresssion
            if (The_Type /= null) and then Element_Is (Termination_Element) then
              --TEST--------------------------------------------------------------------------
              --Write_Log ("-> Qualified Expression");
              --Write_Log ("  Result Type Name: " & Data.Full_Name_Of (The_Type));
              --Write_Log ("              Type: " & Ada.Tags.External_Tag (The_Type.all'tag));
              --------------------------------------------------------------------------------
              if Termination_Element = Lexical.Right_Bracket then
                if Data.Base_Type_Of (The_Type).all in Data.Discrete_Type'class | Data.Access_Type'class then
                  return The_Within.Sub_Type;
                end if;
              end if;
              return The_Type; -- qualified expresssion
            end if;
            The_Token := Saved_Token;
          end;
        when Lexical.Is_If =>
          The_Result_Type := If_Expression (Within);
          Get_Element (Lexical.Right_Parenthesis);
          --TEST---------------------------
          --Write_Log ("-> if expression");
          ---------------------------------
          return The_Result_Type;
        when Lexical.Is_Case =>
          The_Result_Type := Case_Expression (Within);
          Get_Element (Lexical.Right_Parenthesis);
          --TEST-----------------------------
          --Write_Log ("-> case expression");
          -----------------------------------
          return The_Result_Type;
        when Lexical.Is_For =>
          if Termination_Element = Lexical.Right_Bracket then
            The_Result_Type := Reduction_Attribute_Reference (Within);
            --TEST---------------------------------
            --Write_Log ("-> reduction attribute");
            ---------------------------------------
            return The_Result_Type;
          else
            The_Result_Type := Quantified_Expression (Within);
            Get_Element (Lexical.Right_Parenthesis);
            --TEST-----------------------------------
            --Write_Log ("-> quantified expression");
            -----------------------------------------
            return The_Result_Type;
          end if;
        when Lexical.Is_Declare =>
          The_Result_Type := Declare_Expression (Within);
          Get_Element (Lexical.Right_Parenthesis);
          --TEST--------------------------------
          --Write_Log ("-> declare expression");
          --------------------------------------
          return The_Result_Type;
        when Lexical.Left_Bracket =>
          if Next_Element_Ahead_Is (Lexical.Is_For) then
            The_Result_Type := Reduction_Attribute_Reference (Within);
            --TEST----------------------------------
            --Write_Log ("-> reduction expression");
            ----------------------------------------
            if Termination_Element = Lexical.Right_Parenthesis then
              Get_Element (Lexical.Right_Parenthesis);
            end if;
            return The_Result_Type;
          end if;
        when others =>
          null;
        end case;
        The_Base_Type := Within.Sub_Type;
        if The_Base_Type.all in Data.Instantiated_Item'class then
          The_Instantiation := Data.Item_Instantiation(The_Base_Type).Instantiation;
          The_Base_Type := Data.Item_Instantiation(The_Base_Type).Item;
          --TEST-----------------------------------------------------------------------
          --Write_Log ("INSTANTIATION = " & Image_Of (The_Instantiation.Location.all));
          -----------------------------------------------------------------------------
          declare
            Iterable_Type : constant Data_Handle := Data.Iterable_Type_Of (The_Base_Type);
          begin
            if Iterable_Type /= null then
              if Iterable_Type.all in Data.Formal_Type'class then
                The_Element_Type := Data.Actual_Type_Of (Data.Formal_Handle(Iterable_Type), The_Instantiation);
                --TEST-------------------------------------------------------------------------
                --Write_Log ("ITERABLE Element Type: " & Data.Full_Name_Of (The_Element_Type));
                -------------------------------------------------------------------------------
              end if;
            end if;
          end;
        end if;
        The_Base_Type := Data.Base_Type_Of (The_Base_Type);
        if The_Base_Type = null then
          --TEST-------------------------------------------------
          --Write_Log ("UNKNOWN AGGREGATE - NO BASE TYPE FOUND");
          -------------------------------------------------------
          Unknown_Aggregate;
        else
          --TEST---------------------------------------------------------------------------
          --Write_Log ("-> AGGREGATE of " & Ada.Tags.External_Tag (The_Base_Type.all'tag));
          ---------------------------------------------------------------------------------
          case Data_Kind_Of (The_Base_Type.all) is
          when Is_Array_Type =>
            Array_Aggregate (Data.Array_Handle(The_Base_Type).Definition);
          when Is_Enumeration_Type =>
            Enumeration_Aggregate (The_Base_Type);
          when Is_Record_Type =>
            if not Is_Extension_Aggregate (Data.Record_Handle(The_Base_Type)) then
              Record_Component_Association_List (Data.Record_Handle(The_Base_Type));
            end if;
          when Is_Object =>
            Not_Implemented ("Aggregate of OBJECT: " & Image_Of (The_Token.all));
          when Is_Private_Type | Is_Private_Extension_Type =>
            The_Base_Type := The_Base_Type.Location.Data;
            --TEST-----------------------------------------------------------------------------
            --Write_Log ("Aggregate of Private");
            --Write_Log ("  Base_Type Name: " & Data.Full_Name_Of (The_Base_Type));
            --Write_Log ("            Type: " & Ada.Tags.External_Tag (The_Base_Type.all'tag));
            -----------------------------------------------------------------------------------
            case Data_Kind_Of (The_Base_Type.all) is
            when Is_Array_Type =>
              Array_Aggregate (Data.Array_Handle(The_Base_Type).Definition);
            when Is_Enumeration_Type =>
              Enumeration_Aggregate (The_Base_Type);
            when Is_Record_Type =>
              if not Is_Extension_Aggregate (Data.Record_Handle(The_Base_Type)) then
                declare
                  Saved_Token : constant Lexical_Handle := The_Token;
                begin
                  Record_Component_Association_List (Data.Record_Handle(The_Base_Type));
                  if Element_Is (Lexical.Association) then
                    The_Token := Saved_Token;
                    --TEST-----------------------------------------
                    --Write_Log ("-> AGGREGATE OF CONTAINER TYPE");
                    -----------------------------------------------
                    Unknown_Aggregate (The_Element_Type);
                  end if;
                end;
              end if;
            when Is_Object =>
              Not_Implemented ("Aggregate of private OBJECT: " & Image_Of (The_Token.all));
            when others =>
              --TEST-----------------------------------------------
              --Write_Log ("-> AGGREGATE OF UNKNOWN PRIVATE TYPE");
              -----------------------------------------------------
              Unknown_Aggregate;
            end case;
          when others =>
            --TEST--------------------------------------------
            --Write_Log ("-> AGGREGATE OF UNKNOWN BASE TYPE");
            --------------------------------------------------
            Unknown_Aggregate;
          end case;
        end if;
      end if;
      if Element_Is (Lexical.Range_Delimiter) then
        --TEST----------------------------------------------
        --Write_Log ("-> AGGREGATE OF UNKNOWN AFTER RANGE");
        ----------------------------------------------------
        Unknown_Aggregate;
      end if;
      if Element_Is (Lexical.Association) then
        --TEST-----------------------------------------
        --Write_Log ("-> UNKNOWN CONTAINER AGGREGATE");
        -----------------------------------------------
        Unknown_Aggregate;
      end if;
      Get_Element (Termination_Element);
      --TEST-------------------------------------------------------------------------------
      --if The_Result_Type = null then
      --  Write_Log ("Aggregate type: NULL");
      --else
      --  Write_Log ("Aggregate type: " & Ada.Tags.External_Tag (The_Result_Type.all'tag));
      --end if;
      --TEST-------------------------------------------------------------------------------
      return The_Result_Type;
    end Aggregate;


    -- aspect_mark :==
    --    Aspect_Identifier [ ' class]
    --
    function Aspect_Mark return Lexical.Aspect_Id is
      The_Aspect_Mark : Lexical.Aspect_Id;
      use type Lexical.Attribute_Id;
    begin
      Check (Lexical.Aspect);
      The_Aspect_Mark := Aspect_Handle(The_Token).Designator;
      Get_Next_Token;
      if Token_Element = Lexical.Apostrophe then
        if Next_Element_Ahead_Is (Lexical.Attribute) then
          if Attribute_Designator ((Unit, null)) /= Lexical.Is_Class then
            Syntax_Error;
          end if;
        end if;
      end if;
      return The_Aspect_Mark;
    end Aspect_Mark;


    Build_Parameters_Defined : Boolean;
    Special_Comment_Detected : Boolean;

    -- pragma ::=
    --    pragma identifier [ ( pragma_argument_association {, pragma_argument_association} ) ] ;
    --
    -- pragma_argument_association ::=
    --      [pragma_argument_identifier =>] name
    --    | [pragma_argument_identifier =>] expression
    --    | aspect_mark => name
    --    | aspect_mark => expression
    --
    procedure Pragma_Call (Scope : Data.Unit_Handle) is

      Pragma_Name : Lexical_Handle;
      The_Handle  : Pragma_Identifier_Handle;
      Is_Found    : Boolean := True;

      procedure Pragma_Argument_Identifier is
        Argument_Identifier : constant Token.Identifier_Handle := Actual_Identifier;
      begin
        Argument_Identifier.Data := Data.Predefined_Name;
      end Pragma_Argument_Identifier;

      procedure Pragma_Parameter (Handled : Boolean := False) is
      begin
        loop
          if Element_Ahead_Is (Lexical.Association) then
            Pragma_Argument_Identifier;
            Get_Element (Lexical.Association);
          end if;
          Dummy := Expression ((Scope, null));
          if not Is_Null (The_Actual_Identifier) then
            if The_Actual_Identifier.Data = null then
              if Handled then
                Is_Found := False;
              else
                The_Actual_Identifier.Data := Data.Predefined_Name;
              end if;
            end if;
          end if;
          exit when not Element_Is (Lexical.Comma);
        end loop;
        Get_Element (Lexical.Right_Parenthesis);
      end Pragma_Parameter;

      procedure Handle_Other_Pragmas is
      begin
        The_Handle.Is_Used := True;
        if Next_Element_Is (Lexical.Left_Parenthesis) then
          Pragma_Parameter;
        end if;
      end Handle_Other_Pragmas;


      procedure Handle_Build_Parameters is

        Argument_Handle     : Lexical_Handle := The_Token;
        String_Token        : Lexical_Handle;
        Icon_True_Handle    : Lexical_Handle;
        Icon_Defined        : Boolean := False;
        Kind_Defined        : Boolean := False;
        Version_Defined     : Boolean := False;
        Description_Defined : Boolean := False;
        Compilers_Defined   : Boolean := False;
        Libraries_Defined   : Boolean := False;
        Interface_Defined   : Boolean := False;
        Resource_Defined    : Boolean := False;

        function Version_Number return Build.Version_Number is
          Number_Token : constant Lexical_Handle := The_Token;
        begin
          if Kind_Of (Number_Token.all) /= Is_Numeric_Literal then
            Syntax_Error;
          end if;
          Get_Next_Token;
          declare
            Number_Image : constant String := Image_Of (Number_Token.all);
          begin
            return Build.Version_Number'value(Number_Image);
          exception
          when others =>
            Report_Error (Error.Version_Number_Out_Of_Range, Number_Token);
          end;
        end Version_Number;

        procedure Check (Is_Defined : Boolean;
                         Error_Kind : Error.Kind) is
        begin
          if not Is_Defined then
            Report_Error (Error_Kind, Pragma_Name);
          end if;
        end Check;

        function Next_String return String is
        begin
          String_Token := The_Token;
          if Kind_Of (String_Token.all) /= Is_String_Literal then
            Syntax_Error;
          end if;
          Get_Next_Token;
          declare
            Image : constant String := Image_Of (String_Token.all);
          begin
            return Image(Image'first + 1 .. Image'last - 1);
          end;
        end Next_String;

        function String_Element (Is_Defined : in out Boolean) return String is
          Image : constant String := Next_String;
        begin
          if Is_Defined then
            Report_Error (Error.Already_Defined, Argument_Handle);
          end if;
          Is_Defined := True;
          return Image;
        end String_Element;

        procedure Define_Icon is
          Id_Handle   : constant Lexical_Handle := The_Token;
          Id          : constant Identifier_Handle := Actual_Identifier;
          The_Boolean : constant String := Image_Of (Id.all);
        begin
          if Icon_Defined then
            Report_Error (Error.Already_Defined, Argument_Handle);
          end if;
          Id.Data := Data.Predefined_Name;
          if not Build.Defined_Icon (The_Boolean) then
            Report_Error (Error.Unknown_Boolean_Value, Id_Handle);
          end if;
          if The_Boolean = "True" then
            Icon_True_Handle := Id_Handle;
            if Kind_Defined and then Build.Is_Dll then
              Report_Error (Error.Icon_Not_Allowed_For_Dlls, Icon_True_Handle);
            end if;
          end if;
          Icon_Defined := True;
        end Define_Icon;

        procedure Define_Kind is
          Id_Handle : constant Lexical_Handle := The_Token;
          Id        : constant Identifier_Handle := Actual_Identifier;
          The_Kind  : constant String := Image_Of (Id.all);
        begin
          if Kind_Defined then
            Report_Error (Error.Already_Defined, Argument_Handle);
          end if;
          Id.Data := Data.Predefined_Name;
          if not Build.Defined_Kind (The_Kind) then
            Report_Error (Error.Unknown_Project_Kind, Id_Handle);
          end if;
          if Build.Is_Dll and then Icon_True_Handle /= null then
            Report_Error (Error.Icon_Not_Allowed_For_Dlls, Icon_True_Handle);
          end if;
          Kind_Defined := True;
        end Define_Kind;

        procedure Define_Version is
          The_Version : Build.Version;
        begin
          if Version_Defined then
            Report_Error (Error.Already_Defined, Argument_Handle);
          end if;
          Get_Element (Lexical.Left_Parenthesis);
          The_Version.Major := Version_Number;
          Get_Element (Lexical.Comma);
          The_Version.Minor := Version_Number;
          Get_Element (Lexical.Comma);
          The_Version.Variant := Version_Number;
          Get_Element (Lexical.Comma);
          The_Version.Revision := Version_Number;
          Get_Element (Lexical.Right_Parenthesis);
          Build.Define (The_Version);
          Version_Defined := True;
        end Define_Version;

        procedure Define_Compiler is
        begin
          if not Build.Defined_Compiler (String_Element (Compilers_Defined)) then
            Report_Error (Error.Unknown_Tools_Directory, String_Token);
          end if;
        end Define_Compiler;

        procedure Define_Compilers is
        begin
          if Compilers_Defined then
            Report_Error (Error.Already_Defined, Argument_Handle);
          end if;
          Get_Element (Lexical.Left_Parenthesis);
          declare
            First_Compiler : constant String := Next_String;
          begin
            if not Build.Exists (First_Compiler) then
              Report_Error (Error.Unknown_Tools_Directory, String_Token);
            end if;
            if Element_Is (Lexical.Comma) then
              declare
                Second_Compiler : constant String := Next_String;
              begin
                if not Build.Exists (Second_Compiler) then
                  Report_Error (Error.Unknown_Tools_Directory, String_Token);
                end if;
                Compilers_Defined := Build.Defined_Compilers (First  => First_Compiler,
                                                              Second => Second_Compiler);
                if not Compilers_Defined then
                  Report_Error (Error.Tools_Not_32_And_64_Bit, String_Token);
                end if;
              end;
            else
              Compilers_Defined := Build.Defined_Compilers (First  => First_Compiler,
                                                            Second => "");

            end if;
          end;
          Get_Element (Lexical.Right_Parenthesis);
        end Define_Compilers;

        type Library_Info (Size : Natural) is record
          Name        : String(1..Size);
          Error_Token : Lexical_Handle;
        end record;

        package Library_List is new Indefinite_Doubly_Linked_Lists (Library_Info);

        The_Library_List : Library_List.Item;
        The_Libraries    : Text.List;

        procedure Parse_Libraries is
        begin
          if Libraries_Defined then
            Report_Error (Error.Already_Defined, Argument_Handle);
          end if;
          Get_Element (Lexical.Left_Parenthesis);
          loop
            declare
              Library : constant String := Next_String;
            begin
              The_Library_List.Append (Library_Info'(Name        => Library,
                                                     Size        => Library'length,
                                                     Error_Token => String_Token));
              The_Libraries.Append (Library);
            end;
            exit when not Element_Is (Lexical.Comma);
          end loop;
          Get_Element (Lexical.Right_Parenthesis);
          Libraries_Defined := True;
        end Parse_Libraries;

        procedure Check_Libraries is
        begin
          for The_Info of The_Library_List loop
            declare
              Library : constant String := The_Info.Name;
            begin
              case Build.Check_Of (Library) is
              when Build.Library_Ok =>
                null;
              when Build.Library_Not_Found =>
                Report_Error (Error.Library_Not_Found, The_Info.Error_Token);
              end case;
            end;
          end loop;
        end Check_Libraries;

        procedure Define_Library is
        begin
          Check_Libraries;
          Build.Define_Libraries (The_Libraries);
        end Define_Library;

        procedure Define_Libraries is
        begin
          Check_Libraries;
          if Build.Has_Second_Tools_Directory then
            Check_Libraries;
            Build.Set_Back_To_First;
          end if;
          Build.Define_Libraries (The_Libraries);
        end Define_Libraries;

        procedure Define_Interface is

          The_Interface_Unit : Data_Handle;

          function Image_Of_Unit return String is
            Actual_Token : constant Lexical_Handle := The_Token;
          begin
            The_Token := String_Token;
            declare
              Image : constant String := Token.Image_Of (Unit_Name, '.');
            begin
              The_Token := Actual_Token;
              return Image;
            end;
          end Image_Of_Unit;

          The_Interface : Text.List;

        begin
          loop
            String_Token := The_Token;
            The_Interface_Unit := Name_Of (Scope);
            if The_Interface_Unit = null or else Data_Kind_Of (The_Interface_Unit.all) /= Is_Package_Specification then
              Report_Error (Error.Interface_Specification_Expected, String_Token);
            else
              declare
                Unit_Image : constant String := Image_Of_Unit;
              begin
                if The_Interface.Contains (Unit_Image) then
                  Report_Error (Error.Already_Defined, String_Token);
                end if;
                The_Interface.Append (Image_Of_Unit);
              end;
            end if;
            exit when not Element_Is (Lexical.Plus);
          end loop;
          Build.Define_Interface (The_Interface);
          Interface_Defined := True;
        end Define_Interface;

        procedure Define_Resource is
        begin
          if not Build.Defined_Resource (String_Element (Resource_Defined)) then
            Report_Error (Error.Resource_File_Not_Found, String_Token);
          end if;
        end Define_Resource;

      begin -- Handle_Build_Parameters
        if Build_Parameters_Defined then
          Report_Error (Error.Already_Defined, The_Token);
        end if;
        The_Handle.Is_Used := True;
        Get_Next_Token;
        Get_Element (Lexical.Left_Parenthesis);
        loop
          Argument_Handle := The_Token;
          declare
            Id          : constant Identifier_Handle := Declaring_Identifier;
            Argument    : constant String := Image_Of (Id.all);
          begin
            Id.Data := Data.Predefined_Name;
            Get_Element (Lexical.Association);
            if Argument = "Icon" then
              Define_Icon;
            elsif Argument = "Kind" then
              Define_Kind;
            elsif Argument = "Version" then
              Define_Version;
            elsif Argument = "Description" then
              Build.Define_Description (String_Element (Description_Defined));
            elsif Argument = "Compiler" then
              Define_Compiler;
              Define_Library;
            elsif Argument = "Compilers" then
              Define_Compilers;
              Define_Libraries;
            elsif Argument = "Libraries" then
              Parse_Libraries;
            elsif Argument = "Use_Interface" then
              Define_Interface;
            elsif Argument = "Resource" then
              Define_Resource;
            else
              Report_Error (Error.Syntax_Error, Argument_Handle);
            end if;
          end;
          exit when not Element_Is (Lexical.Comma);
        end loop;
        Get_Element (Lexical.Right_Parenthesis);
        Check (Compilers_Defined, Error.Compiler_Not_Defined);
        Check (Kind_Defined, Error.Kind_Not_Defined);
        Check (Version_Defined, Error.Version_Not_Defined);
        if Build.Is_Dll then
          Check (Interface_Defined, Error.Interface_Not_Defined);
        elsif Interface_Defined then
          Report_Error (Error.Only_For_Dlls, String_Token);
        end if;
        Build_Parameters_Defined := True;
      end Handle_Build_Parameters;

      use type Lexical.Style_Pragma;

    begin -- Pragma_Call
      Get_Next_Token;
      Check (Lexical.Pragma_Identifier);
      Pragma_Name := The_Token;
      The_Handle := Pragma_Identifier_Handle(The_Token);
      case The_Handle.Designator is
      when Lexical.Style_Pragma =>
        if Special_Comment_Detected then
          Report_Error (Error.Special_Comment_In_Use, The_Token);
        elsif The_Handle.Designator /= The_Style then
          The_Style := The_Handle.Designator;
          case The_Style is
          when Lexical.Is_Style_Soudronic =>
            Build.Define_Company ("Soudronic AG");
          when Lexical.Is_Style_Astronomy | Lexical.Is_Style_White_Elephant =>
            Build.Define_Company ("White Elephant GmbH");
          when Lexical.Is_Style_Unrestricted | Lexical.Is_Style_None =>
            null;
          end case;
        else
          Report_Error (Error.Style_Already_Set, The_Token);
        end if;
        The_Handle.Is_Used := True;
        Get_Next_Token;
      when Lexical.Is_Console_Application =>
        Report_Error (Error.Obsolescent_Pragma_Call, The_Token);
      when Lexical.Is_Build =>
        Handle_Build_Parameters;
      when Lexical.Obsolescent_Single_Pragma | Lexical.Obsolescent_Compound_Pragma =>
        if not Special_Comment_Detected then
          Conditional_Style_Error (Error.Obsolescent_Pragma_Call);
        end if;
        Handle_Other_Pragmas;
      when Lexical.Is_Unreferenced =>
        Get_Next_Element (Lexical.Left_Parenthesis);
        Pragma_Parameter (Handled => True);
        The_Handle.Is_Used := Is_Found;
      when others =>
        Handle_Other_Pragmas;
      end case;
      Get_Element (Lexical.Semicolon);
    end Pragma_Call;


    -- condition ::=
    --      boolean_expression
    --
    procedure Condition (Scope : Data.Unit_Handle) is
    begin
      Dummy := Expression ((Scope, Data.Predefined_Boolean));
    end Condition;


    -- range ::=
    --      range_attribute_reference
    --    | simple_expression .. simple_expression
    --
    --    range_attribute_reference ::=
    --         name ' range_attribute_designator
    --
    --       range_attribute_designator ::=
    --            range [ ( static_expression ) ]
    --
    function Range_Production (Within : Data.Context) return Data_Handle is
      Type_Mark : constant Data_Handle := Simple_Expression (Within);
    begin
      case Token_Element is
      when Lexical.Range_Delimiter =>
        Get_Next_Token;
        declare
          Saved_Token : constant Lexical_Handle := The_Token;
        begin
          if Simple_Expression (Within) = null then
            The_Token := Saved_Token;
            Dummy := Simple_Expression ((Within.Scope, Type_Mark));
          end if;
        end;
      when others =>
        null;
      end case;
      return Type_Mark;
    end Range_Production;


    -- range_constraint ::=
    --      range range
    --
    function Range_Constraint (Within : Data.Context) return Data_Handle is
    begin
      Get_Next_Token;
      return Range_Production (Within);
    end Range_Constraint;


    -- delta_constraint ::=
    --      delta static_expression [range_constraint]
    --
    procedure Delta_Constraint (Within : Data.Context) is
    begin
      Get_Next_Token;
      Dummy := Expression (Within);
      if Token_Element = Lexical.Is_Range then
        Dummy := Range_Constraint (Within);
      end if;
    end Delta_Constraint;


    -- digits_constraint ::=
    --      digits static_expression [range_constraint]
    --
    procedure Digits_Constraint (Within : Data.Context) renames Delta_Constraint;


    -- [not null]
    --
    procedure Conditional_Null_Exclusion is
    begin
      if Element_Is (Lexical.Is_Not) then
        Get_Element (Lexical.Is_Null);
      end if;
    end Conditional_Null_Exclusion;


    -- subtype_indication ::=
    --      [null_exlusion] subtype_indication_part
    --
    function Subtype_Indication (Within : Data.Context) return Data_Handle is
    begin
      Conditional_Null_Exclusion;
      return Subtype_Indication_Part (Within);
    end Subtype_Indication;


    -- subtype_indication_part ::=
    --      subtype_mark [constraint]
    --
    function Subtype_Indication_Part (Within : Data.Context) return Data_Handle is
      Type_Mark     : constant Data_Handle := Subtype_Mark (Within.Scope, Is_Subtype_Indication => True);
      Is_Class_Wide : constant Boolean := Is_Class_Wide_Type;
    begin
      Conditional_Constraint ((Within.Scope, Type_Mark));
      Is_Class_Wide_Type := Is_Class_Wide;
      return Type_Mark;
    end Subtype_Indication_Part;


    -- discrete_range ::=
    --      discrete_subtype_indication
    --    | range
    --
    function Discrete_Range_Part (Within     : Data.Context;
                                  First_Item : Data_Handle) return Data_Handle is
    begin
      case Token_Element is
      when Lexical.Range_Delimiter =>
        ---------------------------------------
        --Write_Log ("DISCRETE_RANGE (range)");
        ---------------------------------------
        Get_Next_Token;
        return Simple_Expression ((Within.Scope, First_Item));
      when others =>
        --TEST-------------------------------------------------------
        --Write_Log ("DISCRETE_RANGE (discrete_subtype_indication)");
        -------------------------------------------------------------
        Conditional_Constraint (Within);
        return First_Item;
      end case;
    end Discrete_Range_Part;


    function Discrete_Range (Within : Data.Context) return Data_Handle is
    begin
      return Discrete_Range_Part (Within, Simple_Expression (Within));
    end Discrete_Range;


    function Discrete_Range_Or_Expression (Within : Data.Context) return Data_Handle is
    begin
      return Discrete_Range_Part (Within, Expression (Within));
    end Discrete_Range_Or_Expression;


    -- discrete_subtype_definition ::=
    --      discrete_subtype_indication
    --    | range
    --
    function Discrete_Subtype_Definition (Scope : Data.Unit_Handle) return Data_Handle is
    begin
      return Discrete_Range ((Scope, null));
    end Discrete_Subtype_Definition;


    -- discrete_choice_list ::=
    --      discrete_choice { | discrete_choice}
    --
    --   discrete_choice ::=
    --        expression | discrete_range | others
    --
    function Discrete_Choice_List (Within : Data.Context) return Boolean is
    begin
      loop
        if Element_Is (Lexical.Is_Others) then
          return True;
        else
          Dummy := Discrete_Range_Or_Expression (Within);
        end if;
        exit when not Element_Is (Lexical.Vertical_Line);
      end loop;
      return False;
    end Discrete_Choice_List;


    -- attribute_designator ::=
    --      identifier[ ( static_expression ) ]
    --    | Access
    --    | Delta
    --    | Digits
    --
    function Attribute_Designator (Within : Data.Context) return Lexical.Attribute_Id is
      Designator : constant Lexical.Attribute_Id := Attribute_Handle(The_Token).Designator;
    begin
      if Next_Element_Is (Lexical.Left_Parenthesis) then
        loop
          Dummy := Expression (Within); -- handle argument
          if Element_Is (Lexical.Range_Delimiter) then -- handle range
            Dummy := Expression (Within);
          end if;
          exit when not Element_Is (Lexical.Comma);
        end loop;
        Get_Element (Lexical.Right_Parenthesis);
      end if;
      return Designator;
    end Attribute_Designator;


    -- selector_name ::=
    --      identifier
    --    | character_literal
    --    | operator_symbol
    --
    function Selector_Name return Identifier_Handle is
    begin
      case Token_Element is
      when Lexical.Identifier =>
        return Actual_Identifier;
      when Lexical.String_Literal =>
        Get_Next_Token;
      when others =>
        Syntax_Error;
      end case;
      return null;
    end Selector_Name;


    -- parameter_specification ::=
    --      defining_identifier_list : [aliased] mode [null_exlusion] subtype_mark [ := default_expression]
    --    | defining_identifier_list : access_definition [ := default_expression]
    --
    --    access_definition ::=
    --          [null_exlusion] access access_definition_part
    --
    function Parameter_Specification (Scope : Data.Unit_Handle) return Data.List.Item is

      Defining_Identifiers : constant Identifiers := Next_Defining_Identifier_List;

      The_Type    : Data_Handle;
      Has_Default : Boolean := False;

    begin
      Get_Element (Lexical.Colon);
      if Is_In_Standard and then Element_Is (Lexical.Special_Id) then
        The_Type := Data.Any_Type (Actual_Identifier);
        Is_Class_Wide_Type := False;
      else
        if Element_Is (Lexical.Is_Not) then
          Get_Element (Lexical.Is_Null);
          if Element_Is (Lexical.Is_Access) then
            The_Type := Access_Definition_Part (Scope);
          else
            The_Type := Subtype_Mark (Scope);
          end if;
        elsif Element_Is (Lexical.Is_Access) then
          The_Type := Access_Definition_Part (Scope);
        else
          Get_Conditional (Lexical.Is_Aliased);
          Mode;
          Conditional_Null_Exclusion;
          The_Type := Subtype_Mark (Scope);
        end if;
      end if;
      declare
        Is_Class_Wide : constant Boolean := Is_Class_Wide_Type;
      begin
        if Element_Is (Lexical.Assignment) then
          Has_Default := True;
          The_Target_Type := The_Type;
          Dummy := Expression ((Scope, The_Type));
        end if;
        return Data.New_Parameter_List (Parameter_Names => Defining_Identifiers,
                                        Subtype_Mark    => The_Type,
                                        Is_Class_Wide   => Is_Class_Wide,
                                        Has_Default     => Has_Default,
                                        Parent          => Scope);
      end;
    end Parameter_Specification;


    -- formal_part ::=
    --      ( parameter_specification { ; parameter_specification} )
    --
    function Formal_Part (Scope : Data.Unit_Handle) return Data.List.Item is
      The_List : Data.List.Item;
    begin
      The_List := Parameter_Specification (Scope);
      while Token_Element = Lexical.Semicolon loop
        The_List.Append (Parameter_Specification (Scope));
      end loop;
      Get_Element (Lexical.Right_Parenthesis);
      return The_List;
    end Formal_Part;


    -- parameter_profile ::=
    --      [formal_part]
    --
    -- parameter_and_result_profile ::=
    --      [formal_part] return ([null_exclusion] subtype_mark) | access_definition
    --
    function Subprogram_Profile (Scope       : Data.Unit_Handle;
                                 Is_Function : Boolean := False) return Data.Subprogram_Profile is
      The_Profile : Data.Subprogram_Profile;
    begin
      if Token_Element = Lexical.Left_Parenthesis then
        The_Profile := Data.New_Profile (Formal_Part (Scope));
      else
        The_Profile := Data.New_Profile;
      end if;
      if Is_Function then
        Get_Element (Lexical.Is_Return);
        if Element_Is (Lexical.Is_Not) then
          Get_Element (Lexical.Is_Null);
          if Element_Is (Lexical.Is_Access) then
            The_Profile.Result_Type := Access_Definition_Part (Scope);
          else
            The_Profile.Result_Type := Subtype_Mark (Scope);
          end if;
        elsif Element_Is (Lexical.Is_Access) then
          The_Profile.Result_Type := Access_Definition_Part (Scope);
        else
          The_Profile.Result_Type := Data.Type_Of(Subtype_Mark (Scope));
        end if;
      end if;
      return The_Profile;
    end Subprogram_Profile;


    -- [actual_parameter_part] ::=
    --      [ ( parameter_association { , parameter_association} ) ]
    --
    --    parameter_association ::=
    --         [formal_parameter_selector_name => ] explicit_actual_parameter
    --
    --       explicit_actual_parameter ::=
    --            expression
    --          | variable_name
    --
    function Found_Actual_Parameters (Scope         : Data.Unit_Handle;
                                      Profile       : Data.Subprogram_Profile;
                                      Instantiation : Data.Instantiation_Handle := null;
                                      Parent_Class  : Data.Type_Handle := null;
                                      Complete      : Boolean := False;
                                      Is_Equality   : Boolean := False) return Boolean is

      use type Data.List.Elements_Access;

      Has_Parameters : constant Boolean := Element_Is (Lexical.Left_Parenthesis);

      Parameters_First : Natural;
      Parameters_Last  : Natural;

    begin
      --TEST-----------------------------------------------------------------------------------------------------
      --Write_Log ("Found_Actual_Parameters (Scope         : " & Data.Full_Name_Of (Data_Handle(Scope)));
      --Write_Log ("                         Instantiation : " & Data.Full_Name_Of (Data_Handle(Instantiation)));
      --Write_Log ("                         Parent_Class  : " & Data.Full_Name_Of (Data_Handle(Parent_Class)));
      --Write_Log ("                         Complete      : " & Boolean'image(Complete));
      -----------------------------------------------------------------------------------------------------------
      if Profile.Parameters = null then
        if Has_Parameters then
          Parameters_First := 1;
          Parameters_Last := 0;
        else
          --TEST------------------------
          --Write_Log ("NO PARAMETERS");
          ------------------------------
          return True;
        end if;
      else
        Parameters_First := Profile.Parameters'first;
        Parameters_Last := Profile.Parameters'last;
      end if;
      declare
        Not_Found : constant Natural := Data.Parameter_Not_Found;

        The_Position      : Natural := Not_Found;
        Position_Is_Known : Boolean := True;

        function Position_Found return Boolean with Inline is
        begin
          if Element_Ahead_Is (Lexical.Association) then
            The_Position := Data.Parameter_Index_Of (Profile, Selector_Name);
            if The_Position = Not_Found then
              return False;
            end if;
            Get_Element (Lexical.Association);
            Position_Is_Known := False;
          elsif not Position_Is_Known then
            Syntax_Error;
          else
            The_Position := Natural'succ(The_Position);
          end if;
          return The_Position <= Parameters_Last;
        end Position_Found;

        function Found_Equal_Parameters return Boolean is
          Saved_Token : constant Lexical_Handle := The_Token;
          The_Type    : Data_Handle;
        begin
          --TEST------------------------------------
          --Write_Log ("Match Predefined Equality");
          ------------------------------------------
          if Position_Found then
            The_Type := Expression ((Scope, null));
            if Element_Is (Lexical.Comma) then
              if Position_Found then
                if Data.Matches (The_Type, Expression ((Scope, The_Type)), Instantiation) then
                  if Element_Is (Lexical.Right_Parenthesis) then
                    --TEST-----------------------------------------
                    --Write_Log ("-> MATCHED PREDEFINED EQUALITY");
                    -----------------------------------------------
                    return True;
                  end if;
                end if;
              end if;
            end if;
          end if;
          The_Token := Saved_Token;
          The_Position := Not_Found;
          Position_Is_Known := True;
          return False;
        end Found_Equal_Parameters;


        type Parameters is array (Parameters_First..Parameters_Last) of Boolean;

        Found_Parameters : Parameters := [others => False];
        Associated       : Boolean := True;

        function Parameter_Associated return Boolean with Inline is

          The_Parameter     : Data.Object_Handle;
          The_Expected_Type : Data_Handle;
          The_Type          : Data_Handle;

          use type Data.Instantiation_Handle;
          use type Data.Object_Handle;

        begin
          --TEST-------------------------------------
          --Write_Log ("CHECK Parameter_Associated");
          -------------------------------------------
          The_Parameter := Data.Parameter_Object_Of (Profile, The_Position);
          if The_Parameter = null or else The_Parameter.Object_Type = null then
            --TEST---------------------------------
            --Write_Log ("Unknown Parameter Type");
            ---------------------------------------
            The_Type := Expression ((Scope, null));
            return False;
          end if;
          declare
            Saved_Token       : constant Lexical_Handle := The_Token;
            The_Instantiation : Data.Instantiation_Handle := Instantiation;
          begin
            loop
              The_Expected_Type := The_Parameter.Object_Type;
              if The_Instantiation /= null then
                --TEST-------------------------------------------------------------------------------------------------
                --Write_Log ("Parameter_Associated - Instantiation: " & Name.Image_Of (The_Instantiation.Location.Id));
                --Write_Log ("                     - Expected_Type: " & Data.Full_Name_Of (The_Expected_Type));
                -------------------------------------------------------------------------------------------------------
                if The_Expected_Type /= null and then The_Expected_Type.all in Data.Formal_Type'class then
                  The_Expected_Type := Data.Actual_Type_Of (Data.Formal_Handle(The_Expected_Type), The_Instantiation);
                  --TEST-----------------------------------------------------------------------------------------
                  --Write_Log ("Formal Instantiation - Expected_Type: " & Data.Full_Name_Of (The_Expected_Type));
                  -----------------------------------------------------------------------------------------------
                  if The_Expected_Type /= null and then The_Expected_Type.all in Data.Formal_Type'class then
                    The_Expected_Type := Data.Actual_Type_Of (Data.Formal_Handle(The_Expected_Type), The_Instantiation);
                    --TEST---------------------------------------------------------------------------------------
                    --Write_Log ("Formal Instantiation - Actual_Type: " & Data.Full_Name_Of (The_Expected_Type));
                    ---------------------------------------------------------------------------------------------
                  end if;
                else
                  The_Expected_Type := Data.New_Instantiation (The_Expected_Type, The_Instantiation);
                  --TEST----------------------------
                  --Write_Log ("New Instantiation");
                  ----------------------------------
                end if;
              end if;
              The_Type := Expression ((Scope, The_Expected_Type));
              if The_Type /= null and then The_Type.all in Data.Instantiated_Type'class then
                The_Type := Data.Item_Instantiation(The_Type).Item;
              end if;
              if Data.Matches (The_Parameter     => The_Type,
                               Profile_Parameter => The_Parameter,
                               Instantiation     => The_Instantiation)
              then
                return True;
              end if;
              exit when The_Instantiation = null;
              The_Instantiation := The_Instantiation.Parent_Instantiation;
              exit when The_Instantiation = null;
              The_Token := Saved_Token;
            end loop;
            return False;
          end;
        end Parameter_Associated;


        The_Parameter     : Data.Object_Handle;
        The_Expected_Type : Data_Handle;

        function Parent_Parameter_Associated return Boolean is
          The_Type : Data_Handle;
        begin
          --TEST------------------------------------------------------------------------------
          --Write_Log ("CHECK Parent_Parameter_Associated");
          --Write_Log ("- PARENT   : " & Image_Of (Parent_Class.Location.all));
          --Write_Log ("      TYPE : " & Ada.Tags.External_Tag (Parent_Class.all'tag));
          --if The_Expected_Type = null then
          --  Write_Log ("- EXPECTED : UNKNOWN TYPE");
          --else
          --  if Is_Null (The_Expected_Type.Location) then
          --    Write_Log ("- EXPECTED : UNKNOWN NAME");
          --  else
          --    Write_Log ("- EXPECTED : " & Image_Of (The_Expected_Type.Location.all));
          --  end if;
          --  Write_Log ("      TYPE : " & Ada.Tags.External_Tag (The_Expected_Type.all'tag));
          --end if;
          ------------------------------------------------------------------------------------
          --if Instantiation /= null and then The_Expected_Type.all in Data.Formal_Type'class then
          --  The_Expected_Type := Data.Actual_Type_Of (Data.Formal_Handle(The_Expected_Type), Instantiation);
          --end if;
          if Data.Matches (The_Expected_Type, Data_Handle(Parent_Class), Instantiation) then
            --TEST---------------------------------
            --Write_Log ("MATCH WITH PARENT TYPE");
            ---------------------------------------
            The_Type := Expression ((Scope, The_Expected_Type));
            --TEST------------------------------------------------------------------------
            --if The_Type /= null then
            --  if Is_Null (The_Type.Location) then
            --    Write_Log ("- ACTUAL      : NO NAME");
            --  else
            --    Write_Log ("- ACTUAL      : " & Image_Of (The_Type.Location.all));
            --  end if;
            --  Write_Log ("         TYPE : " & Ada.Tags.External_Tag (The_Type.all'tag));
            --end if;
            ------------------------------------------------------------------------------
            if Data.Is_In (Data_Handle(Parent_Class), The_Type) then
              --TEST---------------------
              --Write_Log ("-> MATCHED");
              ---------------------------
              return True;
            --TEST---------------------------
            --else
            --  Write_Log ("-> NOT MATCHED");
            ---------------------------------
            end if;
          end if;
          return False;
        end Parent_Parameter_Associated;

        use type Data.Type_Handle;

      begin -- Found_Actual_Parameters
        if Has_Parameters then
          if Is_Equality then
            if Found_Equal_Parameters then
              return True;
            end if;
          end if;
          loop
            if Position_Found then
              declare
                Saved_Token : constant Lexical_Handle := The_Token;
              begin
                if Parameter_Associated then
                  Found_Parameters(The_Position) := True;
                else
                  if Parent_Class = null then
                    if Complete then
                      Associated := False;
                    else
                      return False;
                    end if;
                  else
                    The_Parameter := Data.Parameter_Object_Of (Profile, The_Position);
                    The_Expected_Type := The_Parameter.Object_Type;
                    The_Token := Saved_Token;
                    if Parent_Parameter_Associated then
                      if The_Position /= Not_Found then
                        Found_Parameters(The_Position) := True;
                      end if;
                    else
                      if The_Expected_Type /= null and then
                        Is_Null (The_Expected_Type.Location) and then The_Expected_Type.all in Data.Access_Type'class
                      then -- check for formal anonyme access type
                        The_Expected_Type := Data.Type_Handle(The_Expected_Type).Parent_Type;
                        The_Token := Saved_Token;
                        if Parent_Parameter_Associated then
                          if The_Position /= Not_Found then
                            Found_Parameters(The_Position) := True;
                          end if;
                        else
                          if Complete then
                            Associated := False;
                          else
                            return False;
                          end if;
                        end if;
                      else
                        if Complete then
                          Associated := False;
                        else
                          return False;
                        end if;
                      end if;
                    end if;
                  end if;
                end if;
              end;
            elsif Complete then
              Associated := False;
              if Element_Is (Lexical.Association) then
                Position_Is_Known := False;
              elsif not Position_Is_Known then
                Syntax_Error;
              end if;
              Dummy := Expression ((Scope, null));
            else
              --TEST--------------------------------
              --Write_Log ("-> POSITION NOT FOUND");
              --------------------------------------
              return False;
            end if;
            if not Associated and then Element_Is (Lexical.Range_Delimiter) then -- array slice
              Dummy := Expression ((Scope, Dummy));
            end if;
            exit when Element_Is (Lexical.Right_Parenthesis);
            Get_Element (Lexical.Comma);
          end loop;
        end if;
        for Index in Found_Parameters'range loop
          if not Found_Parameters(Index) and then not Data.Has_Default_Parameter (Profile, Index) then
            return False;
          end if;
        end loop;
        return Associated;
      end;
    end Found_Actual_Parameters;


    function Found_Method_Parameters (Scope         : Data.Unit_Handle;
                                      Profile       : Data.Subprogram_Profile;
                                      Instantiation : Data.Instantiation_Handle) return Boolean is

      Before_Profile : constant Lexical_Handle := The_Token;
      Has_Parameters : constant Boolean := Element_Is (Lexical.Left_Parenthesis);

      Parameters_First : constant Natural := Profile.Parameters'first + 1;
      Parameters_Last  : constant Natural := Profile.Parameters'last;

      Not_Found : constant Natural := Profile.Parameters'first;

      The_Position      : Natural := Not_Found;
      Position_Is_Known : Boolean := True;

      function Position_Found return Boolean with Inline is
      begin
        if Element_Ahead_Is (Lexical.Association) then
          The_Position := Data.Parameter_Index_Of (Profile, Selector_Name);
          if The_Position = Data.Parameter_Not_Found then
            return False;
          end if;
          Get_Element (Lexical.Association);
          Position_Is_Known := False;
        elsif not Position_Is_Known then
          Syntax_Error;
        else
          The_Position := Natural'succ(The_Position);
        end if;
        return The_Position <= Parameters_Last;
      end Position_Found;

      type Parameters is array (Parameters_First..Parameters_Last) of Boolean;

      Found_Parameters : Parameters := [others => False];

      function Parameter_Associated return Boolean with Inline is

        The_Parameter     : Data.Object_Handle;
        The_Expected_Type : Data_Handle;
        The_Type          : Data_Handle;

        use type Data.Instantiation_Handle;
        use type Data.Object_Handle;

      begin
        --TEST-------------------------------------
        --Write_Log ("CHECK Parameter_Associated");
        -------------------------------------------
        The_Parameter := Data.Parameter_Object_Of (Profile, The_Position);
        if The_Parameter = null or else The_Parameter.Object_Type = null then
          --TEST---------------------------------
          --Write_Log ("Unknown Parameter Type");
          ---------------------------------------
          The_Type := Expression ((Scope, null));
          return False;
        end if;
        declare
          Saved_Token       : constant Lexical_Handle := The_Token;
          The_Instantiation : Data.Instantiation_Handle := Instantiation;
        begin
          loop
            The_Expected_Type := The_Parameter.Object_Type;
            if The_Instantiation /= null then
              --TEST-------------------------------------------------------------------------------------------------
              --Write_Log ("Parameter_Associated - Instantiation: " & Name.Image_Of (The_Instantiation.Location.Id));
              --Write_Log ("                     - Expected_Type: " & Data.Full_Name_Of (The_Expected_Type));
              -------------------------------------------------------------------------------------------------------
              if The_Expected_Type /= null and then The_Expected_Type.all in Data.Formal_Type'class then
                The_Expected_Type := Data.Actual_Type_Of (Data.Formal_Handle(The_Expected_Type), The_Instantiation);
                --TEST-----------------------------------------------------------------------------------------
                --Write_Log ("Formal Instantiation - Expected_Type: " & Data.Full_Name_Of (The_Expected_Type));
                -----------------------------------------------------------------------------------------------
                if The_Expected_Type /= null and then The_Expected_Type.all in Data.Formal_Type'class then
                  The_Expected_Type := Data.Actual_Type_Of (Data.Formal_Handle(The_Expected_Type), The_Instantiation);
                  --TEST---------------------------------------------------------------------------------------
                  --Write_Log ("Formal Instantiation - Actual_Type: " & Data.Full_Name_Of (The_Expected_Type));
                  ---------------------------------------------------------------------------------------------
                  if The_Expected_Type /= null and then The_Expected_Type.all in Data.Formal_Type'class then
                    The_Type := Expression ((Scope, null));
                    return True;
                  end if;
                end if;
              else
                The_Expected_Type := Data.New_Instantiation (The_Expected_Type, The_Instantiation);
                --TEST----------------------------
                --Write_Log ("New Instantiation");
                ----------------------------------
              end if;
            end if;
            The_Type := Expression ((Scope, The_Expected_Type));
            --TEST-------------------------------------------------------------------------------------
            --Write_Log ("Match - Expected_Type : " & Data.Full_Name_Of (The_Expected_Type));
            --if The_Type /= null then
            --  Write_Log ("      - Actual_Type   : " & Data.Full_Name_Of (The_Type));
            --end if;
            --if The_Parameter.Object_Type /= null then
            --  Write_Log ("      - Parameter_Type: " & Data.Full_Name_Of (The_Parameter.Object_Type));
            --end if;
            -------------------------------------------------------------------------------------------
            if The_Type /= null and then The_Type.all in Data.Instantiated_Type'class then
              The_Type := Data.Item_Instantiation(The_Type).Item;
            end if;
            if Data.Matches (The_Parameter     => The_Type,
                             Profile_Parameter => The_Parameter,
                             Instantiation     => The_Instantiation)
            then
              --TEST----------------------------
              --Write_Log ("Parameter matches");
              ----------------------------------
              return True;
            end if;
            exit when The_Instantiation = null;
            The_Instantiation := The_Instantiation.Parent_Instantiation;
            exit when The_Instantiation = null;
            The_Token := Saved_Token;
          end loop;
          The_Token := Before_Profile;
          return False;
        end;
      end Parameter_Associated;

    begin -- Found_Method_Parameters
      --TEST-------------------------------------------------------------------------------------
      --Write_Log ("Found_Method_Parameters - Scope: " & Data.Full_Name_Of (Data_Handle(Scope)));
      -------------------------------------------------------------------------------------------
      if not Has_Parameters then
        --TEST------------------------
        --Write_Log ("NO PARAMETERS");
        ------------------------------
        return True;
      end if;
      loop
        if Position_Found then
          if Parameter_Associated then
            Found_Parameters(The_Position) := True;
          else
            The_Token := Before_Profile;
            return False;
          end if;
        else
          if Element_Is (Lexical.Association) then
            Position_Is_Known := False;
          elsif not Position_Is_Known then
            Syntax_Error;
          end if;
          Dummy := Expression ((Scope, null));
          if Element_Is (Lexical.Range_Delimiter) then
            Dummy := Expression ((Scope, Dummy));
          end if;
        end if;
        exit when Element_Is (Lexical.Right_Parenthesis);
        Get_Element (Lexical.Comma);
      end loop;
      for Index in Found_Parameters'range loop
        if not Found_Parameters(Index) and then not Data.Has_Default_Parameter (Profile, Index) then
          The_Token := Before_Profile;
          return False;
        end if;
      end loop;
      return True;
    end Found_Method_Parameters;


    The_Parameter_Type : Data_Handle;

    function Conditional_Parameter_Slice_Or_Type_Conversion (Scope : Data.Unit_Handle;
                                                             The_Instantiation : out Data.Instantiation_Handle)
                                                            return Data.Type_Handle is

      The_Parent_Class  : Data.Type_Handle;
      Position_Is_Known : Boolean := True;

      use type Data.Type_Handle;

      procedure Parameter_Association is
        The_Type : Data_Handle;
      begin
        if Next_Element_Ahead_Is (Lexical.Association) then
          Get_Element (Lexical.Association);
          Position_Is_Known := False;
        elsif not Position_Is_Known then
          Syntax_Error;
        end if;
        The_Type := Expression ((Scope, null));
        if The_Type /= null then
          The_Parameter_Type := The_Type;
          if The_Parent_Class = null then
            The_Parent_Class := Data.Parent_Type_Of (The_Parameter_Type, The_Instantiation);
          end if;
        end if;
        if Element_Is (Lexical.Apostrophe) then -- qualified_expression
          Dummy := Aggregate ((Scope, The_Type));
        end if;
        if Element_Is (Lexical.Range_Delimiter) then
          Dummy := Expression ((Scope, The_Type));
          The_Parent_Class := null;
        end if;
      end Parameter_Association;

    begin -- Conditional_Parameter_Slice_Or_Type_Conversion
      The_Parameter_Type := null;
      The_Instantiation := null;
      if Element_Is (Lexical.Left_Parenthesis) then
        Parameter_Association;
        while Element_Is (Lexical.Comma) loop
          Parameter_Association;
        end loop;
        Get_Element (Lexical.Right_Parenthesis);
      end if;
      return The_Parent_Class;
    end Conditional_Parameter_Slice_Or_Type_Conversion;


    -- name ::=
    --      direct_name
    --    | explicit_dereference
    --    | indexed_component
    --    | slice
    --    | selected_component
    --    | attribute_reference
    --    | type_conversion
    --    | function_call
    --    | character_literal
    --    | target_name
    --
    --    explicit_dereference ::=
    --         name . all
    --
    --    indexed_component ::=
    --         name ( expression {, expression} )
    --
    --    slice ::=
    --         name ( discrete_range )
    --
    --    selected_component ::=
    --         name . selector_name
    --
    --    attribute_reference ::=
    --         name ' attribute_designator
    --
    --    type_conversion ::=
    --         subtype_mark ( expression )
    --       | subtype_mark ( name )
    --
    --    function_call ::=
    --         name [actual_parameter_part]
    --
    The_Actual_Record_Definition : Data.Record_Handle;

    function Name_Of (Within                : Data.Context;
                      Procedure_Allowed     : Boolean := False;
                      No_Association        : Boolean := False;
                      Is_Subtype_Mark       : Boolean := False;
                      Is_Subtype_Indication : Boolean := False) return Data_Handle is

      The_Item        : Identifier_Handle;
      The_Declaration : Data_Handle;
      Is_Component    : Boolean := False;


      procedure Name_Continuation (Result_Type  : Data_Handle;
                                   Instatiation : Data.Instantiation_Handle) is

        Scope : constant Data.Unit_Handle := Within.Scope;

        The_Attribute     : Lexical.Attribute_Id;
        The_Subprogram    : Identifier_Handle;
        The_Instantiation : Data.Instantiation_Handle := Instatiation;


        function Found_Identifier_In_Unit return Boolean is
          The_Unit : constant Data.Unit_Handle := Data.Unit_Handle (The_Declaration);
        begin
          The_Item := Actual_Identifier;
          The_Declaration := Data.Declaration_From (The_Unit, The_Item);
          if The_Declaration = null then
            The_Declaration := Data_Handle(Data.Declaration_From (Resource.Generations, The_Unit, The_Item));
          end if;
          return The_Declaration /= null;
        end Found_Identifier_In_Unit;


        procedure Conditional_Actual_Parameter_Part is
          Profile     : constant Data.Subprogram_Profile := Data.Profile_Of (Data.Declaration_Handle(The_Declaration));
          Saved_Token : constant Lexical_Handle := The_Token;
        begin
          if Found_Actual_Parameters (Scope, Profile) then
            The_Declaration := Profile.Result_Type;
          else
            The_Token := Saved_Token;
            The_Declaration := null;
          end if;
        end Conditional_Actual_Parameter_Part;


        function Associated_Call return Boolean is

          Actual_Token : constant Lexical_Handle := The_Token;

          The_Call      : Data.Declaration_Handle := Data.Declaration_Handle(The_Declaration);
          Is_Overloaded : Boolean := False;

          The_Used_Call        : Data.Declaration_Handle;
          The_Used_Token       : Lexical_Handle;
          The_Used_Declaration : Data_Handle;

          The_Best_Match : Data.Declaration_Handle;
          The_Best_Token : Lexical_Handle;

          use type Data.Declaration_Handle;

        begin
          --TEST--------------------------------------------------------------------------
          --Write_Log ("ASSOCIATE CALL " & Image_Of (The_Call.Location.all));
          --if Result_Type = null then
          --  Write_Log ("- Result_Type: NULL");
          --else
          --  Write_Log ("- Result_Type: " & Ada.Tags.External_Tag (Result_Type.all'tag));
          --end if;
          --------------------------------------------------------------------------------
          if Data.Overload_Of (The_Call) /= null then
            Is_Overloaded := True;
            --TEST--------------------------
            --Write_Log ("- Is Overloaded");
            --------------------------------
          end if;
          loop
            --TEST---------------------------------------
            --Write_Log ("- COMPARE PROFILE PARAMETERS");
            ---------------------------------------------
            if Found_Actual_Parameters (Scope,
                                        Data.Profile_Of (The_Call),
                                        The_Instantiation,
                                        null,
                                        Complete    => not Is_Overloaded,
                                        Is_Equality => The_Call.all in Data.Predefined_Equality_Operation'class)
            then
              --TEST-------------------------------------
              --Write_Log ("-> FOUND ACTUAL PARAMETERS");
              -------------------------------------------
              declare
                Profile_Result_Type : constant Data_Handle := Data.Profile_Of(The_Call).Result_Type;
              begin
                if (Profile_Result_Type = null) and Procedure_Allowed then
                  if The_Call.all in Data.Used_Subprogram'class then
                    The_Used_Call := The_Call;
                    The_Used_Token := The_Token;
                    The_Used_Declaration := null;
                  else
                    The_Declaration := null;
                    The_Subprogram.Data := Data_Handle(The_Call);
                    The_Call.Is_Used := True;
                    --TEST----------------------------------
                    --Write_Log ("-> PROCEDURE ASSOCIATED");
                    ----------------------------------------
                    return True;
                  end if;
                elsif Result_Type = null then
                  --TEST---------------------------------
                  --Write_Log ("-> Result type unknown");
                  ---------------------------------------
                  if The_Call.all in Data.Used_Subprogram'class then
                    The_Used_Call := The_Call;
                    The_Used_Token := The_Token;
                    The_Used_Declaration := null;
                  else
                    The_Best_Match := The_Call;
                    The_Best_Token := The_Token;
                    The_Token := Actual_Token;
                  end if;
                  exit;
                elsif Is_Overloaded then -- is overloaded function
                  declare
                    Saved_Declaration : constant Data_Handle := The_Declaration;
                    The_Matching_Type : Data_Handle := Profile_Result_Type;
                  begin
                    case Token_Element is
                    when Lexical.Period
                       | Lexical.Left_Parenthesis
                       | Lexical.Apostrophe
                    =>
                      --TEST----------------------------------
                      --Write_Log ("- FUNCTION CONTINUATION");
                      ----------------------------------------
                      The_Declaration := Profile_Result_Type;
                      Name_Continuation (Result_Type, The_Instantiation);
                      The_Matching_Type := The_Declaration;
                      The_Declaration := Saved_Declaration;
                    when others =>
                      null;
                    end case;
                    if Data.Matches (The_Matching_Type, Result_Type, The_Instantiation) then
                      if not (The_Call.all in Data.Used_Subprogram'class) then
                        The_Declaration := The_Matching_Type;
                        The_Subprogram.Data := Data_Handle(The_Call);
                        The_Call.Is_Used := True;
                        --TEST---------------------------------
                        --Write_Log ("-> FUNCTION ASSOCIATED");
                        ---------------------------------------
                        return True;
                      end if;
                      --TEST-----------------------------------
                      --Write_Log ("-> Used function matched");
                      -----------------------------------------
                      The_Used_Call := The_Call;
                      The_Used_Token := The_Token;
                      The_Used_Declaration := The_Matching_Type;
                    else
                      The_Best_Match := The_Call;
                      The_Best_Token := The_Token;
                    end if;
                  end;
                elsif Data.Matches (Profile_Result_Type, Result_Type, The_Instantiation) then
                  The_Subprogram.Data := Data_Handle(The_Call);
                  if The_Call.all in Data.Used_Subprogram'class then
                    Data.Set_Used (The_Call.Location.Data);
                  else
                    The_Call.Is_Used := True;
                  end if;
                  The_Declaration := Profile_Result_Type;
                  --TEST--------------------------------------------------------
                  --Write_Log ("-> FUNCTION ASSOCIATED (No Result Type Check)");
                  --------------------------------------------------------------
                  return True;
                else
                  The_Best_Match := The_Call;
                  The_Best_Token := The_Token;
                  The_Token := Actual_Token;
                  --TEST---------------------------------
                  --Write_Log ("-> Best function match");
                  ---------------------------------------
                  exit;
                end if;
              end;
            elsif not Is_Overloaded then
              The_Best_Match := The_Call;
              The_Best_Token := The_Token;
              The_Token := Actual_Token;
              --TEST--------------------------------------
              --Write_Log ("-> Not overloaded procedure");
              --------------------------------------------
              exit;
            end if;
            The_Call := Data.Overload_Of (The_Call);
            The_Token := Actual_Token;
            exit when The_Call = null;
          end loop;
          if The_Declaration.all in Data.Unit_Type'class then
            declare
              The_Unit               : Data.Unit_Handle := Data.Unit_Handle(The_Declaration);
              The_Generic_Parameters : Data.Formal_Block_Handle;
              use type Data.Formal_Block_Handle;
            begin
              loop
                Data.Get_Inner_Generic_Parameters_Of (The_Unit, The_Generic_Parameters);
                if The_Generic_Parameters = null then
                  exit;
                else
                  declare
                    Call : constant Data_Handle := Data.Declaration_Of (The_Subprogram, The_Generic_Parameters.all);
                  begin
                    if Call /= null and then Call.all in Data.Formal_Subprogram'class then
                      The_Call := Data.Declaration_Handle(Call);
                      loop
                        if Result_Type = null or else
                          Data.Matches (Data.Formal_Subprogram_Handle(The_Call).Profile.Result_Type,
                                        Result_Type,
                                        The_Instantiation)
                        then
                          --TEST---------------------------------------------------------------
                          --Write_Log ("-> Generic subprogram parameter - Result type mached");
                          ---------------------------------------------------------------------
                          if Found_Actual_Parameters (Scope, Data.Formal_Subprogram_Handle(The_Call).Profile) then
                            --TEST------------------------------------------------
                            --Write_Log ("-> FOUND GENERIC SUBPROGRAM PARAMETER");
                            ------------------------------------------------------
                            The_Declaration := Data.Formal_Subprogram_Handle(The_Call).Profile.Result_Type;
                            The_Subprogram.Data := Data_Handle(The_Call);
                            Data.Set_Used (The_Call.Location.Data);
                            return True;
                          end if;
                        end if;
                        The_Token := Actual_Token;
                        The_Call := Data.Declaration_Handle(Data.Formal_Subprogram_Handle(The_Call).Overload);
                        exit when The_Call = null;
                      end loop;
                    end if;
                    The_Unit := The_Unit.Parent;
                  end;
                end if;
              end loop;
            end;
          end if;
          if The_Used_Call /= null then
            --TEST------------------------------
            --Write_Log ("ASSOCIATE USED CALL");
            ------------------------------------
            The_Subprogram.Data := Data_Handle(The_Used_Call);
            Data.Set_Used (The_Used_Call.Location.Data);
            The_Declaration := The_Used_Declaration;
            The_Token := The_Used_Token;
            return True;
          elsif The_Best_Match /= null then
            --TEST-------------------------------------
            --Write_Log ("ASSOCIATED WITH BEST MATCH");
            -------------------------------------------
            The_Declaration := Data.Profile_Of(The_Best_Match).Result_Type;
            The_Subprogram.Data := Data_Handle(The_Best_Match);
            The_Best_Match.Is_Used := True;
            The_Token := The_Best_Token;
            return True;
          end if;
          --TEST-------------------------
          --Write_Log ("NOT ASSOCIATED");
          -------------------------------
          return False;
        end Associated_Call;


        procedure Associate_Call is
        begin
          if Associated_Call then
            return;
          end if;
          The_Declaration := null;
        end Associate_Call;


        procedure Associate_Formal_Call is

          Caller       : constant Identifier_Handle := The_Actual_Identifier;
          Actual_Token : constant Lexical_Handle := The_Token;

          The_Call             : Data.Formal_Subprogram_Handle;
          Unused_Instantiation : Data.Instantiation_Handle;
          The_Scope            : Data.Unit_Handle := Data.Declaration_Handle(The_Declaration).Parent;

          use type Data.Unit_Handle;
          use type Data.Formal_Subprogram_Handle;

        begin
          loop
            The_Call := Data.Formal_Subprogram_Handle (The_Declaration);
            loop
              --TEST--------------------------------------------------------------------
              --Write_Log ("ASSOCIATE FORMAL CALL " & Image_Of (The_Call.Location.all));
              --------------------------------------------------------------------------
              if Result_Type = null or else
                Data.Matches (The_Call.Profile.Result_Type,
                              Result_Type,
                              Instantiation => null)
              then
                --TEST---------------------------
                --Write_Log ("-> Return Mached");
                ---------------------------------
                if Found_Actual_Parameters (Scope, The_Call.Profile) then
                  --TEST-------------------------------------
                  --Write_Log ("-> FOUND ACTUAL PARAMETERS");
                  -------------------------------------------
                  The_Declaration := The_Call.Profile.Result_Type;
                  return;
                end if;
                --TEST-----------------------------------------
                --Write_Log ("-> NOT FOUND ACTUAL PARAMETERS");
                -----------------------------------------------
              end if;
              The_Token := Actual_Token;
              exit when The_Call.Overload = null;
              The_Call := The_Call.Overload;
              Caller.Data := Data_Handle(The_Call);
            end loop;
            The_Scope := The_Scope.Parent;
            exit when The_Scope = null;
            The_Declaration := Data.Inner_Declaration_From (The_Scope, The_Call.Location);
            exit when The_Declaration = null or else not (The_Declaration.all in Data.Formal_Subprogram'class);
            Caller.Data := The_Declaration;
          end loop;
          --TEST-------------------------
          --Write_Log ("NOT ASSOCIATED");
          -------------------------------
          The_Declaration := Data_Handle(Conditional_Parameter_Slice_Or_Type_Conversion (Scope, Unused_Instantiation));
        end Associate_Formal_Call;


        procedure Associate_Subprogram_Access is

          The_Call    : Data.Declaration_Handle := Data.Declaration_Handle(The_Declaration);
          The_Type    : Data_Handle := Result_Type;
          The_Profile : Data.Subprogram_Profile;

          use type Data.Declaration_Handle;

        begin
          --TEST--------------------------------------------------------------------------
          --Write_Log ("ASSOCIATE SUBPROGRAM ACCESS " & Image_Of (The_Call.Location.all));
          --TEST--------------------------------------------------------------------------
          if The_Type = null then
            --TEST--------------------
            --Write_Log ("- NO TYPE");
            --TEST--------------------
            The_Declaration := null;
            return;
          end if;
          --TEST--------------------------------------------------------------
          --Write_Log ("- TYPE: " & Ada.Tags.External_Tag (The_Type.all'tag));
          --TEST--------------------------------------------------------------
          if The_Type.all in Data.Instantiated_Item'class then
            The_Instantiation := Data.Item_Instantiation(The_Type).Instantiation;
            The_Type := Data.Item_Instantiation(The_Type).Item;
          end if;
          The_Profile := Data.Profile_Of (Data.Declaration_Handle(Data.Base_Type_Of (The_Type)));
          if not Data.Is_Overloaded (The_Call) then
            The_Call.Is_Used := True;
          end if;
          loop
            --TEST--------------------------
            --Write_Log ("COMPARE PROFILE");
            --------------------------------
            if Data.Matches (Data.Profile_Of(The_Call), The_Profile, The_Instantiation) then
              --TEST-----------------------
              --Write_Log ("- ASSOCIATED");
              -----------------------------
              The_Subprogram.Data := Data_Handle(The_Call);
              The_Call.Is_Used := True;
              The_Declaration := Data.Predefined_Root_Access;
              return;
            end if;
            The_Call := Data.Overload_Of (The_Call);
            exit when The_Call = null;
          end loop;
          --TEST-------------------------
          --Write_Log ("NOT ASSOCIATED");
          -------------------------------
          The_Declaration := null;
        end Associate_Subprogram_Access;


        procedure Associate_Instantiation (Subprogram : Data.Formal_Subprogram_Handle) is
          The_Call : Data.Declaration_Handle := Data.Declaration_Handle(The_Declaration);
          use type Data.Declaration_Handle;
        begin
          --TEST------------------------------------------------------------------------
          --Write_Log ("ASSOCIATE INSTANTIATION " & Image_Of (Subprogram.Location.all));
          ------------------------------------------------------------------------------
          if Data.Is_Overloaded (The_Call) then
            --TEST---------------------------------------------------------
            --Write_Log ("Overloaded " & Image_Of (The_Call.Location.all));
            ---------------------------------------------------------------
            loop
              if Data.Matches (Subprogram.Profile, Data.Profile_Of (The_Call), The_Instantiation) then
                The_Declaration := Data_Handle(The_Call);
                exit;
              end if;
              The_Call := Data.Overload_Of (The_Call);
              exit when The_Call = null;
            end loop;
          end if;
          The_Subprogram.Data := The_Declaration;
          Data.Set_Used (The_Declaration);
        end Associate_Instantiation;


        function Found_Method (Item       : Identifier_Handle;
                               The_Method : Data_Handle) return Boolean is
        begin
          if Item /= The_Method.Location then
            return False;
          end if;
          declare
            Profile : constant Data.Subprogram_Profile := Data.Profile_Of (Data.Declaration_Handle(The_Method));
            use type Data.Declaration_Handle;
          begin
            --TEST------------------------------------------------------------------------------------------
            --Write_Log ("Check Method " & Image_Of (Item.all) & " with " & Data.Full_Name_Of (The_Method));
            --Write_Log ("      Result " & Data.Full_Name_Of (Within.Sub_Type));
            ------------------------------------------------------------------------------------------------
            if Found_Method_Parameters (Within.Scope, Profile, The_Instantiation) then
              if The_Method.all in Data.Subprogram_Declaration'class then
                if Profile.Result_Type /= null and then
                  Data.Subprogram_Declaration_Handle(The_Method).Overload /= null and then
                  Within.Sub_Type /= null and then not Is_Null (Within.Sub_Type.Location)
                then
                  if not Data.Matches (Within.Sub_Type, Profile.Result_Type, The_Instantiation) then
                    return False;
                  end if;
                end if;
              end if;
              Item.Data := The_Method;
              Data.Declaration_Handle(The_Method).Is_Used := True;
              The_Declaration := Profile.Result_Type;
              return True;
            end if;
          end;
          return False;
        end Found_Method;


        function Handled_Aspect_Indexing return Boolean is
          Saved_Token : Token.Lexical_Handle;
        begin
          if The_Declaration.all in Data.Tagged_Private_Type'class then
            declare
              Aspects : constant Data.Private_Aspect_Handle := Data.Tagged_Private_Handle(The_Declaration).Aspects;
              use type Data.Private_Aspect_Handle;
            begin
              if Aspects /= null and then
                not Is_Null (Aspects.Iterator.Constant_Indexing) and then
                Aspects.Iterator.Constant_Indexing.Data /= null
                and then Aspects.Iterator.Constant_Indexing.Data.all in Data.Subprogram_Declaration'class
              then
                Saved_Token := The_Token;
                if Found_Method (Aspects.Iterator.Constant_Indexing, Aspects.Iterator.Constant_Indexing.Data) then
                  return True;
                else
                  The_Token := Saved_Token;
                end if;
              end if;
            end;
          end if;
          return False;
        end Handled_Aspect_Indexing;


        procedure Handle_Attribute (Context   : Data.Context := Within;
                                    Component : Data_Handle := Data.Predefined_Root_Integer) is
        begin
          The_Attribute := Attribute_Designator (Context);
          --TEST-----------------------------------------------------------------------------
          --Write_Log ("Attribute " & Lexical.Attribute_Id'image(The_Attribute));
          --Write_Log (" - Scope       : " & Data.Full_Name_Of (Data_Handle(Context.Scope)));
          --Write_Log (" - Subtype     : " & Data.Full_Name_Of (Context.Sub_Type));
          --Write_Log (" - Is_Component: " & Boolean'image (Is_Component));
          --if Component = null then
          --  Write_Log (" - Component   : NULL");
          --else
          --  Write_Log (" - Component   : " & Data.Full_Name_Of (Component) &
          --             " - type: " & Ada.Tags.External_Tag (Component.all'tag));
          --end if;
          -----------------------------------------------------------------------------------
          case The_Attribute is
          when Lexical.Is_Address =>
            if Data.Is_Subprogram (Component) then
              The_Subprogram.Data := The_Declaration;
              Data.Set_Used (The_Declaration);
            end if;
            The_Declaration := Data.System_Address;
          when Lexical.Is_Access
             | Lexical.Is_Unchecked_Access
             | Lexical.Is_Unrestricted_Access
          =>
            if Data.Is_Subprogram (Component) then
              Associate_Subprogram_Access;
            else
              The_Declaration := Data.Predefined_Root_Access;
            end if;
          when Lexical.Is_Aft
             | Lexical.Is_Alignment
             | Lexical.Is_Component_Size
             | Lexical.Is_Count
             | Lexical.Is_Digits
             | Lexical.Is_First_Bit
             | Lexical.Is_Fore
             | Lexical.Is_Last_Bit
             | Lexical.Is_Length
             | Lexical.Is_Machine_Emax
             | Lexical.Is_Machine_Emin
             | Lexical.Is_Machine_Mantissa
             | Lexical.Is_Machine_Radix
             | Lexical.Is_Max_Size_In_Storage_Elements
             | Lexical.Is_Model_Emin
             | Lexical.Is_Model_Epsilon
             | Lexical.Is_Model_Mantissa
             | Lexical.Is_Modulus
             | Lexical.Is_Partition_Id
             | Lexical.Is_Pos
             | Lexical.Is_Position
             | Lexical.Is_Scale
             | Lexical.Is_Size
             | Lexical.Is_Storage_Size
             | Lexical.Is_Width
          =>
            if Data.Is_Subprogram (Component) then
              The_Subprogram.Data := Component;
              Data.Set_Used (Component);
            end if;
            The_Declaration := Data.Predefined_Root_Integer;
            Is_Component := True;
          when Lexical.Is_Asm_Input =>
            The_Declaration := Data.Asm_Input;
            Is_Component := True;
          when Lexical.Is_Asm_Output =>
            The_Declaration := Data.Asm_Output;
            Is_Component := True;
          when Lexical.Is_Image
             | Lexical.Is_Img
             | Lexical.Is_Wide_Image
             | Lexical.Is_Wide_Wide_Image
          =>
            The_Declaration := Data.Predefined_Root_String;
          when Lexical.Is_First
             | Lexical.Is_Last
             | Lexical.Is_Val
             | Lexical.Is_Result
          =>
            if Data.Is_Subprogram (Component) then
              The_Subprogram.Data := Component;
              The_Declaration := Data.Profile_Of(Data.Declaration_Handle(Component)).Result_Type;
            else
              The_Declaration := Component;
            end if;
            Is_Component := True;
          when Lexical.Is_Range =>
            if Data.Is_Subprogram (Component) then
              The_Subprogram.Data := Component;
              The_Declaration := Data.Profile_Of(Data.Declaration_Handle(Component)).Result_Type;
            elsif Is_Component and The_Declaration.all in Data.Array_Type'class then
              The_Declaration := Component;
              The_Instantiation := null;
            end if;
            Is_Component := False;
          when Lexical.Is_Callable
             | Lexical.Is_Terminated
          =>
            The_Declaration := Data.Predefined_Boolean;
          when Lexical.Is_Tag =>
            The_Declaration := Data.Ada_Tag;
          when Lexical.Is_Class =>
            if Is_Subtype_Mark then
              Is_Class_Wide_Type := True;
            end if;
          when others =>
            if Data.Is_Subprogram (Component) then
              The_Declaration := Data.Predefined_Root_Access;
            end if;
          end case;
          case Token_Element is
          when Lexical.Apostrophe =>
            if Next_Element_Ahead_Is (Lexical.Attribute) then
              Handle_Attribute;
            end if;
          when others =>
            null;
          end case;
        end Handle_Attribute;


        procedure Handle_Type_Conversion is
          Saved_Token : constant Lexical_Handle := The_Token;
        begin
          Get_Next_Token;
          Dummy := Expression ((Scope, null)); -- type conversion
          if Element_Is (Lexical.Right_Parenthesis) then
            Is_Component := True;
          else
            The_Token := Saved_Token; -- incorrect semantic
            The_Declaration := null;
          end if;
        end Handle_Type_Conversion;

        use type Data.Type_Handle;
        use type Data.Instantiation_Handle;

        Last_Instantiation : Data.Instantiation_Handle;

      begin -- Name_Continuation
        while The_Declaration /= null loop
          --TEST---------------------------------------------------------------
          --Write_Log ("TOKEN => " & Image_Of (Lexical_Before(The_Token).all));
          ---------------------------------------------------------------------
          declare
            The_Kind : constant Data_Kind := Data_Kind_Of (The_Declaration.all);
          begin
            --TEST------------------------------------------------------------------------
            --Write_Log ("- Name: " & Data.Full_Name_Of (The_Declaration));
            --Write_Log ("- Kind: " & Data_Kind'image(The_Kind));
            --Write_Log ("- Type: " & Ada.Tags.External_Tag (The_Declaration.all'tag));
            --Write_Log ("- Inst: " & Data.Full_Name_Of (Data_Handle(The_Instantiation)));
            --Write_Log ("- Comp: " & Is_Component'img);
            ------------------------------------------------------------------------------
            case The_Kind is
            when Is_Access_Type =>
              case Token_Element is
              when Lexical.Period =>
                if Next_Element_Ahead_Is (Lexical.Is_All) then
                  Get_Next_Token;
                end if;
                The_Declaration := Data.Type_Handle(The_Declaration).Parent_Type;
              when Lexical.Left_Parenthesis =>
                if Is_Component then
                  The_Declaration := Data.Type_Handle(The_Declaration).Parent_Type;
                else
                  Handle_Type_Conversion;
                end if;
              when Lexical.Apostrophe =>
                if Is_Component then
                  The_Declaration := Data.Type_Handle(The_Declaration).Parent_Type;
                else
                  if Next_Element_Ahead_Is (Lexical.Attribute) then
                    The_Declaration := Data.Type_Handle(The_Declaration).Parent_Type;
                    Handle_Attribute (Context   => (Scope, The_Declaration),
                                      Component => The_Declaration);
                  elsif Next_Element_Ahead_Is (Lexical.Left_Bracket) then
                    Dummy := Aggregate ((Within.Scope, The_Declaration));
                  elsif Next_Element_Ahead_Is (Lexical.Left_Parenthesis) then
                    Dummy := Aggregate ((Within.Scope, The_Declaration));
                  else
                    Syntax_Error;
                  end if;
                end if;
              when others =>
                exit;
              end case;
            when Is_Subprogram_Access_Type =>
              case Token_Element is
              when Lexical.Period =>
                if Next_Element_Ahead_Is (Lexical.Is_All) then
                  Get_Next_Token;
                end if;
                Conditional_Actual_Parameter_Part;
              when Lexical.Left_Parenthesis =>
                if Is_Component then
                  Conditional_Actual_Parameter_Part;
                else
                  Handle_Type_Conversion;
                end if;
              when Lexical.Apostrophe =>
                if Next_Element_Ahead_Is (Lexical.Attribute) then
                  Handle_Attribute (Component => The_Declaration);
                elsif Next_Element_Ahead_Is (Lexical.Left_Bracket) then
                  Dummy := Aggregate ((Within.Scope, The_Declaration));
                elsif Next_Element_Ahead_Is (Lexical.Left_Parenthesis) then
                  Dummy := Aggregate ((Within.Scope, The_Declaration));
                else
                  Syntax_Error;
                end if;
              when others =>
                exit;
              end case;
            when Is_Incomplete_Type =>
              The_Declaration := The_Declaration.Location.Data;
              case Token_Element is
              when Lexical.Apostrophe
                 | Lexical.Period
                 | Lexical.Left_Parenthesis
              =>
                if The_Declaration.all in Data.Incomplete_Type'class then
                  declare
                    Declaration : constant Data_Handle := Data.Completion_Of (Data.Type_Handle(The_Declaration));
                  begin
                    if Declaration /= null then
                      if Declaration = The_Declaration then
                        The_Declaration := null;
                      else
                        The_Declaration := Declaration;
                        Data.Declaration_Handle(Declaration).Is_Used := True;
                      end if;
                    else
                      if Token_Element = Lexical.Apostrophe then -- example: 'class
                        if Next_Element_Ahead_Is (Lexical.Attribute) then
                          Handle_Attribute (Component => The_Declaration);
                        end if;
                      end if;
                      exit;
                    end if;
                  end;
                end if;
              when others =>
                if The_Declaration.all in Data.Incomplete_Type'class then
                  exit;
                end if;
              end case;
            when Is_Instantiation =>
              declare
                Declaration : constant Data.Item_Instantiation := Data.Item_Instantiation(The_Declaration);
              begin
                case Token_Element is
                when Lexical.Apostrophe =>
                  The_Instantiation := Declaration.Instantiation;
                  The_Declaration := Declaration.Item;
                  if Element_Ahead_Is (Lexical.Attribute) then
                    null;
                  elsif Next_Element_Ahead_Is (Lexical.Left_Bracket) then
                    Dummy := Aggregate ((Within.Scope, The_Declaration));
                  elsif Next_Element_Ahead_Is (Lexical.Left_Parenthesis) then
                    Dummy := Aggregate ((Within.Scope, The_Declaration));
                  else
                    Syntax_Error;
                  end if;
                when Lexical.Period
                   | Lexical.Left_Parenthesis
                =>
                  The_Instantiation := Declaration.Instantiation;
                  The_Declaration := Declaration.Item;
                when others =>
                  exit;
                end case;
              end;
            when Is_Interface_Type =>
              case Token_Element is
              when Lexical.Apostrophe =>
                if Next_Element_Ahead_Is (Lexical.Attribute) then
                  Handle_Attribute;
                elsif Next_Element_Ahead_Is (Lexical.Left_Bracket) then
                  Dummy := Aggregate ((Within.Scope, The_Declaration));
                elsif Next_Element_Ahead_Is (Lexical.Left_Parenthesis) then
                  Dummy := Aggregate ((Within.Scope, The_Declaration));
                else
                  Syntax_Error;
                end if;
              when Lexical.Left_Parenthesis =>
                Handle_Type_Conversion;
              when Lexical.Period =>
                Get_Next_Token;
                declare
                  Actual_Method : constant Identifier_Handle := Actual_Identifier;
                begin
                  for The_Method of Data.Interface_Handle(The_Declaration).Methods loop
                    exit when Found_Method (Actual_Method, The_Method);
                  end loop;
                end;
              when others =>
                exit;
              end case;
            when Is_Formal_Type =>
              if The_Instantiation = null then
                case Token_Element is -- formal not replaced
                when Lexical.Apostrophe =>
                  if Next_Element_Ahead_Is (Lexical.Attribute) then
                    Handle_Attribute (Component => The_Declaration);
                  elsif Next_Element_Ahead_Is (Lexical.Left_Bracket) then
                    Dummy := Aggregate ((Within.Scope, The_Declaration));
                  elsif Next_Element_Ahead_Is (Lexical.Left_Parenthesis) then
                    Dummy := Aggregate ((Within.Scope, The_Declaration));
                  else
                    Syntax_Error;
                  end if;
                when Lexical.Left_Parenthesis =>
                  declare
                    Formal_Type : constant Data_Handle := Data.Formal_Handle(The_Declaration).Declaration;
                  begin
                    if Formal_Type /= null and then Formal_Type.all in Data.Array_Type'class then
                      The_Declaration := Formal_Type;
                    else
                      Handle_Type_Conversion;
                    end if;
                  end;
                when Lexical.Period =>
                  The_Declaration := null;
                when others =>
                  exit;
                end case;
              else
                The_Declaration := Data.Actual_Declaration_Of (Data.Formal_Handle(The_Declaration),
                                                               The_Instantiation);
                The_Instantiation := Last_Instantiation;
                Last_Instantiation := null;
                if Token_Element = Lexical.Period and then Next_Element_Ahead_Is (Lexical.Is_All) then
                  if The_Declaration /= null and then The_Declaration.all in Data.Access_Type'class then
                    The_Declaration := Data.Type_Handle(The_Declaration).Parent_Type;
                  end if;
                  Get_Next_Token;
                end if;
              end if;
            when Is_Private_Type | Is_Private_Extension_Type =>
              case Token_Element is
              when Lexical.Apostrophe =>
                if Next_Element_Ahead_Is (Lexical.Attribute) then
                  Handle_Attribute;
                elsif Next_Element_Ahead_Is (Lexical.Left_Bracket) then
                  Dummy := Aggregate ((Within.Scope, The_Declaration));
                elsif Next_Element_Ahead_Is (Lexical.Left_Parenthesis) then
                  Dummy := Aggregate ((Within.Scope, The_Declaration));
                else
                  Syntax_Error;
                end if;
              when Lexical.Period =>
                The_Declaration := The_Declaration.Location.Data;
                if (The_Declaration.all in Data.Private_Type'class) or
                   (The_Declaration.all in Data.Private_Extension_Type'class)
                then
                  if The_Declaration /= null then
                    if The_Declaration.all in Ada_95.Token.Data.Tagged_Private_Type'class then
                      if Next_Element_Ahead_Is (Lexical.Identifier) then
                        declare
                          The_Actual_Method : constant Identifier_Handle := Actual_Identifier;
                        begin
                          Lookup_Method:
                          loop
                            --TEST-------------------------------------------------------------------------
                            --Write_Log ("Find Method " & Image_Of (The_Actual_Method.all));
                            --Write_Log ("   The_Type " & Ada.Tags.External_Tag (The_Declaration.all'tag));
                            -------------------------------------------------------------------------------
                            for The_Method of Data.Tagged_Private_Handle(The_Declaration).Methods loop
                              exit Lookup_Method when Found_Method (The_Actual_Method, The_Method);
                            end loop;
                            The_Declaration := null;
                            exit Lookup_Method;
                          end loop Lookup_Method;
                        end;
                      else
                        The_Declaration := null;
                      end if;
                    else
                      The_Declaration := null;
                    end if;
                  end if;
                end if;
              when Lexical.Left_Parenthesis =>
                if not Handled_Aspect_Indexing then
                  Conditional_Constraint ((Scope, The_Declaration));
                end if;
              when others =>
                exit;
              end case;
            when Is_Derived_Type =>
              case Token_Element is
              when Lexical.Apostrophe =>
                if Element_Ahead_Is (Lexical.Attribute) then
                  declare
                    Base_Type : constant Data_Handle := Data.Base_Type_Of (The_Declaration);
                  begin
                    if Base_Type.all in Data.Array_Type'class then
                      The_Declaration := Base_Type;
                    else
                      Get_Next_Token;
                      Handle_Attribute (Component => The_Declaration);
                    end if;
                  end;
                elsif Next_Element_Ahead_Is (Lexical.Left_Bracket) then
                  Dummy := Aggregate ((Within.Scope, The_Declaration));
                elsif Next_Element_Ahead_Is (Lexical.Left_Parenthesis) then
                  Dummy := Aggregate ((Within.Scope, The_Declaration));
                else
                  Syntax_Error;
                end if;
              when Lexical.Left_Parenthesis =>
                if Is_Component then
                  The_Declaration := Data.Type_Handle(The_Declaration).Parent_Type;
                elsif Is_Subtype_Indication then
                  exit;
                else
                  Handle_Type_Conversion;
                end if;
              when Lexical.Period =>
                The_Declaration := Data.Type_Handle(The_Declaration).Parent_Type;
              when others =>
                exit;
              end case;
            when Is_Enumeration_Type
               | Is_Discrete_Type
               | Is_Integer_Type
               | Is_Real_Type
            =>
              case Token_Element is
              when Lexical.Apostrophe =>
                if Next_Element_Ahead_Is (Lexical.Attribute) then
                  Handle_Attribute (Context   => (Scope, The_Declaration),
                                    Component => The_Declaration);
                elsif Next_Element_Ahead_Is (Lexical.Left_Bracket) then
                  Dummy := Aggregate ((Within.Scope, The_Declaration));
                elsif Next_Element_Ahead_Is (Lexical.Left_Parenthesis) then
                  Dummy := Aggregate ((Within.Scope, The_Declaration));
                else
                  Syntax_Error;
                end if;
              when Lexical.Left_Parenthesis =>
                Handle_Type_Conversion;
              when Lexical.Period =>
                Log.Write ("--- Unexpected Period");
                The_Declaration := null;
              when others =>
                exit;
              end case;
            when Is_Array_Type =>
              declare
                Definition : constant Data.Array_Definition_Handle
                                        := Data.Array_Handle(The_Declaration).Definition;
              begin
                case Token_Element is
                when Lexical.Apostrophe =>
                  if Next_Element_Ahead_Is (Lexical.Attribute) then
                    if Definition.Index_Subtypes'length = 1 then
                      Handle_Attribute (Component => Data.Index_Subtype_Of (Definition,
                                                                            Definition.Index_Subtypes'first,
                                                                            The_Instantiation));
                    else
                      Handle_Attribute;
                    end if;
                  elsif Next_Element_Ahead_Is (Lexical.Left_Bracket) then
                    Dummy := Aggregate ((Within.Scope, The_Declaration));
                  elsif Next_Element_Ahead_Is (Lexical.Left_Parenthesis) then
                    Dummy := Aggregate ((Within.Scope, The_Declaration));
                  else
                    Syntax_Error;
                  end if;
                when Lexical.Left_Parenthesis => -- slice or element selection
                  Get_Next_Token;
                  declare
                    The_Index   : Positive   := Definition.Index_Subtypes'first;
                    Index_Type  : Data_Handle;
                    Unused_Type : Data_Handle;
                    Is_Slice    : Boolean := not Is_Component;
                  begin
                    loop
                      Index_Type := Data.Index_Subtype_Of (Definition, The_Index, The_Instantiation);
                      Unused_Type := Expression ((Scope, Index_Type));
                      if The_Token.Element = Lexical.Is_Range then
                        Unused_Type := Range_Constraint (Within);
                        exit;
                      end if;
                      Is_Slice := Is_Slice or not Was_Component;
                      --TEST------------------------------------------------
                      --Write_Log ("Is_Slice : " & Boolean'image(Is_Slice));
                      ------------------------------------------------------
                      case Token_Element is
                      when Lexical.Range_Delimiter =>
                        Get_Next_Token;
                        Is_Slice := True;
                        Dummy := Simple_Expression ((Scope, Index_Type));
                      when Lexical.Right_Parenthesis =>
                        exit;
                      when others =>
                        null;
                      end case;
                      exit when The_Index = Definition.Index_Subtypes'last;
                      The_Index := The_Index + 1;
                      Get_Element (Lexical.Comma);
                      Is_Slice := False;
                    end loop;
                    Get_Element (Lexical.Right_Parenthesis);
                    case Token_Element is
                    when Lexical.Period =>
                      The_Declaration := Definition.Component_Type;
                    when others =>
                      if not Is_Slice then
                        The_Declaration := Definition.Component_Type;
                      end if;
                    end case;
                  end;
                when Lexical.Period =>
                  The_Declaration := null;
                when others =>
                  exit;
                end case;
              end;
            when Is_Record_Type =>
              case Token_Element is
              when Lexical.Period =>
                Get_Next_Token;
                case Token_Element is
                when Lexical.Identifier =>
                  declare
                    The_Type : Data_Handle := The_Declaration;
                  begin
                    The_Declaration := Data.Component_Choice_Of (Actual_Identifier,
                                                                 Data.Record_Handle(The_Declaration));
                    if The_Declaration = null then
                      declare
                        The_Actual_Method : constant Identifier_Handle := The_Actual_Identifier;
                      begin
                        Find_Method:
                        loop
                          --TEST------------------------------------------------------------------
                          --Write_Log ("Find Method " & Image_Of (The_Actual_Method.all));
                          --Write_Log ("   The_Type " & Ada.Tags.External_Tag (The_Type.all'tag));
                          ------------------------------------------------------------------------
                          if The_Type.all in Data.Tagged_Record_Type'class then
                            for The_Method of Data.Tagged_Record_Handle(The_Type).Methods loop
                              exit Find_Method when Found_Method (The_Actual_Method, The_Method);
                            end loop;
                            exit when Is_Null (The_Type.Location);
                            declare
                              Private_Type : Data_Handle;
                            begin
                              Private_Type := The_Type.Location.Data;
                              if Private_Type /= null and then Private_Type.all in Data.Tagged_Private_Type'class then
                                for The_Method of Data.Tagged_Private_Handle(Private_Type).Methods loop
                                  exit Find_Method when Found_Method (The_Actual_Method, The_Method);
                                end loop;
                              end if;
                              The_Type := Data.Tagged_Record_Handle(The_Type).Parent_Type;
                            end;
                          elsif The_Type.all in Data.Tagged_Private_Type'class then
                            for The_Method of Data.Tagged_Private_Handle(The_Type).Methods loop
                              exit Find_Method when Found_Method (The_Actual_Method, The_Method);
                            end loop;
                            The_Type := Data.Tagged_Private_Handle(The_Type).Parent_Type;
                          elsif The_Type.all in Data.Instantiated_Type'class then
                            if The_Instantiation /= null then
                              Last_Instantiation := The_Instantiation;
                            end if;
                            The_Instantiation := Data.Item_Instantiation(The_Type).Instantiation;
                            The_Type := Data.Item_Instantiation(The_Type).Item;
                            if The_Type /= null and then The_Type.all in Data.Record_Type'class then
                              The_Declaration := Data.Component_Choice_Of (The_Actual_Identifier,
                                                                           Data.Record_Handle(The_Type));
                              exit when The_Declaration /= null;
                            end if;
                          else
                            The_Declaration := The_Type;
                            exit;
                          end if;
                          exit when The_Type = null;
                        end loop Find_Method;
                      end;
                    end if;
                  end;
                when Lexical.Is_All =>
                  Get_Next_Token;
                when others =>
                  Not_Implemented ("RECORD ELEMENT in Name_Of");
                end case;
              when Lexical.Apostrophe =>
                if Next_Element_Ahead_Is (Lexical.Attribute) then
                  Handle_Attribute;
                elsif Next_Element_Ahead_Is (Lexical.Left_Bracket) then
                  Dummy := Aggregate ((Within.Scope, The_Declaration));
                elsif Next_Element_Ahead_Is (Lexical.Left_Parenthesis) then
                  Dummy := Aggregate ((Within.Scope, The_Declaration));
                else
                  Syntax_Error;
                end if;
              when Lexical.Left_Parenthesis =>
                if not Is_Component or else Data.Has_Discriminats (The_Declaration) then
                  Conditional_Constraint ((Scope, The_Declaration)); -- or type conversion;
                else
                  declare
                    The_Type : constant Data_Handle := Data.Iterable_Type_Of (The_Declaration);
                  begin
                    Conditional_Constraint ((Scope, The_Declaration));
                    The_Declaration := The_Type;
                  end;
                end if;
              when others =>
                exit;
              end case;
            when Is_Exception =>
              if (Token_Element = Lexical.Apostrophe) and then Next_Element_Ahead_Is (Lexical.Attribute) then
                Handle_Attribute;
              else
                exit;
              end if;
            when Is_Generic_Subprogram_Renaming =>
              The_Declaration := Data_Handle(Data.Subprogram_Renaming_Handle(The_Declaration).Renamed_Unit);
            when Is_In_Subprogram
               | Is_Enumeration_Value
               | Is_Generic_Subprogram_Declaration
               | Is_Used_Subprogram
               | Is_Used_Generic_Subprogram
               | Is_Subprogram_Renaming
            =>
              Is_Component := True;
              declare
                Previous_Token : constant Lexical_Handle := Lexical_Before(The_Token);
              begin
                if Previous_Token.Element = Lexical.Identifier then
                  The_Subprogram := Identifier_Handle(Previous_Token);
                else
                  exit;
                end if;
              end;
              if No_Association then
                The_Subprogram.Data := Data_Handle(The_Instantiation);
                The_Token := Lexical_Handle(The_Subprogram);
                return; -- no new instantiation
              end if;
              case Token_Element is
              when Lexical.Apostrophe =>
                The_Subprogram.Data := The_Declaration;
                Data.Set_Used (The_Declaration);
                if Next_Element_Ahead_Is (Lexical.Attribute) then
                  Handle_Attribute (Component => The_Declaration);
                else
                  The_Declaration := null;
                end if;
              when Lexical.Comma
                 | Lexical.Right_Parenthesis
              =>
                if Within.Sub_Type /= null then
                  if Within.Sub_Type.all in Data.Formal_Subprogram'class then
                    Associate_Instantiation (Data.Formal_Subprogram_Handle(Within.Sub_Type));
                    exit;
                  elsif Within.Sub_Type.all in Data.Instantiated_Item'class then
                    declare
                      Sub_Type : constant Data.Item_Instantiation := Data.Item_Instantiation(Within.Sub_Type);
                    begin
                      if Sub_Type.Item.all in Data.Formal_Subprogram'class then
                        The_Instantiation := Sub_Type.Instantiation;
                        Associate_Instantiation (Data.Formal_Subprogram_Handle(Sub_Type.Item));
                        exit;
                      end if;
                    end;
                  end if;
                end if;
                Associate_Call;
              when others =>
                Associate_Call;
              end case;
            when Is_Formal_Object =>
              Is_Component := True;
              The_Declaration := Data.Formal_Object_Handle(The_Declaration).Declaration;
            when Is_Object =>
              Is_Component := True;
              The_Declaration := Data.Object_Handle(The_Declaration).Object_Type;
              --TEST-----------------------------------------------------------------------------
              --if The_Declaration = null then
              --  Write_Log ("OBJECT TYPE - NULL");
              --else
              --  Write_Log ("OBJECT TYPE - " & Ada.Tags.External_Tag (The_Declaration.all'tag));
              --end if;
              -----------------------------------------------------------------------------------
              if The_Declaration /= null and then The_Declaration.all in Data.Access_Type'class then
                case Token_Element is
                when Lexical.Period =>
                  if Next_Element_Ahead_Is (Lexical.Is_All) then
                    Get_Next_Token;
                  end if;
                  if The_Declaration.all in Data.Subprogram_Access_Type'class then
                    Conditional_Actual_Parameter_Part;
                  else
                    The_Declaration := Data.Type_Handle(The_Declaration).Parent_Type;
                  end if;
                when Lexical.Left_Parenthesis =>
                  if The_Declaration.all in Data.Subprogram_Access_Type'class then
                    Conditional_Actual_Parameter_Part;
                  else
                    The_Declaration := Data.Type_Handle(The_Declaration).Parent_Type;
                  end if;
                when Lexical.Apostrophe =>
                  The_Declaration := Data.Type_Handle(The_Declaration).Parent_Type;
                when others =>
                  null;
                end case;
              end if;
            when Is_Subtype =>
              The_Declaration := Data.Type_Of(The_Declaration);
            when Is_Entry_Body | Is_Entry_Declaration =>
              The_Subprogram := Identifier_Handle(Lexical_Before(The_Token));
              declare
                Index_Type : constant Data_Handle := Data.Index_Type_Of (The_Declaration);
              begin
                if Index_Type /= null then
                  Get_Element (Lexical.Left_Parenthesis);
                  Dummy := Expression ((Scope, Index_Type));
                  Get_Element (Lexical.Right_Parenthesis);
                end if;
              end;
              Associate_Call;
            when Is_Task_Type | Is_Protected_Type =>
              case Token_Element is
              when Lexical.Period
                 | Lexical.Apostrophe
              =>
                The_Declaration := Data_Handle(Data.Active_Type_Handle(The_Declaration).Object);
              when others =>
                exit;
              end case;
            when Is_Formal_Subprogram =>
              Associate_Formal_Call;
              case Token_Element is
              when Lexical.Apostrophe =>
                if Next_Element_Ahead_Is (Lexical.Attribute) then
                  Handle_Attribute;
                else
                  The_Declaration := null;
                end if;
              when others =>
                null;
              end case;
            when Is_Package_Body
               | Is_Package_Specification
               | Is_Generic_Package_Declaration
               | Is_Task_Declaration
               | Is_Protected_Declaration
            =>
              --TEST---------------------------------------------------------
              --Write_Log ("SCOPE : " & Data.Full_Name_Of (The_Declaration));
              ---------------------------------------------------------------
              case Token_Element is
              when Lexical.Period =>
                Get_Next_Token;
                case Token_Element is
                when Lexical.Identifier =>
                  if not Found_Identifier_In_Unit then
                    exit;
                  end if;
                when others =>
                  The_Declaration := null;
                end case;
              when Lexical.Apostrophe =>
                if Next_Element_Ahead_Is (Lexical.Attribute) then
                  Handle_Attribute;
                else
                  The_Declaration := null;
                end if;
              when others =>
                exit;
              end case;
            when Is_With_Declaration =>
              --TEST--------------------------------------------------------------------------------------------------
              --Write_Log ("WITH : " & Image_Of (Data.With_Handle(The_Declaration).Unit.Location.all));
              --Write_Log ("  PARENT : " & Data.Full_Name_Of (Data_Handle(Data.With_Handle(The_Declaration).Parent)));
              --Write_Log ("  TYPE   : " & Ada.Tags.External_Tag (Data.With_Handle(The_Declaration).Parent.all'tag));
              --------------------------------------------------------------------------------------------------------
              if Token_Element = Lexical.Period then
                declare
                  Actual_Token : constant Lexical_Handle := The_Token;
                begin
                  Get_Next_Token;
                  if Token_Element = Lexical.Identifier then
                    declare
                      Import : constant Data.With_Handle := Data.With_Handle(The_Declaration);
                    begin
                      --TEST--------------------------------------------
                      --Write_Log ("  get ." & Image_Of (The_Item.all));
                      --------------------------------------------------
                      The_Declaration := Data_Handle(Data.Declaration_From (Import, Actual_Identifier));
                      Import.Unit.Is_Used := True;
                      if The_Declaration = null then
                        The_Declaration := Data_Handle(Import.Unit);
                        The_Token := Actual_Token;
                      end if;
                    end;
                  else
                    The_Declaration := null;
                    exit;
                  end if;
                end;
              else
                The_Declaration := Data_Handle(Data.With_Handle(The_Declaration).Unit);
                Data.Unit_Handle(The_Declaration).Is_Used := True;
              end if;
            when Is_In_Package_Renaming =>
              The_Declaration := Data.Package_Renaming_Handle(The_Declaration).Renamed_Item;
            when Is_Package_Instantiation =>
              The_Instantiation := Data.Instantiation_Handle(The_Declaration);
              The_Declaration := Data_Handle(The_Instantiation.Generic_Package);
            when Is_Formal_Package =>
              The_Declaration := Data_Handle(Data.Formal_Package_Handle(The_Declaration).Generic_Package);
            when Is_Accept_Declaration
               | Is_Block
               | Is_End_Identifier
               | Is_Formal_Block
               | Is_Label
               | Is_Predefined_Operator_Type
               | Is_Private_Block
               | Is_Predefined_Pragma_Argument_Id_Type
               | Is_Protected_Body
               | Is_Task_Body
               | Is_Unknown
            =>
              exit;
            end case;
          end;
        end loop;
        if The_Instantiation /= null and then The_Declaration /= null then
          The_Declaration := Data.New_Instantiation (The_Declaration, The_Instantiation);
        end if;
      end Name_Continuation;


      Sub_Type  : constant Data_Handle := Within.Sub_Type;
      The_Scope : Data.Unit_Handle := Within.Scope;

      The_Self_Renamed_Scope   : Data.Unit_Handle;
      The_Self_Renamed_Nesting : Natural := 0;

      function Is_Self_Renaming return Boolean is
        Declaration : constant Data_Handle := Data.Package_Renaming_Handle(The_Declaration).Renamed_Item;
        The_Unit    : Data.Unit_Handle := The_Scope;
        use type Data.Unit_Handle;
      begin
        if Declaration /= null and then Declaration.all in Data.Unit_Type'class then
          while The_Unit /= null loop
            if Data.Unit_Handle(Declaration) = The_Unit then
              The_Self_Renamed_Scope := The_Unit;
              return True;
            end if;
            The_Self_Renamed_Nesting := The_Self_Renamed_Nesting + 1;
            The_Unit := The_Unit.Parent;
          end loop;
        end if;
        return False;
      end Is_Self_Renaming;


      function Found_Inner_Local (Suppress_Used : Boolean := False) return Boolean with Inline is
      begin
        The_Declaration := Data.Inner_Declaration_From (The_Scope, The_Item, Suppress_Used);
        return The_Declaration /= null;
      end Found_Inner_Local;


      function Found_Self return Boolean with Inline is
        use type Name.Handle;
        use type Data.Unit_Handle;
      begin
        if not Is_Null (The_Scope.Location) and then The_Item.Id = The_Scope.Location.Id then
          if Token_Element = Lexical.Period then
            if Element_Ahead_Is (Lexical.Is_All) then
              return False;
            end if;
            The_Item.Data := Data_Handle(The_Scope);
            The_Scope.Is_Used := True;
          elsif not Data.Is_Subprogram (Data_Handle(The_Scope)) then
            if The_Scope.all in Data.Accept_Declaration'class then
              return False;
            end if;
            The_Item.Data := Data_Handle(The_Scope);
            The_Scope.Is_Used := True;
          end if;
          return True;
        elsif The_Self_Renamed_Scope = The_Scope then
          --TEST----------------------------------------------------------------------------------
          --Write_Log ("Found self renamed scope: " & Data.Full_Name_Of (Data_Handle(The_Scope)));
          ----------------------------------------------------------------------------------------
          return True;
        end if;
        return False;
      end Found_Self;


      function Found_Generation_At (Index : Positive) return Boolean with Inline is
        use type Data.Resource_Handle;
      begin
        The_Declaration := Data_Handle(Data.Declaration_From (Resource.Generations, Index, The_Item));
        return The_Declaration /= null;
      end Found_Generation_At;


      function Found_Generation return Boolean is
        Actual_Resource : constant Data.Resource_Handle := Data.Resource (The_Scope.all);
        use type Data.Resource_Handle;
      begin
        if Actual_Resource /= null then
          declare
            Index : constant Positive := Actual_Resource.Generations'length;
          begin
            --TEST-----------------------------------------------------------
            --Write_Log ("Find generation - index: " & Natural'image(Index));
            -----------------------------------------------------------------
            return Found_Generation_At (Index);
          end;
        end if;
        return False;
      end Found_Generation;


      function Found_In_Generations  return Boolean is
      begin
        for Index in reverse Resource.Generations'range loop
          if Found_Generation_At (Index) then
            return True;
          end if;
        end loop;
        return False;
      end Found_In_Generations;


      function Has_Dot_Id return Boolean is
      begin
        if Token_Element = Lexical.Period then
          Get_Next_Token;
          if Token_Element = Lexical.Identifier then
            The_Item := Actual_Identifier;
            return True;
          end if;
        end if;
        return False;
      end Has_Dot_Id;


      function Found_Inner_Local_Only return Boolean with Inline is
      begin
        --TEST----------------------------------------------------------------------------------------
        --Write_Log ("FIND INNER LOCAL ONLY IN SCOPE: " & Data.Full_Name_Of (Data_Handle(The_Scope)));
        ----------------------------------------------------------------------------------------------
        The_Declaration := Data.Inner_Declaration_From (Scope         => The_Scope,
                                                        Item          => The_Item,
                                                        Suppress_Used => Is_Subtype_Mark or else
                                                                         Token_Element = Lexical.Period);
        if The_Declaration /= null then
          if The_Declaration.all in Data.Subprogram_Declaration'class then
            Data.Chain_In (Data.Declaration_Handle(The_Declaration));
            --TEST-----------------------------------------------------------------------------------
            --Write_Log ("Found inner local only subprogram " & Data.Full_Name_Of (The_Declaration));
            -----------------------------------------------------------------------------------------
          else
            if The_Declaration.all in Data.Package_Renaming'class and then Is_Self_Renaming then
              --TEST--------------------------------------------------------------------------
              --Write_Log ("Found self renaming only " & Data.Full_Name_Of (The_Declaration));
              --------------------------------------------------------------------------------
              return False;
            end if;
            --TEST------------------------------------------------------------------------
            --Write_Log ("Found inner local only " & Data.Full_Name_Of (The_Declaration));
            ------------------------------------------------------------------------------
          end if;
          return True;
        end if;
        --TEST-------------------------------------
        --Write_Log ("NOT FOUND INNER LOCAL ONLY");
        -------------------------------------------
        return False;
      end Found_Inner_Local_Only;


      function Found_In_Substructure return Boolean is
        use type Data.Record_Handle;
      begin
        if The_Actual_Record_Definition /= null then
          The_Declaration := Data.Component_Choice_Of (The_Item, The_Actual_Record_Definition);
        end if;
        return False;
      end Found_In_Substructure;


      function Found_In_Scope return Boolean is

        The_Actual_Scope  : Data.Unit_Handle := The_Scope;
        The_Nesting_Index : Natural := 1;
        use type Data.Unit_Handle;

      begin
        --TEST-----------------------------------------------------------------------
        --Write_Log ("FIND IN SCOPE: " & Data.Full_Name_Of (Data_Handle(The_Scope)));
        -----------------------------------------------------------------------------
        loop
          if Found_Self then
            --TEST-------------------------------------------------------------------
            --Write_Log ("Found self : " & Data.Full_Name_Of (Data_Handle(The_Scope))
            --         & " - nesting" & Natural'image(The_Nesting_Index));
            -------------------------------------------------------------------------
            if not Has_Dot_Id then
              if not Found_Inner_Local (Suppress_Used => Token_Element = Lexical.Apostrophe) then
                The_Declaration := Data_Handle(The_Scope);
                --TEST--------------------------
                --Write_Log ("Found self only");
                --------------------------------
              end if;
              --TEST---------------------------------------------------------------------
              --Write_Log ("Found inner local : " & Data.Full_Name_Of (The_Declaration));
              ---------------------------------------------------------------------------
              return True;
            elsif The_Nesting_Index > 1 then -- search inner self path
              declare
                type Scopes is array (Natural range 1 .. The_Nesting_Index) of Data.Unit_Handle;
                The_Scopes : Scopes;
              begin
                for Index in Scopes'range loop
                  The_Scopes(Index) := The_Actual_Scope;
                  if The_Actual_Scope = null then
                    return False;
                  end if;
                  The_Actual_Scope := The_Actual_Scope.Parent;
                end loop;
                for Index in reverse Scopes'first .. Scopes'last - 1 loop
                  The_Scope := The_Scopes(Index);
                  if Found_Self then
                    --TEST-------------------------------------------------------------------
                    --Write_Log ("Found self : " & Data.Full_Name_Of (Data_Handle(The_Scope))
                    --         & " - index" & Natural'image(Index));
                    -------------------------------------------------------------------------
                    if not Has_Dot_Id then
                      The_Declaration := Data_Handle(The_Scope);
                      --TEST----------------------------------------------------------------------------
                      --Write_Log ("Found self only in scope : " & Data.Full_Name_Of (The_Declaration));
                      ----------------------------------------------------------------------------------
                      return True;
                    end if;
                  else
                    The_Scope := The_Scopes(Index + 1);
                    exit;
                  end if;
                end loop;
                --TEST---------------------------------------------------------------------------
                --Write_Log ("Found self scope : " & Data.Full_Name_Of (Data_Handle(The_Scope)));
                ---------------------------------------------------------------------------------
                if Found_Inner_Local (Suppress_Used => True) then
                  --TEST----------------------------------------------------------------------------
                  --Write_Log ("Found nested inner local : " & Data.Full_Name_Of (The_Declaration));
                  ----------------------------------------------------------------------------------
                  return True;
                elsif Found_Generation then
                  --TEST---------------------------------------------------------------------------
                  --Write_Log ("Found nested generation : " & Data.Full_Name_Of (The_Declaration));
                  ---------------------------------------------------------------------------------
                  return True;
                end if;
              end;
            elsif Found_Inner_Local (Suppress_Used => True) then
              --TEST---------------------------------------------------------------------
              --Write_Log ("Found inner local : " & Data.Full_Name_Of (The_Declaration));
              ---------------------------------------------------------------------------
              return True;
            elsif Found_Generation then
              --TEST---------------------------------------------------------------
              --Write_Log ("Found child : " & Data.Full_Name_Of (The_Declaration));
              ---------------------------------------------------------------------
              return True;
            end if;
            --TEST-------------------------
            --Write_Log ("SELF NOT FOUND");
            -------------------------------
            The_Scope := The_Actual_Scope;
            The_Declaration := null;
            return False;
          else
            The_Scope := The_Scope.Parent;
            exit when The_Scope = null;
            The_Nesting_Index := The_Nesting_Index + 1;
            if Found_Inner_Local (Suppress_Used => (Token_Element in Lexical.Period | Lexical.Apostrophe)) then
              if The_Declaration.all in Data.Subprogram_Declaration'class then
                Data.Chain_In (Data.Declaration_Handle(The_Declaration));
                --TEST-----------------------------------------------------------------------------
                --Write_Log ("Found inner local subprogram: " & Data.Full_Name_Of (The_Declaration)
                --         & " - nesting" & Natural'image(The_Nesting_Index));
                -----------------------------------------------------------------------------------
                return True;
              elsif The_Declaration.all in Data.Package_Renaming'class and then Is_Self_Renaming then
                --TEST---------------------------------------------------------------------
                --Write_Log ("Found self renaming " & Data.Full_Name_Of (The_Declaration));
                ---------------------------------------------------------------------------
                The_Scope := The_Self_Renamed_Scope;
                The_Nesting_Index := The_Nesting_Index + The_Self_Renamed_Nesting;
              else
                --TEST------------------------------------------------------------------
                --Write_Log ("Found inner local: " & Data.Full_Name_Of (The_Declaration)
                --         & " - nesting" & Natural'image(The_Nesting_Index));
                ------------------------------------------------------------------------
                return True;
              end if;
            end if;
          end if;
        end loop;
        The_Scope := The_Actual_Scope;
        The_Declaration := null;
        if Found_In_Generations then
          --TEST-----------------------------------------------------------------------
          --Write_Log ("Found_In_Generations: " & Data.Full_Name_Of (The_Declaration));
          -----------------------------------------------------------------------------
          return True;
        else
          --TEST-----------------------------
          --Write_Log ("NOT FOUND IN SCOPE");
          -----------------------------------
          return False;
        end if;
      end Found_In_Scope;


      function Found_In_Context return Boolean is
        use type Data.Unit_Handle;
        use type Name.Handle;
      begin
        if Found_In_Generations then
          return True;
        else
          for Index in Resource.Generations'first + 1 .. Resource.Generations'last loop
            if The_Item.Id = Resource.Generations(Index).Unit.Location.Id then
              The_Scope := Resource.Generations(Index).Unit;
              The_Item.Data := Data_Handle(The_Scope);
              if not Has_Dot_Id then
                return False; -- self only
              end if;
            else
              exit;
            end if;
          end loop;
          if The_Scope = null then
            return False;
          else
            --TEST----------------------------------------------------------------------------
            --Write_Log ("Found Self Context: " & Data.Full_Name_Of (Data_Handle(The_Scope)));
            ----------------------------------------------------------------------------------
            return Found_Generation;
          end if;
        end if;
      end Found_In_Context;


      function Found_In_Used_Package (The_Instantiation : out Data.Instantiation_Handle) return Boolean is
        use type Data.Use_List_Handle;
        use type Data.Unit_Handle;
      begin
        The_Scope := Within.Scope;
        loop
          if The_Scope.Used_Packages /= null then
            The_Declaration := Data.Declaration_From (The_Scope.Used_Packages,
                                                      Resource.Generations,
                                                      The_Item,
                                                      The_Instantiation);
            if The_Declaration /= null then
              return True;
            end if;
          end if;
          The_Scope := The_Scope.Parent;
          exit when The_Scope = null;
        end loop;
        The_Scope := Within.Scope;
        The_Declaration := null;
        return False;
      end Found_In_Used_Package;


      function Found_Inherited_Call return Boolean is

        The_Object : constant Lexical_Handle := Lexical_Before (The_Token);

        The_Class : Data.Type_Handle;

        use type Data.Type_Handle;

        function Is_Inherited_Call (Class_Result  : Boolean;
                                    Instantiation : Data.Instantiation_Handle := null) return Boolean is

          function Associated_Call return Boolean is

            Subprogram      : constant Identifier_Handle := Identifier_Handle(The_Token);
            The_Result_Type : Data_Handle;

            Actual_Token : constant Lexical_Handle := Next_Token;

            The_Call : Data.Declaration_Handle;

            use type Data.Declaration_Handle;

          begin
            if Class_Result then
              The_Result_Type := Data_Handle(The_Class);
            else
              The_Result_Type := Sub_Type;
            end if;
            The_Call := Data.Declaration_Handle(The_Declaration);
            --TEST------------------------------------------------------------------------------
            --Write_Log ("ASSOCIATE INHERITED CALL " & Image_Of (The_Call.Location.all));
            --if The_Result_Type = null then
            --  Write_Log ("- Result_Type: NULL");
            --else
            --  Write_Log ("- Result_Type: " & Ada.Tags.External_Tag (The_Result_Type.all'tag));
            --end if;
            --Write_Log ("- Class: " & Image_Of (The_Class.Location.all));
            ------------------------------------------------------------------------------------
            if not Data.Is_Overloaded (The_Call) then
              Subprogram.Data := The_Declaration;
              Data.Set_Used (The_Declaration);
              The_Result_Type := null; -- no checks for Result_Type
              --TEST-------------------------------
              --Write_Log ("NO RESULT TYPE CHECK");
              -------------------------------------
            end if;
            loop
              --TEST--------------------------
              --Write_Log ("COMPARE PROFILE");
              --------------------------------
              if ((Data.Profile_Of(The_Call).Result_Type = null) -- procedure
                  and Procedure_Allowed)
                or else ((Data.Profile_Of(The_Call).Result_Type /= null) -- function
                  and (The_Result_Type = null or else Data.Matches (Data.Profile_Of(The_Call).Result_Type,
                                                                    The_Result_Type,
                                                                    null)))
              then
                --TEST---------------------------
                --Write_Log ("-> Return Mached");
                ---------------------------------
                if Found_Actual_Parameters (Within.Scope,
                                            Data.Profile_Of(The_Call),
                                            Instantiation,
                                            The_Class)
                then
                  --TEST-------------------------------------
                  --Write_Log ("-> FOUND ACTUAL PARAMETERS");
                  -------------------------------------------
                  The_Declaration := Data.Profile_Of(The_Call).Result_Type;
                  Subprogram.Data := Data_Handle(The_Call);
                  The_Call.Is_Used := True;
                  return True; -- associated
                --TEST-------------------------------------------
                --else
                --  Write_Log ("-> NOT FOUND ACTUAL PARAMETERS");
                -------------------------------------------------
                end if;
              --TEST----------------------------------
              --else
              --  Write_Log ("-> RETURN NOT MATCHED");
              ----------------------------------------
              end if;
              The_Call := Data.Overload_Of (The_Call);
              exit when The_Call = null;
              The_Token := Actual_Token;
            end loop;
            --TEST-------------------------
            --Write_Log ("NOT ASSOCIATED");
            -------------------------------
            The_Token := Actual_Token;
            return False;
          end Associated_Call;

        begin -- Is_Inherited_Call
          while The_Class /= null loop
            --TEST------------------------------------------------------------------
            --Write_Log ("INHERITED UNKNOWN ITEM => " & Image_Of (Object.all));
            --Write_Log ("find in " & Name.Image_Of (The_Class.Parent.Location.Id));
            ------------------------------------------------------------------------
            The_Token := The_Object;
            The_Declaration := Name_Of (Within            => (The_Class.Parent, null),
                                        Procedure_Allowed => Procedure_Allowed,
                                        No_Association    => True);
            if Data.Is_Subprogram (The_Declaration) then
              if Associated_Call then
                return True;
              end if;
            end if;
            The_Class := Data.Parent_Type_Of (Data_Handle(The_Class));
          end loop;
          --TEST---------------------------
          --Write_Log ("-> NOT INHERITED");
          ---------------------------------
          return False;
        end Is_Inherited_Call;

        function Is_Operator_Call return Boolean is

          procedure Parameter_Association is
          begin
            if Next_Element_Ahead_Is (Lexical.Association) then
              Get_Element (Lexical.Association);
            end if;
            Dummy := Expression ((Within.Scope, The_Parameter_Type));
          end Parameter_Association;

        begin -- Is_Operator_Call
          if The_Object.all in Operator_Symbol'class then
            --TEST-----------------------------------------
            --Write_Log ("-> CHECK FOR STANDARD OPERATOR");
            -----------------------------------------------
            The_Token := The_Object;
            Identifier_Handle(The_Token).Data := Data.Predefined_Operator;
            if Next_Element_Is (Lexical.Left_Parenthesis) then
              Parameter_Association;
              while Element_Is (Lexical.Comma) loop
                Parameter_Association;
              end loop;
              Get_Element (Lexical.Right_Parenthesis);
              return True;
            end if;
          end if;
          return False;
        end Is_Operator_Call;

        The_Instantiation : Data.Instantiation_Handle;

      begin -- Found_Inherited_Call
        if No_Association then
          return False;
        end if;
        if The_Object.all in Token.Identifier'class then
          The_Class := Data.Parent_Type_Of (Sub_Type);
          if The_Class /= null then
            --TEST-----------------------------------------------------------------------------------
            --Write_Log ("-> SEARCH CLASS RETURN FOR " & Data.Full_Name_Of (Data_Handle(The_Class)));
            -----------------------------------------------------------------------------------------
            if Is_Inherited_Call (Class_Result => True) then
              if Data_Handle(The_Class) = The_Declaration then
                The_Declaration := Sub_Type;
              end if;
              return True;
            end if;
          end if;
          if Token_Element = Lexical.Left_Parenthesis then
            The_Class := Conditional_Parameter_Slice_Or_Type_Conversion (Within.Scope, The_Instantiation);
            if The_Class = null then
              if Is_Operator_Call then
                return True;
              end if;
            else
              --TEST--------------------------------------------------------------------------------------
              --Write_Log ("-> SEARCH CLASS PARAMETER FOR " & Data.Full_Name_Of (Data_Handle(The_Class)));
              --------------------------------------------------------------------------------------------
              if Is_Inherited_Call (Class_Result  => False,
                                    Instantiation => The_Instantiation) then
                if Data_Handle(The_Class) = The_Declaration then
                  The_Declaration := Sub_Type;
                end if;
                return True;
              elsif Is_Operator_Call then
                return True;
              end if;
            end if;
          end if;
        end if;
        return False;
      end Found_Inherited_Call;

      procedure Skip_To_End_Of_Name is
        type Token_Type is (Start, Separator, Item);
        The_Count      : Natural := 0;
        The_Expected_Type : Token_Type := Start;
      begin
        loop
          case Token_Element is
          when Lexical.Is_If =>
            --TEST---------------------------------
            --Write_Log ("%%% SKIP if expression");
            ---------------------------------------
            Dummy := If_Expression (Within);
            The_Expected_Type := Separator;
            The_Count := The_Count - 1;
            exit when (The_Count = 0);
          when Lexical.Is_Case =>
            --TEST-----------------------------------
            --Write_Log ("%%% SKIP case expression");
            -----------------------------------------
            Dummy := Case_Expression (Within);
            The_Expected_Type := Separator;
            The_Count := The_Count - 1;
            exit when (The_Count = 0);
          when Lexical.Is_For =>
            --TEST-----------------------------------------
            --Write_Log ("%%% SKIP quantified expression");
            -----------------------------------------------
            Dummy := Quantified_Expression (Within);
            The_Expected_Type := Separator;
            The_Count := The_Count - 1;
            exit when (The_Count = 0);
          when Lexical.Is_Declare =>
            --TEST--------------------------------------
            --Write_Log ("%%% SKIP declare expression");
            --------------------------------------------
            Dummy := Declare_Expression (Within);
            The_Expected_Type := Separator;
            The_Count := The_Count - 1;
            exit when (The_Count = 0);
          when Lexical.Identifier
             | Lexical.Is_All
             | Lexical.Is_Null
             | Lexical.Unconstrained
             | Lexical.Integer_Literal
             | Lexical.Real_Literal
             | Lexical.String_Literal
          =>
            exit when The_Expected_Type /= Item;
            The_Expected_Type := Separator;
          when Lexical.Period =>
            exit when The_Expected_Type = Item;
            --TEST-----------------------------
            --if The_Expected_Type = Start then
            --  Write_Log ("%%% START SKIP");
            --end if;
            -----------------------------------
            The_Expected_Type := Item;
          when Lexical.Is_Abs
             | Lexical.Is_New
             | Lexical.Is_Not
          =>
            exit when The_Expected_Type /= Item;
            The_Expected_Type := Item;
          when Lexical.Plus
             | Lexical.Minus
          =>
            exit when The_Expected_Type = Start;
            The_Expected_Type := Item;
          when Lexical.Association
             | Lexical.Comma
             | Lexical.Equal
             | Lexical.Not_Equal
             | Lexical.Greater
             | Lexical.Greater_Or_Equal
             | Lexical.Less
             | Lexical.Less_Or_Equal
             | Lexical.Ampersand
             | Lexical.Asterisk
             | Lexical.Slash
             | Lexical.Exponentiation
             | Lexical.Range_Delimiter
             | Lexical.Is_Or
             | Lexical.Is_And
             | Lexical.Is_Xor
             | Lexical.Is_Mod
             | Lexical.Is_Rem
          =>
            exit when (The_Count = 0) or (The_Expected_Type /= Separator);
            The_Expected_Type := Item;
          when Lexical.Apostrophe =>
            exit when The_Expected_Type = Item;
            if Next_Element_Ahead_Is (Lexical.Attribute) then
              The_Expected_Type := Separator;
            elsif Element_Ahead_Is (Lexical.Left_Parenthesis) then
              exit when The_Count = 0;
              The_Expected_Type := Separator;
            else
              exit;
            end if;
          when Lexical.Left_Parenthesis =>
            The_Count := The_Count + 1;
            --TEST-----------------------------
            --if The_Expected_Type = Start then
            --  Write_Log ("%%% START SKIP");
            --end if;
            -----------------------------------
            The_Expected_Type := Item;
          when Lexical.Right_Parenthesis =>
            exit when The_Count = 0 or (The_Expected_Type = Item);
            The_Count := The_Count - 1;
            The_Expected_Type := Separator;
          when others =>
            exit;
          end case;
          --TEST-----------------------------------------------------
          --Write_Log ("%%% SKIP TOKEN " & Image_Of (The_Token.all));
          -----------------------------------------------------------
          Get_Next_Token;
        end loop;
      exception
      when others =>
        --TEST--------------------------------
          Write_Log ("%%% SKIP TOKEN Failed");
        --------------------------------------
      end Skip_To_End_Of_Name;

      use type Data.Unit_Handle;

      The_Instantiation : Data.Instantiation_Handle;

    begin -- Name_Of
      --TEST---------------------------------------------------------------------------
      --Write_Log   ("NAME_OF - TOKEN: " & Image_Of (The_Token.all));
      --if Sub_Type = null then
      --  Write_Log ("        - SUBTYPE: NULL");
      --else
      --  Write_Log ("        - SUBTYPE: " & Ada.Tags.External_Tag (Sub_Type.all'tag));
      --end if;
      --Write_Log ("        - Procedure_Allowed     : " & Procedure_Allowed'image);
      --Write_Log ("        - No_Association        : " & No_Association'image);
      --Write_Log ("        - Is_Subtype_Mark       : " & Is_Subtype_Mark'image);
      --Write_Log ("        - Is_Subtype_Indication : " & Is_Subtype_Indication'image);
      --Increment_Log_Indent;
      ---------------------------------------------------------------------------------
      if Element_Is (Lexical.Target_Name) then
        The_Declaration := The_Target_Type;
        Name_Continuation (The_Target_Type, The_Instantiation);
      else
        The_Item := Actual_Identifier;
        if The_Scope = null then
          if not Found_In_Context then
            The_Declaration := null; -- not found;
          end if;
        else
          if not Found_Inner_Local_Only and then
             not Found_In_Substructure and then
             not Found_In_Scope and then
             not Found_In_Used_Package (The_Instantiation)
          then
            The_Declaration := null; -- not found;
          end if;
        end if;
        if The_Declaration /= null then
          Name_Continuation (Sub_Type, The_Instantiation);
        end if;
        if The_Declaration = null then
          if Found_In_Substructure or else Found_Inherited_Call then
            if The_Declaration /= null then
              Name_Continuation (Sub_Type, The_Instantiation);
            end if;
          else
            if not No_Association then
              Skip_To_End_Of_Name;
            end if;
            The_Declaration := null;
          end if;
        end if;
      end if;
      if Token_Element = Lexical.Period then
        Skip_To_End_Of_Name;
      end if;
      --TEST--------------------------------------------------------------------------------------
      --Decrement_Log_Indent;
      --if The_Declaration = null then
      --  Write_Log ("NAME_OF EXIT: RESULT - NULL");
      --else
      --  Write_Log ("NAME_OF EXIT: RESULT - " & Ada.Tags.External_Tag (The_Declaration.all'tag));
      --  Write_Log ("              ID     - " & Data.Full_Name_Of (The_Declaration));
      --end if;
      --------------------------------------------------------------------------------------------
      Was_Component := Is_Component;
      return The_Declaration;
    end Name_Of;


    function Unit_Of (Scope : Data.Unit_Handle) return Data.Unit_Handle is
      The_Handle : constant Data_Handle := Name_Of ((Scope, null), No_Association => True);
    begin
      if Data.Is_Subprogram (The_Handle) then
        The_Actual_Identifier := Identifier_Handle(The_Token);
        Get_Next_Token;
        return Data.Unit_Handle(The_Handle);
      elsif The_Handle /= null and then The_Handle.all in Data.Unit_Type'class then
        return Data.Unit_Handle(The_Handle);
      end if;
      return null;
    end Unit_Of;


    function Next_Unit_Of (Scope : Data.Unit_Handle) return Data.Unit_Handle is
    begin
      Get_Next_Token;
      return Unit_Of (Scope);
    end Next_Unit_Of;


    -- component_definition ::=
    --      [aliased] [not null] Subtype_Indication_Part
    --      [aliased] [not null] access access_definition_part
    --
    function Component_Definition (Within : Data.Context) return Data_Handle is
    begin
      Get_Conditional (Lexical.Is_Aliased);
      Conditional_Null_Exclusion;
      if Element_Is (Lexical.Is_Access) then
        return Access_Definition_Part (Within.Scope);
      else
        return Subtype_Indication_Part (Within);
      end if;
    end Component_Definition;


    -- component_declaration ::=
    --      defining_identifier_list : component_definition [ := default_expression] [aspect_specification] ;
    --
    function Component_Declaration (Within : Data.Context) return Data.List.Item is

      Defining_Identifiers : constant Identifiers := Defining_Identifier_List;

      The_Type    : Data_Handle;
      Has_Default : Boolean := False;

    begin
      Get_Element (Lexical.Colon);
      The_Type := Component_Definition (Within);
      declare
        The_Components : Data.List.Item;
        Is_Class_Wide  : constant Boolean := Is_Class_Wide_Type;
      begin
        if Element_Is (Lexical.Assignment) then
          Has_Default := True;
          if The_Type = null then
            Dummy := Expression (Within);
          else
            The_Target_Type := The_Type;
            Dummy := Expression ((Within.Scope, The_Type));
          end if;
        end if;
        The_Components := Data.New_Component_List (Component_Names => Defining_Identifiers,
                                                   Subtype_Mark    => The_Type,
                                                   Is_Class_Wide   => Is_Class_Wide,
                                                   Has_Default     => Has_Default,
                                                   Parent          => Within.Scope);
        Conditional_Aspect_Specification ((Within.Scope, The_Type), Defining_Identifiers);
        Get_Element (Lexical.Semicolon);
        return The_Components;
      end;
    end Component_Declaration;


    procedure Component_Declaration (Scope : Data.Unit_Handle) is

      Defining_Identifiers : constant Identifiers := Defining_Identifier_List;

      The_Type    : Data_Handle;
      Has_Default : Boolean := False;

    begin
      Get_Element (Lexical.Colon);
      The_Type := Component_Definition ((Scope, null));
      declare
        Is_Class_Wide : constant Boolean := Is_Class_Wide_Type;
      begin
        if Element_Is (Lexical.Assignment) then
          Has_Default := True;
          The_Target_Type := The_Type;
          Dummy := Expression ((Scope, The_Type));
        end if;
        Data.New_Objects (Names         => Defining_Identifiers,
                          Subtype_Mark  => The_Type,
                          Is_Class_Wide => Is_Class_Wide,
                          Has_Default   => Has_Default,
                          Parent        => Scope);
        Get_Element (Lexical.Semicolon);
      end;
    end Component_Declaration;


    -- access_type_definition ::=
    --      [null_exclusion] access_to_object_definition
    --    | [null_exclusion] access_to_subprogram_definition
    --
    --    access_to_object_definition ::=
    --         access [general_access_modifier] subtype_indication
    --
    --       general_access_modifier ::=
    --             all
    --           | constant
    --
    --    access_to_subprogram_definition ::=
    --         access [protected] procedure parameter_profile
    --       | access [protected] function  parameter_and_result_profile
    --
    function Access_Type_Definition (Id       : Identifier_Handle;
                                     Within   : Data.Context) return Data_Handle is
    begin
      if Element_Is (Lexical.Is_Not) then
        Get_Element (Lexical.Is_Null);
        Check (Lexical.Is_Access);
      end if;
      case Next_Token.Element is
      when Lexical.Identifier =>
        null;
      when Lexical.Is_All | Lexical.Is_Constant =>
        Get_Next_Token;
      when Lexical.Is_Protected =>
        case Next_Token.Element is
        when Lexical.Is_Function | Lexical.Is_Procedure =>
          return Data.New_Protected_Subprogram_Access_Type
                     (Id      => Id,
                      Parent  => Within.Scope,
                      Profile => Subprogram_Profile (Within.Scope,
                                                     Parsed_Element_Is (Lexical.Is_Function)));
        when others =>
          Syntax_Error;
        end case;
      when Lexical.Is_Function | Lexical.Is_Procedure =>
          return Data.New_Subprogram_Access_Type
                     (Id      => Id,
                      Parent  => Within.Scope,
                      Profile => Data.Used (Subprogram_Profile (Within.Scope,
                                                                Parsed_Element_Is (Lexical.Is_Function))));
      when others =>
        Syntax_Error;
      end case;
      return Access_Type_Of (Id, Within.Scope, Subtype_Indication_Part (Within));
    end Access_Type_Definition;


    --       discrete_subtype_definition ::= --> :== Discrete_Range
    --            subtype_indication
    --          | range
    --

    -- array_type_definition ::=
    --      unconstrained_array_definition
    --    | constrained_array_definition
    --
    --    unconstrained_array_definition ::=
    --         array ( index_subtype_definition { , index_subtype_definition} ) of component_definition
    --
    --       index_subtype_definition ::=
    --            subtype_mark range <>
    --
    --    constrained_array_definition ::=
    --         array ( discrete_subtype_definition { , discrete_subtype_definition} ) of component_definition
    --
    function Array_Type_Definition (Scope : Data.Unit_Handle) return Data.Array_Definition_Handle is
      The_Actual_Token   : Lexical_Handle;
      The_Index_Subtypes : Data.List.Item;
      The_Subtype        : Data_Handle;
    begin
      Get_Next_Token;
      Get_Element (Lexical.Left_Parenthesis);
      loop
        The_Actual_Token := The_Token;
        if Token_Element = Lexical.Identifier then
          The_Subtype := Name_Of (Scope);
        else
          The_Subtype := null;
        end if;
        if Token_Element /= Lexical.Apostrophe and then Data.Is_Subtype (The_Subtype) then
          if Element_Ahead_Is (Lexical.Unconstrained) then
            Get_Element (Lexical.Is_Range);
            Get_Element (Lexical.Unconstrained);
          elsif Element_Is (Lexical.Range_Delimiter) then
            Dummy := Simple_Expression ((Scope, The_Subtype));
          else
            Conditional_Constraint ((Scope, The_Subtype));
          end if;
        elsif Element_Is (Lexical.Apostrophe) then -- qualified_expression
          Dummy := Aggregate ((Scope, The_Subtype));
          if Element_Is (Lexical.Range_Delimiter) then
            Dummy := Simple_Expression ((Scope, The_Subtype));
          end if;
        else
          The_Token := The_Actual_Token;
          The_Subtype := Range_Production ((Scope, null));
          Conditional_Constraint ((Scope, null));
        end if;
        The_Index_Subtypes.Append (The_Subtype);
        exit when not Element_Is (Lexical.Comma);
      end loop;
      Get_Element (Lexical.Right_Parenthesis);
      Get_Element (Lexical.Is_Of);
      return Data.New_Array_Definition (The_Index_Subtypes, Component_Definition ((Scope, null)));
    end Array_Type_Definition;


    -- discriminant_specifications ::=
    --      discriminant_specification { ; discriminant_specification}
    --
    --
    function Discriminant_Specifications (Scope : Data.Unit_Handle) return Data.List.Item is

      The_List : Data.List.Item;

      --    discriminant_specification ::=
      --         defining_identifier_list : [ null_exclusion ] subtype_mark [ := default_expression]
      --       | defining_identifier_list : access_definition [ := default_expression]
      --
      --       access_definition ::=
      --             access access_definition_part
      --
      function Discriminant_Specification return Data.List.Item is

        Defining_Identifiers : constant Identifiers := Defining_Identifier_List;

        The_Type    : Data_Handle;
        Has_Default : Boolean := False;

      begin
        Get_Element (Lexical.Colon);
        if Element_Is (Lexical.Is_Not) then
          Get_Element (Lexical.Is_Null);
        end if;
        if Element_Is (Lexical.Is_Access) then
          The_Type := Access_Definition_Part (Scope);
        elsif Element_Is (Lexical.Is_Access) then
          The_Type := Access_Definition_Part (Scope);
        else
          The_Type := Subtype_Mark (Scope);
        end if;
        declare
          Is_Class_Wide : constant Boolean := Is_Class_Wide_Type;
        begin
          if Element_Is (Lexical.Assignment) then
            Has_Default := True;
            The_Target_Type := The_Type;
            Dummy := Expression ((Scope, The_Type));
          end if;
          return Data.New_Discriminant_List (Component_Names => Defining_Identifiers,
                                             Subtype_Mark    => The_Type,
                                             Is_Class_Wide   => Is_Class_Wide,
                                             Has_Default     => Has_Default,
                                             Parent          => Scope);
        end;
      end Discriminant_Specification;

    begin
      The_List := Discriminant_Specification;
      while Element_Is (Lexical.Semicolon) loop
        The_List.Append (Discriminant_Specification);
      end loop;
      return The_List;
    end Discriminant_Specifications;


    -- interface_list ::= subtype_mark {and subtype_mark}
    --
    function Interface_List (Scope : Data.Unit_Handle) return Data.List.Item is
      The_Interfaces : Data.List.Item;
    begin
      The_Interfaces.Append (Subtype_Mark (Scope));
      while Element_Is (Lexical.Is_And) loop
        The_Interfaces.Append (Subtype_Mark (Scope));
      end loop;
      return The_Interfaces;
    end Interface_List;


    -- known_discriminant_part ::=
    --      ( discriminant_specifications )
    --
    function Known_Discriminant_Part (Scope : Data.Unit_Handle) return Data.List.Item is
      The_Discriminants : Data.List.Item;
    begin
      Get_Element (Lexical.Left_Parenthesis);
      The_Discriminants := Discriminant_Specifications (Scope);
      Get_Element (Lexical.Right_Parenthesis);
      return The_Discriminants;
    end Known_Discriminant_Part;


    -- discriminant_part ::=
    --      unknown_discriminant_part
    --    | known_discriminant_part
    --
    --    unknown_discriminant_part ::=
    --         ( <> )
    function Conditional_Discriminant_Part (Scope : Data.Unit_Handle) return Data.List.Item is
      The_Discriminants : Data.List.Item;
    begin
      if Token_Element = Lexical.Left_Parenthesis then
        if not Next_Element_Is (Lexical.Unconstrained) then
          The_Discriminants := Discriminant_Specifications (Scope);
        end if;
        Get_Element (Lexical.Right_Parenthesis);
      end if;
      return The_Discriminants;
    end Conditional_Discriminant_Part;


    -- generic_formal_part ::=
    --      generic {generic_formal_parameter_declaration | use_clause}
    --
    --    generic_formal_parameter_declaration ::=
    --         formal_object_declaration
    --       | formal_type_declaration
    --       | formal_subprogram_declaration
    --       | formal_package_declaration
    --
    function Generic_Formal_Part (Scope : Data.Unit_Handle) return Data.Formal_Block_Handle is

      Parameters : constant Data.Formal_Block_Handle := Data.New_Formal_Part (Scope);

      -- formal_object_declaration ::=
      --       defining_identifier_list : mode subtype_mark [ := default_expression] ;
      --
      procedure Formal_Object_Declaration is

        Defining_Identifiers : constant Identifiers := Defining_Identifier_List;

        The_Type    : Data_Handle;
        Has_Default : Boolean := False;

      begin
        Get_Element (Lexical.Colon);
        Mode;
        The_Type := Subtype_Mark (Data.Unit_Handle(Parameters));
        if Element_Is (Lexical.Assignment) then
          Has_Default := True;
          The_Target_Type := The_Type;
          Dummy := Expression ((Data.Unit_Handle(Parameters), The_Type));
        end if;
        Get_Element (Lexical.Semicolon);
        Data.New_Formal_Objects(Formal_Names => Defining_Identifiers,
                                Subtype_Mark => The_Type,
                                Has_Default  => Has_Default,
                                Parameters   => Parameters);
      end Formal_Object_Declaration;


      -- formal_type_declaration ::=
      --      formal_complete_type_declaration
      --    | formal_incomplete_type_definition
      --
      -- formal_complete_type_declaration ::=
      --      type defining_identifier [discriminant_part] is formal_type_definition [aspect_specification] ;
      --
      -- formal_inomplete_type_declaration ::=
      --      type defining_identifier [discriminant_part] [is tagged] ;
      --
      --
      --    formal_type_definition ::=
      --          formal_private_type_definition
      --        | formal_derived_type_definition
      --        | formal_discrete_type_definition
      --        | formal_signed_integer_type_definition
      --        | formal_modular_type_definition
      --        | formal_floating_point_definition
      --        | formal_ordinary_fixed_point_definition
      --        | formal_decimal_fixed_point_definition
      --        | formal_array_type_definition
      --        | formal_access_type_definition
      --
      --       formal_private_type_definition ::=
      --             [[abstract] tagged] [limited] private
      --
      --       formal_derived_type_definition ::=
      --             [abstract] new subtype_mark [with private]
      --
      --       formal_discrete_type_definition ::=
      --             ( <> )
      --
      --       formal_signed_integer_type_definition ::=
      --             range <>
      --
      --       formal_modular_type_definition ::=
      --             mod <>
      --
      --       formal_floating_point_definition ::=
      --             digits <>
      --
      --       formal_ordinary_fixed_point_definition ::=
      --             delta <>
      --
      --       formal_decimal_fixed_point_definition ::=
      --             delta <> digits <>
      --
      --       formal_array_type_definition ::=
      --             array_type_definition
      --
      --       formal_access_type_definition ::=
      --             access_type_definition
      --
      Unused_Not_Implememnted_Discriminant_Handling : Data.List.Item;

      procedure Formal_Type_Declaration is

        Defining_Identifier : constant Identifier_Handle := Next_Declaring_Identifier;

        From_Type : Data_Handle;

      begin
        Unused_Not_Implememnted_Discriminant_Handling := Conditional_Discriminant_Part (Data.Unit_Handle(Parameters));
        if Element_Is (Lexical.Is_Is) then
          case Token_Element is
          when Lexical.Is_Abstract =>
            Get_Next_Token;
            case Token_Element is
            when Lexical.Is_New =>
              Get_Next_Token;
              From_Type := Subtype_Mark (Data.Unit_Handle(Parameters));
              Get_Element (Lexical.Is_With);
              Data.New_Formal_Abstract_Private_Extension_Type (Defining_Identifier, From_Type, Parameters);
              Get_Element (Lexical.Is_Private);
            when Lexical.Is_Tagged =>
              Get_Next_Token;
              if Element_Is (Lexical.Is_Limited) then
                Log.Write ("%%% formal parameter 'abstract tagged limited'");
              else
                Log.Write ("%%% formal parameter 'abstract tagged'");
              end if;
              if Element_Is (Lexical.Is_Private) then
                Data.New_Formal_Limited_Private_Type (Defining_Identifier, Parameters);
              else
                Not_Implemented ("Generic_Formal_Part (abstract tagged)");
              end if;
            when others =>
              Not_Implemented ("Generic_Formal_Part (abstract)");
            end case;
          when Lexical.Is_Tagged =>
            Get_Next_Token;
            if Element_Is (Lexical.Is_Limited) then
              Log.Write ("%%% formal parameter 'tagged limited'");
            else
              Log.Write ("%%% formal parameter 'tagged'");
            end if;
            if Element_Is (Lexical.Is_Private) then
              Data.New_Formal_Limited_Private_Type (Defining_Identifier, Parameters);
            else
              Not_Implemented ("Generic_Formal_Part (tagged)");
            end if;
          when Lexical.Is_Limited =>
            Get_Next_Element (Lexical.Is_Private);
            Data.New_Formal_Limited_Private_Type (Defining_Identifier, Parameters);
          when Lexical.Is_Private =>
            Get_Next_Token;
            Data.New_Formal_Private_Type (Defining_Identifier, Parameters);
          when Lexical.Is_New =>
            Get_Next_Token;
            From_Type := Subtype_Mark (Data.Unit_Handle(Parameters));
            if Element_Is (Lexical.Is_With) then
              Data.New_Formal_Private_Extension_Type (Defining_Identifier, From_Type, Parameters);
              Get_Element (Lexical.Is_Private);
            else
              Data.New_Formal_Derived_Type (Defining_Identifier, From_Type, Parameters);
            end if;
          when Lexical.Left_Parenthesis =>
            Get_Next_Element (Lexical.Unconstrained);
            Get_Element (Lexical.Right_Parenthesis);
            Data.New_Formal_Discrete_Type (Defining_Identifier, Parameters);
          when Lexical.Is_Range =>
            Get_Next_Element (Lexical.Unconstrained);
            Data.New_Formal_Signed_Integer_Type (Defining_Identifier, Parameters);
          when Lexical.Is_Mod =>
            Get_Next_Element (Lexical.Unconstrained);
            Data.New_Formal_Modular_Type (Defining_Identifier, Parameters);
          when Lexical.Is_Digits =>
            Get_Next_Element (Lexical.Unconstrained);
            Data.New_Formal_Floating_Point_Type (Defining_Identifier, Parameters);
          when Lexical.Is_Delta =>
            Get_Next_Element (Lexical.Unconstrained);
            if Element_Is (Lexical.Is_Digits) then
              Get_Element (Lexical.Unconstrained);
              Data.New_Formal_Decimal_Fixed_Point_Type (Defining_Identifier, Parameters);
            else
              Data.New_Formal_Ordinary_Fixed_Point_Type (Defining_Identifier, Parameters);
            end if;
          when Lexical.Is_Array =>
            Data.New_Formal_Array_Type (Id         => Defining_Identifier,
                                        Definition => Array_Type_Definition (Data.Unit_Handle(Parameters)),
                                        Parameters => Parameters);
          when Lexical.Is_Access =>
            case Next_Token.Element is
            when Lexical.Identifier =>
              Data.New_Formal_Access_Type (Id         => Defining_Identifier,
                                           To_Type    => Subtype_Indication_Part ((Data.Unit_Handle(Parameters), null)),
                                           Parameters => Parameters);
            when Lexical.Is_All | Lexical.Is_Constant =>
              Get_Next_Token;
              Data.New_Formal_Access_Type (Id         => Defining_Identifier,
                                           To_Type    => Subtype_Indication_Part ((Data.Unit_Handle(Parameters), null)),
                                           Parameters => Parameters);
            when others =>
              Not_Implemented ("Generic_Formal_Part (formal_access_type_definition)");
            end case;
          when others =>
            Syntax_Error;
          end case;
          Conditional_Aspect_Specification ((Scope, null));
        else
          Data.New_Formal_Incomplete_Type (Defining_Identifier, Parameters);
        end if;
        Get_Element (Lexical.Semicolon);
      end Formal_Type_Declaration;


      -- formal_subprogram_declaration ::=
      --      with subprogram_specification [is [abstract | [abstract] subprogram_default] [aspect_specification] ;
      --
      --    subprogram_default ::=
      --          default_name
      --        | <>
      --        | null
      --
      procedure Formal_Subprogram_Declaration (Is_Function : Boolean := False) is
        Id      : constant Identifier_Handle := Next_Declaring_Identifier;
        Profile : constant Data.Subprogram_Profile := Data.Used (Subprogram_Profile (Data.Unit_Handle(Parameters),
                                                                                     Is_Function));
        The_Default_Name : Identifier_Handle;
        Skip_Default     : Boolean := False;
      begin
        if Element_Is (Lexical.Is_Is) then
          if Element_Is (Lexical.Is_Abstract) then
            Skip_Default := Token_Element in Lexical.Is_When | Lexical.Semicolon;
          end if;
          if not Skip_Default and then
             not Element_Is (Lexical.Unconstrained) and then not Element_Is (Lexical.Is_Null)
          then
            Dummy := Name_Of (Data.Unit_Handle(Parameters));
            The_Default_Name := The_Actual_Identifier;
          end if;
        end if;
        Data.New_Formal_Subprogram_Declaration (Id           => Id,
                                                Default_Name => The_Default_Name,
                                                Profile      => Profile,
                                                Parameters   => Parameters);
        Conditional_Aspect_Specification ((Scope, null));
        Get_Element (Lexical.Semicolon);
      end Formal_Subprogram_Declaration;


      -- formal_package_declaration ::=
      --       with package defining_identifier is new generic_package_name formal_package_actual_part ;
      --
      --    formal_package_actual_part ::=
      --         ( [others =>] <> )
      --       | [generic_actual_part]
      --       | (formal_package_association {, formal_package_association} [, others => <>])
      --
      procedure Formal_Package_Declaration is
        Id : constant Identifier_Handle := Next_Declaring_Identifier;
      begin
        Get_Element (Lexical.Is_Is);
        Get_Element (Lexical.Is_New);
        declare
          Generic_Package : constant Data.Unit_Handle := Unit_Of (Scope);
          Actual_Part     : Data.List.Elements_Access;
        begin
          if Element_Ahead_Is (Lexical.Unconstrained) then
            Get_Element (Lexical.Left_Parenthesis);
            Get_Element (Lexical.Unconstrained);
            Get_Element (Lexical.Right_Parenthesis);
          elsif Element_Ahead_Is (Lexical.Is_Others) then
            Get_Element (Lexical.Left_Parenthesis);
            Get_Element (Lexical.Is_Others);
            Get_Element (Lexical.Association);
            Get_Element (Lexical.Unconstrained);
            Get_Element (Lexical.Right_Parenthesis);
          else
            Actual_Part := new Data.List.Elements'(Conditional_Generic_Actual_Part (Generic_Package,
                                                                                    Data.Unit_Handle(Parameters)));
          end if;
          Data.New_Formal_Package_Declaration (Id              => Id,
                                               Generic_Package => Generic_Package,
                                               Actual_Part     => Actual_Part,
                                               Parameters      => Parameters);
        end;
        Get_Element (Lexical.Semicolon);
      end Formal_Package_Declaration;

    begin -- Generic_Formal_Part
      Get_Next_Token;
      loop
        case Token_Element is
        when Lexical.Identifier =>
          Formal_Object_Declaration;
        when Lexical.Is_Type =>
          Formal_Type_Declaration;
        when Lexical.Is_With =>
          case Next_Token.Element is
          when Lexical.Is_Function =>
            Formal_Subprogram_Declaration (Is_Function => True);
          when Lexical.Is_Package =>
            Formal_Package_Declaration;
          when Lexical.Is_Procedure =>
            Formal_Subprogram_Declaration;
          when others =>
            Syntax_Error;
          end case;
        when Lexical.Is_Use =>
          Use_Clause (Scope);
        when Lexical.Is_Pragma =>
          Pragma_Call (Scope);
        when others =>
          exit;
        end case;
      end loop;
      return Parameters;
    end Generic_Formal_Part;


    -- [generic_actual_part] ::=
    --      ( generic_association {, generic_association} )
    --
    --    generic_association ::=
    --         [generic_formal_parameter_selector_name =>] explicit_generic_actual_parameter
    --
    --       explicit_generic_actual_parameter ::=
    --            expression
    --          | variable_name
    --          | subprogram_name
    --          | entry_name
    --          | subtype_mark
    --          | package_instance_name
    --
    function Conditional_Generic_Actual_Part (Generic_Unit      : Data.Unit_Handle;
                                              Scope             : Data.Unit_Handle;
                                              The_Instantiation : Data.Instantiation_Handle := null)
      return Data.List.Elements
    is
      The_Formal_Part : constant Data.Formal_Block_Handle := Data.Generic_Parameters_Of (Generic_Unit);

      First : constant Positive := Positive'first;

      The_Index         : Natural := Data.List.Not_Found;
      Position_Is_Known : Boolean := True;

      The_Type : Data_Handle;
      The_Last : Natural;

      use type Data.Formal_Block_Handle;
      use type Data.Instantiation_Handle;

    begin
      if The_Formal_Part = null then
        The_Last := 0;
      else
        The_Last := The_Formal_Part.Last_Position;
      end if;
      declare
        Actual_Part : Data.List.Elements(First .. The_Last);
      begin
        if Element_Is (Lexical.Left_Parenthesis) then
          loop
            if Element_Ahead_Is (Lexical.Association) then
              Position_Is_Known := False;
              declare
                Selector : constant Identifier_Handle := Selector_Name;
              begin
                if Is_Null (Selector) then
                  The_Index := Data.List.Not_Found;
                else
                  The_Index := Data.Formal_Position_Of (Selector, The_Formal_Part);
                  The_Type := Selector.Data;
                  if The_Type /= null and then The_Type.all in Data.Formal_Object'class then
                    The_Type := Data.Formal_Object_Handle(The_Type).Declaration;
                    if The_Type.all in Data.Formal_Type'class then
                      declare
                        Type_Index : constant Natural := Data.Formal_Position_Of (The_Type.Location, The_Formal_Part);
                      begin
                        if Type_Index /= Data.List.Not_Found then -- own formal type
                          The_Type := Actual_Part (Type_Index);   -- use actual type from own actual part
                        end if;
                      end;
                    end if;
                  end if;
                end if;
              end;
              Get_Element (Lexical.Association);
            elsif not Position_Is_Known then
              Syntax_Error;
            else
              The_Index := The_Index + 1;
              The_Type := Data.Formal_Type_At (The_Index, The_Formal_Part);
            end if;
            if The_Instantiation /= null and then The_Type /= null and then
              The_Type.all in Data.Formal_Subprogram'class
            then
              The_Type := Data.New_Instantiation (The_Type, The_Instantiation);
            end if;
            if not Element_Is (Lexical.Unconstrained) then -- GNAT 2022 ???
              The_Type := Expression ((Scope, The_Type));
              if The_Index /= Data.List.Not_Found then
                if The_Index <= Actual_Part'last then
                  if The_Type = null and then The_Actual_Identifier.all in Operator_Symbol'class then
                    The_Type := Data.Predefined_Operator;
                    The_Actual_Identifier.Data := The_Type;
                  end if;
                  Actual_Part(The_Index) := The_Type;
                end if;
              end if;
            end if;
            exit when not Element_Is (Lexical.Comma);
          end loop;
          Get_Element (Lexical.Right_Parenthesis);
        end if;
        return Actual_Part;
      end;
    end Conditional_Generic_Actual_Part;


    -- package defining_program_unit_name is new generic_package_name [generic_actual_part] ;
    --
    procedure Package_Instantiation (Id    : Identifier_Handle;
                                     Scope : Data.Unit_Handle) is

      The_Package       : Data.Unit_Handle;
      The_Instantiation : Data.Instantiation_Handle;

    begin
      Get_From (Scope, The_Package, The_Instantiation);
      Data.New_Package_Instantiation (Id                   => Id,
                                      Parent               => Scope,
                                      Generic_Package      => The_Package,
                                      Parent_Instantiation => The_Instantiation,
                                      Actual_Part          => Conditional_Generic_Actual_Part (The_Package, Scope));
      Get_Element (Lexical.Semicolon);
    end Package_Instantiation;


    -- procedure defining_program_unit_name is new generic_procedure_name [generic_actual_part] ;
    -- function defining_designator is new generic_function_name [generic_actual_part] ;
    --
    procedure Subprogram_Instantiation (Id    : Identifier_Handle;
                                        Scope : Data.Unit_Handle) is

      The_Unit          : Data.Unit_Handle;
      The_Subprogram    : Identifier_Handle;
      The_Instantiation : Data.Instantiation_Handle;

      use type Data.Unit_Handle;

    begin
      --TEST--------------------------------------------------------
      --Write_Log ("Subprogram Instantiation " & Image_Of (Id.all));
      --------------------------------------------------------------
      Get_Next_Token;
      The_Unit := Unit_Of (Scope);
      The_Subprogram := The_Actual_Identifier;
      if not Is_Null (The_Subprogram) and then The_Subprogram.Data /= null and then
        The_Subprogram.Data.all in Data.Package_Instantiation'class
      then
        The_Instantiation := Data.Instantiation_Handle(The_Subprogram.Data);
        --TEST------------------------------------------------------------------------------------
        --Write_Log ("- Package Instantiation: " & Name.Image_Of (The_Instantiation.Location.Id));
        ------------------------------------------------------------------------------------------
      end if;
      declare
        Actual_Part : constant Data.List.Elements := Conditional_Generic_Actual_Part (The_Unit,
                                                                                      Scope,
                                                                                      The_Instantiation);
      begin
        if The_Unit /= null then
          The_Unit := Data.New_Subprogram_Declaration
                        (Id      => Id,
                         Parent  => Scope,
                         Profile => Data.Actual_Profile_For (Generic_Unit => The_Unit,
                                                             Subprogram   => The_Subprogram,
                                                             Actual_Part  => Actual_Part));
        end if;
      end;
      Get_Element (Lexical.Semicolon);
    end Subprogram_Instantiation;


    procedure Package_Renaming (Id    : Identifier_Handle;
                                Scope : Data.Unit_Handle) is
    begin
      Data.New_Package_Renaming (Id, Scope, Next_Name_Of (Scope));
      Get_Element (Lexical.Semicolon);
    end Package_Renaming;


    procedure Subprogram_Renaming (Id      : Identifier_Handle;
                                   Scope   : Data.Unit_Handle;
                                   Profile : Data.Subprogram_Profile) is

      Actual_Token : constant Lexical_Handle := Next_Token;

      The_Unit : Data_Handle;

      use type Data.Type_Handle;

    begin
      The_Unit := Name_Of ((Scope, null), No_Association => True);
      --TEST-----------------------------------------------------
      --Write_Log ("Rename Subprogram " & Name.Image_Of (Id.Id));
      --Write_Log ("- Unit: " & Data.Full_Name_Of (The_Unit));
      -----------------------------------------------------------
      if The_Unit = null then
        if Lexical_Before(The_Token).all in Operator_Symbol'class then --%%%
          The_Unit := Data.Predefined_Operator;
          Identifier_Handle(Lexical_Before(The_Token)).Data := The_Unit;
          Data.New_Subprogram_Renaming (Id, Scope, Profile, null, null);
          Get_Element (Lexical.Semicolon);
          return;
        end if;
        declare
          Class : constant Data.Type_Handle := Data.Class_Of (Profile);
        begin
          if Class /= null then
            --TEST-----------------------------------------------------------------------------------------------
            --Write_Log ("RENAME INHERITED SUBPROGRAM " & Name.Image_Of (Id.Id));
            --Write_Log ("Search in: " & Name.Image_Of (Data.Type_Handle(Class.Parent_Type).Parent.Location.Id));
            -----------------------------------------------------------------------------------------------------
            The_Token := Actual_Token;
            The_Unit := Name_Of ((Data.Type_Handle(Class.Parent_Type).Parent, null), No_Association => True);
          end if;
        end;
      end if;
      if The_Unit = null then
        Get_Element (Lexical.Semicolon);
        return;
      elsif The_Unit.all in Data.Unit_Type'class and then The_Token.all in Identifier'class then
        Data.New_Subprogram_Renaming (Id, Scope, Profile, Data.Unit_Handle(The_Unit), Identifier_Handle(The_Token));
      end if;
      Get_Next_Element (Lexical.Semicolon);
    end Subprogram_Renaming;


    function Expression_Function (The_Identifier : Identifier_Handle;
                                  Scope          : Data.Unit_Handle;
                                  Profile        : Data.Subprogram_Profile) return Data.Unit_Handle is
      The_Unit : Data.Unit_Handle;
    begin
      The_Unit := Data.New_Function_Expression_Declaration (The_Identifier, Scope, Profile);
      case Token_Element is
      when Lexical.Is_Declare =>
        Dummy := Declare_Expression ((The_Unit, Profile.Result_Type));
      when Lexical.Is_If =>
        Dummy := If_Expression ((The_Unit, Profile.Result_Type));
      when Lexical.Is_Case =>
        Dummy := Case_Expression ((The_Unit, Profile.Result_Type));
      when Lexical.Is_For =>
        Dummy := Quantified_Expression ((The_Unit, Profile.Result_Type));
      when others =>
        Dummy := Expression ((The_Unit, Profile.Result_Type));
      end case;
      Get_Element (Lexical.Right_Parenthesis);
      if Token_Element = Lexical.Is_With then
        Aspect_Specification ((The_Unit, null));
      end if;
      return The_Unit;
    end Expression_Function;


    procedure Subprogram_Declaration_Or_Body (Scope         : Data.Unit_Handle;
                                              Is_Function   : Boolean := False;
                                              Is_Overriding : Boolean := False) with Inline is
      The_Identifier : constant Identifier_Handle := Next_Declaring_Identifier;
      Profile        : constant Data.Subprogram_Profile := Subprogram_Profile (Scope, Is_Function);
      Unused_Unit    : Data.Unit_Handle;
    begin
      case Token_Element is
      when Lexical.Is_Is =>
        if Is_Function and then Next_Element_Ahead_Is (Lexical.Left_Parenthesis) then
          Get_Next_Token;
          Unused_Unit := Expression_Function (The_Identifier, Scope, Profile);
          Get_Element (Lexical.Semicolon);
        else
          Subprogram_Body (Data.New_Subprogram_Body (The_Identifier, Scope, Profile), Is_Overriding);
        end if;
      when Lexical.Semicolon =>
        Unused_Unit := Data.New_Subprogram_Declaration (The_Identifier, Scope, Profile);
        Get_Next_Token;
      when others =>
        Syntax_Error;
      end case;
    end Subprogram_Declaration_Or_Body;


    -- aspect_clause ::=
    --      attribute_definition_clause
    --    | enumeration_representation_clause
    --    | record_representation_clause
    --    | at_clause
    --
    --    attribute_definition_clause ::=
    --         for local_name ' attribute_designator use expression ;
    --       | for local_name ' attribute_designator use name ;
    --
    --       local_name ::=
    --            direct_name
    --          | direct_name ' attribute_designator
    --          | library_unit_name
    --
    --    enumeration_representation_clause ::=
    --         for first_subtype_local_name use enumeration_aggregate ;
    --
    --    record_representation_clause ::=
    --         for first_subtype_local_name use record [mod_clause]
    --           {component_clause}
    --         end record ;
    --
    --       mod_clause ::=
    --            at mod static_expression ;
    --
    --       component_clause ::=
    --             component_local_name at position range first_bit .. last_bit ;
    --
    --          position ::=
    --                static_expression
    --
    --          first_bit ::=
    --                static_simple_expression
    --
    --          last_bit ::=
    --                static_simple_expression
    --
    --    at_clause ::=
    --         for direct_name use at expression ;
    --
    procedure Aspect_Clause (Scope : Data.Unit_Handle) is
      The_Type : Data_Handle;
    begin
      The_Type := Next_Name_Of (Scope);
      Get_Element (Lexical.Is_Use);
      case Token_Element is
      when Lexical.Is_Record =>
        if Next_Element_Is (Lexical.Is_At) then
          Get_Element (Lexical.Is_Mod);
          Dummy := Expression ((Scope, null));
          Get_Element (Lexical.Semicolon);
        end if;
        while Token_Element = Lexical.Identifier loop
          declare
            Item : constant Identifier_Handle := Actual_Identifier;
          begin
            if The_Type.all in Data.Record_Type'class then
              Dummy := Data.Component_Choice_Of (Item, Data.Record_Handle(The_Type));
            end if;
          end;
          Get_Element (Lexical.Is_At);
          Dummy := Expression ((Scope, null));
          Get_Element (Lexical.Is_Range);
          Dummy := Simple_Expression ((Scope, null));
          Get_Element (Lexical.Range_Delimiter);
          Dummy := Simple_Expression ((Scope, Dummy));
          Get_Element (Lexical.Semicolon);
        end loop;
        Get_Element (Lexical.Is_End);
        Get_Element (Lexical.Is_Record);
      when Lexical.Is_At =>
        Get_Next_Token;
        Dummy := Expression ((Scope, null));
      when others =>
        Dummy := Expression ((Scope, The_Type));
        if The_Actual_Identifier.Data = null then
          The_Actual_Identifier.Data := Data.Predefined_Operator;
        end if;
      end case;
      Get_Element (Lexical.Semicolon);
    end Aspect_Clause;


    -- entry_body ::=
    --      entry defining_identifier entry_body_formal_part entry_barrier is
    --        declarative_part
    --      begin
    --        handled_sequence_of_statements
    --      end [entry_identifier] ;
    --
    --    entry_body_formal_part ::=
    --         [ ( entry_index_specification ) ] parameter_profile
    --
    --       entry_index_specification ::=
    --            for defining_identifier in discrete_subtype_definition
    --
    --    entry_barrier ::=
    --         when condition
    --
    procedure Entry_Body (Scope : Data.Unit_Handle) is

      Id : constant Identifier_Handle := Next_Declaring_Identifier;

      The_Index_Id      : Identifier_Handle;
      The_Index_Subtype : Data_Handle;
      The_Profile       : Data.Subprogram_Profile;
      Self              : Data.Unit_Handle;

    begin
      if Element_Ahead_Is (Lexical.Is_For) then
        Get_Element (Lexical.Left_Parenthesis);
        Get_Element (Lexical.Is_For);
        The_Index_Id := Declaring_Identifier;
        Get_Element (Lexical.Is_In);
        The_Index_Subtype := Discrete_Subtype_Definition (Scope);
        if Is_Class_Wide_Type then
          Write_Log ("%%% Is_Class_Wide ENTRY");
        end if;
        Get_Element (Lexical.Right_Parenthesis);
      end if;
      if Token_Element = Lexical.Left_Parenthesis then
        The_Profile := Data.New_Profile (Formal_Part (Scope));
      else
        The_Profile := Data.New_Profile;
      end if;
      Self := Data.New_Entry_Body (Id, Scope, The_Profile, The_Index_Subtype);
      if not Is_Null (The_Index_Id) then
        Data.New_Object (Id            => The_Index_Id,
                         Subtype_Mark  => The_Index_Subtype,
                         Is_Class_Wide => False,
                         Has_Default   => False,
                         Parent        => Self);
      end if;
      if Element_Is (Lexical.Is_When) then
        Condition (Self);
      end if;
      Get_Element (Lexical.Is_Is);
      Declarative_Part (Self);
      Get_Element (Lexical.Is_Begin);
      Handled_Sequence_Of_Statements (Self);
      Check_Declaration_End (Self);
      Get_Element (Lexical.Semicolon);
    end Entry_Body;

    -- protected_body ::=
    --     protected body defining_identifier is
    --       { protected_operation_item }
    --     end [protected_identifier] ;
    --
    --     protected_operation_item ::=
    --          subprogram_declaration
    --        | subprogram_body
    --        | null_procedure_declaration
    --        | expression_function_declaration
    --        | entry_body
    --        | aspect_clause
    --
    -- protected_body_stub ::=
    --      protected body defining_identifier is separate ;
    --
    procedure Protected_Body (Scope : Data.Unit_Handle) is

      Id   : constant Identifier_Handle := Next_Declaring_Identifier;
      Self : Data.Unit_Handle;

    begin
      Get_Element (Lexical.Is_Is);
      Self := Data.New_Protected_Body (Id, Scope);
      if not Element_Is (Lexical.Is_Separate) then
        loop
          case Token_Element is
          when Lexical.Is_Function =>
            Subprogram_Declaration_Or_Body (Self, Is_Function => True);
          when Lexical.Is_Procedure =>
            Subprogram_Declaration_Or_Body (Self);
          when Lexical.Is_Entry =>
            Entry_Body (Self);
          when Lexical.Is_For =>
            Aspect_Clause (Self);
          when Lexical.Is_Pragma =>
            Pragma_Call (Self);
          when Lexical.Is_End =>
            exit;
          when others =>
            Syntax_Error;
          end case;
        end loop;
        Check_Declaration_End (Self);
      end if;
      Get_Element (Lexical.Semicolon);
    end Protected_Body;


    -- task_body ::=
    --      task body defining_identifier is
    --        declarative_part
    --      begin
    --        handled_sequence_of_statements
    --      end [task_identifier] ;
    --
    -- task_body_stub ::=
    --      task body defining_identifier is separate ;

    procedure Task_Body (Scope : Data.Unit_Handle) is
      Id   : constant Identifier_Handle := Next_Declaring_Identifier;
      Self : Data.Unit_Handle;
    begin
      Get_Element (Lexical.Is_Is);
      Self := Data.New_Task_Body (Id, Scope);
      if not Element_Is (Lexical.Is_Separate) then
        Declarative_Part (Self);
        Get_Element (Lexical.Is_Begin);
        Handled_Sequence_Of_Statements (Self, Is_In_Task_Body => True);
        Check_Declaration_End (Self);
      end if;
      Get_Element (Lexical.Semicolon);
    end Task_Body;


    -- Overriding_Indicator ::=
    --     [ not ] overriding
    function Has_Overriding_Indicator return Boolean is
    begin
      case Token_Element is
      when Lexical.Is_Overriding =>
        Get_Next_Token;
      when Lexical.Is_Not =>
        Get_Next_Element (Lexical.Is_Overriding);
      when others =>
        return False;
      end case;
      return True;
    end Has_Overriding_Indicator;


    -- declarative_part ::=
    --      {declarative_item}
    --
    -- declarative_item ::=
    --      basic_declarative_item
    --    | body
    --
    --    basic_declarative_item ::=
    --         basic_declaration
    --       | aspect_clause
    --       | use_clause
    --
    --       basic_declaration ::=
    --            type_declaration
    --          | subtype_declaration
    --          | object_declaration
    --          | number_declaration
    --          | subprogram_declaration
    --          | abstract_subprogram_declaration
    --          | expression_function_declaration
    --          | package_declaration
    --          | renaming_declaration
    --          | exception_declaration
    --          | generic_declaration
    --          | generic_instantiation
    --
    --          renaming_declaration ::=
    --                object_renaming_declaration
    --              | exception_renaming_declaration
    --              | package_renaming_declaration
    --              | subprogram_renaming_declaration
    --              | generic_renaming_declaration
    --
    --          expression_function_declaration ::=
    --               [overriding_indicator]
    --               function_specification is
    --                  ( expression )
    --                  [aspect_specification] ;
    --
    --    body ::=
    --         proper_body
    --       | body_stub
    --
    --       proper_body ::=
    --            subprogram_body
    --          | package_body
    --          | task_body
    --          | protected_body
    --
    --       body_stub ::=
    --            subprogram_body_stub
    --          | package_body_stub
    --          | task_body_stub
    --          | protected_body_stub
    --
    procedure Declarative_Part (Scope    : Data.Unit_Handle;
                                Is_Basic : Boolean := False) is

      -- subtype_declaration ::=
      --      subtype defining_identifier is subtype_indication [aspect_specification] ;
      --
      procedure Subtype_Declaration is

        Defining_Identifier : constant Identifier_Handle := Next_Declaring_Identifier;

      begin
        Get_Element (Lexical.Is_Is);
        declare
          The_Subtype_Mark : constant Data_Handle := Subtype_Indication ((Scope, null));
        begin
          Data.New_Subtype (Id            => Defining_Identifier,
                            From_Type     => The_Subtype_Mark,
                            Is_Class_Wide => Is_Class_Wide_Type,
                            Parent        => Scope);
          Conditional_Aspect_Specification ((Scope, The_Subtype_Mark));
        end;
        Get_Element (Lexical.Semicolon);
      end Subtype_Declaration;


      -- Entry_Declaration_Only ::=
      --      entry defining_identifier [(discrete_subtype_definition)] parameter_profile
      --        [aspect_specification] ;
      --
      procedure Entry_Declaration_Only (Self : Data.Unit_Handle) is
      begin
        Get_Element (Lexical.Is_Entry);
        declare
          Id : constant Identifier_Handle := Declaring_Identifier;

          Start_Token : constant Lexical_Handle := The_Token;

          The_Subtype : Data_Handle;
          The_Profile : Data.Subprogram_Profile;

        begin
          if Element_Is (Lexical.Left_Parenthesis) then
            The_Subtype := Discrete_Subtype_Definition (Scope);
            if not Element_Is (Lexical.Right_Parenthesis) then
              The_Token := Start_Token; -- not a family entry
              The_Subtype := null;
            end if;
          end if;
          if Token_Element = Lexical.Left_Parenthesis then
            The_Profile := Data.New_Profile (Formal_Part (Scope));
          else
            The_Profile := Data.New_Profile;
          end if;
          Conditional_Aspect_Specification ((Self, null));
          Data.New_Entry_Declaration (Id, Self, The_Profile, The_Subtype);
          Get_Element (Lexical.Semicolon);
        end;
      end Entry_Declaration_Only;


      function Found_Subprogram_Declaration return Boolean is

        Saved_Token : constant Lexical_Handle := The_Token;

        Found_Declaration : Boolean := False;

      begin
        if Token_Element = Lexical.Is_With then
          begin
            loop
              Get_Next_Token;
              case Token_Element is
              when Lexical.Is_Is =>
                Get_Next_Token;
                case Token_Element is
                when Lexical.Left_Parenthesis | Lexical.Is_Null | Lexical.Is_Abstract =>
                  Found_Declaration := True;
                when others =>
                  null;
                end case;
                exit;
              when Lexical.Semicolon =>
                Found_Declaration := True;
                exit;
              when others =>
                null;
              end case;
            end loop;
          exception
          when others =>
            null;
          end;
        end if;
        The_Token := Saved_Token;
        return Found_Declaration;
      end Found_Subprogram_Declaration;


      procedure Subprogram (Self               : Data.Unit_Handle;
                            Is_Function        : Boolean := False;
                            Is_Overriding_Body : Boolean := False) with Inline is
        The_Identifier    : constant Identifier_Handle := Next_Declaring_Identifier;
        Profile           : constant Data.Subprogram_Profile := Subprogram_Profile (Scope, Is_Function);
        Found_Declaration : constant Boolean := Found_Subprogram_Declaration;
        The_Unit          : Data.Unit_Handle;
      begin
        case Token_Element is
        when Lexical.Is_With =>
          if Found_Declaration then
            The_Unit := Data.New_Subprogram_Declaration (The_Identifier, Self, Profile);
            Aspect_Specification ((The_Unit, null), [The_Identifier]);
            Get_Element (Lexical.Semicolon);
          else
            if Is_Basic then
              Syntax_Error;
            else
              The_Unit := Data.New_Subprogram_Body (The_Identifier, Self, Profile);
              Aspect_Specification ((The_Unit, null));
              Subprogram_Body (The_Unit, Is_Overriding_Body);
            end if;
          end if;
        when Lexical.Is_Is =>
          if Is_Basic then
            Syntax_Error;
          else
            The_Unit := Data.New_Subprogram_Body (The_Identifier, Self, Profile);
            Subprogram_Body (The_Unit, Is_Overriding => Is_Overriding_Body);
          end if;
        when Lexical.Semicolon =>
          The_Unit := Data.New_Subprogram_Declaration (The_Identifier, Self, Profile);
          Get_Next_Token;
        when others =>
          Syntax_Error;
        end case;
      end Subprogram;


      procedure Subprogram (Is_Function        : Boolean := False;
                            Is_Overriding_Body : Boolean := False) with Inline is
        The_Identifier : constant Identifier_Handle := Next_Declaring_Identifier;
      begin
        if Token_Element = Lexical.Is_Is and then Next_Element_Ahead_Is (Lexical.Is_New) then
          Subprogram_Instantiation (The_Identifier, Scope);
        else
          declare

            Profile           : constant Data.Subprogram_Profile := Subprogram_Profile (Scope, Is_Function);
            Found_Declaration : constant Boolean := Found_Subprogram_Declaration;
            The_Unit          :          Data.Unit_Handle;

          begin
            case Token_Element is
            when Lexical.Is_With =>
              if Found_Declaration then
                if Element_Is (Lexical.Left_Parenthesis) then
                  The_Unit := Expression_Function (The_Identifier, Scope, Profile);
                else
                  The_Unit := Data.New_Subprogram_Declaration (The_Identifier, Scope, Profile);
                  Aspect_Specification ((The_Unit, null), [The_Identifier]);
                end if;
                Get_Element (Lexical.Semicolon);
              else
                if Is_Basic then
                  The_Unit := Data.New_Subprogram_Declaration (The_Identifier, Scope, Profile);
                  Aspect_Specification ((The_Unit, null), [The_Identifier]);
                  Get_Element (Lexical.Semicolon);
                else
                  The_Unit := Data.New_Subprogram_Body (The_Identifier, Scope, Profile);
                  Aspect_Specification ((The_Unit, null));
                  Subprogram_Body (The_Unit, Is_Overriding => Is_Overriding_Body);
                end if;
              end if;
            when Lexical.Is_Is =>
              if Next_Element_Ahead_Is (Lexical.Is_Abstract) or else
                 Next_Element_Ahead_Is (Lexical.Is_Null) or else
                 Next_Element_Ahead_Is (Lexical.Left_Parenthesis)
              then
                if Element_Is (Lexical.Left_Parenthesis) then
                  The_Unit := Expression_Function (The_Identifier, Scope, Profile);
                else
                  The_Unit := Data.New_Subprogram_Declaration (The_Identifier, Scope, Profile);
                  Data.Declaration_Handle(The_Unit).Is_Used := True;
                  Get_Next_Token;
                  Conditional_Aspect_Specification ((The_Unit, null));
                end if;
                Get_Element (Lexical.Semicolon);
              elsif Is_Basic then
                Syntax_Error;
              else
                Subprogram_Body (Data.New_Subprogram_Body (The_Identifier, Scope, Profile,
                                                           Next_Element_Ahead_Is (Lexical.Is_Separate)),
                                 Is_Overriding_Body);
              end if;
            when Lexical.Semicolon =>
              The_Unit := Data.New_Subprogram_Declaration (The_Identifier, Scope, Profile);
              Get_Next_Token;
            when Lexical.Is_Renames =>
              Subprogram_Renaming (The_Identifier, Scope, Data.Used(Profile));
            when others =>
              Syntax_Error;
            end case;
          end;
        end if;
      end Subprogram;


      -- protected_definition ::=
      --        { protected_operation_declaration }
      --      [ private
      --        { protected_element_declaration } ]
      --      end [protected_identifier]
      --
      --    protected_operation_declaration ::=
      --         subprogram_declaration
      --       | entry_declaration
      --       | aspect_clause
      --
      --    protected_element_declaration ::=
      --         protected_operation_declaration
      --       | component_declaration
      --
      procedure Protected_Definition (Self : Data.Unit_Handle) is
        Is_Private : Boolean := False;
      begin
        loop
          if Has_Overriding_Indicator then
            case Token_Element is
            when Lexical.Is_Function =>
              Subprogram (Self, Is_Function => True);
            when Lexical.Is_Procedure =>
              Subprogram (Self);
            when Lexical.Is_Entry =>
              Entry_Declaration_Only (Self);
            when others =>
              Syntax_Error;
            end case;
          else
            case Token_Element is
            when Lexical.Is_Function =>
              Subprogram (Self, Is_Function => True);
            when Lexical.Is_Procedure =>
              Subprogram (Self);
            when Lexical.Is_Entry =>
              Entry_Declaration_Only (Self);
            when Lexical.Identifier =>
              if Is_Private then
                Component_Declaration (Self);
              else
                Syntax_Error;
              end if;
            when Lexical.Is_For =>
              Aspect_Clause (Self);
            when Lexical.Is_Pragma =>
              Pragma_Call (Self);
            when Lexical.Is_Private =>
              Get_Next_Token;
              if Is_Private then
                Syntax_Error;
              else
                Is_Private := True;
              end if;
            when Lexical.Is_End =>
              Check_Declaration_End (Self);
              exit;
            when others =>
              Syntax_Error;
            end case;
          end if;
        end loop;
      end Protected_Definition;


      -- task_definition ::=
      --        {task_item}
      --      [ private
      --        {task_item}]
      --      end [task_identifier]
      --
      --    task_item ::=
      --         entry_declaration
      --       | aspect_clause
      --
      procedure Task_Definition (Self : Data.Unit_Handle) is
        Is_Private : Boolean := False;
      begin
        loop
          if Has_Overriding_Indicator then
            Entry_Declaration_Only (Self);
          else
            case Token_Element is
            when Lexical.Is_Entry =>
              Entry_Declaration_Only (Self);
            when Lexical.Is_For =>
              Aspect_Clause (Self);
            when Lexical.Is_Pragma =>
              Pragma_Call (Self);
            when Lexical.Is_Private =>
              Get_Next_Token;
              if Is_Private then
                Syntax_Error;
              else
                Is_Private := True;
              end if;
            when Lexical.Is_End =>
              Check_Declaration_End (Self);
              exit;
            when others =>
              Syntax_Error;
            end case;
          end if;
        end loop;
      end Task_Definition;


      -- type_declaration ::=
      --      full_type_declaration
      --    | incomplete_type_declaration
      --    | private_type_declaration
      --    | private_extension_declaration
      --
      --    full_type_declaration ::=
      --         type defining_identifier [known_discriminant_part] is type_definition [aspect_specification] ;
      --       | task_type_declaration      -> coded in Task_Type_Declaration
      --       | protected_type_declaration -> coded in Protected_Type_Declaraction
      --
      --    type_definition ::=
      --         enumeration_type_definition
      --       | integer_type_definition
      --       | real_type_definition
      --       | array_type_definition
      --       | record_type_definition
      --       | access_type_definition
      --       | derived_type_definition
      --       | interface_type_definition
      --
      --       integer_type_definition ::=
      --            signed_integer_type_definition
      --          | modular_type_definition
      --
      --       interface_type_definition ::=
      --            [limited | task | protected | synchronized] interface [and interface_list]
      --
      --       real_type_definition ::=
      --            floating_point_definition
      --          | fixed_point_definition
      --
      --       record_type_definition ::=
      --             [[abstract] tagged] [limited] record_definition
      --
      --    incomplete_type_declaration ::=
      --         type defining_identifier [discriminant_part] ;
      --
      --    private_type_declaration ::=
      --         type defining_identifier [discriminant_part] is [[abstract] tagged] [limited] private
      --           [aspect_specification] ;
      --
      --    private_extension_declaration ::=
      --         type defining_identifier [discriminant_part] is
      --           [abstract] [limited | synchronized] new ancestor_subtype_indication
      --           [and interface_List] with private
      --           [aspect_specification] ;
      --
      procedure Type_Declaration is

        Defining_Identifier : constant Identifier_Handle := Next_Declaring_Identifier;

        The_Discriminants : Data.List.Item;
        The_Type          : Data_Handle;

        procedure Array_Type_Definition with Inline is
        begin
          The_Type := Data.New_Array_Type (Id         => Defining_Identifier,
                                           Parent     => Scope,
                                           Definition => Array_Type_Definition (Scope));
          Conditional_Aspect_Specification ((Scope, The_Type));
        end Array_Type_Definition;


        -- enumeration_type_definition ::=
        --     ( enumeration_literal_specification {, enumeration_literal_specification} )
        --
        --    enumeration_literal_specification ::=
        --         defining_identifier
        --       | defining_character_literal
        --
        procedure Enumeration_Type_Definition is
        begin
          The_Type := Data.New_Enumeration_Type (Id     => Defining_Identifier,
                                                 Parent => Scope);
          loop
            Get_Next_Token;
            case Token_Element is
            when Lexical.Identifier =>
              Data.New_Enumeration_Value (Id           => Declaring_Identifier,
                                          Parent       => Scope,
                                          Subtype_Mark => The_Type);
            when others =>
              Syntax_Error;
            end case;
            exit when Token_Element /= Lexical.Comma;
          end loop;
          Get_Element (Lexical.Right_Parenthesis);
          Conditional_Aspect_Specification ((Scope, The_Type));
        end Enumeration_Type_Definition;


        -- signed_integer_type_definition ::=
        --      range static_simple_expression .. static_simple_expression
        --
        procedure Signed_Integer_Type_Definition is
        begin
          Get_Next_Token;
          The_Type := Data.New_Signed_Integer_Type (Id     => Defining_Identifier,
                                                    Parent => Scope);
          Dummy := Simple_Expression ((Scope, The_Type));
          Get_Element (Lexical.Range_Delimiter);
          Dummy := Simple_Expression ((Scope, The_Type));
          Conditional_Aspect_Specification ((Scope, The_Type));
        end Signed_Integer_Type_Definition;


        -- modular_type_definition ::=
        --      mod static_expression
        --
        procedure Modular_Type_Definition is
        begin
          Get_Next_Token;
          The_Type := Data.New_Modular_Integer_Type (Id     => Defining_Identifier,
                                                     Parent => Scope);
          Dummy := Expression ((Scope, The_Type));
          Conditional_Aspect_Specification ((Scope, The_Type));
        end Modular_Type_Definition;


        -- real_range_specification ::=
        --      range static_simple_expression .. static_simple_expression
        --
        procedure Real_Range_Specification with Inline is
        begin
          Get_Next_Token;
          Dummy := Simple_Expression ((Scope, The_Type));
          Get_Element (Lexical.Range_Delimiter);
          Dummy := Simple_Expression ((Scope, The_Type));
          Conditional_Aspect_Specification ((Scope, The_Type));
        end Real_Range_Specification;


        -- fixed_point_definition ::=
        --      ordinary_fixed_point_definition
        --    | decimal_fixed_point_definition
        --
        --    ordinary_fixed_point_definition ::=
        --         delta static_expression  real_range_specification
        --
        --    decimal_fixed_point_definition ::=
        --         delta static_expression digits static_expression [real_range_specification]
        --
        procedure Fixed_Point_Definition is
        begin
          Get_Next_Token;
          The_Type := Data.New_Fixed_Point_Type (Id     => Defining_Identifier,
                                                 Parent => Scope);
          Dummy := Expression ((Scope, The_Type));
          case Token_Element is
          when Lexical.Is_Range => -- ordinary_fixed_point_definition
            Real_Range_Specification;
          when Lexical.Is_Digits => -- decimal_fixed_point_definition
            Get_Next_Token;
            Dummy := Expression ((Scope, The_Type));
            if Token_Element = Lexical.Is_Range then
              Real_Range_Specification;
            end if;
          when others =>
            Syntax_Error;
          end case;
          Conditional_Aspect_Specification ((Scope, The_Type));
        end Fixed_Point_Definition;


        -- floating_point_definition ::=
        --      digits static_expression [real_range_specification]
        --
        procedure Floating_Point_Definition is
        begin
          Get_Next_Token;
          The_Type := Data.New_Floating_Point_Type (Id     => Defining_Identifier,
                                                    Parent => Scope);
          Dummy := Expression ((Scope, The_Type));
          if Token_Element = Lexical.Is_Range then
            Real_Range_Specification;
          end if;
          Conditional_Aspect_Specification ((Scope, The_Type));
        end Floating_Point_Definition;


        -- record_definition ::=
        --      record component_list end record
        --    | null record
        --
        procedure Record_Definition (Is_Abstract : Boolean := False;
                                     Is_Tagged   : Boolean := False;
                                     Ancestor    : Data_Handle := null;
                                     Interfaces  : Data.List.Item := Data.List.Empty) is

          function Components return Data.List.Item is

            The_List : Data.List.Item;

            --  component_list ::=
            --        component_item {component_item}
            --     | {component_item} variant_part
            --     |  null ;
            --
            procedure Component_List is

              -- component_item ::=
              --      component_declaration
              --    | aspect_clause
              --
              procedure Component_Item is
              begin
                case Token_Element is
                when Lexical.Identifier =>
                  The_List.Append (Component_Declaration ((Scope, The_Type)));
                when Lexical.Is_For =>
                  Aspect_Clause (Scope);
                when others =>
                  Syntax_Error;
                end case;
              end Component_Item;

              -- variant_part ::=
              --       case discriminant_direct_name is
              --           variant
              --          {variant}
              --       end case ;
              --
              --    variant ::=
              --          when discrete_choice_list => component_list
              --
              procedure Variant_Part is

                Discriminant : constant Data_Handle := Data.Discriminant_Of (Next_Identifier, The_Type);

                At_End : Boolean := False;

              begin
                Get_Element (Lexical.Is_Is);
                loop
                  Get_Element (Lexical.Is_When);
                  At_End := Discrete_Choice_List ((Scope, Data.Object_Type_Of(Discriminant)));
                  Get_Element (Lexical.Association);
                  Component_List;
                  exit when Token_Element /= Lexical.Is_When;
                  if At_End then
                    Syntax_Error;
                  end if;
                end loop;
                Get_Element (Lexical.Is_End);
                Get_Element (Lexical.Is_Case);
                Get_Element (Lexical.Semicolon);
              end Variant_Part;

            begin
              case Token_Element is
              when Lexical.Identifier | Lexical.Is_For =>
                Component_Item;
                loop
                  case Token_Element is
                  when Lexical.Identifier | Lexical.Is_For =>
                    Component_Item;
                  when Lexical.Is_Pragma =>
                    Pragma_Call (Scope);
                  when Lexical.Is_Case =>
                    Variant_Part;
                    exit;
                  when others =>
                    exit;
                  end case;
                end loop;
              when Lexical.Is_Case =>
                Variant_Part;
              when Lexical.Is_Null =>
                Get_Next_Element (Lexical.Semicolon);
              when Lexical.Is_Pragma =>
                Pragma_Call (Scope);
              when others =>
                Syntax_Error;
              end case;
            end Component_List;

          begin
            Component_List;
            Get_Element (Lexical.Is_End);
            Get_Element (Lexical.Is_Record);
            return The_List;
          end Components;

        begin --Record_Definition
          Get_Next_Token;
          The_Type := Data.New_Record_Type (Id            => Defining_Identifier,
                                            Is_Abstract   => Is_Abstract,
                                            Is_Tagged     => Is_Tagged,
                                            Parent        => Scope,
                                            Ancestor      => Ancestor,
                                            Discriminants => The_Discriminants,
                                            Interfaces    => Interfaces);
          The_Actual_Record_Definition := Data.Record_Handle(The_Type);
          Data.Record_Handle(The_Type).Components := Components;
          The_Actual_Record_Definition := null;
          Data.Add_Iterator_Aspects (The_Type, Aspect_Specification ((Scope, The_Type)));
        end Record_Definition;


        procedure Null_Record_Definition  (Is_Abstract : Boolean := False;
                                           Is_Tagged   : Boolean := False;
                                           Ancestor    : Data_Handle := null;
                                           Interfaces  : Data.List.Item := Data.List.Empty) is
        begin
          The_Type := Data.New_Record_Type (Id            => Defining_Identifier,
                                            Is_Abstract   => Is_Abstract,
                                            Is_Tagged     => Is_Tagged,
                                            Parent        => Scope,
                                            Ancestor      => Ancestor,
                                            Discriminants => The_Discriminants,
                                            Interfaces    => Interfaces);
          Get_Next_Element (Lexical.Is_Record);
          Conditional_Aspect_Specification ((Scope, The_Type));
        end Null_Record_Definition;


        procedure Declare_Private_Type (Is_Tagged : Boolean := False) is
        begin
          if Scope.all in Data.Private_Block then
            Syntax_Error;
          end if;
          Get_Next_Token;
          The_Type := Data.New_Private_Type (Id            => Defining_Identifier,
                                             Is_Tagged     => Is_Tagged,
                                             Parent        => Scope,
                                             Discriminants => The_Discriminants);
          Data.Add_Iterator_Aspects (The_Type, Aspect_Specification ((Scope, The_Type)));
        end Declare_Private_Type;


        procedure Declare_Incomplete_Type is
        begin
          The_Type := Data.New_Incomplete_Type (Id     => Defining_Identifier,
                                                Parent => Scope);
        end Declare_Incomplete_Type;


        procedure Declare_Interface_Type is
          The_Interfaces : Data.List.Item;
        begin
          if Next_Element_Is (Lexical.Is_And) then
            The_Interfaces := Interface_List (Scope);
          end if;
          The_Type := Data.New_Interface_Type (Id         => Defining_Identifier,
                                               Parent     => Scope,
                                               Interfaces => The_Interfaces);
          Conditional_Aspect_Specification ((Scope, The_Type));
        end Declare_Interface_Type;


        procedure Derived_Type_Definition (Is_Abstract : Boolean := False);

        procedure Declare_Limited_Type (Is_Abstract : Boolean := False;
                                        Is_Tagged   : Boolean := False) is
        begin
          case Next_Token.Element is
          when Lexical.Is_Private =>
            Declare_Private_Type (Is_Tagged);
          when Lexical.Is_Record =>
            Record_Definition (Is_Abstract, Is_Tagged);
          when Lexical.Is_Null =>
            Null_Record_Definition (Is_Abstract, Is_Tagged);
          when Lexical.Is_Interface =>
            Declare_Interface_Type;
          when Lexical.Is_New =>
            Derived_Type_Definition (Is_Abstract => Is_Abstract);
          when others =>
            Syntax_Error;
          end case;
        end Declare_Limited_Type;


        procedure Declare_Tagged_Type (Is_Abstract : Boolean := False) is
        begin
          case Next_Token.Element is
          when Lexical.Is_Record =>
            Record_Definition (Is_Abstract, Is_Tagged => True);
          when Lexical.Is_Null =>
            Null_Record_Definition (Is_Abstract, Is_Tagged => True);
          when Lexical.Is_Limited =>
            Declare_Limited_Type (Is_Abstract, Is_Tagged => True);
          when Lexical.Is_Private =>
            Declare_Private_Type (Is_Tagged => True);
          when Lexical.Semicolon =>
            Declare_Incomplete_Type;
          when others =>
            Syntax_Error;
          end case;
        end Declare_Tagged_Type;


        procedure Access_Type_Definition is
        begin
          The_Type := Access_Type_Definition (Defining_Identifier, (Scope, null));
          Conditional_Aspect_Specification ((Scope, The_Type));
        end Access_Type_Definition;


        -- derived_type_definition ::=
        --      [abstract] [limited] new parent_subtype_indication [[and interface_list] record_extension_part]
        --
        --    record_extension_part ::=
        --         with record_definition
        --
        -- private_record_extension_part ::=
        --      [abstract] new ancestor_subtype_indication with private ;
        --
        procedure Derived_Type_Definition (Is_Abstract : Boolean := False) is
        begin
          Get_Next_Token;
          declare
            Ancestor       : constant Data_Handle := Subtype_Indication_Part ((Scope, null));
            The_Interfaces : Data.List.Item;
          begin
            --TEST----------------------------------------------------------------------------------
            --if Ancestor /= null then
            --  Write_Log ("Derived_Type - Ancestor : " & Image_Of (Ancestor.Location.all));
            --  Write_Log ("                   TYPE : " & Ada.Tags.External_Tag (Ancestor.all'tag));
            --end if;
            ----------------------------------------------------------------------------------------
            if Element_Is (Lexical.Is_And) then
              The_Interfaces := Interface_List (Scope);
            end if;
            if Element_Is (Lexical.Is_With) then
              case Token_Element is
              when Lexical.Is_Private =>
                Get_Next_Token;
                The_Type := Data.New_Private_Extension_Type (Id            => Defining_Identifier,
                                                             Is_Abstract   => Is_Abstract,
                                                             Parent        => Scope,
                                                             From_Type     => Ancestor,
                                                             Discriminants => The_Discriminants,
                                                             Interfaces    => The_Interfaces);
                Data.Add_Iterator_Aspects (The_Type, Aspect_Specification ((Scope, The_Type)));
              when Lexical.Is_Null =>
                Null_Record_Definition (Is_Abstract => Is_Abstract,
                                        Is_Tagged   => True,
                                        Ancestor    => Ancestor,
                                        Interfaces  => The_Interfaces);
              when Lexical.Is_Record =>
                Record_Definition (Is_Abstract => Is_Abstract,
                                   Is_Tagged   => True,
                                   Ancestor    => Ancestor,
                                   Interfaces  => The_Interfaces);
              when others =>
                The_Type := Data.New_Derived_Type (Id        => Defining_Identifier,
                                                   Parent    => Scope,
                                                   From_Type => Ancestor);
                Data.Add_Iterator_Aspects (The_Type, Aspects_Elements ((Scope, The_Type)));
              end case;
            else
              The_Type := Data.New_Derived_Type (Id        => Defining_Identifier,
                                                 Parent    => Scope,
                                                 From_Type => Ancestor);
            end if;
          end;
        end Derived_Type_Definition;


        procedure Declare_Abstract_Type is
        begin
          case Next_Token.Element is
          when Lexical.Is_Tagged =>
            Declare_Tagged_Type (Is_Abstract => True);
          when Lexical.Is_New =>
            Derived_Type_Definition (Is_Abstract => True);
          when others =>
            Syntax_Error;
          end case;
        end Declare_Abstract_Type;


        procedure Declare_Protected_Interface_Type is
        begin
          case Next_Token.Element is
          when Lexical.Is_Interface =>
            Declare_Interface_Type;
          when others =>
            Syntax_Error;
          end case;
        end Declare_Protected_Interface_Type;


        procedure Declare_Synchronized_Interface_Type is
        begin
          case Next_Token.Element is
          when Lexical.Is_Interface =>
            Declare_Interface_Type;
          when others =>
            Syntax_Error;
          end case;
        end Declare_Synchronized_Interface_Type;


        procedure Declare_Task_Interface_Type is
        begin
          case Next_Token.Element is
          when Lexical.Is_Interface =>
            Declare_Interface_Type;
          when others =>
            Syntax_Error;
          end case;
        end Declare_Task_Interface_Type;

      begin -- Type_Declaration
        The_Discriminants := Conditional_Discriminant_Part (Scope);
        if Token_Element = Lexical.Semicolon then
          Declare_Incomplete_Type;
        else
          Get_Element (Lexical.Is_Is);
          case Token_Element is
          when Lexical.Left_Parenthesis =>
            Enumeration_Type_Definition;
          when Lexical.Is_Range =>
            Signed_Integer_Type_Definition;
          when Lexical.Is_Mod =>
            Modular_Type_Definition;
          when Lexical.Is_Delta =>
            Fixed_Point_Definition;
          when Lexical.Is_Digits =>
            Floating_Point_Definition;
          when Lexical.Is_Array =>
            Array_Type_Definition;
          when Lexical.Is_Record =>
            Record_Definition;
          when Lexical.Is_Null =>
            Null_Record_Definition;
          when Lexical.Is_Not | Lexical.Is_Access =>
            Access_Type_Definition;
          when Lexical.Is_New =>
            Derived_Type_Definition;
          when Lexical.Is_Private =>
            Declare_Private_Type;
          when Lexical.Is_Abstract =>
            Declare_Abstract_Type;
          when Lexical.Is_Tagged =>
            Declare_Tagged_Type;
          when Lexical.Is_Limited =>
            Declare_Limited_Type;
          when Lexical.Is_Interface =>
            Declare_Interface_Type;
          when Lexical.Is_Protected =>
            Declare_Protected_Interface_Type;
          when Lexical.Is_Synchronized =>
            Declare_Synchronized_Interface_Type;
          when Lexical.Is_Task =>
            Declare_Task_Interface_Type;
          when others =>
            Syntax_Error;
          end case;
        end if;
        Get_Element (Lexical.Semicolon);
      end Type_Declaration;


      procedure Local_Package with Inline is
        Id : Identifier_Handle;
      begin
        if not (Scope.all in Data.Private_Block) and then Next_Element_Ahead_Is (Lexical.Is_Body) then
          Id := Next_Declaring_Identifier;
          Package_Body (Data.New_Package_Body (Id, Scope));
        else
          Id := Next_Declaring_Identifier;
          case Token_Element is
          when Lexical.Is_Is =>
            if Next_Element_Ahead_Is (Lexical.Is_New) then
              Package_Instantiation (Id, Scope);
            else
              Package_Specification (Data.New_Package_Specification (Style_Checked(Id), Scope));
            end if;
          when Lexical.Is_With =>
            Aspect_Specification ((Scope, null));
            Package_Specification (Data.New_Package_Specification (Style_Checked(Id), Scope));
          when Lexical.Is_Renames =>
            Package_Renaming (Id, Scope);
          when others =>
            Syntax_Error;
          end case;
        end if;
      end Local_Package;


      -- object_declaration ::=
      --     defining_identifier_list : [aliased] [constant]
      --       (subtype_indication | access_definition | array_type_definition)
      --       [ := expression] [aspect_specification] ;
      --   | single_task_declaration
      --   | single_protected_declaration
      --
      -- object_renaming_declaration ::=
      --      defining_identifier : [null_exclusion] subtype_mark renames object_name [aspect_specification] ;
      --   |  defining_identifier : access_definition renames object_name [aspect_specification] ;
      --
      -- exception_declaration ::=
      --      defining_identifier_list : exception ;
      --
      -- exception_renaming_declaration ::=
      --     defining_identifier : exception renames exception_name ;
      --
      -- number_declaration ::=
      --      defining_identifier_list : constant := static_expression ;
      --
      procedure Object_Declaration is

        Defining_Identifiers : constant Identifiers := Defining_Identifier_List;

        Is_Constant   : Boolean := False;
        Is_Class_Wide : Boolean := False;
        Has_Default   : Boolean := False;

        The_Subtype : Data_Handle;

      begin
        Get_Element (Lexical.Colon);
        case Token_Element is
        when Lexical.Is_Constant =>
          Get_Next_Token;
          Is_Constant := True;
        when Lexical.Is_Exception =>
          Get_Next_Token;
          if (Defining_Identifiers'length = 1) and then Element_Is (Lexical.Is_Renames) then
            Dummy := Name_Of (Scope);
          end if;
          Data.New_Exceptions (Names  => Defining_Identifiers,
                               Parent => Scope);
          Get_Element (Lexical.Semicolon);
          return;
        when Lexical.Is_Aliased =>
          Is_Constant := Next_Element_Is (Lexical.Is_Constant);
        when others =>
          null;
        end case;
        case Token_Element is
        when Lexical.Is_Array =>
          The_Subtype := Data.New_Array_Type (Id         => null,
                                              Parent     => Scope,
                                              Definition => Array_Type_Definition (Scope));
        when Lexical.Is_Not =>
          Get_Next_Element (Lexical.Is_Null);
          if Element_Is (Lexical.Is_Access) then
            The_Subtype := Access_Definition_Part (Scope);
          else
            The_Subtype := Subtype_Mark (Scope);
          end if;
        when Lexical.Is_Access =>
          Get_Next_Token;
          The_Subtype := Access_Definition_Part (Scope);
        when Lexical.Assignment =>
          if not Is_Constant then
            Syntax_Error;
          end if;
          Get_Next_Token;
          Data.New_Constants (Names         => Defining_Identifiers,
                              Subtype_Mark  => Data.Root_Type_Of (Expression ((Scope, null))),
                              Is_Class_Wide => Is_Class_Wide,
                              Has_Default   => Has_Default,
                              Parent        => Scope);
          Conditional_Aspect_Specification ((Scope, null), Defining_Identifiers);
          Get_Element (Lexical.Semicolon);
          return;
        when others =>
          The_Subtype := Subtype_Indication_Part ((Scope, null));
          Is_Class_Wide := Is_Class_Wide_Type;
        end case;
        case Token_Element is
        when Lexical.Is_Renames =>
          if Defining_Identifiers'length /= 1 then
            Syntax_Error;
          end if;
          Get_Next_Token;
          Dummy := Name_Of ((Scope, The_Subtype));
          Data.New_Objects (Names         => Defining_Identifiers,
                            Subtype_Mark  => The_Subtype,
                            Is_Class_Wide => Is_Class_Wide_Type,
                            Has_Default   => False,
                            Parent        => Scope);
          Conditional_Aspect_Specification ((Scope, The_Subtype), Defining_Identifiers);
          Get_Element (Lexical.Semicolon);
          return;
        when Lexical.Assignment =>
          Get_Next_Token;
          Has_Default := True;
          The_Target_Type := The_Subtype;
          Dummy := Expression ((Scope, The_Subtype));
          if The_Subtype = null then
            The_Subtype := Dummy;
          end if;
        when others =>
          null;
        end case;
        if Is_Constant then
          Data.New_Constants (Names         => Defining_Identifiers,
                              Subtype_Mark  => The_Subtype,
                              Is_Class_Wide => Is_Class_Wide,
                              Has_Default   => Has_Default,
                              Parent        => Scope);
        else
          Data.New_Objects (Names         => Defining_Identifiers,
                            Subtype_Mark  => The_Subtype,
                            Is_Class_Wide => Is_Class_Wide,
                            Has_Default   => Has_Default,
                            Parent        => Scope);
        end if;
        Conditional_Aspect_Specification ((Scope, The_Subtype), Defining_Identifiers);
        Get_Element (Lexical.Semicolon);
      end Object_Declaration;


      -- generic_declaration ::=
      --      generic_subprogram_declaration
      --    | generic_package_declaration
      --
      procedure Generic_Declaration is

        Parameters : constant Data.Formal_Block_Handle := Generic_Formal_Part (Scope);

        -- generic_subprogram_declaration ::=
        --      generic_formal_part subprogram_specification [aspect_specification] ;
        --
        procedure Generic_Subprogram (Is_Function : Boolean := False) is
          Id : constant Identifier_Handle := Next_Declaring_Identifier;
        begin
          if Token_Element = Lexical.Is_Renames then
            declare
              Renamed_Unit : constant Data.Unit_Handle := Next_Unit_Of (Unit);
              use type Data.Unit_Handle;
            begin
              Data.New_Generic_Subprogram_Renaming
                (Id                 => Id,
                 Parent             => Scope,
                 Renamed_Subprogram => Renamed_Unit);
              if Renamed_Unit /= null then
                The_Actual_Identifier.Data := Data_Handle(Renamed_Unit);
                Renamed_Unit.Is_Used := True;
              end if;
            end;
          else
            Data.New_Generic_Subprogram_Declaration
              (Id                 => Id,
               Parent             => Scope,
               Generic_Parameters => Parameters,
               Profile            => Subprogram_Profile (Data.Unit_Handle(Parameters), Is_Function));
          end if;
          Conditional_Aspect_Specification ((Scope, null));
          Get_Element (Lexical.Semicolon);
        end Generic_Subprogram;


        -- generic_package_declaration ::=
        --      generic_formal_part package_specification ;
        --
        -- generic_package_Renaming ::=
        --      generic package defining_program_unit_name renames generic_package_name ;
        --
        procedure Generic_Package is
          Id : constant Identifier_Handle := Next_Identifier;
        begin
          case Token_Element is
          when Lexical.Is_Is =>
            Package_Specification (Data.New_Generic_Package_Declaration (Id                 => Style_Checked (Id),
                                                                         Parent             => Scope,
                                                                         Generic_Parameters => Parameters));
          when Lexical.Is_With =>
            Aspect_Specification ((Scope, null));
            Package_Specification (Data.New_Generic_Package_Declaration (Id                 => Style_Checked (Id),
                                                                         Parent             => Scope,
                                                                         Generic_Parameters => Parameters));
          when Lexical.Is_Renames =>
            if Parameters.Last_Position /= 0 then
              Syntax_Error;
            end if;
            Package_Renaming (Id, Scope);
          when others =>
            Syntax_Error;
          end case;
        end Generic_Package;

      begin -- Generic_Declaration
        case Token_Element is
        when Lexical.Is_Function =>
          Generic_Subprogram (Is_Function => True);
        when Lexical.Is_Package =>
          Generic_Package;
        when Lexical.Is_Procedure =>
          Generic_Subprogram;
        when others =>
          Syntax_Error;
        end case;
      end Generic_Declaration;


      -- single_protected_declaration ::=
      --      protected defining_identifier [aspect_specification] is [new interface_list with] protected_definition;
      --
      procedure Single_Protected_Declaration is
        Id                : constant Identifier_Handle := Declaring_Identifier;
        Unused_Interfaces : Data.List.Item;
      begin
        Conditional_Aspect_Specification ((Scope, null));
        if Element_Is (Lexical.Is_Is) then
          if Element_Is (Lexical.Is_New) then
            Unused_Interfaces := Interface_List (Scope);
            Get_Element (Lexical.Is_With);
          end if;
          Protected_Definition (Data.New_Protected_Declaration (Id, Scope));
        else
          Not_Implemented ("Incomplete protected definition");
        end if;
        Get_Element (Lexical.Semicolon);
      end Single_Protected_Declaration;


      -- protected_type_declaration ::=
      --      protected type defining_identifier [known_discriminant_part] [aspect_specification]
      --         is [new interface_list with] protected_definition ;
      --
      procedure Protected_Type_Declaration is
        Id                : constant Identifier_Handle := Next_Declaring_Identifier;
        The_Discriminants : Data.List.Item;
        Unused_Interfaces : Data.List.Item;
        The_Type          : Data.Unit_Handle;
      begin
        if Token_Element = Lexical.Left_Parenthesis then
          The_Discriminants := Known_Discriminant_Part (Scope);
        end if;
        The_Type := Data.New_Protected_Type (Id, Scope, The_Discriminants);
        Conditional_Aspect_Specification ((The_Type, null));
        Get_Element (Lexical.Is_Is);
        if Element_Is (Lexical.Is_New) then
          Unused_Interfaces := Interface_List (Scope);
          Get_Element (Lexical.Is_With);
        end if;
        Protected_Definition (The_Type);
        Get_Element (Lexical.Semicolon);
      end Protected_Type_Declaration;


      -- single_task_declaration ::=
      --      task defining_identifier [is task_definition] ;
      --
      procedure Single_Task_Declaration is
        Id : constant Identifier_Handle := Declaring_Identifier;
      begin
        Conditional_Aspect_Specification ((Scope, null));
        if Element_Is (Lexical.Is_Is) then
          Task_Definition (Data.New_Task_Declaration (Id, Scope));
        else
          Dummy := Data_Handle(Data.New_Task_Declaration (Id, Scope));
        end if;
        Get_Element (Lexical.Semicolon);
      end Single_Task_Declaration;


      -- task_type_declaration ::=
      --      task type defining_identifier [known_discriminant_part]  [aspect_specification] [is task_definition] ;
      --
      procedure Task_Type_Declaration is
        Id                : constant Identifier_Handle := Next_Declaring_Identifier;
        The_Discriminants : Data.List.Item;
      begin
        if Token_Element = Lexical.Left_Parenthesis then
          The_Discriminants := Known_Discriminant_Part (Scope);
        end if;
        Conditional_Aspect_Specification ((Scope, null));
        case Token_Element is
        when Lexical.Is_Is =>
          Get_Next_Token;
          Task_Definition (Data.New_Task_Type (Id, Scope, The_Discriminants));
          Get_Element (Lexical.Semicolon);
        when Lexical.Semicolon =>
          Get_Next_Token;
          Dummy := Data_Handle(Data.New_Task_Type (Id, Scope, The_Discriminants));
        when others =>
          Syntax_Error;
        end case;
      end Task_Type_Declaration;

    begin -- Declarative_Part
      loop
        if Has_Overriding_Indicator then
          case Token_Element is
          when Lexical.Is_Function =>
            Subprogram (Is_Function => True, Is_Overriding_Body => True);
          when Lexical.Is_Procedure =>
            Subprogram (Is_Overriding_Body => True);
          when others =>
            Syntax_Error;
          end case;
        else
          case Token_Element is
          when Lexical.Is_Type =>
            Type_Declaration;
          when Lexical.Is_Subtype =>
            Subtype_Declaration;
          when Lexical.Identifier =>
            Object_Declaration;
          when Lexical.Is_Function =>
            Subprogram (Is_Function => True);
          when Lexical.Is_Package =>
            Local_Package;
          when Lexical.Is_Procedure =>
            Subprogram;
          when Lexical.Is_Protected =>
            case Next_Token.Element is
            when Lexical.Is_Body =>
              if Is_Basic then
                Syntax_Error;
              end if;
              Protected_Body (Scope);
            when Lexical.Is_Type =>
              Protected_Type_Declaration;
            when others =>
              Single_Protected_Declaration;
            end case;
          when Lexical.Is_Task =>
            case Next_Token.Element is
            when Lexical.Is_Body =>
              if Is_Basic then
                Syntax_Error;
              end if;
              Task_Body (Scope);
            when Lexical.Is_Type =>
              Task_Type_Declaration;
            when others =>
              Single_Task_Declaration;
            end case;
          when Lexical.Is_Generic =>
            Generic_Declaration;
          when Lexical.Is_For =>
            Aspect_Clause (Scope);
          when Lexical.Is_Use =>
            Use_Clause (Scope);
          when Lexical.Is_Pragma =>
            Pragma_Call (Scope);
          when others =>
            exit;
          end case;
        end if;
      end loop;
    end Declarative_Part;


    -- declare_expression ::=
    --      declare {declare_item} begin expression
    --
    --    declare_item :== object_declaration | object_renaming_declaration
    --
    function Declare_Expression (Within : Data.Context) return Data_Handle is
      Scope : constant Data.Unit_Handle := Data.New_Block (null, Within.Scope);
    begin
      Get_Next_Token;
      Declarative_Part (Scope);
      Get_Element (Lexical.Is_Begin);
      return Expression ((Scope, Within.Sub_Type));
    end Declare_Expression;


    -- assignment_statement ::=
    --      variable_name := expression ;
    --
    -- entry_call_statement ::=
    --      entry_name [actual_parameter_part] ;
    --
    -- procedure_call_statement ::=
    --      name [actual_parameter_part] ;
    --
    -- code_statement ::=
    --      qualified_expression ;
    --
    procedure Assignment_Call_Or_Code_Statement (Scope : Data.Unit_Handle) is

      Actual_Token : constant Lexical_Handle := The_Token;
      Sub_Type     : constant Data_Handle := Name_Of (Within            => (Scope, null),
                                                      Procedure_Allowed => True);
      The_Result_Type : Data_Handle;

    begin
      if Element_Is (Lexical.Assignment) then
        The_Target_Type := Sub_Type;
        The_Result_Type := Expression ((Scope, Sub_Type));
        if The_Result_Type /= null and then not Data."=" (The_Result_Type, Sub_Type) then
          The_Token := Actual_Token;
          The_Result_Type := Name_Of (Within            => (Scope, The_Result_Type),
                                      Procedure_Allowed => True);
          if Element_Is (Lexical.Assignment) then
            The_Target_Type := Sub_Type;
            The_Result_Type := Expression ((Scope, Sub_Type));
          end if;
        end if;
      end if;
      Get_Element (Lexical.Semicolon);
    end Assignment_Call_Or_Code_Statement;


    -- block_statement ::=
    --      [block_statement_identifier : ]
    --        [declare
    --           declarative_part]
    --        begin
    --
    -- loop_statement ::=
    --      [loop_statement_identifier : ]
    --        [iteration_scheme] loop
    --
    --    iteration_scheme ::=
    --         while condition
    --       | for for_loop_condition
    --
    procedure Block_Or_Loop_Statement (Scope : Data.Unit_Handle) is
      Id : constant Identifier_Handle := Declaring_Identifier;
    begin
      Get_Element (Lexical.Colon);
      case Token_Element is
      when Lexical.Is_Begin | Lexical.Is_Declare =>
        Block_Statement (Scope, Id);
      when Lexical.Is_For =>
        For_Loop_Statement (Scope, Id);
      when Lexical.Is_Loop =>
        Loop_Statement (Data.New_Block (Id, Scope), Has_Id => True);
      when Lexical.Is_While =>
        While_Loop_Statement (Data.New_Block (Id, Scope), Has_Id => True);
      when others =>
        Syntax_Error;
      end case;
    end Block_Or_Loop_Statement;


    -- [block_identifier] ;
    --
    procedure Check_Block_End (Scope  : Data.Unit_Handle;
                               Has_Id : Boolean) is
    begin
      if Has_Id then
        Check_Designator (Scope);
      end if;
      Get_Element (Lexical.Semicolon);
    end Check_Block_End;


    -- abort_statement ::=
    --      abort task_name {, task_name};
    --
    procedure Abort_Statement (Scope : Data.Unit_Handle) is
    begin
      loop
        Dummy := Next_Name_Of (Scope);
        exit when Token_Element /= Lexical.Comma;
      end loop;
      Get_Element (Lexical.Semicolon);
    end Abort_Statement;


    -- accept_statement ::=
    --      accept entry_direct_name [ ( entry_index ) ] parameter_profile [do
    --        handled_sequence_of_statements
    --      end [entry_identifier]];
    --
    --    entry_index ::=
    --         expression
    --
    procedure Accept_Statement (Scope : Data.Unit_Handle) is

      Id                : constant Identifier_Handle := Next_Identifier;
      Entry_Declaration : constant Data.Unit_Handle := Data.Entry_Of (Id, Scope);

      Self        : Data.Unit_Handle;
      The_Profile : Data.Subprogram_Profile;

      use type Data.Unit_Handle;

    begin
      if Entry_Declaration /= null then
        declare
          Index_Type : constant Data_Handle := Data.Index_Type_Of (Data_Handle(Entry_Declaration));
        begin
          if Index_Type /= null then
            Get_Element (Lexical.Left_Parenthesis);
            Dummy := Expression ((Scope, Index_Type));
            Get_Element (Lexical.Right_Parenthesis);
          end if;
        end;
      end if;
      if Token_Element = Lexical.Left_Parenthesis then
        The_Profile := Data.New_Profile (Formal_Part (Scope));
      else
        The_Profile := Data.New_Profile;
      end if;
      Data.Associate_Entry (Id, Entry_Declaration, The_Profile); -- %%% family entry overload not supported
      if Element_Is (Lexical.Is_Do) then
        Self := Data.New_Accept_Declaration (Id, Scope, The_Profile);
        Handled_Sequence_Of_Statements (Self);
        Check_End (Self);
      else
        Get_Element (Lexical.Semicolon);
      end if;
    end Accept_Statement;


    -- block_statement ::=
    --      [block_statement_identifier : ]
    --        [declare
    --           declarative_part]
    --        begin
    --          handled_sequence_of_statements
    --        end [block_identifier] ;
    --
    procedure Block_Statement (Parent : Data.Unit_Handle;
                               Id     : Identifier_Handle := null) is
      Scope : constant Data.Unit_Handle := Data.New_Block (Id, Parent);
    begin
      Checker.Define_Block_Label_Usage (Scope, The_Style);
      if Element_Is (Lexical.Is_Declare) then
        Declarative_Part (Scope);
      end if;
      Get_Element (Lexical.Is_Begin);
      Handled_Sequence_Of_Statements (Scope);
      Get_Element (Lexical.Is_End);
      Check_Block_End (Scope, not Is_Null (Id));
    end Block_Statement;


    -- case_statement ::=
    --      case expression is
    --        case_statement_alternative {case_statement_alternative}
    --      end case;
    --
    --    case_statement_alternative ::=
    --          when discrete_choice_list => sequence_of_statements
    --
    procedure Case_Statement (Scope : Data.Unit_Handle) is
      The_Selector : Data_Handle;
      Is_Last      : Boolean := False;
    begin
      Get_Next_Token;
      The_Selector := Expression ((Scope, null));
      Get_Element (Lexical.Is_Is);
      Get_Element (Lexical.Is_When);
      loop
        Is_Last := Discrete_Choice_List ((Scope, The_Selector));
        Get_Element (Lexical.Association);
        Sequence_Of_Statements (Scope);
        exit when Is_Last or else not Element_Is (Lexical.Is_When);
      end loop;
      Get_Element (Lexical.Is_End);
      Get_Element (Lexical.Is_Case);
      Get_Element (Lexical.Semicolon);
    end Case_Statement;


    -- delay_statement ::=
    --      delay_until_statement
    --    | delay_relative_statement
    --
    --    delay_until_statement ::=
    --         delay until delay_expression ;
    --
    --    delay_relative_statement ::=
    --         delay delay_expression ;
    --
    procedure Delay_Statement (Scope : Data.Unit_Handle) is
    begin
      if not Next_Element_Is (Lexical.Is_Until) then
        Style_Error_If_Real_Time (Error.Relative_Delay_Not_Allowed);
      end if;
      declare
        Error_Token : constant Lexical_Handle := The_Token;
        Timer_Type  : constant Data_Handle := Expression ((Scope, null));
      begin
        if Timer_Type /= null then
          if Timer_Type.all in Data.Declaration_Type'class then
            declare
              Parent : constant Data.Unit_Handle := Data.Declaration_Handle(Timer_Type).Parent;
              use type Data.Unit_Handle;
            begin
              if Parent /= null then
                declare
                  Timer_Unit : constant String := Parent.Location.Image_Of;
                begin
                  if Timer_Unit = "Calendar" then
                    Style_Error_If_Real_Time (Error.Calendar_Time_Not_Allowed, Error_Token);
                  end if;
                end;
              end if;
            end;
          end if;
        end if;
      end;
      Get_Element (Lexical.Semicolon);
    end Delay_Statement;


    -- exit_statement ::=
    --      exit [loop_name] [when condition] ;
    --
    procedure Exit_Statement (Scope : Data.Unit_Handle) is
    begin
      Get_Next_Token;
      if Token_Element = Lexical.Identifier then
        Dummy := Name_Of (Scope);
      end if;
      if Element_Is (Lexical.Is_When) then
        Condition (Scope);
      end if;
      Get_Element (Lexical.Semicolon);
    end Exit_Statement;


    -- goto_statement ::=
    --      goto label_name ;
    --
    procedure Goto_Statement (Scope : Data.Unit_Handle) is
    begin
      Style_Error_If_Restricted (Error.Goto_Not_Allowed);
      if Next_Name_Of (Scope) = null then
        The_Actual_Identifier.Data := Data.Predefined_Name;
      end if;
      Get_Element (Lexical.Semicolon);
    end Goto_Statement;


    -- if_statement ::=
    --      if condition then
    --        sequence_of_statements
    --      [{elsif condition then
    --        sequence_of_statements}]
    --      [else
    --        sequence_of_statements]
    --      end if ;
    --
    procedure If_Statement (Scope : Data.Unit_Handle) is
    begin
      Get_Next_Token;
      loop
        Condition (Scope);
        Get_Element (Lexical.Is_Then);
        Sequence_Of_Statements (Scope);
        case Token_Element is
        when Lexical.Is_Elsif =>
          Get_Next_Token;
        when Lexical.Is_Else =>
          Get_Next_Token;
          Sequence_Of_Statements (Scope);
          exit;
        when others =>
          exit;
        end case;
      end loop;
      Get_Element (Lexical.Is_End);
      Get_Element (Lexical.Is_If);
      Get_Element (Lexical.Semicolon);
    end If_Statement;


    -- label ::=
    --      <<label_statement_identifier>>
    --
    procedure Label (Scope : Data.Unit_Handle) is
    begin
      Style_Error_If_Restricted (Error.Goto_Label_Not_Allowed);
      Data.New_Label (Next_Declaring_Identifier, Scope);
      Get_Element (Lexical.End_Label);
    end Label;


    -- loop
    --   sequence_of_statements
    -- end loop [loop_identifier] ;
    --
    procedure Loop_Statement (Scope  : Data.Unit_Handle;
                              Has_Id : Boolean := False) is
    begin
      Get_Element (Lexical.Is_Loop);
      Sequence_Of_Statements (Scope);
      Get_Element (Lexical.Is_End);
      Get_Element (Lexical.Is_Loop);
      Check_Block_End (Scope, Has_Id);
    end Loop_Statement;


    -- for_loop_condition := loop_parameter_specification | iterator_specification
    --
    --    loop_parameter_specification ::=
    --         defining_identifier in [reverse] discrete_subtype_definition [iterator_filter]
    --
    --    iterator_specification ::=
    --         defining_identifier in [reverse] iterator_name
    --       | defining_identifier [ : subtype_indication] of [reverse] iterable_name
    --
    procedure For_Loop_Condition (Scope : Data.Unit_Handle) is
      Defining_Identifier : constant Identifier_Handle := Declaring_Identifier;
      The_Type            : Data_Handle;
      Iterable_Type       : Data_Handle;
    begin
      case Token_Element is
      when Lexical.Is_In | Lexical.Is_Of | Lexical.Colon =>
        if Element_Is (Lexical.Colon) then
          The_Type := Subtype_Indication_Part ((Scope, null));
        end if;
        if Token_Element = Lexical.Is_Of then
          Get_Next_Conditional (Lexical.Is_Reverse);
          Iterable_Type := Data.Iterable_Type_Of (Name_Of (Scope));
          if The_Type = null then
            The_Type := Iterable_Type;
          end if;
        else
          Get_Next_Conditional (Lexical.Is_Reverse);
          The_Type := Data.Range_Type_Of (Discrete_Subtype_Definition (Scope));
        end if;
        Data.New_Object (Id            => Defining_Identifier,
                         Subtype_Mark  => The_Type,
                         Is_Class_Wide => False,
                         Has_Default   => True,
                         Parent        => Scope);
        if Element_Is (Lexical.Is_When) then
          Condition (Scope);
        end if;
      when others =>
        Syntax_Error;
      end case;
    end For_Loop_Condition;


    -- for for_loop_condition loop
    --   sequence_of_statements
    -- end loop [loop_identifier] ;
    --
    procedure For_Loop_Statement (Parent : Data.Unit_Handle;
                                  Id     : Identifier_Handle := null) is

      Scope : constant Data.Unit_Handle := Data.New_Block (Id, Parent);
    begin
      Get_Next_Token;
      For_Loop_Condition (Scope);
      Loop_Statement (Scope, not Is_Null (Id));
    end For_Loop_Statement;


    -- while condition loop
    --   sequence_of_statements
    -- end loop [loop_identifier] ;
    --
    procedure While_Loop_Statement (Scope  : Data.Unit_Handle;
                                    Has_Id : Boolean := False) is
    begin
      Get_Next_Token;
      Condition (Scope);
      Loop_Statement (Scope, Has_Id);
    end While_Loop_Statement;


    -- raise_expression ::=
    --      raise exception_name [with string_expression]
    --
    function Raise_Expression (Within : Data.Context) return Data_Handle is
      The_Type : Data_Handle;
    begin
      The_Type := Name_Of (Within);
      if Element_Is (Lexical.Is_With) then
        Dummy := Expression (Within);
      end if;
      return The_Type;
    end Raise_Expression;


    -- raise_statement ::=
    --      raise [exception_name [with string_expression]] ;
    --
    procedure Raise_Statement (Scope : Data.Unit_Handle) is
    begin
      Get_Next_Token;
      if Token_Element /= Lexical.Semicolon then
        Dummy := Name_Of (Scope);
        if Element_Is (Lexical.Is_With) then
          Dummy := Expression ((Scope, null));
        end if;
      end if;
      Get_Element (Lexical.Semicolon);
    end Raise_Statement;


    -- iterated_element_association ::=
    --      for loop_parameter_specification [use key_expression] => expression
    --    | for iterator_specification [use key_expression] => expression
    --
    function Iterated_Element_Association (Within : Data.Context) return Data_Handle is
      The_Predicate : Data_Handle;
    begin
      Get_Element (Lexical.Is_For);
      For_Loop_Condition (Within.Scope);
      if Element_Is (Lexical.Is_Use) then
        Dummy := Expression (Within);
      end if;
      Get_Element (Lexical.Association);
      The_Predicate := Expression (Within);
      return The_Predicate;
    end Iterated_Element_Association;


    -- reduction_attribute_reference ::=
    --      value_sequence ' reduction_attribute_designator
    --    | prefix ' reduction_attribute_designator
    --
    -- value_Sequence ::=
    --      [ [parallel [(chunk_specification)] [aspect_specification]] iterated_element_association ]
    --
    -- chunk_specification ::=
    --      integer_simple_expression
    --    | defining_identifier in descrete_subtype_definition
    function Reduction_Attribute_Reference (Within : Data.Context) return Data_Handle is
      The_Predicate : Data_Handle;
      use type Lexical.Attribute_Id;
    begin
      The_Predicate := Iterated_Element_Association (Within);
      Get_Element (Lexical.Right_Bracket);
      if Token_Element = Lexical.Apostrophe then
        if Next_Element_Ahead_Is (Lexical.Attribute) then
          if Attribute_Designator (Within) /= Lexical.Is_Reduce then
            Syntax_Error;
          end if;
        end if;
      end if;
      return The_Predicate;
    end Reduction_Attribute_Reference;


    -- requeue_statement ::=
    --      requeue entry_name [with abort] ;
    --
    procedure Requeue_Statement (Scope : Data.Unit_Handle) is
    begin
      Get_Next_Token;
      Dummy := Name_Of (Within            => (Scope, null),
                        Procedure_Allowed => True);
      if Element_Is (Lexical.Is_With) then
        Get_Element (Lexical.Is_Abort);
      end if;
      Get_Element (Lexical.Semicolon);
    end Requeue_Statement;


    -- return_statement ::=
    --      simple_return_statement
    --    | extended_return_statement
    --
    --    simple_return_statement ::=
    --         return [expression] ;
    --
    --    extended_return_statement ::=
    --         return extended_return_object_declaration [do handled_sequence_of_statements end return] ;
    --
    --       extended_return_object_declaration ::=
    --            defining_identifier : [aliased] [constant] return_subtype_indication [ := expression]
    --
    procedure Return_Statement (Scope : Data.Unit_Handle) is
      Profile : constant Data.Subprogram_Profile := Data.Profile_Of (Data.Declaration_Handle(Scope));
    begin
      Get_Next_Token;
      if Token_Element /= Lexical.Semicolon then
        if Profile.Result_Type = null then
          Dummy := Expression ((Scope, null));
        else
          declare
            Saved_Token : constant Lexical_Handle := The_Token;
            Result_Type : Data_Handle;
          begin
            Result_Type := Expression ((Scope, Profile.Result_Type));
            if Result_Type = null then -- if no result (tagged type ?)
              The_Token := Saved_Token;
              Result_Type := Expression ((Scope, null)); -- don't use profile
            end if;
          end;
        end if;
        if Element_Is (Lexical.Colon) then
          declare
            Object_Id    : constant Identifier_Handle := The_Actual_Identifier;
            Return_Block : constant Data.Unit_Handle := Data.New_Block (null, Scope);
            Is_Constant  : Boolean := False;
            Has_Default  : Boolean := False;
            The_Subtype  : Data_Handle;
          begin
            case Token_Element is
            when Lexical.Is_Constant =>
              Get_Next_Token;
              Is_Constant := True;
            when Lexical.Is_Aliased =>
              Is_Constant := Next_Element_Is (Lexical.Is_Constant);
            when others =>
              null;
            end case;
            if Element_Is (Lexical.Is_Not) then
              Get_Element (Lexical.Is_Null);
              Check (Lexical.Is_Access);
            end if;
            if Element_Is (Lexical.Is_Access) then
              The_Subtype := Access_Definition_Part (Return_Block);
            else
              The_Subtype := Subtype_Indication_Part ((Return_Block, null));
            end if;
            if Is_Constant then
              Get_Element (Lexical.Assignment);
              The_Target_Type := The_Subtype;
              Data.New_Constants (Names         => [Object_Id],
                                  Subtype_Mark  => Data.Root_Type_Of (Expression ((Return_Block, The_Subtype))),
                                  Is_Class_Wide => Is_Class_Wide_Type,
                                  Has_Default   => True,
                                  Parent        => Return_Block);
            else
              if Element_Is (Lexical.Assignment) then
                The_Target_Type := The_Subtype;
                The_Subtype := Expression ((Return_Block, The_Subtype));
                Has_Default := True;
              end if;
              Data.New_Object (Id            => Object_Id,
                               Subtype_Mark  => The_Subtype,
                               Is_Class_Wide => Is_Class_Wide_Type,
                               Has_Default   => Has_Default,
                               Parent        => Return_Block);
            end if;
            if Element_Is (Lexical.Is_Do) then
              Handled_Sequence_Of_Statements (Return_Block);
              Get_Element (Lexical.Is_End);
              Get_Element (Lexical.Is_Return);
            end if;
          end;
        end if;
      end if;
      Get_Element (Lexical.Semicolon);
    end Return_Statement;


    -- select_statement ::=
    --      selective_accept
    --    | timed_entry_call
    --    | conditional_entry_call
    --    | asynchronous_select
    --
    --    selective_accept ::=
    --         select [guard]
    --           select_alternative
    --         { or [guard]
    --           select_alternative }
    --         [ else
    --           sequence_of_statements ]
    --         end select ;
    --
    --    timed_entry_call ::=
    --         select
    --           entry_call_alternative
    --         or
    --           delay_alternative
    --         end select ;
    --
    --       entry_call_alternative ::=
    --            entry_call_statement [sequence_of_statements]
    --
    --    conditional_entry_call ::=
    --         select
    --           entry_call_alternative
    --         else
    --           sequence_of_statements
    --         end select ;
    --
    --    asynchronous_select ::=
    --         select
    --           triggering_alternative
    --         then abort
    --           abortable_part
    --         end select ;
    --
    --       triggering_alternative ::=
    --            triggering_statement [sequence_of_statements]
    --
    --          triggering_statement ::=
    --                entry_call_statement
    --             | delay_statement
    --
    --       abortable_part ::=
    --            sequence_of_statements
    --
    procedure Select_Statement (Scope : Data.Unit_Handle) is

      -- select_alternative ::=
      --      accept_alternative
      --    | delay_alternative
      --    | terminate_alternative
      --
      --    accept_alternative ::=
      --         accept_statement [sequence_of_statements]
      --
      --    delay_alternative ::=
      --         delay_statement [sequence_of_statements]
      --
      --    terminate_alternative ::=
      --         terminate ;
      --
      procedure Select_Alternative is
      begin
        case Token_Element is
        when Lexical.Is_Accept =>
          Accept_Statement (Scope);
          Sequence_Of_Statements (Scope, Is_Conditional => True);
        when Lexical.Is_Delay =>
          Delay_Statement (Scope);
          Sequence_Of_Statements (Scope, Is_Conditional => True);
        when Lexical.Is_Terminate =>
          Get_Next_Element (Lexical.Semicolon);
        when others =>
          Syntax_Error;
        end case;
      end Select_Alternative;

      -- guard ::=
      --      when condition =>
      --
      procedure Guard is
      begin
        Get_Next_Token;
        Condition (Scope);
        Get_Element (Lexical.Association);
      end Guard;

      -- { or [guard]
      --   select_alternative }
      -- [ else
      --   sequence_of_statements ]
      procedure Select_Or_Else is
      begin
        while Element_Is (Lexical.Is_Or) loop
          if Token_Element = Lexical.Is_When then
            Guard;
          end if;
          Select_Alternative;
        end loop;
        if Element_Is (Lexical.Is_Else) then
          Sequence_Of_Statements (Scope);
        end if;
      end Select_Or_Else;


      procedure Select_Accept_Or_Else is
      begin
        Accept_Statement (Scope);
        Sequence_Of_Statements (Scope, Is_Conditional => True);
        Select_Or_Else;
      end Select_Accept_Or_Else;

      --  entry_call_alternative
      -- else
      --   sequence_of_statements
      -- end select ;
      --
      --   entry_call_alternative
      -- or
      --   delay_alternative
      -- end select ;
      --
      --    entry_call_alternative ::=
      --         entry_call_statement [sequence_of_statements]
      --
      --    delay_alternative ::=
      --         delay_statement [sequence_of_statements]
      --
      --   entry_call
      -- then abort
      --   sequence_of_statements
      -- end select ;
      --
      procedure Selective_Entry_Call is
      begin
        Assignment_Call_Or_Code_Statement (Scope);
        case Token_Element is
        when Lexical.Is_Else =>
          Get_Next_Token;
          if Token_Element = Lexical.Is_Delay then
            Style_Error_If_Restricted (Error.Suspicious_Form_Of_Entry_Call);
          end if;
          Sequence_Of_Statements (Scope);
        when Lexical.Is_Or =>
          Get_Next_Token;
          Delay_Statement (Scope);
          Sequence_Of_Statements (Scope, Is_Conditional => True);
        when Lexical.Is_Then =>
          Get_Next_Element (Lexical.Is_Abort);
          Sequence_Of_Statements (Scope);
        when others =>
          Sequence_Of_Statements (Scope);
          case Token_Element is
          when Lexical.Is_Else =>
            Get_Next_Token;
            Sequence_Of_Statements (Scope);
          when Lexical.Is_Or =>
            Get_Next_Token;
            Delay_Statement (Scope);
            Sequence_Of_Statements (Scope, Is_Conditional => True);
          when others =>
            Syntax_Error;
          end case;
        end case;
      end Selective_Entry_Call;

    begin -- Select_Statement
      case Next_Token.Element is
      when Lexical.Is_Accept =>
        Select_Accept_Or_Else;
      when Lexical.Is_Delay => -- delay_alternative in selective accept or delay_statement in asynchronous_select
        Delay_Statement (Scope);
        case Token_Element is
        when Lexical.Is_Or => -- selective accept
          Select_Or_Else;
        when Lexical.Is_Then => -- asynchronous_select
          Get_Next_Element (Lexical.Is_Abort);
          Sequence_Of_Statements (Scope);
        when others =>
          Sequence_Of_Statements (Scope, Is_Conditional => True);
          Select_Or_Else;
        end case;
      when Lexical.Identifier => -- entry_call in conditional_entry_call or asynchronous_select
        Selective_Entry_Call;
      when Lexical.Is_Terminate =>
        Get_Next_Element (Lexical.Semicolon);
        Select_Or_Else;
      when Lexical.Is_When =>
        Guard;
        Select_Alternative;
        Select_Or_Else;
      when others =>
        Syntax_Error;
      end case;
      Get_Element (Lexical.Is_End);
      Get_Element (Lexical.Is_Select);
      Get_Element (Lexical.Semicolon);
    end Select_Statement;


    -- sequence_of_statements ::=
    --      statement {statement}
    --
    --    statement ::=
    --          {label} simple_statement
    --        | {label} compound_statement
    --
    --        simple_statement ::=
    --              null_Statement
    --            | assignment_statement
    --            | exit_statement
    --            | goto_statement
    --            | procedure_call_statement
    --            | return_statement
    --            | entry_call_statement
    --            | requeue_statement
    --            | delay_statement
    --            | abort_statement
    --            | raise_statement
    --            | code_statement
    --
    --        compound_statement ::=
    --              if_statement
    --            | case_statement
    --            | loop_statement
    --            | block_statement
    --            | accept_statement
    --            | select_statement
    --
    procedure Sequence_Of_Statements (Scope          : Data.Unit_Handle;
                                      Is_Conditional : Boolean := False) is
      Is_Ok : Boolean := Is_Conditional;
    begin
      loop
        case Token_Element is
        when Lexical.Is_Abort =>
          Abort_Statement (Scope);
        when Lexical.Is_Accept =>
          Accept_Statement (Scope);
        when Lexical.Is_Null =>
          The_Statement_Count := The_Statement_Count - 1;
          Get_Next_Element (Lexical.Semicolon);
        when Lexical.Start_Label =>
          Label (Scope);
        when Lexical.Identifier =>
          if Element_Ahead_Is (Lexical.Colon) then
            Block_Or_Loop_Statement (Scope);
          else
            Assignment_Call_Or_Code_Statement (Scope);
          end if;
        when Lexical.Is_Exit =>
          Exit_Statement (Scope);
        when Lexical.Is_Goto =>
          Goto_Statement (Scope);
        when Lexical.Is_Return =>
          if The_Statement_Count = 0 and Had_Raise_Statement then
            The_Statement_Count := The_Statement_Count - 1;
            Had_Raise_Statement := False;
          end if;
          Return_Statement (Scope);
        when Lexical.Is_Requeue =>
          Requeue_Statement (Scope);
        when Lexical.Is_Delay =>
          Delay_Statement (Scope);
        when Lexical.Is_Raise =>
          The_Statement_Count := The_Statement_Count - 1;
          Raise_Statement (Scope);
          Had_Raise_Statement := True;
        when Lexical.Is_If =>
          If_Statement (Scope);
        when Lexical.Is_Case =>
          Case_Statement (Scope);
        when Lexical.Is_Loop =>
          Loop_Statement (Scope);
        when Lexical.Is_For =>
          For_Loop_Statement (Scope);
        when Lexical.Is_While =>
          While_Loop_Statement (Scope);
        when Lexical.Is_Begin | Lexical.Is_Declare =>
          Block_Statement (Scope);
        when Lexical.Is_Select =>
          Select_Statement (Scope);
        when Lexical.Is_Pragma =>
          The_Statement_Count := The_Statement_Count - 1;
          Pragma_Call (Scope);
        when others =>
          if Is_Ok then
            exit;
          else
            Syntax_Error;
          end if;
        end case;
        The_Statement_Count := The_Statement_Count + 1;
        Is_Ok := True;
      end loop;
    end Sequence_Of_Statements;


    -- handled_sequence_of_statements ::=
    --        sequence_of_statements
    --      [exception
    --        exception_handler
    --      {exception_handler}]
    --
    --    exception_handler ::=
    --         when [choice_parameter_specification : ] exception_choice { | exception_choice} =>
    --           sequence_of_statements
    --
    --       choice_parameter_specification ::=
    --            defining_identifier
    --
    --       exception_choice ::=
    --            exception_name
    --          | others
    --
    procedure Handled_Sequence_Of_Statements (Scope           : Data.Unit_Handle;
                                              Is_In_Task_Body : Boolean := False) is
      The_Scope : Data.Unit_Handle := Scope;
    begin
      Sequence_Of_Statements (Scope);
      if Element_Is (Lexical.Is_Exception) then
        if Token_Element = Lexical.Is_Pragma then
          Pragma_Call (Scope);
        end if;
        Get_Element (Lexical.Is_When);
        loop
          if Element_Ahead_Is (Lexical.Colon) then
            The_Scope := Data.New_Exception_Block (Declaring_Identifier, Scope);
            Get_Element (Lexical.Colon);
          end if;
          if Element_Is (Lexical.Is_Others) then
            Get_Element (Lexical.Association);
            Sequence_Of_Statements (The_Scope);
            exit;
          else
            loop
              Dummy := Name_Of (Scope);
              exit when not Element_Is (Lexical.Vertical_Line);
            end loop;
            Get_Element (Lexical.Association);
            Sequence_Of_Statements (The_Scope);
            exit when not Element_Is (Lexical.Is_When);
          end if;
        end loop;
      elsif Is_In_Task_Body then
        Style_Error_If_Restricted (Error.Missing_Exception_Handler);
      end if;
    end Handled_Sequence_Of_Statements;


    -- use_package_clause ::=
    --      use package_name { , package_name} ;
    --
    procedure Use_Package_Clause (Scope      : Data.Unit_Handle;
                                  In_Context : Boolean) is

      function Used_Unit return Data_Handle is
      begin
        if In_Context then
          return Name_Of (null);
        else
          return Name_Of (Scope);
        end if;
      end Used_Unit;

    begin
      Data.Use_Package (Used_Unit, Scope);
      while Token_Element = Lexical.Comma loop
        Get_Next_Token;
        Data.Use_Package (Used_Unit, Scope);
      end loop;
      Get_Element (Lexical.Semicolon);
    end Use_Package_Clause;


    -- use_type_clause ::=
    --      use [all] type subtype_mark { , subtype_mark} ;
    --
    procedure Use_Type_Clause (Scope   : Data.Unit_Handle;
                               Use_All : Boolean := False) is
    begin
      Data.Use_Type (Subtype_Mark (Scope), Scope, Use_All);
      while Element_Is (Lexical.Comma) loop
        Data.Use_Type (Subtype_Mark (Scope), Scope, Use_All);
      end loop;
      Get_Element (Lexical.Semicolon);
    end Use_Type_Clause;


    -- use_clause ::=
    --      use_package_clause
    --    | use_type_clause
    --
    procedure Use_Clause (Scope      : Data.Unit_Handle;
                          In_Context : Boolean := False) is
    begin
      case Next_Token.Element is
      when Lexical.Identifier =>
        Use_Package_Clause (Scope, In_Context);
      when Lexical.Is_Type =>
        Get_Next_Token;
        Use_Type_Clause (Scope);
      when Lexical.Is_All =>
        Get_Next_Element (Lexical.Is_Type);
        Use_Type_Clause (Scope, Use_All => True);
      when others =>
        Syntax_Error;
      end case;
    end Use_Clause;


    --  with_clause ::=
    --       with library_unit_name { , library_unit_name} ;
    --
    procedure With_Clause (Is_Private : Boolean := False) is
    begin
      Data.Import (Next_Unit_Name, Unit, Is_Private);
      while Token_Element = Lexical.Comma loop
        Data.Import (Next_Unit_Name, Unit, Is_Private);
      end loop;
      Get_Element (Lexical.Semicolon);
    end With_Clause;

    -- compilation_unit ::=
    --      context_clause library_item
    --    | context_clause subunit
    --
    --    context_clause ::=
    --         {context_item}
    --
    --       context_item ::=
    --            with_clause
    --          | use_clause
    --
    --    library_item ::=
    --         [private] library_unit_declaration
    --       | library_unit_body
    --       | [private] library_unit_renaming_declaration
    --
    --       library_unit_declaration ::=
    --            subprogram_declaration
    --          | package_declaration
    --          | generic_declaration
    --          | generic_instantiation
    --
    --          subprogram_declaration ::=
    --               subprogram_specification;
    --
    --             subprogram_specification ::=
    --                  procedure defining_program_unit_name  parameter_profile
    --                | function defining_designator  parameter_and_result_profile
    --
    --          package_declaration ::=
    --               package_specification;
    --
    --          generic_instantiation ::=
    --               package defining_program_unit_name is new generic_package_name [generic_actual_part] ;
    --             | procedure defining_program_unit_name is new generic_procedure_name [generic_actual_part] ;
    --             | function defining_designator is new generic_function_name [generic_actual_part] ;
    --
    --       library_unit_body ::=
    --            subprogram_body
    --          | package_body
    --
    --       library_unit_renaming_declaration ::=
    --            package_renaming_declaration
    --          | generic_renaming_declaration
    --          | subprogram_renaming_declaration
    --
    --          package_renaming_declaration ::=
    --              package defining_program_unit_name renames package_name ;
    --
    --          generic_renaming_declaration ::=
    --               generic package defining_program_unit_name renames generic_package_name ;
    --             | generic procedure defining_program_unit_name renames generic_procedure_name ;
    --             | generic function defining_program_unit_name renames generic_function_name ;
    --
    --          subprogram_renaming_declaration ::=
    --              subprogram_specification renames callable_entity_name ;
    --
    --    subunit ::=
    --         separate (parent_unit_name) proper_body
    --
    procedure Compilation_Unit is

      use type Data.Unit_Handle;

      Is_Private : Boolean := False;


      procedure Library_Subprogram (Is_Function : Boolean := False) is
        Unused : constant Identifiers := Next_Unit_Name;
      begin
        if Token_Element = Lexical.Is_Is and then Next_Element_Ahead_Is (Lexical.Is_New) then
          --TEST---------------------------------------------------------------------------
          --Write_Log ("Library Subprogram Instantiation " & Image_Of (Unit.Location.all));
          ---------------------------------------------------------------------------------
          declare
            Generic_Unit : constant Data.Unit_Handle   := Next_Unit_Of (Unit);
            Subprogram   : constant Identifier_Handle  := The_Actual_Identifier;
            Actual_Part  : constant Data.List.Elements := Conditional_Generic_Actual_Part (Generic_Unit, Unit);
          begin
            Data.Add_Library_Subprogram_Instantiation
              (Self    => Unit,
               Profile => Data.Actual_Profile_For (Generic_Unit => Generic_Unit,
                                                   Subprogram   => Subprogram,
                                                   Actual_Part  => Actual_Part));
          end;
          Get_Element (Lexical.Semicolon);
        else
          declare
            Profile : Data.Subprogram_Profile := Subprogram_Profile (Unit, Is_Function);
          begin
            case Token_Element is
            when Lexical.Is_Is =>
              Data.Add_Library_Subprogram_Body (Unit, Profile);
              Subprogram_Body (Unit);
            when Lexical.Semicolon =>
              Data.Add_Library_Subprogram_Declaration (Unit, Profile);
              Get_Next_Token;
            when Lexical.Is_Renames =>
              declare
                Renamed_Unit : constant Data.Unit_Handle := Next_Unit_Of (Unit);
              begin
                if Renamed_Unit /= null then
                  The_Actual_Identifier.Data := Data_Handle(Renamed_Unit);
                  Renamed_Unit.Is_Used := True;
                  Profile := Data.Used (Profile);
                  Data.Add_Library_Subprogram_Renaming (Unit, Renamed_Unit);
                end if;
              end;
              Get_Element (Lexical.Semicolon);
            when Lexical.Is_With =>
              Aspect_Specification ((Unit, null));
              Get_Element (Lexical.Semicolon);
            when others =>
              Syntax_Error;
            end case;
          end;
        end if;
      end Library_Subprogram;


      procedure Library_Package with Inline is
      begin
        if not Is_Private and then Next_Element_Ahead_Is (Lexical.Is_Body) then
          Get_Next_Unit_Name;
          Data.Add_Library_Package_Body (Unit);
          Package_Body (Unit);
        else
          Get_Next_Unit_Name;
          case Token_Element is
          when Lexical.Is_Is =>
            if Next_Element_Ahead_Is (Lexical.Is_New) then
              declare
                The_Package       : Data.Unit_Handle;
                The_Instantiation : Data.Instantiation_Handle;
              begin
                Get_From (Unit, The_Package, The_Instantiation);
                Data.Add_Library_Package_Instantiation (Self                 => Unit,
                                                        Generic_Package      => The_Package,
                                                        Parent_Instantiation => The_Instantiation,
                                                        Actual_Part          => Conditional_Generic_Actual_Part
                                                                                  (The_Package, Unit));
                Get_Element (Lexical.Semicolon);
              end;
            else
              Data.Add_Library_Package_Specification (Unit);
              Package_Specification (Unit);
            end if;
          when Lexical.Is_With =>
            Aspect_Specification ((Unit, null));
            Data.Add_Library_Package_Specification (Unit);
            Package_Specification (Unit);
          when Lexical.Is_Renames =>
            Data.Add_Library_Package_Renaming (Unit, Next_Name_Of (Unit));
            Get_Element (Lexical.Semicolon);
          when others =>
            Syntax_Error;
          end case;
        end if;
      end Library_Package;


      procedure Generic_Subprogram (Is_Function        : Boolean := False;
                                    Generic_Parameters : Data.Formal_Block_Handle) is
      begin
        Get_Next_Unit_Name;
        case Token_Element is
        when Lexical.Is_Renames =>
          declare
            Renamed_Unit : constant Data.Unit_Handle := Next_Unit_Of (Unit);
          begin
            if Renamed_Unit /= null then
              The_Actual_Identifier.Data := Data_Handle(Renamed_Unit);
              Renamed_Unit.Is_Used := True;
              Data.Add_Generic_Library_Subprogram_Renaming (Unit, Renamed_Unit);
            end if;
          end;
        when others =>
          Data.Add_Generic_Library_Subprogram_Declaration
                                  (Self               => Unit,
                                   Generic_Parameters => Generic_Parameters,
                                   Profile            => Subprogram_Profile (Data.Unit_Handle(Generic_Parameters),
                                                                             Is_Function));
        end case;
        Conditional_Aspect_Specification ((Unit, null));
        Get_Element (Lexical.Semicolon);
      end Generic_Subprogram;


      procedure Generic_Package (Generic_Parameters : Data.Formal_Block_Handle) is
      begin
        Get_Next_Unit_Name;
        case Token_Element is
        when Lexical.Is_Is =>
          Data.Add_Generic_Library_Package_Declaration (Unit, Generic_Parameters);
          Package_Specification (Unit);
        when Lexical.Is_With =>
          Aspect_Specification ((Unit, null));
          Data.Add_Generic_Library_Package_Declaration (Unit, Generic_Parameters);
          Package_Specification (Unit);
        when Lexical.Is_Renames =>
          declare
            Renamed_Unit : constant Data.Unit_Handle := Next_Unit_Of (Unit);
          begin
            if Renamed_Unit /= null then
              Data.Add_Generic_Library_Package_Renaming (Unit, Renamed_Unit);
            end if;
          end;
          Get_Element (Lexical.Semicolon);
        when others =>
          Syntax_Error;
        end case;
      end Generic_Package;


      procedure Library_Generic with Inline is

        Parameters : constant Data.Formal_Block_Handle := Generic_Formal_Part (Unit);

      begin
        case Token_Element is
        when Lexical.Is_Function =>
          Generic_Subprogram (Is_Function        => True,
                              Generic_Parameters => Parameters);
        when Lexical.Is_Package =>
          Generic_Package (Parameters);
        when Lexical.Is_Procedure =>
          Generic_Subprogram (Is_Function        => False,
                              Generic_Parameters => Parameters);
        when others =>
          Syntax_Error;
        end case;
      end Library_Generic;


      procedure Subunit is
      begin
        Get_Next_Element (Lexical.Left_Parenthesis);
        declare
          Unused : constant Identifiers := Unit_Name;
        begin
          Get_Element (Lexical.Right_Parenthesis);
          case Token_Element is
          when Lexical.Is_Procedure =>
            Library_Subprogram;
          when Lexical.Is_Function =>
            Library_Subprogram (Is_Function => True);
          when Lexical.Is_Package =>
            Get_Next_Element (Lexical.Is_Body);
            Get_Next_Token;
            Data.Add_Library_Package_Body (Unit);
            Package_Body (Unit);
          when Lexical.Is_Task =>
            Not_Implemented ("Task Subunit");
          when others =>
            Syntax_Error;
          end case;
        end;
      end Subunit;

    begin -- Compilation_Unit
      The_Style := Next_Style (Resource.Tokens.First, Special_Comment_Detected);
      Build_Parameters_Defined := False;
      The_Token := Lexical_After (Resource.Tokens.First);
      Data.Add_Unit (Unit);
      Unused_Id := Style_Checked (Unit.Location);
      loop
        case Token_Element is
        when Lexical.Is_With =>
          With_Clause;
        when Lexical.Is_Use =>
          Use_Clause (Unit, In_Context => True);
        when Lexical.Is_Limited =>
          case Next_Token.Element is
          when Lexical.Is_Private =>
            case Next_Token.Element is
            when Lexical.Is_With =>
              With_Clause (Is_Private => True);
            when others =>
              Syntax_Error;
            end case;
          when Lexical.Is_With =>
            With_Clause (Is_Private => False);
          when others =>
            Syntax_Error;
          end case;
        when Lexical.Is_Private =>
          Is_Private := True;
          case Next_Token.Element is
          when Lexical.Is_Procedure =>
            Library_Subprogram;
            exit;
          when Lexical.Is_Function =>
            Library_Subprogram (Is_Function => True);
            exit;
          when Lexical.Is_Package =>
            Library_Package;
            exit;
          when Lexical.Is_Generic =>
            Library_Generic;
            exit;
          when Lexical.Is_With =>
            With_Clause (Is_Private => True);
          when others =>
            Syntax_Error;
          end case;
        when Lexical.Is_Procedure =>
          Library_Subprogram;
          exit;
        when Lexical.Is_Function =>
          Library_Subprogram (Is_Function => True);
          exit;
        when Lexical.Is_Package =>
          Library_Package;
          exit;
        when Lexical.Is_Generic =>
          Library_Generic;
          exit;
        when Lexical.Is_Separate =>
          Subunit;
          exit;
        when Lexical.Is_Pragma =>
          Pragma_Call (Unit);
        when others =>
          Syntax_Error;
        end case;
      end loop;
      while not End_Of_File and then Token_Element = Lexical.Is_Pragma loop
        Pragma_Call (Unit);
      end loop;
      if Is_In_Standard then
        Data.Add_Standard (Unit);
      end if;
    exception
    when Data.Unknown_Specification =>
      Report_Error (Error.Unknown_Specification, Lexical_Handle(Unit.Location));
    end Compilation_Unit;

  ----------------------------------------------------------------------------------------------------------------------

  begin
    Compilation_Unit;
    if not End_Of_File then
      Report_Error (Error.End_Of_File_Expected, The_Token); -- only one compilation unit per file
    end if;
  exception
  when Reported_Error =>
    null;
  when Occurrence: others =>
    Log.Write ("!!! PARSER EXCEPTION", Occurrence);
  end Process;

end Ada_95.Token.Parser;
