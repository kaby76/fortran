grammar Fortran;


 program : program_unit
                                   ( program_unit )*  ;
 program_unit : main_program
              | external_subprogram
              | module
              | submodule
              | block_data ;
 main_program : ( program_stmt )?
                                   ( specification_part )?
                                   ( execution_part )?
                                   ( internal_subprogram_part )?
                   end_program_stmt ;
 external_subprogram : function_subprogram
              | subroutine_subprogram ;
 function_subprogram : function_stmt
                                   ( specification_part )?
                                   ( execution_part )?
                                   ( internal_subprogram_part )?
                   end_function_stmt ;
 subroutine_subprogram : subroutine_stmt
                                   ( specification_part )?
                                   ( execution_part )?
                                   ( internal_subprogram_part )?
                   end_subroutine_stmt ;
 module : module_stmt
                                   ( specification_part )?
                                   ( module_subprogram_part )?
                   end_module_stmt ;
 submodule : submodule_stmt
                                   ( specification_part )?
                                   ( module_subprogram_part )?
                   end_submodule_stmt ;
 block_data : block_data_stmt
                   ( specification_part )?
                      end_block_data_stmt ;
 specification_part : ( use_stmt )* 
                                   ( import_stmt )* 
                                   ( implicit_part )?
                                   ( declaration_construct )*  ;
 implicit_part : ( implicit_part_stmt )* 
                   implicit_stmt ;
 implicit_part_stmt : implicit_stmt
              | parameter_stmt
              | format_stmt
              | entry_stmt ;
 declaration_construct : specification_construct
              | data_stmt
              | format_stmt
              | entry_stmt
              | stmt_function_stmt ;
 specification_construct : derived_type_def
              | enum_def
              | enumeration_type_def
              | generic_stmt
              | interface_block
              | parameter_stmt
              | procedure_declaration_stmt
              | other_specification_stmt
              | type_declaration_stmt ;
 execution_part : executable_construct
                                   ( execution_part_construct )*  ;
 execution_part_construct : executable_construct
              | format_stmt
              | entry_stmt
              | data_stmt ;
 internal_subprogram_part : contains_stmt
                                   ( internal_subprogram )*  ;
 internal_subprogram : function_subprogram
              | subroutine_subprogram ;
 module_subprogram_part : contains_stmt
                                   ( module_subprogram )*  ;
 module_subprogram : function_subprogram
              | subroutine_subprogram
              | separate_module_subprogram ;
 separate_module_subprogram : mp_subprogram_stmt
                   ( specification_part )?
                   ( execution_part )?
                   ( internal_subprogram_part )?
                   end_mp_subprogram_stmt ;
 other_specification_stmt : access_stmt
              | allocatable_stmt
              | asynchronous_stmt
              | bind_stmt
              | codimension_stmt
              | contiguous_stmt
              | dimension_stmt
              | external_stmt
              | intent_stmt
              | intrinsic_stmt
              | namelist_stmt
              | optional_stmt
              | pointer_stmt
              | protected_stmt
              | save_stmt
              | target_stmt
              | volatile_stmt
              | value_stmt
              | common_stmt
              | equivalence_stmt ;
 executable_construct : action_stmt
              | associate_construct
              | block_construct
              | case_construct
              | change_team_construct
              | critical_construct
              | do_construct
              | if_construct
              | select_rank_construct
              | select_type_construct
              | where_construct
              | forall_construct ;
 action_stmt : allocate_stmt
              | assignment_stmt
              | backspace_stmt
              | call_stmt
              | close_stmt
              | continue_stmt
              | cycle_stmt
              | deallocate_stmt
              | endfile_stmt
              | error_stop_stmt
              | event_post_stmt
              | event_wait_stmt
              | exit_stmt
              | fail_image_stmt
              | flush_stmt
              | form_team_stmt
              | goto_stmt
              | if_stmt
              | inquire_stmt
              | lock_stmt
              | notify_wait_stmt
              | nullify_stmt
              | open_stmt
              | pointer_assignment_stmt
              | print_stmt
              | read_stmt
              | return_stmt
              | rewind_stmt
              | stop_stmt
              | sync_all_stmt
              | sync_images_stmt
              | sync_memory_stmt
              | sync_team_stmt
              | unlock_stmt
              | wait_stmt
              | where_stmt
              | write_stmt
              | computed_goto_stmt
              | forall_stmt ;
 keyword : name ;
 alphanumeric_character : letter
              | digit
              | underscore ;
 underscore : '_' ;
 name : letter ( alphanumeric_character )*  ;
 constant : literal_constant
              | named_constant ;
 literal_constant : int_literal_constant
              | real_literal_constant
              | complex_literal_constant
              | logical_literal_constant
              | char_literal_constant
              | boz_literal_constant ;
 named_constant : name ;
 int_constant : constant ;
 intrinsic_operator : power_op
              | mult_op
              | add_op
              | concat_op
              | rel_op
              | not_op
              | and_op
              | or_op
              | equiv_op ;
 power_op : '**' ;
 mult_op : '*'
              | '/' ;
 add_op : '+'
              | '-' ;
 concat_op : ; //
 rel_op : '.EQ.'
              | '.NE.'
              | '.LT.'
              | '.LE.'
              | '.GT.'
              | '.GE.'
              | '=='
              | '/='
              | '<'
              | '<='
              | '>'
              | '>=' ;
 not_op : '.NOT.' ;
 and_op : '.AND.' ;
 or_op : '.OR.' ;
 equiv_op : '.EQV.'
              | '.NEQV.' ;
 defined_operator : defined_unary_op
              | defined_binary_op
              | extended_intrinsic_op ;
 defined_unary_op : '.' letter ( letter )*  '.' ;
 defined_binary_op : '.' letter ( letter )*  '.' ;
 extended_intrinsic_op : intrinsic_operator ;
 label : digit ( digit ( digit ( digit ( digit )? )? )? )? ;
 type_param_value : scalar_int_expr
              | '*'
              | ':' ;
 type_spec : intrinsic_type_spec
              | derived_type_spec
              | enum_type_spec
              | enumeration_type_spec ;
 declaration_type_spec : intrinsic_type_spec
              | 'TYPE' '(' intrinsic_type_spec ')'
              | 'TYPE' '(' derived_type_spec ')'
              | 'TYPE' '(' enum_type_spec ')'
              | 'TYPE' '(' enumeration_type_spec ')'
              | 'CLASS' '(' derived_type_spec ')'
              | 'CLASS' '(' '*' ')'
              | 'TYPE' '(' '*' ')'
              | 'TYPEOF' '(' data_ref ')'
              | 'CLASSOF' '(' data_ref ')' ;
 intrinsic_type_spec : integer_type_spec
              | 'REAL' ( kind_selector )?
              | 'DOUBLE' 'PRECISION'
              | 'COMPLEX' ( kind_selector )?
              | 'CHARACTER' ( char_selector )?
              | 'LOGICAL' ( kind_selector )? ;
 integer_type_spec : 'INTEGER' ( kind_selector )? ;
 kind_selector : '(' ( 'KIND' '=' )? scalar_int_constant_expr ')' ;
 signed_int_literal_constant : ( sign )? int_literal_constant ;
 int_literal_constant : digit_string ( '_' kind_param )? ;
 kind_param : digit_string
              | scalar_int_constant_name ;
 signed_digit_string : ( sign )? digit_string ;
 digit_string : digit ( digit )*  ;
 sign : '+'
              | '-' ;
 signed_real_literal_constant : ( sign )? real_literal_constant ;
 real_literal_constant : significand ( exponent_letter exponent )? ( '_' kind_param )?
              | digit_string exponent_letter exponent ( '_' kind_param )? ;
 significand : digit_string '.' ( digit_string )?
              | '.' digit_string ;
 exponent_letter : 'E'
              | 'D' ;
 exponent : signed_digit_string ;
 complex_literal_constant : '(' real_part ',' imag_part ')' ;
 real_part : signed_int_literal_constant
              | signed_real_literal_constant
              | named_constant ;
 imag_part : signed_int_literal_constant
              | signed_real_literal_constant
              | named_constant ;
 char_selector : length_selector
              | '(' 'LEN' '=' type_param_value ','                   
'KIND' '=' scalar_int_constant_expr ')'
              | '(' type_param_value ','
                   ( 'KIND' '=' )? scalar_int_constant_expr ')'
              | '(' 'KIND' '=' scalar_int_constant_expr
                   ( ',' 'LEN' '='type_param_value )? ')' ;
 length_selector : '(' ( 'LEN' '=' )? type_param_value ')'
              | '*' char_length ( ',' )? ;
 char_length : '(' type_param_value ')'
              | int_literal_constant ;
char_literal_constant :
	( kind_param '_' )? SQUOTE_REP_CHAR
	| ( kind_param '_' )? DQUOTE_REP_CHAR ;

 logical_literal_constant : '.TRUE.' ( '_' kind_param )?
              | '.FALSE.' ( '_' kind_param )? ;
 derived_type_def : derived_type_stmt
                   ( type_param_def_stmt )* 
                   ( private_or_sequence )* 
                   ( component_part )?
                   ( type_bound_procedure_part )?
                   end_type_stmt ;
 derived_type_stmt : 'TYPE' ( ( ',' type_attr_spec_list )? '::' )? type_name
                   ( '(' type_param_name_list ')' )? ;
 type_attr_spec : 'ABSTRACT'
              | access_spec
              | 'BIND' '(C)'
              | 'EXTENDS' '(' parent_type_name ')' ;
 private_or_sequence : private_components_stmt
              | sequence_stmt ;
 end_type_stmt : 'END' 'TYPE' ( type_name )? ;
 sequence_stmt : 'SEQUENCE' ;
 type_param_def_stmt : integer_type_spec',' type_param_attr_spec '::'
                  type_param_decl_list ;
 type_param_decl : type_param_name ( '=' scalar_int_constant_expr )? ;
 type_param_attr_spec : 'KIND'
              | 'LEN' ;
 component_part : ( component_def_stmt )*  ;
 component_def_stmt : data_component_def_stmt
              | proc_component_def_stmt ;
 data_component_def_stmt : declaration_type_spec ( ( ',' component_attr_spec_list )? '::' )?
                  component_decl_list ;
 component_attr_spec : access_spec
              | 'ALLOCATABLE'
              | 'CODIMENSION' lbracket coarray_spec rbracket
              | 'CONTIGUOUS'
              | 'DIMENSION' '(' component_array_spec ')'
              | 'POINTER' ;
 component_decl : component_name ( '(' component_array_spec ')' )?
                   ( lbracket coarray_spec rbracket )?
                   ( '*' char_length )? ( component_initialization )? ;
 component_array_spec : explicit_shape_spec_list
              | deferred_shape_spec_list ;
 proc_component_def_stmt : 'PROCEDURE' '(' ( proc_interface )? ')' ','
                  proc_component_attr_spec_list '::' proc_decl_list ;
 proc_component_attr_spec : access_spec
              | 'NOPASS'
              | 'PASS' ( '('arg_name')' )?
              | 'POINTER' ;
 component_initialization : '=' constant_expr
              | '=>' null_init
              | '=>' initial_data_target ;
 initial_data_target : designator ;
 private_components_stmt : 'PRIVATE' ;
 type_bound_procedure_part : contains_stmt
                   ( binding_private_stmt )?
                   ( type_bound_proc_binding )*  ;
 binding_private_stmt : 'PRIVATE' ;
 type_bound_proc_binding : type_bound_procedure_stmt
              | type_bound_generic_stmt
              | final_procedure_stmt ;
 type_bound_procedure_stmt : 'PROCEDURE' ( ( ',' binding_attr_list )? '::' )? type_bound_proc_decl_list
              | 'PROCEDURE' '('interface_name'),' binding_attr_list '::' binding_name_list ;
 type_bound_proc_decl : binding_name ( '=>' procedure_name )? ;
 type_bound_generic_stmt : 'GENERIC' ( ',' access_spec )? '::' generic_spec '=>' binding_name_list ;
 binding_attr : access_spec
              | 'DEFERRED'
              | 'NON_OVERRIDABLE'
              | 'NOPASS'
              | 'PASS' ( '('arg_name')' )? ;
 final_procedure_stmt : 'FINAL' ( '::' )? final_subroutine_name_list ;
 derived_type_spec : type_name ( '(' type_param_spec_list ')' )? ;
 type_param_spec : ( keyword '=' )? type_param_value ;
 structure_constructor : derived_type_spec '(' ( component_spec_list )? ')' ;
 component_spec : ( keyword '=' )? component_data_source ;
 component_data_source : expr
              | data_target
              | proc_target ;
 enum_def : enum_def_stmt
                   enumerator_def_stmt
                   ( enumerator_def_stmt )* 
                   end_enum_stmt ;
 enum_def_stmt : 'ENUM,' 'BIND(C)' ( '::' enum_type_name )? ;
 enumerator_def_stmt : 'ENUMERATOR' ( '::' )? enumerator_list ;
 enumerator : named_constant ( '=' scalar_int_constant_expr )? ;
 end_enum_stmt : 'END' 'ENUM' ;
 enum_type_spec : enum_type_name ;
 enum_constructor : enum_type_spec '(' scalar_expr ')' ;
 enumeration_type_def : enumeration_type_stmt
                   enumeration_enumerator_stmt
                   ( enumeration_enumerator_stmt )*
                   end_enumeration_type_stmt ;
 enumeration_type_stmt : 'ENUMERATION' 'TYPE' ( ( ',' access_spec )? '::' )? enumeration_type_name ;
 enumeration_enumerator_stmt : 'ENUMERATOR' ( '::' )? enumerator_name_list ;
 end_enumeration_type_stmt : 'END' 'ENUMERATION' 'TYPE' ( enumeration_type_name )? ;
 enumeration_type_spec : enumeration_type_name ;
 enumeration_constructor : enumeration_type_spec '(' scalar_int_expr ')' ;
boz_literal_constant : BINARY_CONSTANT | OCTAL_CONSTANT | HEX_CONSTANT ;
 hex_digit : digit
              | 'A'
              | 'B'
              | 'C'
              | 'D'
              | 'E'
              | 'F' ;
 array_constructor : '(/' ac_spec '/)'
              | lbracket ac_spec rbracket ;
 ac_spec : type_spec '::'
              | (type_spec '::')? ac_value_list ;
 lbracket : '[' ;
 rbracket : ']' ;
 ac_value : expr
              | ac_implied_do ;
 ac_implied_do : '(' ac_value_list ',' ac_implied_do_control ')' ;
 ac_implied_do_control : ( integer_type_spec '::' )? ac_do_variable '=' scalar_int_expr ','
                  scalar_int_expr ( ',' scalar_int_expr )? ;
 ac_do_variable : do_variable ;
 type_declaration_stmt : declaration_type_spec ( ( ',' attr_spec )*  '::' )? entity_decl_list ;
 attr_spec : access_spec
              | 'ALLOCATABLE'
              | 'ASYNCHRONOUS'
              | 'CODIMENSION' lbracket coarray_spec rbracket
              | 'CONTIGUOUS'
              | 'DIMENSION' '(' array_spec ')'
              | 'EXTERNAL'
              | 'INTENT' '(' intent_spec ')'
              | 'INTRINSIC'
              | language_binding_spec
              | 'OPTIONAL'
              | 'PARAMETER'
              | 'POINTER'
              | 'PROTECTED'
              | rank_clause
              | 'SAVE'
              | 'TARGET'
              | 'VALUE'
              | 'VOLATILE' ;
 entity_decl : object_name ( '(' array_spec ')' )?
                   ( lbracket coarray_spec rbracket )?
                   ( '*' char_length )? ( initialization )?
              | function_name ( '*' char_length )? ;
 object_name : name ;
 initialization : '=' constant_expr
              | '=>' null_init
              | '=>' initial_data_target ;
 null_init : function_reference ;
 access_spec : 'PUBLIC'
              | 'PRIVATE' ;
 language_binding_spec : 'BIND' '(C' ( ',' 'NAME' '=' scalar_default_char_constant_expr )?')' ;
 coarray_spec : deferred_coshape_spec_list
              | explicit_coshape_spec ;
 deferred_coshape_spec : ':' ;
 explicit_coshape_spec : ( ( lower_cobound ':' )? upper_cobound',' )*
                   ( lower_cobound ':' )? '*' ;
 lower_cobound : specification_expr ;
 upper_cobound : specification_expr ;
 array_spec : explicit_shape_spec_list
              | explicit_shape_bounds_spec
              | assumed_shape_spec_list
              | assumed_shape_bounds_spec
              | deferred_shape_spec_list
              | assumed_size_spec
              | implied_shape_spec
              | implied_shape_or_assumed_size_spec
              | assumed_rank_spec ;
 explicit_shape_spec : ( lower_bound ':' )? upper_bound ;
 lower_bound : specification_expr ;
 upper_bound : specification_expr ;
 explicit_shape_bounds_spec : ( explicit_bounds_expr ':' )? explicit_bounds_expr
              | lower_bound ':' explicit_bounds_expr
              | explicit_bounds_expr ':' upper_bound ;
 explicit_bounds_expr : int_expr ;
 assumed_shape_spec : ( lower_bound )? ':' ;
 assumed_shape_bounds_spec : explicit_bounds_expr ':' ;
 deferred_shape_spec : ':' ;
 assumed_implied_spec : ( lower_bound ':' )? '*' ;
 assumed_size_spec : explicit_shape_spec_list',' assumed_implied_spec ;
 implied_shape_or_assumed_size_spec : assumed_implied_spec ;
 implied_shape_spec : assumed_implied_spec',' assumed_implied_spec_list ;
 assumed_rank_spec : '..' ;
 intent_spec : 'IN'
              | 'OUT'
              | 'INOUT' ;
 rank_clause : 'RANK' '(' scalar_int_constant_expr ')' ;
 access_stmt : access_spec ( ( '::' )? access_id_list )? ;
 access_id : access_name
              | generic_spec ;
 allocatable_stmt : 'ALLOCATABLE' ( '::' )? allocatable_decl_list ;
 allocatable_decl : object_name ( '(' array_spec ')' )?
                   ( lbracket coarray_spec rbracket )? ;
 asynchronous_stmt : 'ASYNCHRONOUS' ( '::' )? object_name_list ;
 bind_stmt : language_binding_spec ( '::' )? bind_entity_list ;
 bind_entity : entity_name
              | '/' common_block_name '/' ;
 codimension_stmt : 'CODIMENSION' ( '::' )? codimension_decl_list ;
 codimension_decl : coarray_name lbracket coarray_spec rbracket ;
 contiguous_stmt : 'CONTIGUOUS' ( '::' )? object_name_list ;
 data_stmt : 'DATA' data_stmt_set ( ( ',' )? data_stmt_set )*  ;
 data_stmt_set : data_stmt_object_list '/' data_stmt_value_list '/' ;
 data_stmt_object : variable
              | data_implied_do ;
 data_implied_do : '(' data_i_do_object_list ',' ( integer_type_spec '::' )? data_i_do_variable '='
                  scalar_int_constant_expr ','
                  scalar_int_constant_expr
                   ( ',' scalar_int_constant_expr )? ')' ;
 data_i_do_object : array_element
              | scalar_structure_component
              | data_implied_do ;
 data_i_do_variable : do_variable ;
 data_stmt_value : ( data_stmt_repeat '*' )? data_stmt_constant ;
 data_stmt_repeat : scalar_int_constant
              | scalar_int_constant_subobject ;
 data_stmt_constant : scalar_constant
              | scalar_constant_subobject
              | signed_int_literal_constant
              | signed_real_literal_constant
              | null_init
              | initial_data_target
              | structure_constructor
              | enum_constructor
              | enumeration_constructor ;
 int_constant_subobject : constant_subobject ;
 constant_subobject : designator ;
 dimension_stmt : 'DIMENSION' ( '::' )? array_name '(' array_spec ')'
                   ( ',' array_name '(' array_spec ')' )*  ;
 intent_stmt : 'INTENT' '(' intent_spec ')' ( '::' )? dummy_arg_name_list ;
 optional_stmt : 'OPTIONAL' ( '::' )? dummy_arg_name_list ;
 parameter_stmt : 'PARAMETER' '(' named_constant_def_list ')' ;
 named_constant_def : named_constant '=' constant_expr ;
 pointer_stmt : 'POINTER' ( '::' )? pointer_decl_list ;
 pointer_decl : object_name ( '(' deferred_shape_spec_list ')' )?
              | procptr_entity_name ;
 protected_stmt : 'PROTECTED' ( '::' )? entity_name_list ;
 save_stmt : 'SAVE' ( ( '::' )? saved_entity_list )? ;
 saved_entity : object_name
              | proc_pointer_name
              | '/' common_block_name '/' ;
 proc_pointer_name : name ;
 target_stmt : 'TARGET' ( '::' )? target_decl_list ;
 target_decl : object_name ( '(' array_spec ')' )?
                   ( lbracket coarray_spec rbracket )? ;
 value_stmt : 'VALUE' ( '::' )? dummy_arg_name_list ;
 volatile_stmt : 'VOLATILE' ( '::' )? object_name_list ;
 implicit_stmt : 'IMPLICIT' implicit_spec_list
              | 'IMPLICIT' 'NONE' ( '(' ( implicit_none_spec_list )? ')' )? ;
 implicit_spec : declaration_type_spec '(' letter_spec_list ')' ;
 letter_spec : letter ( '-' letter )? ;
 implicit_none_spec : 'EXTERNAL'
              | 'TYPE' ;
 import_stmt : 'IMPORT' (( '::' )? import_name_list )?
              | 'IMPORT,' 'ONLY' ':' import_name_list
              | 'IMPORT,' 'NONE'
              | 'IMPORT,' 'ALL' ;
 namelist_stmt : 'NAMELIST'                   
'/' namelist_group_name '/' namelist_group_object_list
                   ( ( ',' )? '/' namelist_group_name '/'
                  namelist_group_object_list )*  ;
 namelist_group_object : variable_name ;
 equivalence_stmt : 'EQUIVALENCE' equivalence_set_list ;
 equivalence_set : '(' equivalence_object ',' equivalence_object_list ')' ;
 equivalence_object : variable_name
              | array_element
              | substring ;
 common_stmt : 'COMMON'
                      ( '/' ( common_block_name )? '/' )? common_block_object_list
                      ( ( ',' )? '/' ( common_block_name )? '/'
                     common_block_object_list )*  ;
 common_block_object : variable_name ( '(' array_spec ')' )? ;
 designator : object_name
              | array_element
              | array_section
              | coindexed_named_object
              | complex_part_designator
              | structure_component
              | substring ;
 variable : designator
              | function_reference ;
 variable_name : name ;
 logical_variable : variable ;
 char_variable : variable ;
 default_char_variable : variable ;
 int_variable : variable ;
 substring : parent_string '(' substring_range ')' ;
 parent_string : scalar_variable_name
              | array_element
              | coindexed_named_object
              | scalar_structure_component
              | scalar_constant ;
 substring_range : ( scalar_int_expr )? ':' ( scalar_int_expr )? ;
 data_ref : part_ref ( '%' part_ref )*  ;
 part_ref : part_name ( '(' section_subscript_list ')' )? ( image_selector )? ;
 structure_component : data_ref ;
 coindexed_named_object : data_ref ;
 complex_part_designator : designator '%' 'RE'
              | designator '%' 'IM' ;
 type_param_inquiry : designator '%' type_param_name ;
 array_element : data_ref ;
 array_section : data_ref ( '(' substring_range ')' )?
              | complex_part_designator ;
 subscript : scalar_int_expr ;
 multiple_subscript : '@' int_expr ;
 section_subscript : subscript
              | multiple_subscript
              | subscript_triplet
              | multiple_subscript_triplet
              | vector_subscript ;
 subscript_triplet : ( subscript )? ':' ( subscript )? ( ':' stride )? ;
 multiple_subscript_triplet : '@' ( int_expr )? ':' ( int_expr )? ( ':' int_expr )? ;
 stride : scalar_int_expr ;
 vector_subscript : int_expr ;
 image_selector : lbracket cosubscript_list ( ',' image_selector_spec_list )? rbracket ;
 cosubscript : scalar_int_expr ;
 image_selector_spec : 'NOTIFY' '=' notify_variable
              | 'STAT' '=' stat_variable
              | 'TEAM' '=' team_value
              | 'TEAM_NUMBER' '=' scalar_int_expr ;
 allocate_stmt : 'ALLOCATE' '(' ( type_spec '::' )? allocation_list
                   ( ',' alloc_opt_list )? ')' ;
 alloc_opt : 'ERRMSG' '=' errmsg_variable
              | 'MOLD' '=' source_expr
              | 'SOURCE' '=' source_expr
              | 'STAT' '=' stat_variable ;
 errmsg_variable : scalar_default_char_variable ;
 source_expr : expr ;
 allocation : allocate_object ( '(' allocate_shape_spec_list ')' )?
                   ( lbracket allocate_coarray_spec rbracket )?
              | '(' ( lower_bounds_expr ':' )? upper_bounds_expr ')'
                   ( lbracket allocate_coarray_spec rbracket )? ;
 allocate_object : variable_name
              | structure_component ;
 allocate_shape_spec : ( lower_bound_expr ':' )? upper_bound_expr ;
 lower_bound_expr : scalar_int_expr ;
 lower_bounds_expr : int_expr ;
 upper_bound_expr : scalar_int_expr ;
 upper_bounds_expr : int_expr ;
 allocate_coarray_spec : ( allocate_coshape_spec_list ',' )? ( lower_bound_expr ':' )? '*' ;
 allocate_coshape_spec : ( lower_bound_expr ':' )? upper_bound_expr ;
 nullify_stmt : 'NULLIFY' '(' pointer_object_list ')' ;
 pointer_object : variable_name
              | structure_component
              | proc_pointer_name ;
 deallocate_stmt : 'DEALLOCATE' '(' allocate_object_list ( ',' dealloc_opt_list )? ')' ;
 dealloc_opt : 'STAT' '=' stat_variable
              | 'ERRMSG' '=' errmsg_variable ;
 stat_variable : scalar_int_variable ;
 primary : literal_constant
              | designator
              | array_constructor
              | structure_constructor
              | enum_constructor
              | enumeration_constructor
              | function_reference
              | type_param_inquiry
              | type_param_name
              | '(' expr ')'
              | conditional_expr ;
 conditional_expr : '(' scalar_logical_expr '?' expr ( ':' scalar_logical_expr '?' expr )* ':' expr ')' ;
 level_1_expr : ( defined_unary_op )? primary ;
 defined_unary_op : '.' letter ( letter )*  '.' ;
 mult_operand : level_1_expr ( power_op mult_operand )? ;
 add_operand : ( add_operand mult_op )? mult_operand ;
 level_2_expr : ( ( level_2_expr )? add_op )? add_operand ;
 power_op : '**' ;
 mult_op : '*'
              | '/' ;
 add_op : '+'
              | '-' ;
 level_3_expr : ( level_3_expr concat_op )? level_2_expr ;
 concat_op : ; //
 level_4_expr : ( level_3_expr rel_op )? level_3_expr ;
 rel_op : '.EQ.'
              | '.NE.'
              | '.LT.'
              | '.LE.'
              | '.GT.'
              | '.GE.'
              | '=='
              | '/='
              | '<'
              | '<='
              | '>'
              | '>=' ;
 and_operand : ( not_op )? level_4_expr ;
 or_operand : ( or_operand and_op )? and_operand ;
 equiv_operand : ( equiv_operand or_op )? or_operand ;
 level_5_expr : ( level_5_expr equiv_op )? equiv_operand ;
 not_op : '.NOT.' ;
 and_op : '.AND.' ;
 or_op : '.OR.' ;
 equiv_op : '.EQV.'
              | '.NEQV.' ;
 expr : ( expr defined_binary_op )? level_5_expr ;
 defined_binary_op : '.' letter ( letter )*  '.' ;
 logical_expr : expr ;
 default_char_expr : expr ;
 int_expr : expr ;
 numeric_expr : expr ;
 specification_expr : scalar_int_expr ;
 constant_expr : expr ;
 default_char_constant_expr : default_char_expr ;
 int_constant_expr : int_expr ;
 assignment_stmt : variable '=' expr ;
 pointer_assignment_stmt : data_pointer_object ( '(' bounds_spec_list ')' )? '=>' data_target
              | data_pointer_object '(' lower_bounds_expr ':' ')' '=>' data_target
              | data_pointer_object '(' bounds_remapping_list ')' '=>' data_target
              | data_pointer_object '(' lower_bounds_expr ':' upper_bounds_expr ')'                   
'=>' data_target
              | proc_pointer_object '=>' proc_target ;
 data_pointer_object : variable_name
              | scalar_variable '%' data_pointer_component_name ;
 bounds_spec : lower_bound_expr ':' ;
 bounds_remapping : lower_bound_expr ':' upper_bound_expr ;
 data_target : expr ;
 proc_pointer_object : proc_pointer_name
              | proc_component_ref ;
 proc_component_ref : scalar_variable '%' procedure_component_name ;
 proc_target : expr
              | procedure_name
              | proc_component_ref ;
 where_stmt : 'WHERE' '(' mask_expr ')' where_assignment_stmt ;
 where_construct : where_construct_stmt
                     ( where_body_construct )* 
                   ( masked_elsewhere_stmt
                     ( where_body_construct )*  )* 
                   ( elsewhere_stmt
                     ( where_body_construct )*  )?
                   end_where_stmt ;
 where_construct_stmt : (where_construct_name':')? 'WHERE' '(' mask_expr ')' ;
 where_body_construct : where_assignment_stmt
              | where_stmt
              | where_construct ;
 where_assignment_stmt : assignment_stmt ;
 mask_expr : logical_expr ;
 masked_elsewhere_stmt : 'ELSEWHERE' '('mask_expr')' (where_construct_name)? ;
 elsewhere_stmt : 'ELSEWHERE' (where_construct_name)? ;
 end_where_stmt : 'END' 'WHERE' (where_construct_name)? ;
 forall_construct : forall_construct_stmt
                       (forall_body_construct )* 
                       end_forall_stmt ;
 forall_construct_stmt : (forall_construct_name ':')? 'FORALL' concurrent_header ;
 forall_body_construct : forall_assignment_stmt
              | where_stmt
              | where_construct
              | forall_construct
              | forall_stmt ;
 forall_assignment_stmt : assignment_stmt
              | pointer_assignment_stmt ;
 end_forall_stmt : 'END' 'FORALL' (forall_construct_name )? ;
 forall_stmt : 'FORALL' concurrent_header forall_assignment_stmt ;
 block : ( execution_part_construct )*  ;
 associate_construct : associate_stmt
                   block
                   end_associate_stmt ;
 associate_stmt : ( associate_construct_name ':' )? 'ASSOCIATE'                   
'('association_list ')' ;
 association : associate_name '=>' selector ;
 selector : expr
              | variable ;
 end_associate_stmt : 'END' 'ASSOCIATE' ( associate_construct_name )? ;
 block_construct : block_stmt
                    ( block_specification_part )?
                   block
                   end_block_stmt ;
 block_stmt : ( block_construct_name ':' )? 'BLOCK' ;
 block_specification_part : ( use_stmt )* 
                  ( import_stmt )* 
                  ( declaration_construct )*  ;
 end_block_stmt : 'END' 'BLOCK' ( block_construct_name )? ;
 change_team_construct : change_team_stmt
                   block
                   end_change_team_stmt ;
 change_team_stmt : ( team_construct_name ':' )? 'CHANGE' 'TEAM' '(' team_value
                   ( ',' coarray_association_list )? ( ',' sync_stat_list )? ')' ;
 coarray_association : codimension_decl '=>' selector ;
 end_change_team_stmt : 'END' 'TEAM' ( '(' ( sync_stat_list )? ')' )? ( team_construct_name )? ;
 team_value : scalar_expr ;
 critical_construct : critical_stmt
                   block
                   end_critical_stmt ;
 critical_stmt : ( critical_construct_name ':' )? 'CRITICAL' ( '(' ( sync_stat_list )? ')' )? ;
 end_critical_stmt : 'END' 'CRITICAL' ( critical_construct_name )? ;
 do_construct : do_stmt
                   block
                   end_do ;
 do_stmt : nonlabel_do_stmt
              | label_do_stmt ;
 label_do_stmt : ( do_construct_name ':' )? 'DO' label ( loop_control )? ;
 nonlabel_do_stmt : ( do_construct_name ':' )? 'DO' ( loop_control )? ;
 loop_control : ( ',' )? do_variable '=' scalar_int_expr',' scalar_int_expr
                   ( ',' scalar_int_expr )?
              | ( ',' )? 'WHILE' '(' scalar_logical_expr ')'
              | ( ',' )? 'CONCURRENT' concurrent_header concurrent_locality ;
 do_variable : scalar_int_variable_name ;
 concurrent_header : '(' ( integer_type_spec '::' )? concurrent_control_list ( ',' scalar_mask_expr )? ')' ;
 concurrent_control : index_name '=' concurrent_limit ':' concurrent_limit ( ':' concurrent_step )? ;
 concurrent_limit : scalar_int_expr ;
 concurrent_step : scalar_int_expr ;
 concurrent_locality : ( locality_spec )* ;
 locality_spec : 'LOCAL' '(' variable_name_list ')'
              | 'LOCAL_INIT' '(' variable_name_list ')'
              | 'REDUCE' '(' reduce_operation ':' variable_name_list ')'
              | 'SHARED' '(' variable_name_list ')'
              | 'DEFAULT' '(' 'NONE' ')' ;
 reduce_operation : binary_reduce_op
              | function_reduction_name ;
 binary_reduce_op : '+'
              | '*'
              | '.AND.'
              | '.OR.'
              | '.EQV.'
              | '.NEQV.' ;
 end_do : end_do_stmt
              | continue_stmt ;
 end_do_stmt : 'END' 'DO' ( do_construct_name )? ;
 cycle_stmt : 'CYCLE' ( do_construct_name )? ;
 if_construct : if_then_stmt
                     block
                   ( else_if_stmt
                     block )* 
                   ( else_stmt
                     block )?
                   end_if_stmt ;
 if_then_stmt : ( if_construct_name ':' )? 'IF' '(' scalar_logical_expr ')' 'THEN' ;
 else_if_stmt : 'ELSE' 'IF' '(' scalar_logical_expr ')' 'THEN' ( if_construct_name )? ;
 else_stmt : 'ELSE' ( if_construct_name )? ;
 end_if_stmt : 'END' 'IF' ( if_construct_name )? ;
 if_stmt : 'IF' '(' scalar_logical_expr ')' action_stmt ;
 case_construct : select_case_stmt
                   ( case_stmt
                     block )* 
                   end_select_stmt ;
 select_case_stmt : ( case_construct_name ':' )? 'SELECT' 'CASE' '(' case_expr ')' ;
 case_stmt : 'CASE' case_selector (case_construct_name)? ;
 end_select_stmt : 'END' 'SELECT' ( case_construct_name )? ;
 case_expr : scalar_expr ;
 case_selector : '(' case_value_range_list ')'
              | 'DEFAULT' ;
 case_value_range : case_value
              | case_value ':'
              | ':' case_value
              | case_value ':' case_value ;
 case_value : scalar_constant_expr ;
 select_rank_construct : select_rank_stmt
                   ( select_rank_case_stmt
                   block )*
                   end_select_rank_stmt ;
 select_rank_stmt : ( select_construct_name ':' )? 'SELECT' 'RANK'                   
'(' ( associate_name '=>' )? selector ')' ;
 select_rank_case_stmt : 'RANK' '(' scalar_int_constant_expr ')' ( select_construct_name )?
              | 'RANK' '(' '*' ')' ( select_construct_name )?
              | 'RANK' 'DEFAULT' ( select_construct_name )? ;
 end_select_rank_stmt : 'END' 'SELECT' ( select_construct_name )? ;
 select_type_construct : select_type_stmt
                   ( type_guard_stmt
                     block )* 
                   end_select_type_stmt ;
 select_type_stmt : ( select_construct_name ':' )? 'SELECT' 'TYPE'                   
'(' ( associate_name '=>' )? selector ')' ;
 type_guard_stmt : 'TYPE' 'IS' '(' type_spec ')' ( select_construct_name )?
              | 'CLASS' 'IS' '(' derived_type_spec ')' ( select_construct_name )?
              | 'CLASS' 'DEFAULT' ( select_construct_name )? ;
 end_select_type_stmt : 'END' 'SELECT' ( select_construct_name )? ;
 exit_stmt : 'EXIT' ( construct_name )? ;
 goto_stmt : 'GO' 'TO' label ;
 computed_goto_stmt : 'GO' 'TO' '(' label_list ')' ( ',' )? scalar_int_expr ;
 continue_stmt : 'CONTINUE' ;
 stop_stmt : 'STOP' ( stop_code )? ( ',' 'QUIET' '=' scalar_logical_expr)? ;
 error_stop_stmt : 'ERROR' 'STOP' ( stop_code )? ( ',' 'QUIET' '=' scalar_logical_expr)? ;
 stop_code : scalar_default_char_expr
              | scalar_int_expr ;
 fail_image_stmt : 'FAIL' 'IMAGE' ;
 notify_wait_stmt : 'NOTIFY' 'WAIT' '(' notify_variable ( ',' event_wait_spec_list )? ')' ;
 notify_variable : scalar_variable ;
 sync_all_stmt : 'SYNC' 'ALL' ( '(' ( sync_stat_list )? ')' )? ;
 sync_stat : 'STAT' '=' stat_variable
              | 'ERRMSG' '=' errmsg_variable ;
 sync_images_stmt : 'SYNC' 'IMAGES' '(' image_set ( ',' sync_stat_list )? ')' ;
 image_set : int_expr
              | '*' ;
 sync_memory_stmt : 'SYNC' 'MEMORY' ( '(' ( sync_stat_list )? ')' )? ;
 sync_team_stmt : 'SYNC' 'TEAM' '(' team_value ( ',' sync_stat_list )? ')' ;
 event_post_stmt : 'EVENT' 'POST' '(' event_variable ( ',' sync_stat_list )? ')' ;
 event_variable : scalar_variable ;
 event_wait_stmt : 'EVENT' 'WAIT' '(' event_variable ( ',' event_wait_spec_list )? ')' ;
 event_wait_spec : until_spec
              | sync_stat ;
 until_spec : 'UNTIL_COUNT' '=' scalar_int_expr ;
 form_team_stmt : 'FORM' 'TEAM' '(' team_number',' team_variable
                   ( ',' form_team_spec_list )? ')' ;
 team_number : scalar_int_expr ;
 team_variable : scalar_variable ;
 form_team_spec : 'NEW_INDEX' '=' scalar_int_expr
              | sync_stat ;
 lock_stmt : 'LOCK' '(' lock_variable ( ',' lock_stat_list )? ')' ;
 lock_stat : 'ACQUIRED_LOCK' '=' scalar_logical_variable
              | sync_stat ;
 unlock_stmt : 'UNLOCK' '(' lock_variable ( ',' sync_stat_list )? ')' ;
 lock_variable : scalar_variable ;
 io_unit : file_unit_number
              | '*'
              | internal_file_variable ;
 file_unit_number : scalar_int_expr ;
 internal_file_variable : char_variable ;
 open_stmt : 'OPEN' '(' connect_spec_list ')' ;
 connect_spec : ( 'UNIT' '=' )? file_unit_number
              | 'ACCESS' '=' scalar_default_char_expr
              | 'ACTION' '=' scalar_default_char_expr
              | 'ASYNCHRONOUS' '=' scalar_default_char_expr
              | 'BLANK' '=' scalar_default_char_expr
              | 'DECIMAL' '=' scalar_default_char_expr
              | 'DELIM' '=' scalar_default_char_expr
              | 'ENCODING' '=' scalar_default_char_expr
              | 'ERR' '=' label
              | 'FILE' '=' file_name_expr
              | 'FORM' '=' scalar_default_char_expr
              | 'IOMSG' '=' iomsg_variable
              | 'IOSTAT' '=' stat_variable
              | 'LEADING_ZERO' '=' scalar_default_char_expr
              | 'NEWUNIT' '=' scalar_int_variable
              | 'PAD' '=' scalar_default_char_expr
              | 'POSITION' '=' scalar_default_char_expr
              | 'RECL' '=' scalar_int_expr
              | 'ROUND' '=' scalar_default_char_expr
              | 'SIGN' '=' scalar_default_char_expr
              | 'STATUS' '=' scalar_default_char_expr ;
 file_name_expr : scalar_default_char_expr ;
 iomsg_variable : scalar_default_char_variable ;
 close_stmt : 'CLOSE' '(' close_spec_list ')' ;
 close_spec : ( 'UNIT' '=' )? file_unit_number
              | 'IOSTAT' '=' stat_variable
              | 'IOMSG' '=' iomsg_variable
              | 'ERR' '=' label
              | 'STATUS' '=' scalar_default_char_expr ;
 read_stmt : 'READ' '(' io_control_spec_list ')' ( input_item_list )?
              | 'READ' format ( ',' input_item_list )? ;
 write_stmt : 'WRITE' '(' io_control_spec_list ')' ( output_item_list )? ;
 print_stmt : 'PRINT' format ( ',' output_item_list )? ;
 io_control_spec : ( 'UNIT' '=' )? io_unit
              | ( 'FMT' '=' )? format
              | ( 'NML' '=' )? namelist_group_name
              | 'ADVANCE' '=' scalar_default_char_expr
              | 'ASYNCHRONOUS' '=' scalar_default_char_constant_expr
              | 'BLANK' '=' scalar_default_char_expr
              | 'DECIMAL' '=' scalar_default_char_expr
              | 'DELIM' '=' scalar_default_char_expr
              | 'END' '=' label
              | 'EOR' '=' label
              | 'ERR' '=' label
              | 'ID' '=' id_variable
              | 'IOMSG' '=' iomsg_variable
              | 'IOSTAT' '=' stat_variable
              | 'LEADING_ZERO' '=' scalar_default_char_expr
              | 'PAD' '=' scalar_default_char_expr
              | 'POS' '=' scalar_int_expr
              | 'REC' '=' scalar_int_expr
              | 'ROUND' '=' scalar_default_char_expr
              | 'SIGN' '=' scalar_default_char_expr
              | 'SIZE' '=' scalar_int_variable ;
 id_variable : scalar_int_variable ;
 format : default_char_expr
              | label
              | '*' ;
 input_item : variable
              | io_implied_do ;
 output_item : expr
              | io_implied_do ;
 io_implied_do : '(' io_implied_do_object_list ',' io_implied_do_control ')' ;
 io_implied_do_object : input_item
              | output_item ;
 io_implied_do_control : do_variable '=' scalar_int_expr ','
                  scalar_int_expr ( ',' scalar_int_expr )? ;
 dtv_type_spec : 'TYPE(' derived_type_spec ')'
              | 'CLASS(' derived_type_spec ')' ;
 wait_stmt : 'WAIT' '('wait_spec_list')' ;
 wait_spec : ( 'UNIT' '=' )? file_unit_number
              | 'END' '=' label
              | 'EOR' '=' label
              | 'ERR' '=' label
              | 'ID' '=' scalar_int_expr
              | 'IOMSG' '=' iomsg_variable
              | 'IOSTAT' '=' stat_variable ;
 backspace_stmt : 'BACKSPACE' file_unit_number
              | 'BACKSPACE' '(' position_spec_list ')' ;
 endfile_stmt : 'ENDFILE' file_unit_number
              | 'ENDFILE' '(' position_spec_list ')' ;
 rewind_stmt : 'REWIND' file_unit_number
              | 'REWIND' '(' position_spec_list ')' ;
 position_spec : ( 'UNIT' '=' )? file_unit_number
              | 'IOMSG' '=' iomsg_variable
              | 'IOSTAT' '=' stat_variable
              | 'ERR' '=' label ;
 flush_stmt : 'FLUSH' file_unit_number
              | 'FLUSH' '(' flush_spec_list ')' ;
 flush_spec : ('UNIT' '=')? file_unit_number
              | 'IOSTAT' '=' stat_variable
              | 'IOMSG' '=' iomsg_variable
              | 'ERR' '=' label ;
 inquire_stmt : 'INQUIRE' '(' inquire_spec_list ')'
              | 'INQUIRE' '(' 'IOLENGTH' '=' scalar_int_variable ')'
                  output_item_list ;
 inquire_spec : ( 'UNIT' '=' )? file_unit_number
              | 'FILE' '=' file_name_expr
              | 'ACCESS' '=' scalar_default_char_variable
              | 'ACTION' '=' scalar_default_char_variable
              | 'ASYNCHRONOUS' '=' scalar_default_char_variable
              | 'BLANK' '=' scalar_default_char_variable
              | 'DECIMAL' '=' scalar_default_char_variable
              | 'DELIM' '=' scalar_default_char_variable
              | 'DIRECT' '=' scalar_default_char_variable
              | 'ENCODING' '=' scalar_default_char_variable
              | 'ERR' '=' label
              | 'EXIST' '=' scalar_logical_variable
              | 'FORM' '=' scalar_default_char_variable
              | 'FORMATTED' '=' scalar_default_char_variable
              | 'ID' '=' scalar_int_expr
              | 'IOMSG' '=' iomsg_variable
              | 'IOSTAT' '=' stat_variable
              | 'LEADING_ZERO' '=' scalar_default_char_variable
              | 'NAME' '=' scalar_default_char_variable
              | 'NAMED' '=' scalar_logical_variable
              | 'NEXTREC' '=' scalar_int_variable
              | 'NUMBER' '=' scalar_int_variable
              | 'OPENED' '=' scalar_logical_variable
              | 'PAD' '=' scalar_default_char_variable
              | 'PENDING' '=' scalar_logical_variable
              | 'POS' '=' scalar_int_variable
              | 'POSITION' '=' scalar_default_char_variable
              | 'READ' '=' scalar_default_char_variable
              | 'READWRITE' '=' scalar_default_char_variable
              | 'RECL' '=' scalar_int_variable
              | 'ROUND' '=' scalar_default_char_variable
              | 'SEQUENTIAL' '=' scalar_default_char_variable
              | 'SIGN' '=' scalar_default_char_variable
              | 'SIZE' '=' scalar_int_variable
              | 'STREAM' '=' scalar_default_char_variable
              | 'UNFORMATTED' '=' scalar_default_char_variable
              | 'WRITE' '=' scalar_default_char_variable ;
 format_stmt : 'FORMAT' format_specification ;
 format_specification : '(' ( format_items )? ')'
              | '(' ( format_items',' )? unlimited_format_item ')' ;
 format_items : format_item ( ( ',' )? format_item )*  ;
 format_item : ( r )? data_edit_desc
              | control_edit_desc
              | char_string_edit_desc
              | ( r )? '(' format_items ')' ;
 unlimited_format_item : '*' '(' format_items ')' ;
 r : int_literal_constant ;
 data_edit_desc : 'I' w ( '.' m )?
              | 'B' w ( '.' m )?
              | 'O' w ( '.' m )?
              | 'Z' w ( '.' m )?
              | 'F' w '.' d
              | 'E' w '.' d ( 'E' e )?
              | 'EN' w '.' d ( 'E' e )?
              | 'ES' w '.' d ( 'E' e )?
              | 'EX' w '.' d ( 'E' e )?
              | 'G' w ( '.' d ( 'E' e )? )?
              | 'L' w
              | 'A' ( w )?
              | 'AT'
              | 'D' w '.' d
              | 'DT' ( char_literal_constant )? ( '(' v_list ')' )? ;
 w : int_literal_constant ;
 m : int_literal_constant ;
 d : int_literal_constant ;
 e : int_literal_constant ;
 v : signed_int_literal_constant ;
 control_edit_desc : blank_interp_edit_desc
              | decimal_edit_desc
              | leading_zero_edit_desc
              | position_edit_desc
              | round_edit_desc
              | sign_edit_desc
              | k 'P'
              | ':'
              | ( r )? '/' ;
 k : signed_int_literal_constant ;
 position_edit_desc : 'T' n
              | 'TL' n
              | 'TR' n
              | n 'X' ;
 n : int_literal_constant ;
 blank_interp_edit_desc : 'BN'
              | 'BZ' ;
 decimal_edit_desc : 'DC'
              | 'DP' ;
 leading_zero_edit_desc : 'LZS'
              | 'LZP'
              | 'LZ' ;
 round_edit_desc : 'RU'
              | 'RD'
              | 'RZ'
              | 'RN'
              | 'RC'
              | 'RP' ;
 sign_edit_desc : 'SS'
              | 'SP'
              | 'S' ;
 char_string_edit_desc : char_literal_constant ;
 hex_digit_string : hex_digit ( hex_digit )*  ;
 main_program : ( program_stmt )?
                   ( specification_part )?
                   ( execution_part )?
                   ( internal_subprogram_part )?
                   end_program_stmt ;
 program_stmt : 'PROGRAM' program_name ;
 end_program_stmt : 'END' ( 'PROGRAM' ( program_name )? )? ;
 module : module_stmt
                   ( specification_part )?
                   ( module_subprogram_part )?
                   end_module_stmt ;
 module_stmt : 'MODULE' module_name ;
 end_module_stmt : 'END' ( 'MODULE' ( module_name )? )? ;
 module_subprogram_part : contains_stmt
                                   ( module_subprogram )*  ;
 module_subprogram : function_subprogram
              | subroutine_subprogram
              | separate_module_subprogram ;
 use_stmt : 'USE' ( ( ',' module_nature )? '::' )? module_name ( ',' rename_list )?
              | 'USE' ( ( ',' module_nature )? '::' )? module_name ','                   
'ONLY' ':' ( only_list )? ;
 module_nature : 'INTRINSIC'
              | 'NON_INTRINSIC' ;
 rename : local_name '=>' use_name
              | 'OPERATOR' '('local_defined_operator')' '=>'                   
'OPERATOR' '('use_defined_operator')' ;
 only : generic_spec
              | only_use_name
              | rename ;
 only_use_name : use_name ;
 local_defined_operator : defined_unary_op
              | defined_binary_op ;
 use_defined_operator : defined_unary_op
              | defined_binary_op ;
 submodule : submodule_stmt
                    ( specification_part )?
                    ( module_subprogram_part )?
                   end_submodule_stmt ;
 submodule_stmt : 'SUBMODULE' '(' parent_identifier ')' submodule_name ;
 parent_identifier : ancestor_module_name ( ':' parent_submodule_name )? ;
 end_submodule_stmt : 'END' ( 'SUBMODULE' ( submodule_name )? )? ;
 block_data : block_data_stmt
                       ( specification_part )?
                      end_block_data_stmt ;
 block_data_stmt : 'BLOCK' 'DATA' ( block_data_name )? ;
 end_block_data_stmt : 'END' ( 'BLOCK' 'DATA' ( block_data_name )? )? ;
 interface_block : interface_stmt
                   ( interface_specification )* 
                   end_interface_stmt ;
 interface_specification : interface_body
              | procedure_stmt ;
 interface_stmt : 'INTERFACE' ( generic_spec )?
              | 'ABSTRACT' 'INTERFACE' ;
 end_interface_stmt : 'END' 'INTERFACE' ( generic_spec )? ;
 interface_body : function_stmt
                   ( specification_part )?
                   end_function_stmt
              | subroutine_stmt
                   ( specification_part )?
                   end_subroutine_stmt ;
 procedure_stmt : ( 'MODULE' )? 'PROCEDURE' ( '::' )? specific_procedure_list ;
 specific_procedure : procedure_name ;
 generic_spec : generic_name
              | 'OPERATOR' '(' defined_operator ')'
              | 'ASSIGNMENT' '(' '=' ')'
              | defined_io_generic_spec ;
 defined_io_generic_spec : 'READ' '(FORMATTED)'
              | 'READ' '(UNFORMATTED)'
              | 'WRITE' '(FORMATTED)'
              | 'WRITE' '(UNFORMATTED)' ;
 generic_stmt : 'GENERIC' ( ',' access_spec )? '::' generic_spec '=>' specific_procedure_list ;
 external_stmt : 'EXTERNAL' ( '::' )? external_name_list ;
 procedure_declaration_stmt : 'PROCEDURE' '(' ( proc_interface )? ')'
                   ( ( ',' proc_attr_spec )*  '::' )? proc_decl_list ;
 proc_interface : interface_name
              | declaration_type_spec ;
 proc_attr_spec : access_spec
              | proc_language_binding_spec
              | 'INTENT' '(' intent_spec ')'
              | 'OPTIONAL'
              | 'POINTER'
              | 'PROTECTED'
              | 'SAVE' ;
 proc_decl : procedure_entity_name ( '=>' proc_pointer_init )? ;
 interface_name : name ;
 proc_pointer_init : null_init
              | initial_proc_target ;
 initial_proc_target : procedure_name ;
 intrinsic_stmt : 'INTRINSIC' ( '::' )? intrinsic_procedure_name_list ;
 function_reference : procedure_designator '(' ( actual_arg_spec_list )? ')' ;
 call_stmt : 'CALL' procedure_designator ( '(' ( actual_arg_spec_list )? ')' )? ;
 procedure_designator : procedure_name
              | proc_component_ref
              | data_ref '%' binding_name ;
 actual_arg_spec : ( keyword '=' )? actual_arg ;
 actual_arg : expr
              | variable
              | procedure_name
              | proc_component_ref
              | conditional_arg
              | alt_return_spec ;
 alt_return_spec : '*' label ;
 conditional_arg : '(' scalar_logical_expr '?' consequent
                   ( ':' scalar_logical_expr '?' consequent )* ':' consequent ')' ;
 consequent : consequent_arg
              | '.NIL.' ;
 consequent_arg : expr
              | variable ;
 prefix : prefix_spec ( prefix_spec )*  ;
 prefix_spec : declaration_type_spec
              | 'ELEMENTAL'
              | 'IMPURE'
              | 'MODULE'
              | 'NON_RECURSIVE'
              | 'PURE'
              | 'RECURSIVE'
              | 'SIMPLE' ;
 proc_language_binding_spec : language_binding_spec ;
 function_subprogram : function_stmt
                   ( specification_part )?
                   ( execution_part )?
                   ( internal_subprogram_part )?
                   end_function_stmt ;
 function_stmt : ( prefix )? 'FUNCTION' function_name                   
'(' ( dummy_arg_name_list )? ')' ( suffix )? ;
 dummy_arg_name : name ;
 suffix : proc_language_binding_spec ( 'RESULT' '(' result_name ')' )?
              | 'RESULT' '(' result_name ')' ( proc_language_binding_spec )? ;
 end_function_stmt : 'END' ( 'FUNCTION' ( function_name )? )? ;
 subroutine_subprogram : subroutine_stmt
                   ( specification_part )?
                   ( execution_part )?
                   ( internal_subprogram_part )?
                   end_subroutine_stmt ;
 subroutine_stmt : ( prefix )? 'SUBROUTINE' subroutine_name
                   ( '(' ( dummy_arg_list )? ')' ( proc_language_binding_spec )? )? ;
 dummy_arg : dummy_arg_name
              | '*' ;
 end_subroutine_stmt : 'END' ( 'SUBROUTINE' ( subroutine_name )? )? ;
 separate_module_subprogram : mp_subprogram_stmt
                   ( specification_part )?
                   ( execution_part )?
                   ( internal_subprogram_part )?
                   end_mp_subprogram_stmt ;
 mp_subprogram_stmt : 'MODULE' 'PROCEDURE' procedure_name ;
 end_mp_subprogram_stmt : 'END' ('PROCEDURE' (procedure_name)?)? ;
 entry_stmt : 'ENTRY' entry_name ( '(' ( dummy_arg_list )? ')' ( suffix )? )? ;
 return_stmt : 'RETURN' ( scalar_int_expr )? ;
 contains_stmt : 'CONTAINS' ;
 stmt_function_stmt : function_name '(' ( dummy_arg_name_list )? ')' '=' scalar_expr ;


ac_value_list : ac_value ( ',' ac_value )*  ;
access_id_list : access_id ( ',' access_id )*  ;
actual_arg_spec_list : actual_arg_spec ( ',' actual_arg_spec )*  ;
alloc_opt_list : alloc_opt ( ',' alloc_opt )*  ;
allocatable_decl_list : allocatable_decl ( ',' allocatable_decl )*  ;
allocate_coshape_spec_list : allocate_coshape_spec ( ',' allocate_coshape_spec )*  ;
allocate_object_list : allocate_object ( ',' allocate_object )*  ;
allocate_shape_spec_list : allocate_shape_spec ( ',' allocate_shape_spec )*  ;
allocation_list : allocation ( ',' allocation )*  ;
association_list : association ( ',' association )*  ;
assumed_implied_spec_list : assumed_implied_spec ( ',' assumed_implied_spec )*  ;
assumed_shape_spec_list : assumed_shape_spec ( ',' assumed_shape_spec )*  ;
bind_entity_list : bind_entity ( ',' bind_entity )*  ;
binding_attr_list : binding_attr ( ',' binding_attr )*  ;
binding_name_list : binding_name ( ',' binding_name )*  ;
bounds_remapping_list : bounds_remapping ( ',' bounds_remapping )*  ;
bounds_spec_list : bounds_spec ( ',' bounds_spec )*  ;
case_value_range_list : case_value_range ( ',' case_value_range )*  ;
close_spec_list : close_spec ( ',' close_spec )*  ;
coarray_association_list : coarray_association ( ',' coarray_association )*  ;
codimension_decl_list : codimension_decl ( ',' codimension_decl )*  ;
common_block_object_list : common_block_object ( ',' common_block_object )*  ;
component_attr_spec_list : component_attr_spec ( ',' component_attr_spec )*  ;
component_decl_list : component_decl ( ',' component_decl )*  ;
component_spec_list : component_spec ( ',' component_spec )*  ;
concurrent_control_list : concurrent_control ( ',' concurrent_control )*  ;
connect_spec_list : connect_spec ( ',' connect_spec )*  ;
cosubscript_list : cosubscript ( ',' cosubscript )*  ;
data_i_do_object_list : data_i_do_object ( ',' data_i_do_object )*  ;
data_stmt_object_list : data_stmt_object ( ',' data_stmt_object )*  ;
data_stmt_value_list : data_stmt_value ( ',' data_stmt_value )*  ;
dealloc_opt_list : dealloc_opt ( ',' dealloc_opt )*  ;
deferred_coshape_spec_list : deferred_coshape_spec ( ',' deferred_coshape_spec )*  ;
deferred_shape_spec_list : deferred_shape_spec ( ',' deferred_shape_spec )*  ;
dummy_arg_list : dummy_arg ( ',' dummy_arg )*  ;
dummy_arg_name_list : dummy_arg_name ( ',' dummy_arg_name )*  ;
entity_decl_list : entity_decl ( ',' entity_decl )*  ;
entity_name_list : entity_name ( ',' entity_name )*  ;
enumerator_list : enumerator ( ',' enumerator )*  ;
enumerator_name_list : enumerator_name ( ',' enumerator_name )*  ;
equivalence_object_list : equivalence_object ( ',' equivalence_object )*  ;
equivalence_set_list : equivalence_set ( ',' equivalence_set )*  ;
event_wait_spec_list : event_wait_spec ( ',' event_wait_spec )*  ;
explicit_shape_spec_list : explicit_shape_spec ( ',' explicit_shape_spec )*  ;
external_name_list : external_name ( ',' external_name )*  ;
final_subroutine_name_list : final_subroutine_name ( ',' final_subroutine_name )*  ;
flush_spec_list : flush_spec ( ',' flush_spec )*  ;
form_team_spec_list : form_team_spec ( ',' form_team_spec )*  ;
image_selector_spec_list : image_selector_spec ( ',' image_selector_spec )*  ;
implicit_none_spec_list : implicit_none_spec ( ',' implicit_none_spec )*  ;
implicit_spec_list : implicit_spec ( ',' implicit_spec )*  ;
import_name_list : import_name ( ',' import_name )*  ;
input_item_list : input_item ( ',' input_item )*  ;
inquire_spec_list : inquire_spec ( ',' inquire_spec )*  ;
intrinsic_procedure_name_list : intrinsic_procedure_name ( ',' intrinsic_procedure_name )*  ;
io_control_spec_list : io_control_spec ( ',' io_control_spec )*  ;
io_implied_do_object_list : io_implied_do_object ( ',' io_implied_do )*  ;
label_list : label ( ',' label )*  ;
letter_spec_list : <i>LETTERSPEC</i> ( ',' <i>LETTERSPEC</i> )*  ;
lock_stat_list : lock_stat ( ',' lock_stat )*  ;
named_constant_def_list : named_constant_def ( ',' named_constant_def )*  ;
namelist_group_object_list : namelist_group_object ( ',' namelist_group_object )*  ;
object_name_list : object_name ( ',' object_name )*  ;
only_list : only ( ',' only )*  ;
output_item_list : output_item ( ',' output_item )*  ;
pointer_decl_list : pointer_decl ( ',' pointer_decl )*  ;
pointer_object_list : pointer_object ( ',' pointer_object )*  ;
position_spec_list : position_spec ( ',' position_spec )*  ;
proc_component_attr_spec_list : proc_component_attr_spec ( ',' proc_component_attr_spec )*  ;
proc_decl_list : proc_decl ( ',' proc_decl )*  ;
rename_list : rename ( ',' rename )*  ;
saved_entity_list : saved_entity ( ',' saved_entity )*  ;
section_subscript_list : section_subscript ( ',' section_subscript )*  ;
specific_procedure_list : specific_procedure ( ',' specific_procedure )*  ;
sync_stat_list : sync_stat ( ',' sync_stat )*  ;
target_decl_list : target_decl ( ',' target_decl )*  ;
type_attr_spec_list : type_attr_spec ( ',' type_attr_spec )*  ;
type_bound_proc_decl_list : type_bound_proc_decl ( ',' type_bound_proc_decl )*  ;
type_param_decl_list : type_param_decl ( ',' type_param_decl )*  ;
type_param_name_list : type_param_name ( ',' type_param_name )*  ;
type_param_spec_list : type_param_spec ( ',' type_param_spec )*  ;
v_list : v ( ',' v )*  ;
variable_name_list : variable_name ( ',' variable_name )*  ;
wait_spec_list : wait_spec ( ',' wait_spec )*  ;

access_name : name ;
ancestor_module_name : name ;
arg_name : name ;
array_name : name ;
associate_construct_name : name ;
associate_name : name ;
binding_name : name ;
block_construct_name : name ;
block_data_name : name ;
case_construct_name : name ;
coarray_name : name ;
common_block_name : name ;
component_name : name ;
construct_name : name ;
critical_construct_name : name ;
data_pointer_component_name : name ;
do_construct_name : name ;
entity_name : name ;
entry_name : name ;
enum_type_name : name ;
enumeration_type_name : name ;
enumerator_name : name ;
external_name : name ;
final_subroutine_name : name ;
forall_construct_name : name ;
function_name : name ;
function_reduction_name : name ;
generic_name : name ;
if_construct_name : name ;
import_name : name ;
index_name : name ;
int_constant_name : name ;
int_variable_name : name ;
intrinsic_procedure_name : name ;
local_name : name ;
module_name : name ;
namelist_group_name : name ;
parent_submodule_name : name ;
parent_type_name : name ;
part_name : name ;
procedure_component_name : name ;
procedure_entity_name : name ;
procedure_name : name ;
procptr_entity_name : name ;
program_name : name ;
result_name : name ;
scalar_constant_expr : constant_expr ;
scalar_constant_subobject : constant_subobject ;
scalar_constant : constant ;
scalar_constant : constant ;
scalar_default_char_constant_expr : default_char_constant_expr ;
scalar_default_char_expr : default_char_expr ;
scalar_default_char_variable : default_char_variable ;
scalar_expr : expr ;
scalar_int_constant_expr : int_constant_expr ;
scalar_int_constant_expr : int_constant_expr ;
scalar_int_constant_name : int_constant_name ;
scalar_int_constant_subobject : int_constant_subobject ;
scalar_int_constant : int_constant ;
scalar_int_expr : int_expr ;
scalar_int_variable_name : int_variable_name ;
scalar_int_variable : int_variable ;
scalar_logical_expr : logical_expr ;
scalar_logical_variable : logical_variable ;
scalar_mask_expr : mask_expr ;
scalar_structure_component : structure_component ;
scalar_variable_name : variable_name ;
scalar_variable : variable ;
select_construct_name : name ;
submodule_name : name ;
subroutine_name : name ;
team_construct_name : name ;
type_name : name ;
type_param_name : name ;
use_name : name ;
where_construct_name : name ;









LINE_COMMENT: '!' .*? '\r'? '\n' -> skip;

BLOCK_COMMENT: '/*' .*? '*/' -> skip;

SPACE: [ ] -> skip;

WS: [\t\r\n]+ -> skip;

PROGRAM: 'PROGRAM';

END: 'END';

COMMA: ',';

FUNCTION: 'FUNCTION';

LPAREN: '(';

RPAREN: ')';

ASTERIK: '*';

SUBROUTINE: 'SUBROUTINE';

MODULE: 'MODULE';

SUBMODULE: 'SUBMODULE';

BLOCK: 'BLOCK';

DATA: 'DATA';

COLON : ':';

INTRINSIC: 'INTRINSIC';

NONINTRINSIC: 'NON_INTRINSIC';

IMPLIES: '=>';

OPERATOR: 'OPERATOR';

POWER: '**';

SLASH: '/';

PLUS: '+';

MINUS: '-';

CONCAT: '//';

EQ: '.EQ.';

NE: '.NE.';

LT: '.LT.';

LE: '.LE.';

GT: '.GT.';

GE: '.GE.';

EQUAL: '==';

NOTEQUAL: '/=';

LESSTHAN: '<';

LESSEQUAL: '<=';

GREATERTHAN: '>';

GREATEREQUAL: '>=';

NOT: '.NOT.';

AND: '.AND.';

OR: '.OR.';

EQV: '.EQV.';

NEQV: '.NEQV.';

READ: 'READ';

FORMATTED: 'FORMATTED';

UNFORMATTED: 'UNFORMATTED';

WRITE: 'WRITE';

ASSIGNMENT: 'ASSIGNMENT';

ASSIGN: '=';

USE: 'USE';

DOUBLECOLON: '::';

ONLY: 'ONLY';

IMPORT : 'IMPORT';

NONE: 'NONE';

ALL: 'ALL';

KIND: 'KIND';

INTEGER: 'INTEGER';

LEN: 'LEN';

REAL: 'REAL';

DOUBLE: 'DOUBLE';

PRECISION: 'PRECISION';

COMPLEX: 'COMPLEX';

CHARACTER: 'CHARACTER';

LOGICAL: 'LOGICAL';

TYPE: 'TYPE';

CLASS: 'CLASS';

EXTERNAL: 'EXTERNAL';

IMPLICIT: 'IMPLICIT';

PARAMETER: 'PARAMETER';

FORMATIN : FORMAT (SPACE)* LPAREN -> pushMode(FORMAT_MODE);

FORMAT: 'FORMAT';

BINDC: BIND SPACE* LPAREN SPACE* C;

BIND: 'BIND';

NAAM: 'NAME';

RESULT: 'RESULT';

ENTRY: 'ENTRY';

DOT: '.';

TRUE: '.TRUE.';

FALSE: '.FALSE.';

SQUOTE: '\'';

DQUOTE: '"';

STAT: 'STAT';

TEAM: 'TEAM';

TEAMNUMBER: 'TEAM_NUMBER';

LBRACKET: '[';

RBRACKET: ']';

RE: 'RE';

IM: 'IM';

PERCENT: '%';

LPARENSLASH: '(/';

RPARENSLASH: '/)';

// R602 UNDERSCORE -> _
UNDERSCORE: '_';

SEQUENCE: 'SEQUENCE';

PRIVATE: 'PRIVATE';

PROCEDURE: 'PROCEDURE';

NOPASS: 'NOPASS';

PASS: 'PASS';

POINTER: 'POINTER';

ALLOCATABLE: 'ALLOCATABLE';

CODIMENSION: 'CODIMENSION';

CONTIGUOUS: 'CONTIGUOUS';

DIMENSION: 'DIMENSION';

PUBLIC: 'PUBLIC';

CONTAINS: 'CONTAINS';

FINAL: 'FINAL';

GENERIC: 'GENERIC';

DEFERRED: 'DEFERRED';

NONOVERRIDABLE: 'NON_OVERRIDABLE';

INTENT: 'INTENT';

OPTIONAL: 'OPTIONAL';

PROTECTED: 'PROTECTED';

SAVE: 'SAVE';

IN: 'IN';

OUT: 'OUT';

INOUT: 'INOUT';

INTERFACE: 'INTERFACE';

ABSTRACT: 'ABSTRACT';

ENUM: 'ENUM';

ENUMERATOR: 'ENUMERATOR';

ASYNCHRONOUS: 'ASYNCHRONOUS';

TARGET: 'TARGET';

VALUE: 'VALUE';

VOLATILE: 'VOLATILE';

EQUIVALENCE: 'EQUIVALENCE';

COMMON: 'COMMON';

NAMELIST: 'NAMELIST';

DOUBLEDOT: '..';

EVENT: 'EVENT';

WAIT: 'WAIT';

UNTILCOUNT: 'UNTIL_COUNT';

POST: 'POST';

ERRMSG: 'ERRMSG';

ERROR: 'ERROR';

STOP: 'STOP';

QUIET: 'QUIET';

ENDFILE: 'ENDFILE';

DEALLOCATE: 'DEALLOCATE';

CYCLE: 'CYCLE';

CONTINUE: 'CONTINUE';

CLOSE: 'CLOSE';

UNIT: 'UNIT';

IOSTAT: 'IOSTAT';

IOMSG: 'IOMSG';

ERR: 'ERR';

STATUS: 'STATUS';

CALL: 'CALL';

BACKSPACE: 'BACKSPACE';

ALLOCATE: 'ALLOCATE';

MOLD: 'MOLD';

SOURCE: 'SOURCE';

OPEN: 'OPEN';

ACCESS: 'ACCESS';

ACTION: 'ACTION';

BLANK: 'BLANK';

DECIMAL: 'DECIMAL';

DELIM: 'DELIM';

ENCODING: 'ENCODING';

FILE: 'FILE';

FORM: 'FORM';

NEWUNIT: 'NEWUNIT';

PAD: 'PAD';

POSITION: 'POSITION';

RECL: 'RECL';

ROUND: 'ROUND';

SIGN: 'SIGN';

NULLIFY: 'NULLIFY';

LOCK: 'LOCK';

ACQUIREDLOCK: 'ACQUIRED_LOCK';

INQUIRE: 'INQUIRE';

IOLENGTH: 'IOLENGTH';

EXIST: 'EXIST';

ID: 'ID';

NAMED: 'NAMED';

NEXTREC: 'NEXTREC';

NUMBER: 'NUMBER';

OPENED: 'OPENED';

PENDING: 'PENDING';

POS: 'POS';

READWRITE: 'READWRITE';

SEQUENTIAL: 'SEQUENTIAL';

SIZE: 'SIZE';

STREAM: 'STREAM';

IF: 'IF';

GOTO: 'GOTO';

NEWINDEX: 'NEW_INDEX';

FLUSH: 'FLUSH';

FAIL: 'FAIL';

IMAGE: 'IMAGE';

EXIT: 'EXIT';

FORALL: 'FORALL';

WHERE: 'WHERE';

EOR: 'EOR';

UNLOCK: 'UNLOCK';

SYNC: 'SYNC';

MEMORY: 'MEMORY';

IMAGES: 'IMAGES';

REWIND: 'REWIND';

RETURN: 'RETURN';

FMT: 'FMT';

NML: 'NML';

ADVANCE: 'ADVANCE';

REC: 'REC';

PRINT: 'PRINT';

CRITICAL: 'CRITICAL';

CHANGE: 'CHANGE';

SELECT: 'SELECT';

CASE: 'CASE';

DEFAULT: 'DEFAULT';

ASSOCIATE: 'ASSOCIATE';

ELSEWHERE: 'ELSEWHERE';

IS: 'IS';

RANK: 'RANK';

ELSE: 'ELSE';

THEN: 'THEN';

DO: 'DO';

CONCURRENT: 'CONCURRENT';

WHILE: 'WHILE';

SHARED: 'SHARED';

LOCAL: 'LOCAL';

LOCALINIT: 'LOCAL_INIT';

RECURSIVE: 'RECURSIVE';

PURE: 'PURE';

NONRECURSIVE: 'NON_RECURSIVE';

IMPURE: 'IMPURE';

ELEMENTAL: 'ELEMENTAL';

ATSYMBOL: '@';

NIL: '.NIL';

QUESTION: '?';

NOTIFY: 'NOTIFY';

TYPEOF: 'TYPEOF';

CLASSOF: 'CLASSOF';

ENUMERATION: 'ENUMERATION';

DIRECT: 'DIRECT';

LEADINGZERO: 'LEADING_ZERO';

REDUCE: 'REDUCE';

SIMPLE: 'SIMPLE';

DEFINEDUNARYBINARYOP: DOT LETTER+? DOT;

// R865 letter-spec -> letter [- letter]
///LEXER RULE: LetterSpec should only be defined as LETTER MINUS LETTER form and not in only LETTER form due to conflict with NAME
LETTERSPEC: LETTER MINUS LETTER;

// R765 binary-constant -> B ' digit [digit]... ' | B " digit [digit]... "
BINARYCONSTANT: B APOSTROPHE DIGIT+? APOSTROPHE | B QUOTE DIGIT+? QUOTE;

// R766 octal-constant -> O ' digit [digit]... ' | O " digit [digit]... "
OCTALCONSTANT: O APOSTROPHE DIGIT+? APOSTROPHE | O QUOTE DIGIT+? QUOTE;

// R767 hex-constant -> Z ' hex-digit [hex-digit]... ' | Z " hex-digit [hex-digit]... "
HEXCONSTANT: Z APOSTROPHE HEXDIGIT+? APOSTROPHE | Z QUOTE HEXDIGIT+? QUOTE;

//R0003 RepChar
SQUOTE_REF_CHAR: SQUOTE (~[\u0000-\u001F])*?  SQUOTE;

DQUOTE_REP_CHAR: DQUOTE (~[\u0000-\u001F])*?  DQUOTE;

REALEXPONENTLETTER: DIGITSTRING DOT DIGITSTRING? EXPONENTLETTER | DOT DIGITSTRING EXPONENTLETTER | DIGITSTRING EXPONENTLETTER;

// R603 name -> letter [alphanumeric-character]...
NAME: LETTER (ALPHANUMERICCHARACTER)*;

// R0002 Letter ->
//         A | B | C | D | E | F | G | H | I | J | K | L | M |
//         N | O | P | Q | R | S | T | U | V | W | X | Y | Z
LETTER: 'A'..'Z'; 

// R711 digit-string -> digit [digit]...
DIGITSTRING: DIGIT+; 

// R0001 Digit -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
DIGIT: '0'..'9';

// R601 alphanumeric-character -> letter | digit | underscore
ALPHANUMERICCHARACTER: LETTER | DIGIT | UNDERSCORE;

C: 'C';

E: 'E';

D: 'D';

// R716 exponent-letter -> E | D
EXPONENTLETTER: E | D;

B: 'B';

O: 'O';

Z: 'Z';

A: 'A';

F: 'F';

// R768 hex-digit -> digit | A | B | C | D | E | F
HEXDIGIT: DIGIT | A | B | C | D | E | F;


///FORMAT MODE

mode FORMAT_MODE;

FORMAT_LPAREN: '(' -> pushMode(FORMAT_MODE), type(LPAREN);

FORMAT_RPAREN: ')' -> popMode, type(RPAREN);

FORMAT_SPACE: ' ' -> skip;

FORMAT_COMMA: ',' -> type(COMMA);

FORMAT_ASTERIK: '*' -> type(ASTERIK);

FORMAT_UNDERSCORE: '_' -> type(UNDERSCORE);

FORMAT_DIGITSTRING: FORMAT_DIGIT+  -> type(DIGITSTRING);

FORMAT_DIGIT: '0'..'9' -> type(DIGIT);

FORMAT_APOSTROPHE: '\'' -> type(APOSTROPHE);

FORMAT_QUOTE: '"' -> type(QUOTE);

FORMAT_APOSTROPHEREPCHAR: FORMAT_APOSTROPHE (~[\u0000-\u001F\u0027])*?  FORMAT_APOSTROPHE -> type(APOSTROPHEREPCHAR);

FORMAT_QUOTEREPCHAR: FORMAT_QUOTE (~[\u0000-\u001F\u0022])*?  FORMAT_QUOTE -> type(QUOTEREPCHAR);

P: 'P';

DC: 'DC';

DP: 'DP';

LZS: 'LZS';

LZP: 'LZP';

LZ: 'LZ';

RU: 'RU';

RD: 'RD';

RZ: 'RZ';

RN: 'RN';

RC: 'RC';

RP: 'RP';

BN: 'BN';

BZ: 'BZ';

SS: 'SS';

SP: 'SP';

S: 'S';

T: 'T';

TL: 'TL';

TR: 'TR';

X: 'X';

I: 'I';

FORMAT_B: 'B' -> type(B);

FORMAT_O: 'O' -> type(O);

FORMAT_Z: 'Z' -> type(Z);

FORMAT_F: 'F' -> type(F);

FORMAT_E: 'E' -> type(E);

EN: 'EN';

ES: 'ES';

EX: 'EX';

G: 'G';

L: 'L';

FORMAT_A: 'A' -> type(A);

AT: 'AT';

FORMAT_D: 'D' -> type(D);

DT: 'DT';

FORMAT_NAME: FORMAT_LETTER (FORMAT_ALPHANUMERICCHARACTER)* -> type(NAME);

FORMAT_LETTER: 'A'..'Z' -> type(LETTER); 

FORMAT_ALPHANUMERICCHARACTER: (FORMAT_LETTER | FORMAT_DIGIT | FORMAT_UNDERSCORE) -> type(ALPHANUMERICCHARACTER);

FORMAT_SLASH: '/' -> type(SLASH);

FORMAT_COLON: ':' -> type(COLON);

FORMAT_PLUS: '+' -> type(PLUS);

FORMAT_MINUS: '-' -> type(MINUS);

FORMAT_DOT: '.' -> type(DOT);