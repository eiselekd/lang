/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of zig, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#ifndef ZIG_ANALYZE_HPP
#define ZIG_ANALYZE_HPP

#include "all_types.hpp"

void semantic_analyze(CodeGen *g);
ErrorMsg *add_node_error(CodeGen *g, AstNode *node, Buf *msg);
ErrorMsg *add_error_note(CodeGen *g, ErrorMsg *parent_msg, AstNode *node, Buf *msg);
TypeTableEntry *new_type_table_entry(TypeTableEntryId id);
TypeTableEntry *get_pointer_to_type(CodeGen *g, TypeTableEntry *child_type, bool is_const);
TypeTableEntry *get_pointer_to_type_extra(CodeGen *g, TypeTableEntry *child_type, bool is_const,
        bool is_volatile, uint32_t byte_alignment, uint32_t bit_offset, uint32_t unaligned_bit_count);
uint64_t type_size(CodeGen *g, TypeTableEntry *type_entry);
uint64_t type_size_bits(CodeGen *g, TypeTableEntry *type_entry);
TypeTableEntry **get_int_type_ptr(CodeGen *g, bool is_signed, uint32_t size_in_bits);
TypeTableEntry *get_int_type(CodeGen *g, bool is_signed, uint32_t size_in_bits);
TypeTableEntry **get_c_int_type_ptr(CodeGen *g, CIntType c_int_type);
TypeTableEntry *get_c_int_type(CodeGen *g, CIntType c_int_type);
TypeTableEntry *get_fn_type(CodeGen *g, FnTypeId *fn_type_id);
TypeTableEntry *get_maybe_type(CodeGen *g, TypeTableEntry *child_type);
TypeTableEntry *get_array_type(CodeGen *g, TypeTableEntry *child_type, uint64_t array_size);
TypeTableEntry *get_slice_type(CodeGen *g, TypeTableEntry *ptr_type);
TypeTableEntry *get_partial_container_type(CodeGen *g, Scope *scope, ContainerKind kind,
        AstNode *decl_node, const char *name, ContainerLayout layout);
TypeTableEntry *get_smallest_unsigned_int_type(CodeGen *g, uint64_t x);
TypeTableEntry *get_error_type(CodeGen *g, TypeTableEntry *child_type);
TypeTableEntry *get_bound_fn_type(CodeGen *g, FnTableEntry *fn_entry);
TypeTableEntry *get_opaque_type(CodeGen *g, Scope *scope, AstNode *source_node, const char *name);
TypeTableEntry *get_struct_type(CodeGen *g, const char *type_name, const char *field_names[],
        TypeTableEntry *field_types[], size_t field_count);
TypeTableEntry *get_test_fn_type(CodeGen *g);
bool handle_is_ptr(TypeTableEntry *type_entry);
void find_libc_include_path(CodeGen *g);
void find_libc_lib_path(CodeGen *g);

bool type_has_bits(TypeTableEntry *type_entry);


ImportTableEntry *add_source_file(CodeGen *g, PackageTableEntry *package, Buf *abs_full_path, Buf *source_code);


// TODO move these over, these used to be static
bool types_match_const_cast_only(TypeTableEntry *expected_type, TypeTableEntry *actual_type);
VariableTableEntry *find_variable(CodeGen *g, Scope *orig_context, Buf *name);
Tld *find_decl(CodeGen *g, Scope *scope, Buf *name);
void resolve_top_level_decl(CodeGen *g, Tld *tld, bool pointer_only, AstNode *source_node);
bool type_is_codegen_pointer(TypeTableEntry *type);
TypeTableEntry *get_codegen_ptr_type(TypeTableEntry *type);
uint32_t get_ptr_align(TypeTableEntry *type);
TypeTableEntry *validate_var_type(CodeGen *g, AstNode *source_node, TypeTableEntry *type_entry);
TypeTableEntry *container_ref_type(TypeTableEntry *type_entry);
bool type_is_complete(TypeTableEntry *type_entry);
bool type_is_invalid(TypeTableEntry *type_entry);
bool type_has_zero_bits_known(TypeTableEntry *type_entry);
void resolve_container_type(CodeGen *g, TypeTableEntry *type_entry);
TypeStructField *find_struct_type_field(TypeTableEntry *type_entry, Buf *name);
ScopeDecls *get_container_scope(TypeTableEntry *type_entry);
TypeEnumField *find_enum_type_field(TypeTableEntry *enum_type, Buf *name);
bool is_container_ref(TypeTableEntry *type_entry);
void scan_decls(CodeGen *g, ScopeDecls *decls_scope, AstNode *node);
void scan_import(CodeGen *g, ImportTableEntry *import);
void preview_use_decl(CodeGen *g, AstNode *node);
void resolve_use_decl(CodeGen *g, AstNode *node);
FnTableEntry *scope_fn_entry(Scope *scope);
ImportTableEntry *get_scope_import(Scope *scope);
void init_tld(Tld *tld, TldId id, Buf *name, VisibMod visib_mod, AstNode *source_node, Scope *parent_scope);
VariableTableEntry *add_variable(CodeGen *g, AstNode *source_node, Scope *parent_scope, Buf *name,
    bool is_const, ConstExprValue *init_value, Tld *src_tld);
TypeTableEntry *analyze_type_expr(CodeGen *g, Scope *scope, AstNode *node);
FnTableEntry *create_fn(AstNode *proto_node);
FnTableEntry *create_fn_raw(FnInline inline_value, GlobalLinkageId linkage);
void init_fn_type_id(FnTypeId *fn_type_id, AstNode *proto_node, size_t param_count_alloc);
AstNode *get_param_decl_node(FnTableEntry *fn_entry, size_t index);
FnTableEntry *scope_get_fn_if_root(Scope *scope);
bool type_requires_comptime(TypeTableEntry *type_entry);
void ensure_complete_type(CodeGen *g, TypeTableEntry *type_entry);
void type_ensure_zero_bits_known(CodeGen *g, TypeTableEntry *type_entry);
void complete_enum(CodeGen *g, TypeTableEntry *enum_type);
bool ir_get_var_is_comptime(VariableTableEntry *var);
bool const_values_equal(ConstExprValue *a, ConstExprValue *b);
void eval_min_max_value(CodeGen *g, TypeTableEntry *type_entry, ConstExprValue *const_val, bool is_max);
void eval_min_max_value_int(CodeGen *g, TypeTableEntry *int_type, BigInt *bigint, bool is_max);

void render_const_value(CodeGen *g, Buf *buf, ConstExprValue *const_val);
void define_local_param_variables(CodeGen *g, FnTableEntry *fn_table_entry, VariableTableEntry **arg_vars);
void analyze_fn_ir(CodeGen *g, FnTableEntry *fn_table_entry, AstNode *return_type_node);

ScopeBlock *create_block_scope(AstNode *node, Scope *parent);
ScopeDefer *create_defer_scope(AstNode *node, Scope *parent);
ScopeDeferExpr *create_defer_expr_scope(AstNode *node, Scope *parent);
Scope *create_var_scope(AstNode *node, Scope *parent, VariableTableEntry *var);
ScopeCImport *create_cimport_scope(AstNode *node, Scope *parent);
ScopeLoop *create_loop_scope(AstNode *node, Scope *parent);
ScopeFnDef *create_fndef_scope(AstNode *node, Scope *parent, FnTableEntry *fn_entry);
ScopeDecls *create_decls_scope(AstNode *node, Scope *parent, TypeTableEntry *container_type, ImportTableEntry *import);
Scope *create_comptime_scope(AstNode *node, Scope *parent);

void init_const_str_lit(CodeGen *g, ConstExprValue *const_val, Buf *str);
ConstExprValue *create_const_str_lit(CodeGen *g, Buf *str);

void init_const_c_str_lit(CodeGen *g, ConstExprValue *const_val, Buf *c_str);
ConstExprValue *create_const_c_str_lit(CodeGen *g, Buf *c_str);

void init_const_unsigned_negative(ConstExprValue *const_val, TypeTableEntry *type, uint64_t x, bool negative);
ConstExprValue *create_const_unsigned_negative(TypeTableEntry *type, uint64_t x, bool negative);

void init_const_signed(ConstExprValue *const_val, TypeTableEntry *type, int64_t x);
ConstExprValue *create_const_signed(TypeTableEntry *type, int64_t x);

void init_const_usize(CodeGen *g, ConstExprValue *const_val, uint64_t x);
ConstExprValue *create_const_usize(CodeGen *g, uint64_t x);

void init_const_float(ConstExprValue *const_val, TypeTableEntry *type, double value);
ConstExprValue *create_const_float(TypeTableEntry *type, double value);

void init_const_enum_tag(ConstExprValue *const_val, TypeTableEntry *type, uint64_t tag);
ConstExprValue *create_const_enum_tag(TypeTableEntry *type, uint64_t tag);

void init_const_bool(CodeGen *g, ConstExprValue *const_val, bool value);
ConstExprValue *create_const_bool(CodeGen *g, bool value);

void init_const_type(CodeGen *g, ConstExprValue *const_val, TypeTableEntry *type_value);
ConstExprValue *create_const_type(CodeGen *g, TypeTableEntry *type_value);

void init_const_runtime(ConstExprValue *const_val, TypeTableEntry *type);
ConstExprValue *create_const_runtime(TypeTableEntry *type);

void init_const_ptr_ref(CodeGen *g, ConstExprValue *const_val, ConstExprValue *pointee_val, bool is_const);
ConstExprValue *create_const_ptr_ref(CodeGen *g, ConstExprValue *pointee_val, bool is_const);

void init_const_ptr_hard_coded_addr(CodeGen *g, ConstExprValue *const_val, TypeTableEntry *pointee_type,
        size_t addr, bool is_const);
ConstExprValue *create_const_ptr_hard_coded_addr(CodeGen *g, TypeTableEntry *pointee_type,
        size_t addr, bool is_const);

void init_const_ptr_array(CodeGen *g, ConstExprValue *const_val, ConstExprValue *array_val,
        size_t elem_index, bool is_const);
ConstExprValue *create_const_ptr_array(CodeGen *g, ConstExprValue *array_val, size_t elem_index, bool is_const);

void init_const_slice(CodeGen *g, ConstExprValue *const_val, ConstExprValue *array_val,
        size_t start, size_t len, bool is_const);
ConstExprValue *create_const_slice(CodeGen *g, ConstExprValue *array_val, size_t start, size_t len, bool is_const);

void init_const_arg_tuple(CodeGen *g, ConstExprValue *const_val, size_t arg_index_start, size_t arg_index_end);
ConstExprValue *create_const_arg_tuple(CodeGen *g, size_t arg_index_start, size_t arg_index_end);

void init_const_undefined(CodeGen *g, ConstExprValue *const_val);

ConstExprValue *create_const_vals(size_t count);

TypeTableEntry *make_int_type(CodeGen *g, bool is_signed, uint32_t size_in_bits);
ConstParent *get_const_val_parent(CodeGen *g, ConstExprValue *value);
TypeTableEntry *create_enum_tag_type(CodeGen *g, TypeTableEntry *enum_type, TypeTableEntry *int_type);
void expand_undef_array(CodeGen *g, ConstExprValue *const_val);
void update_compile_var(CodeGen *g, Buf *name, ConstExprValue *value);

const char *type_id_name(TypeTableEntryId id);
TypeTableEntryId type_id_at_index(size_t index);
size_t type_id_len();
size_t type_id_index(TypeTableEntryId id);
TypeTableEntry *get_generic_fn_type(CodeGen *g, FnTypeId *fn_type_id);
bool type_is_copyable(CodeGen *g, TypeTableEntry *type_entry);
LinkLib *create_link_lib(Buf *name);
bool calling_convention_does_first_arg_return(CallingConvention cc);
LinkLib *add_link_lib(CodeGen *codegen, Buf *lib);
void add_link_lib_symbol(CodeGen *g, Buf *lib_name, Buf *symbol_name);

uint32_t get_abi_alignment(CodeGen *g, TypeTableEntry *type_entry);
TypeTableEntry *get_align_amt_type(CodeGen *g);

#endif
