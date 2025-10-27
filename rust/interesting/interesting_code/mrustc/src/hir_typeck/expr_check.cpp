/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * hir_typeck/expr_check.cpp
 * - Expression type checking (validation pass)
 */
#include <hir/visitor.hpp>
#include <hir/expr.hpp>
#include <hir_typeck/static.hpp>
#include "main_bindings.hpp"
#include <algorithm>

namespace {
    typedef ::std::vector< ::std::pair< ::HIR::Pattern, ::HIR::TypeRef> >   t_args;

    class ExprVisitor_Validate:
        public ::HIR::ExprVisitor
    {
        const StaticTraitResolve&  m_resolve;
        //const t_args&   m_args;
        const ::HIR::TypeRef&   ret_type;
        ::std::vector< const ::HIR::TypeRef*>   closure_ret_types;
        ::std::vector<const ::HIR::ExprNode_Loop*>  m_loops;

        ::HIR::SimplePath   m_lang_Index;

    public:
        ExprVisitor_Validate(const StaticTraitResolve& res, const t_args& args, const ::HIR::TypeRef& ret_type):
            m_resolve(res),
            //m_args(args),
            ret_type(ret_type)
        {
            m_lang_Index = m_resolve.m_crate.get_lang_item_path_opt("index");
        }

        void visit_root(::HIR::ExprPtr& node_ptr)
        {
            const auto& sp = node_ptr->span();
            node_ptr->visit(*this);

            // Monomorphise erased type
            ::HIR::TypeRef  new_ret_type = clone_ty_with(sp, ret_type, [&](const auto& tpl, auto& rv)->bool {
                if( tpl.m_data.is_ErasedType() )
                {
                    const auto& e = tpl.m_data.as_ErasedType();
                    ASSERT_BUG(sp, e.m_index < node_ptr.m_erased_types.size(), "Erased type index OOB - " << e.m_origin << " " << e.m_index << " >= " << node_ptr.m_erased_types.size());
                    // TODO: Emit checks on bounds
                    rv = node_ptr.m_erased_types[e.m_index].clone();
                    return true;
                }
                return false;
                });
            m_resolve.expand_associated_types(sp, new_ret_type);

            check_types_equal(sp, new_ret_type, node_ptr->m_res_type);
        }

        void visit(::HIR::ExprNode_Block& node) override
        {
            TRACE_FUNCTION_F(&node << " { ... }");
            for(auto& n : node.m_nodes)
            {
                n->visit(*this);
            }
            if( node.m_value_node )
            {
                node.m_value_node->visit(*this);
                check_types_equal(node.span(), node.m_res_type, node.m_value_node->m_res_type);
            }
        }
        void visit(::HIR::ExprNode_Asm& node) override
        {
            TRACE_FUNCTION_F(&node << " asm! ...");

            // TODO: Check result types
            for(auto& v : node.m_outputs)
            {
                v.value->visit(*this);
            }
            for(auto& v : node.m_inputs)
            {
                v.value->visit(*this);
            }
        }
        void visit(::HIR::ExprNode_Return& node) override
        {
            TRACE_FUNCTION_F(&node << " return ...");
            // Check against return type
            const auto& ret_ty = ( this->closure_ret_types.size() > 0 ? *this->closure_ret_types.back() : this->ret_type );
            check_types_equal(ret_ty, node.m_value);
            node.m_value->visit(*this);
        }
        void visit(::HIR::ExprNode_Loop& node) override
        {
            TRACE_FUNCTION_F(&node << " loop { ... }");
            m_loops.push_back(&node);
            node.m_code->visit(*this);
            m_loops.pop_back();
        }
        void visit(::HIR::ExprNode_LoopControl& node) override
        {
            TRACE_FUNCTION_F(&node << " " << (node.m_continue ? "continue" : "break") << " '" << node.m_label);
            // TODO: Validate `break` return value
            if( node.m_value )
            {
                node.m_value->visit(*this);
            }

            if( !node.m_continue )
            {
                ::HIR::TypeRef  unit = ::HIR::TypeRef::new_unit();
                const auto& ty = (node.m_value ? node.m_value->m_res_type : unit);
                const ::HIR::ExprNode_Loop* loop;
                if( node.m_label == "" ) {
                    ASSERT_BUG(node.span(), !m_loops.empty(), "Break with no loop");
                    loop = m_loops.back();
                }
                else {
                    auto it = ::std::find_if( m_loops.rbegin(), m_loops.rend(), [&](const auto* lp){ return lp->m_label == node.m_label; } );
                    ASSERT_BUG(node.span(), it != m_loops.rend(), "Break with no matching loop");
                    loop = *it;
                }

                DEBUG("Breaking to " << loop << ", type " << loop->m_res_type);
                check_types_equal(node.span(), loop->m_res_type, ty);
            }
        }
        void visit(::HIR::ExprNode_Let& node) override
        {
            TRACE_FUNCTION_F(&node << " let " << node.m_pattern << ": " << node.m_type);
            if(node.m_value)
            {
                check_types_equal(node.span(), node.m_type, node.m_value->m_res_type);
                node.m_value->visit(*this);
            }
        }
        void visit(::HIR::ExprNode_Match& node) override
        {
            TRACE_FUNCTION_F(&node << " match ...");
            node.m_value->visit(*this);
            for(auto& arm : node.m_arms)
            {
                check_types_equal(node.span(), node.m_res_type, arm.m_code->m_res_type);
                arm.m_code->visit( *this );
            }
        }
        void visit(::HIR::ExprNode_If& node) override
        {
            TRACE_FUNCTION_F(&node << " if ... { ... } else { ... }");
            node.m_cond->visit( *this );
            check_types_equal(node.span(), node.m_res_type, node.m_true->m_res_type);
            if( node.m_false )
            {
                check_types_equal(node.span(), node.m_res_type, node.m_false->m_res_type);
            }
        }
        void visit(::HIR::ExprNode_Assign& node) override
        {
            TRACE_FUNCTION_F(&node << "... ?= ...");

            if( node.m_op == ::HIR::ExprNode_Assign::Op::None ) {
                check_types_equal(node.span(), node.m_slot->m_res_type, node.m_value->m_res_type);
            }
            else {
                // Type inferrence using the +=
                // - "" as type name to indicate that it's just using the trait magic?
                const char *lang_item = nullptr;
                switch( node.m_op )
                {
                case ::HIR::ExprNode_Assign::Op::None:  throw "";
                case ::HIR::ExprNode_Assign::Op::Add: lang_item = "add_assign"; break;
                case ::HIR::ExprNode_Assign::Op::Sub: lang_item = "sub_assign"; break;
                case ::HIR::ExprNode_Assign::Op::Mul: lang_item = "mul_assign"; break;
                case ::HIR::ExprNode_Assign::Op::Div: lang_item = "div_assign"; break;
                case ::HIR::ExprNode_Assign::Op::Mod: lang_item = "rem_assign"; break;
                case ::HIR::ExprNode_Assign::Op::And: lang_item = "bitand_assign"; break;
                case ::HIR::ExprNode_Assign::Op::Or : lang_item = "bitor_assign" ; break;
                case ::HIR::ExprNode_Assign::Op::Xor: lang_item = "bitxor_assign"; break;
                case ::HIR::ExprNode_Assign::Op::Shr: lang_item = "shr_assign"; break;
                case ::HIR::ExprNode_Assign::Op::Shl: lang_item = "shl_assign"; break;
                }
                assert(lang_item);
                const auto& trait_path = this->get_lang_item_path(node.span(), lang_item);

                check_associated_type(node.span(),  ::HIR::TypeRef(),  trait_path, { node.m_value->m_res_type.clone() }, node.m_slot->m_res_type,  "");
            }

            node.m_slot->visit( *this );
            node.m_value->visit( *this );
        }
        void visit(::HIR::ExprNode_BinOp& node) override
        {
            TRACE_FUNCTION_F(&node << "... "<<::HIR::ExprNode_BinOp::opname(node.m_op)<<" ...");

            switch(node.m_op)
            {
            case ::HIR::ExprNode_BinOp::Op::CmpEqu:
            case ::HIR::ExprNode_BinOp::Op::CmpNEqu:
            case ::HIR::ExprNode_BinOp::Op::CmpLt:
            case ::HIR::ExprNode_BinOp::Op::CmpLtE:
            case ::HIR::ExprNode_BinOp::Op::CmpGt:
            case ::HIR::ExprNode_BinOp::Op::CmpGtE: {
                check_types_equal(node.span(), ::HIR::TypeRef(::HIR::CoreType::Bool), node.m_res_type);

                const char* item_name = nullptr;
                switch(node.m_op)
                {
                case ::HIR::ExprNode_BinOp::Op::CmpEqu:  item_name = "eq";  break;
                case ::HIR::ExprNode_BinOp::Op::CmpNEqu: item_name = "eq";  break;
                case ::HIR::ExprNode_BinOp::Op::CmpLt:   item_name = "ord"; break;
                case ::HIR::ExprNode_BinOp::Op::CmpLtE:  item_name = "ord"; break;
                case ::HIR::ExprNode_BinOp::Op::CmpGt:   item_name = "ord"; break;
                case ::HIR::ExprNode_BinOp::Op::CmpGtE:  item_name = "ord"; break;
                default: break;
                }
                assert(item_name);
                const auto& op_trait = this->get_lang_item_path(node.span(), item_name);

                check_associated_type(node.span(),  ::HIR::TypeRef(),  op_trait, { node.m_right->m_res_type.clone() }, node.m_left->m_res_type,  "");
                break; }

            case ::HIR::ExprNode_BinOp::Op::BoolAnd:
            case ::HIR::ExprNode_BinOp::Op::BoolOr:
                // No validation needed, result forced in typeck
                break;
            default: {
                const char* item_name = nullptr;
                switch(node.m_op)
                {
                case ::HIR::ExprNode_BinOp::Op::CmpEqu:  throw "";
                case ::HIR::ExprNode_BinOp::Op::CmpNEqu: throw "";
                case ::HIR::ExprNode_BinOp::Op::CmpLt:   throw "";
                case ::HIR::ExprNode_BinOp::Op::CmpLtE:  throw "";
                case ::HIR::ExprNode_BinOp::Op::CmpGt:   throw "";
                case ::HIR::ExprNode_BinOp::Op::CmpGtE:  throw "";
                case ::HIR::ExprNode_BinOp::Op::BoolAnd: throw "";
                case ::HIR::ExprNode_BinOp::Op::BoolOr:  throw "";

                case ::HIR::ExprNode_BinOp::Op::Add: item_name = "add"; break;
                case ::HIR::ExprNode_BinOp::Op::Sub: item_name = "sub"; break;
                case ::HIR::ExprNode_BinOp::Op::Mul: item_name = "mul"; break;
                case ::HIR::ExprNode_BinOp::Op::Div: item_name = "div"; break;
                case ::HIR::ExprNode_BinOp::Op::Mod: item_name = "rem"; break;

                case ::HIR::ExprNode_BinOp::Op::And: item_name = "bitand"; break;
                case ::HIR::ExprNode_BinOp::Op::Or:  item_name = "bitor";  break;
                case ::HIR::ExprNode_BinOp::Op::Xor: item_name = "bitxor"; break;

                case ::HIR::ExprNode_BinOp::Op::Shr: item_name = "shr"; break;
                case ::HIR::ExprNode_BinOp::Op::Shl: item_name = "shl"; break;
                }
                assert(item_name);
                const auto& op_trait = this->get_lang_item_path(node.span(), item_name);

                check_associated_type(node.span(),  node.m_res_type,  op_trait, { node.m_right->m_res_type.clone() }, node.m_left->m_res_type,  "Output");
                break; }
            }

            node.m_left ->visit( *this );
            node.m_right->visit( *this );
        }

        void visit(::HIR::ExprNode_UniOp& node) override
        {
            TRACE_FUNCTION_F(&node << " " << ::HIR::ExprNode_UniOp::opname(node.m_op) << "...");
            switch(node.m_op)
            {
            case ::HIR::ExprNode_UniOp::Op::Invert:
                check_associated_type(node.span(), node.m_res_type,  this->get_lang_item_path(node.span(), "not"), {}, node.m_value->m_res_type, "Output");
                break;
            case ::HIR::ExprNode_UniOp::Op::Negate:
                check_associated_type(node.span(), node.m_res_type,  this->get_lang_item_path(node.span(), "neg"), {}, node.m_value->m_res_type, "Output");
                break;
            }
            node.m_value->visit( *this );
        }
        void visit(::HIR::ExprNode_Borrow& node) override
        {
            TRACE_FUNCTION_F(&node << " &_ ...");
            check_types_equal(node.span(), node.m_res_type, ::HIR::TypeRef::new_borrow(node.m_type, node.m_value->m_res_type.clone()));
            node.m_value->visit( *this );
        }
        void visit(::HIR::ExprNode_Index& node) override
        {
            TRACE_FUNCTION_F(&node << " ... [ ... ]");
            check_associated_type(node.span(),
                node.m_res_type, m_lang_Index, { node.m_index->m_res_type.clone() }, node.m_value->m_res_type, "Target"
                );

            node.m_value->visit( *this );
            node.m_index->visit( *this );
        }

        void visit(::HIR::ExprNode_Cast& node) override
        {
            TRACE_FUNCTION_F(&node << " ... as " << node.m_res_type);
            const Span& sp = node.span();

            const auto& src_ty = node.m_value->m_res_type;
            const auto& dst_ty = node.m_res_type;

            if( dst_ty == src_ty ) {
                // Would be nice to delete it, but this is a readonly pass
                return ;
            }

            // Check castability
            TU_MATCH_DEF(::HIR::TypeRef::Data, (dst_ty.m_data), (de),
            (
                ERROR(sp, E0000, "Invalid cast to " << dst_ty);
                ),
            (Pointer,
                TU_MATCH_DEF(::HIR::TypeRef::Data, (src_ty.m_data), (se),
                (
                    ERROR(sp, E0000, "Invalid cast to " << dst_ty << " from " << src_ty);
                    ),
                (Pointer,
                    // TODO: Sized check - can't cast to a fat pointer from a thin one
                    //if( ! this->m_resolve.type_is_sized(*de.inner) ) {
                    //    ERROR(sp, E0000, "Invalid cast to fat pointer " << dst_ty << " from " << src_ty);
                    //}
                    ),
                (Primitive,
                    switch(se)
                    {
                    case ::HIR::CoreType::Bool:
                    case ::HIR::CoreType::Char:
                    case ::HIR::CoreType::Str:
                    case ::HIR::CoreType::F32:
                    case ::HIR::CoreType::F64:
                        ERROR(sp, E0000, "Invalid cast to " << dst_ty << " from " << src_ty);
                    default:
                        break;
                    }
                    //if( ! this->m_resolve.type_is_sized(*de.inner) ) {
                    //    ERROR(sp, E0000, "Invalid cast to fat pointer " << dst_ty << " from " << src_ty);
                    //}
                    ),
                (Function,
                    if( *de.inner != ::HIR::TypeRef::new_unit() && *de.inner != ::HIR::CoreType::U8 && *de.inner != ::HIR::CoreType::I8 ) {
                        ERROR(sp, E0000, "Invalid cast to " << dst_ty << " from " << src_ty);
                    }
                    ),
                (Borrow,
                    this->check_types_equal(sp, *de.inner, *se.inner);
                    )
                )
                ),
            (Primitive,
                // TODO: Check cast to primitive
                )
            )

            node.m_value->visit( *this );
        }
        void visit(::HIR::ExprNode_Unsize& node) override
        {
            TRACE_FUNCTION_F(&node << " ... : " << node.m_res_type);
            const Span& sp = node.span();

            const auto& src_ty = node.m_value->m_res_type;
            const auto& dst_ty = node.m_res_type;

            if( src_ty == dst_ty )
            {
            }
            else if( src_ty.m_data.is_Borrow() && dst_ty.m_data.is_Borrow() )
            {
                const auto& se = src_ty.m_data.as_Borrow();
                const auto& de = dst_ty.m_data.as_Borrow();
                if( se.type != de.type ) {
                    ERROR(sp, E0000, "Invalid unsizing operation to " << dst_ty << " from " << src_ty << " - Borrow class mismatch");
                }
                const auto& src_ty = *se.inner;
                const auto& dst_ty = *de.inner;

                const auto& lang_Unsize = this->get_lang_item_path(node.span(), "unsize");
                // _ == < `src_ty` as Unsize< `dst_ty` >::""
                check_associated_type(sp, ::HIR::TypeRef(), lang_Unsize, { dst_ty.clone() }, src_ty, "");
            }
            else if( src_ty.m_data.is_Borrow() || dst_ty.m_data.is_Borrow() )
            {
                ERROR(sp, E0000, "Invalid unsizing operation to " << dst_ty << " from " << src_ty);
            }
            else
            {
                const auto& lang_CoerceUnsized = this->get_lang_item_path(node.span(), "coerce_unsized");
                // _ == < `src_ty` as CoerceUnsized< `dst_ty` >::""
                check_associated_type(sp, ::HIR::TypeRef(), lang_CoerceUnsized, { dst_ty.clone() }, src_ty, "");
            }

            node.m_value->visit( *this );
        }
        void visit(::HIR::ExprNode_Deref& node) override
        {
            TRACE_FUNCTION_F(&node << " *...");
            const auto& ty = node.m_value->m_res_type;

            if( ty.m_data.is_Pointer() ) {
                check_types_equal(node.span(), node.m_res_type, *ty.m_data.as_Pointer().inner);
            }
            else if( ty.m_data.is_Borrow() ) {
                check_types_equal(node.span(), node.m_res_type, *ty.m_data.as_Borrow().inner);
            }
            else {
                check_associated_type(node.span(),
                    node.m_res_type,
                    this->get_lang_item_path(node.span(), "deref"), {}, node.m_value->m_res_type, "Target"
                    );
            }

            node.m_value->visit( *this );
        }
        void visit(::HIR::ExprNode_Emplace& node) override
        {
            switch( node.m_type )
            {
            case ::HIR::ExprNode_Emplace::Type::Noop:
                assert( !node.m_place );

                check_types_equal(node.span(), node.m_res_type, node.m_value->m_res_type);
                break;
            case ::HIR::ExprNode_Emplace::Type::Boxer:
                // TODO: Check trait and associated type
                break;
            case ::HIR::ExprNode_Emplace::Type::Placer:
                // TODO: Check trait
                break;
            }

            if( node.m_place )
                this->visit_node_ptr(node.m_place);
            this->visit_node_ptr(node.m_value);
        }
        void visit(::HIR::ExprNode_TupleVariant& node) override
        {
            TRACE_FUNCTION_F(&node << " " << node.m_path << "(...,) [" << (node.m_is_struct ? "struct" : "enum") << "]");
            const auto& sp = node.span();

            // - Create ivars in path, and set result type
            const auto& ty = node.m_res_type;

            const ::HIR::t_tuple_fields* fields_ptr = nullptr;
            ASSERT_BUG(sp, ty.m_data.is_Path(), "Result type of _TupleVariant isn't Path");
            TU_MATCH(::HIR::TypeRef::TypePathBinding, (ty.m_data.as_Path().binding), (e),
            (Unbound,
                BUG(sp, "Unbound type in _TupleVariant - " << ty);
                ),
            (Opaque,
                BUG(sp, "Opaque type binding in _TupleVariant - " << ty);
                ),
            (Enum,
                const auto& var_name = node.m_path.m_path.m_components.back();
                const auto& enm = *e;
                size_t idx = enm.find_variant(var_name);
                const auto& var_ty = enm.m_data.as_Data()[idx].type;
                const auto& str = *var_ty.m_data.as_Path().binding.as_Struct();
                ASSERT_BUG(sp, str.m_data.is_Tuple(), "Pointed variant of TupleVariant (" << node.m_path << ") isn't a Tuple");
                fields_ptr = &str.m_data.as_Tuple();
                ),
            (Union,
                BUG(sp, "Union in TupleVariant");
                ),
            (Struct,
                ASSERT_BUG(sp, e->m_data.is_Tuple(), "Pointed struct in TupleVariant (" << node.m_path << ") isn't a Tuple");
                fields_ptr = &e->m_data.as_Tuple();
                )
            )
            assert(fields_ptr);
            const ::HIR::t_tuple_fields& fields = *fields_ptr;
            ASSERT_BUG(sp, fields.size() == node.m_args.size(), "");

            // Bind fields with type params (coercable)
            // TODO: Remove use of m_arg_types (maybe assert that cache is correct?)
            for( unsigned int i = 0; i < node.m_args.size(); i ++ )
            {
                const auto& des_ty_r = fields[i].ent;
                const auto* des_ty = &des_ty_r;
                if( monomorphise_type_needed(des_ty_r) ) {
                    assert( node.m_arg_types[i] != ::HIR::TypeRef() );
                    des_ty = &node.m_arg_types[i];
                }

                check_types_equal(*des_ty, node.m_args[i]);
            }

            for( auto& val : node.m_args ) {
                val->visit( *this );
            }
        }
        void visit(::HIR::ExprNode_StructLiteral& node) override
        {
            TRACE_FUNCTION_F(&node << " " << node.m_path << "{...} [" << (node.m_is_struct ? "struct" : "enum") << "]");
            const auto& sp = node.span();
            if( node.m_base_value) {
                check_types_equal( node.m_base_value->span(), node.m_res_type, node.m_base_value->m_res_type );
            }

            // - Create ivars in path, and set result type
            const auto& ty = node.m_res_type;
            ASSERT_BUG(sp, ty.m_data.is_Path(), "Result type of _StructLiteral isn't Path");

            const ::HIR::t_struct_fields* fields_ptr = nullptr;
            TU_MATCH(::HIR::TypeRef::TypePathBinding, (ty.m_data.as_Path().binding), (e),
            (Unbound, ),
            (Opaque, ),
            (Enum,
                const auto& var_name = node.m_path.m_path.m_components.back();
                const auto& enm = *e;
                auto idx = enm.find_variant(var_name);
                ASSERT_BUG(sp, idx != SIZE_MAX, "");
                ASSERT_BUG(sp, enm.m_data.is_Data(), "");
                const auto& var = enm.m_data.as_Data()[idx];

                const auto& str = *var.type.m_data.as_Path().binding.as_Struct();
                ASSERT_BUG(sp, var.is_struct, "Struct literal for enum on non-struct variant");
                fields_ptr = &str.m_data.as_Named();
                ),
            (Union,
                TODO(sp, "Union in StructLiteral");
                ),
            (Struct,
                if( e->m_data.is_Unit() )
                {
                    ASSERT_BUG(node.span(), node.m_values.size() == 0, "Values provided for unit-like struct");
                    ASSERT_BUG(node.span(), ! node.m_base_value, "Values provided for unit-like struct");
                    return ;
                }

                ASSERT_BUG(node.span(), e->m_data.is_Named(), "StructLiteral not pointing to a braced struct, instead " << e->m_data.tag_str() << " - " << ty);
                fields_ptr = &e->m_data.as_Named();
                )
            )
            ASSERT_BUG(node.span(), fields_ptr, "Didn't get field for path in _StructLiteral - " << ty);
            const ::HIR::t_struct_fields& fields = *fields_ptr;

            #if 1
            const auto& ty_params = node.m_path.m_params.m_types;
            auto monomorph_cb = [&](const auto& gt)->const ::HIR::TypeRef& {
                const auto& ge = gt.m_data.as_Generic();
                if( ge.binding == 0xFFFF ) {
                    return ty;
                }
                else if( ge.binding < 256 ) {
                    if( ge.binding >= ty_params.size() ) {
                        BUG(node.span(), "Type parameter index out of range (#" << ge.binding << " " << ge.name << ")");
                    }
                    return ty_params[ge.binding];
                }
                else {
                    BUG(node.span(), "Method-level parameter on struct (#" << ge.binding << " " << ge.name << ")");
                }
                };
            #endif

            // Bind fields with type params (coercable)
            for( auto& val : node.m_values)
            {
                const auto& name = val.first;
                auto it = ::std::find_if(fields.begin(), fields.end(), [&](const auto& v)->bool{ return v.first == name; });
                assert(it != fields.end());
                const auto& des_ty_r = it->second.ent;
                auto& des_ty_cache = node.m_value_types[it - fields.begin()];
                const auto* des_ty = &des_ty_r;

                if( monomorphise_type_needed(des_ty_r) ) {
                    assert( des_ty_cache != ::HIR::TypeRef() );
                    des_ty_cache = monomorphise_type_with(node.span(), des_ty_r, monomorph_cb);
                    m_resolve.expand_associated_types(node.span(), des_ty_cache);
                    des_ty = &des_ty_cache;
                }
                DEBUG("." << name << " : " << *des_ty);
                check_types_equal(*des_ty, val.second);
            }

            for( auto& val : node.m_values ) {
                val.second->visit( *this );
            }
            if( node.m_base_value ) {
                node.m_base_value->visit( *this );
            }
        }
        void visit(::HIR::ExprNode_UnionLiteral& node) override
        {
            // TODO: Check.
            node.m_value->visit(*this);
        }
        void visit(::HIR::ExprNode_UnitVariant& node) override
        {
            TRACE_FUNCTION_F(&node << " " << node.m_path << " [" << (node.m_is_struct ? "struct" : "enum") << "]");
            const auto& sp = node.span();
            const auto& ty = node.m_res_type;
            ASSERT_BUG(sp, ty.m_data.is_Path(), "Result type of _UnitVariant isn't Path");

            TU_MATCH(::HIR::TypeRef::TypePathBinding, (ty.m_data.as_Path().binding), (e),
            (Unbound, ),
            (Opaque, ),
            (Enum,
                const auto& var_name = node.m_path.m_path.m_components.back();
                const auto& enm = *e;
                if(const auto* e = enm.m_data.opt_Data())
                {
                    auto idx = enm.find_variant(var_name);
                    ASSERT_BUG(sp, idx != SIZE_MAX, "");
                    ASSERT_BUG(sp, (*e)[idx].type == ::HIR::TypeRef::new_unit(), "");
                }
                ),
            (Union,
                BUG(sp, "Union with _UnitVariant");
                ),
            (Struct,
                assert( e->m_data.is_Unit() );
                )
            )
        }

        void visit(::HIR::ExprNode_CallPath& node) override
        {
            const auto& sp = node.span();
            TRACE_FUNCTION_F(&node << " " << node.m_path << "(..., )");

            for( auto& val : node.m_args ) {
                val->visit( *this );
            }

            // Do function resolution again, this time with concrete types.
            const auto& path = node.m_path;
            /*const*/ auto& cache = node.m_cache;

            const ::HIR::Function*  fcn_ptr = nullptr;
            ::std::function<const ::HIR::TypeRef&(const ::HIR::TypeRef&)>    monomorph_cb;

            TU_MATCH(::HIR::Path::Data, (path.m_data), (e),
            (Generic,
                const auto& path_params = e.m_params;

                const auto& fcn = m_resolve.m_crate.get_function_by_path(sp, e.m_path);
                fcn_ptr = &fcn;
                cache.m_fcn_params = &fcn.m_params;

                monomorph_cb = [&](const auto& gt)->const ::HIR::TypeRef& {
                        const auto& e = gt.m_data.as_Generic();
                        if( e.name == "Self" || e.binding == 0xFFFF )
                            TODO(sp, "Handle 'Self' when monomorphising");
                        if( e.binding < 256 ) {
                            BUG(sp, "Impl-level parameter on free function (#" << e.binding << " " << e.name << ")");
                        }
                        else if( e.binding < 512 ) {
                            auto idx = e.binding - 256;
                            if( idx >= path_params.m_types.size() ) {
                                BUG(sp, "Generic param out of input range - " << idx << " '"<<e.name<<"' >= " << path_params.m_types.size());
                            }
                            return path_params.m_types[idx];
                        }
                        else {
                            BUG(sp, "Generic bounding out of total range");
                        }
                    };
                ),
            (UfcsKnown,
                const auto& trait_params = e.trait.m_params;
                const auto& path_params = e.params;

                const auto& trait = m_resolve.m_crate.get_trait_by_path(sp, e.trait.m_path);
                if( trait.m_values.count(e.item) == 0 ) {
                    BUG(sp, "Method '" << e.item << "' of trait " << e.trait.m_path << " doesn't exist");
                }

                const auto& fcn = trait.m_values.at(e.item).as_Function();
                cache.m_fcn_params = &fcn.m_params;
                cache.m_top_params = &trait.m_params;

                // Add a bound requiring the Self type impl the trait
                check_associated_type(sp, ::HIR::TypeRef(), e.trait.m_path, e.trait.m_params, *e.type, "");

                fcn_ptr = &fcn;

                monomorph_cb = monomorphise_type_get_cb(sp, &*e.type, &trait_params, &path_params);
                ),
            (UfcsUnknown,
                TODO(sp, "Hit a UfcsUnknown (" << path << ") - Is this an error?");
                ),
            (UfcsInherent,
                // - Locate function (and impl block)
                const ::HIR::TypeImpl* impl_ptr = nullptr;
                m_resolve.m_crate.find_type_impls(*e.type, [&](const auto& ty)->const ::HIR::TypeRef& { return ty; },
                    [&](const auto& impl) {
                        DEBUG("- impl" << impl.m_params.fmt_args() << " " << impl.m_type);
                        auto it = impl.m_methods.find(e.item);
                        if( it == impl.m_methods.end() )
                            return false;
                        fcn_ptr = &it->second.data;
                        impl_ptr = &impl;
                        return true;
                    });
                if( !fcn_ptr ) {
                    ERROR(sp, E0000, "Failed to locate function " << path);
                }
                assert(impl_ptr);

                cache.m_fcn_params = &fcn_ptr->m_params;


                // NOTE: Trusts the existing cache.
                ASSERT_BUG(sp, e.impl_params.m_types.size() == impl_ptr->m_params.m_types.size(), "");
                auto& impl_params = e.impl_params;

                // Create monomorphise callback
                const auto& fcn_params = e.params;
                monomorph_cb = monomorphise_type_get_cb(sp, &*e.type, &impl_params, &fcn_params);
                )
            )

            assert( fcn_ptr );
            const auto& fcn = *fcn_ptr;

            // --- Monomorphise the argument/return types (into current context)
            cache.m_arg_types.clear();
            for(const auto& arg : fcn.m_args) {
                DEBUG("Arg " << arg.first << ": " << arg.second);
                cache.m_arg_types.push_back( monomorphise_type_with(sp, arg.second,  monomorph_cb, false) );
                m_resolve.expand_associated_types(sp, cache.m_arg_types.back());
                DEBUG("= " << cache.m_arg_types.back());
            }
            DEBUG("Ret " << fcn.m_return);
            // Replace ErasedType and monomorphise
            cache.m_arg_types.push_back( clone_ty_with(sp, fcn.m_return, [&](const auto& tpl, auto& rv)->bool {
                if( tpl.m_data.is_Infer() ) {
                    BUG(sp, "");
                }
                else if( tpl.m_data.is_Generic() ) {
                    rv = monomorph_cb(tpl).clone();
                    return true;
                }
                else if( tpl.m_data.is_ErasedType() ) {
                    const auto& e = tpl.m_data.as_ErasedType();

                    ASSERT_BUG(sp, e.m_index < fcn_ptr->m_code.m_erased_types.size(), "");
                    const auto& erased_type_replacement = fcn_ptr->m_code.m_erased_types.at(e.m_index);
                    rv = monomorphise_type_with(sp, erased_type_replacement,  monomorph_cb, false);
                    return true;
                }
                else {
                    return false;
                }
                }) );
            m_resolve.expand_associated_types(sp, cache.m_arg_types.back());
            DEBUG("= " << cache.m_arg_types.back());

            // Check types
            for(unsigned int i = 0; i < fcn.m_args.size(); i ++) {
                DEBUG("CHECK ARG " << i << " " << node.m_cache.m_arg_types[i] << " == " << node.m_args[i]->m_res_type);
                check_types_equal(node.span(), node.m_cache.m_arg_types[i], node.m_args[i]->m_res_type);
            }
            for(unsigned int i = fcn.m_args.size(); i < node.m_args.size(); i ++) {
                DEBUG("CHECK ARG " << i << " *  == " << node.m_args[i]->m_res_type);
                // TODO: Check that the types here are valid.
            }
            DEBUG("CHECK RV " << node.m_res_type << " == " << node.m_cache.m_arg_types.back());
            check_types_equal(node.span(), node.m_res_type,  node.m_cache.m_arg_types.back());

            cache.m_monomorph_cb = mv$(monomorph_cb);

            // Bounds
            for(size_t i = 0; i < cache.m_fcn_params->m_types.size(); i ++)
            {
            }
            for(const auto& bound : cache.m_fcn_params->m_bounds)
            {
                TU_MATCH(::HIR::GenericBound, (bound), (be),
                (Lifetime,
                    ),
                (TypeLifetime,
                    ),
                (TraitBound,
                    auto real_type = monomorphise_type_with(sp, be.type, cache.m_monomorph_cb);
                    m_resolve.expand_associated_types(sp, real_type);
                    auto real_trait = monomorphise_genericpath_with(sp, be.trait.m_path, cache.m_monomorph_cb, false);
                    for(auto& t : real_trait.m_params.m_types)
                        m_resolve.expand_associated_types(sp, t);
                    DEBUG("Bound " << be.type << ":  " << be.trait);
                    DEBUG("= (" << real_type << ": " << real_trait << ")");
                    const auto& trait_params = real_trait.m_params;

                    const auto& trait_path = be.trait.m_path.m_path;
                    check_associated_type(sp, ::HIR::TypeRef(), trait_path, trait_params, real_type, "");

                    // TODO: Either - Don't include the above impl bound, or change the below trait to the one that has that type
                    for( const auto& assoc : be.trait.m_type_bounds ) {
                        ::HIR::GenericPath  type_trait_path;
                        bool has_ty = m_resolve.trait_contains_type(sp, real_trait, *be.trait.m_trait_ptr, assoc.first,  type_trait_path);
                        ASSERT_BUG(sp, has_ty, "Type " << assoc.first << " not found in chain of " << real_trait);

                        auto other_ty = monomorphise_type_with(sp, assoc.second, cache.m_monomorph_cb, true);
                        m_resolve.expand_associated_types(sp, other_ty);

                        check_associated_type(sp, other_ty,  type_trait_path.m_path, type_trait_path.m_params, real_type, assoc.first.c_str());
                    }
                    ),
                (TypeEquality,
                    auto real_type_left = monomorphise_type_with(sp, be.type, cache.m_monomorph_cb);
                    auto real_type_right = monomorphise_type_with(sp, be.other_type, cache.m_monomorph_cb);
                    m_resolve.expand_associated_types(sp, real_type_left);
                    m_resolve.expand_associated_types(sp, real_type_right);
                    check_types_equal(sp, real_type_left, real_type_right);
                    )
                )
            }
        }
        void visit(::HIR::ExprNode_CallValue& node) override
        {
            TRACE_FUNCTION_F(&node << " (...)(..., )");

            const auto& val_ty = node.m_value->m_res_type;

            TU_IFLET( ::HIR::TypeRef::Data, val_ty.m_data, Function, e,
                DEBUG("- Function pointer: " << val_ty);
                if( node.m_args.size() != e.m_arg_types.size() ) {
                    ERROR(node.span(), E0000, "Incorrect number of arguments to call via " << val_ty);
                }
                for( unsigned int i = 0; i < node.m_args.size(); i ++ )
                {
                    check_types_equal(node.m_args[i]->span(), e.m_arg_types[i], node.m_args[i]->m_res_type);
                }
                check_types_equal(node.span(), node.m_res_type, *e.m_rettype);
            )
            else
            {
                // 1. Look up the encoded trait
                const auto& trait = (
                      node.m_trait_used == ::HIR::ExprNode_CallValue::TraitUsed::Fn ? m_resolve.m_crate.get_lang_item_path(node.span(), "fn")
                    : node.m_trait_used == ::HIR::ExprNode_CallValue::TraitUsed::FnMut ? m_resolve.m_crate.get_lang_item_path(node.span(), "fn_mut")
                    : node.m_trait_used == ::HIR::ExprNode_CallValue::TraitUsed::FnOnce ? m_resolve.m_crate.get_lang_item_path(node.span(), "fn_once")
                    : throw ""
                    );

                ::std::vector< ::HIR::TypeRef>  tup_ents;
                for(const auto& arg : node.m_args) {
                    tup_ents.push_back( arg->m_res_type.clone() );
                }
                ::HIR::PathParams   params;
                params.m_types.push_back( ::HIR::TypeRef( mv$(tup_ents) ) );

                bool found = m_resolve.find_impl(node.span(), trait, &params, val_ty, [&](auto , bool fuzzy)->bool{
                    ASSERT_BUG(node.span(), !fuzzy, "Fuzzy match in check pass");
                    return true;
                    });
                if( !found ) {
                    ERROR(node.span(), E0000, "Unable to find a matching impl of " << trait << " for " << val_ty);
                }
                ::HIR::TypeRef  exp_ret( ::HIR::Path(
                    node.m_value->m_res_type.clone(),
                    { m_resolve.m_crate.get_lang_item_path(node.span(), "fn_once"), mv$(params) },
                    "Output", {}
                    ) );
                m_resolve.expand_associated_types(node.span(), exp_ret);
                check_types_equal(node.span(), node.m_res_type, exp_ret);
            }

            node.m_value->visit( *this );
            for( auto& val : node.m_args ) {
                val->visit( *this );
            }
        }
        void visit(::HIR::ExprNode_CallMethod& node) override
        {
            TRACE_FUNCTION_F(&node << " (...)." << node.m_method << "(...,) - " << node.m_method_path);
            // TODO: Don't use m_cache
            ASSERT_BUG(node.span(), node.m_cache.m_arg_types.size() > 0, "CallMethod cache not populated");
            ASSERT_BUG(node.span(), node.m_cache.m_arg_types.size() == 1 + node.m_args.size() + 1, "CallMethod cache mis-sized");
            check_types_equal(node.m_cache.m_arg_types[0], node.m_value);
            for(unsigned int i = 0; i < node.m_args.size(); i ++)
            {
                check_types_equal(node.m_cache.m_arg_types[1+i], node.m_args[i]);
            }
            check_types_equal(node.span(), node.m_res_type, node.m_cache.m_arg_types.back());

            node.m_value->visit( *this );
            for( auto& val : node.m_args ) {
                val->visit( *this );
            }
        }

        void visit(::HIR::ExprNode_Field& node) override
        {
            TRACE_FUNCTION_F(&node << " (...)." << node.m_field);
            const auto& sp = node.span();
            const auto& str_ty = node.m_value->m_res_type;

            bool is_index = ( '0' <= node.m_field[0] && node.m_field[0] <= '9' );
            if( str_ty.m_data.is_Tuple() )
            {
                ASSERT_BUG(sp, is_index, "Non-index _Field on tuple");
            }
            else if( str_ty.m_data.is_Closure() )
            {
                ASSERT_BUG(sp, is_index, "Non-index _Field on closure");
            }
            else
            {
                ASSERT_BUG(sp, str_ty.m_data.is_Path(), "Value type of _Field isn't Path - " << str_ty);
                const auto& ty_e = str_ty.m_data.as_Path();
                if( ty_e.binding.is_Struct() )
                {
                    //const auto& str = *ty_e.binding.as_Struct();
                    // TODO: Triple-check result, but that probably isn't needed
                }
                else if( ty_e.binding.is_Union() )
                {
                }
                else
                {
                    ASSERT_BUG(sp, ty_e.binding.is_Struct() || ty_e.binding.is_Union(), "Value type of _Field isn't a Struct or Union - " << str_ty);
                }
            }

            node.m_value->visit( *this );
        }
        void visit(::HIR::ExprNode_Tuple& node) override
        {
            TRACE_FUNCTION_F(&node << " (...,)");
            const auto& tys = node.m_res_type.m_data.as_Tuple();

            ASSERT_BUG(node.span(), tys.size() == node.m_vals.size(), "Bad element count in tuple literal - " << tys.size() << " != " << node.m_vals.size());
            for(unsigned int i = 0; i < node.m_vals.size(); i ++)
            {
                check_types_equal(node.span(), tys[i], node.m_vals[i]->m_res_type);
            }

            for( auto& val : node.m_vals ) {
                val->visit( *this );
            }
        }
        void visit(::HIR::ExprNode_ArrayList& node) override
        {
            TRACE_FUNCTION_F(&node << " [...,]");
            // Cleanly equate into array (with coercions)
            const auto& inner_ty = *node.m_res_type.m_data.as_Array().inner;
            for( auto& val : node.m_vals ) {
                check_types_equal(val->span(), inner_ty, val->m_res_type);
            }

            for( auto& val : node.m_vals ) {
                val->visit( *this );
            }
        }
        void visit(::HIR::ExprNode_ArraySized& node) override
        {
            TRACE_FUNCTION_F(&node << " [...; "<<node.m_size_val<<"]");

            //check_types_equal(node.m_size->span(), ::HIR::TypeRef(::HIR::Primitive::Usize), node.m_size->m_res_type);
            const auto& inner_ty = *node.m_res_type.m_data.as_Array().inner;
            check_types_equal(node.m_val->span(), inner_ty, node.m_val->m_res_type);

            node.m_val->visit( *this );
            node.m_size->visit( *this );
        }

        void visit(::HIR::ExprNode_Literal& node) override
        {
            // No validation needed
        }
        void visit(::HIR::ExprNode_PathValue& node) override
        {
            TRACE_FUNCTION_F(&node << " " << node.m_path);
            const auto& sp = node.span();

            TU_MATCH(::HIR::Path::Data, (node.m_path.m_data), (e),
            (Generic,
                switch(node.m_target)
                {
                case ::HIR::ExprNode_PathValue::UNKNOWN:
                    BUG(sp, "Unknown target PathValue encountered with Generic path");
                case ::HIR::ExprNode_PathValue::FUNCTION:
                    // TODO: Is validate needed?
                    assert( node.m_res_type.m_data.is_Function() );
                    break;
                case ::HIR::ExprNode_PathValue::STRUCT_CONSTR: {
                    } break;
                case ::HIR::ExprNode_PathValue::ENUM_VAR_CONSTR: {
                    } break;
                case ::HIR::ExprNode_PathValue::STATIC: {
                    } break;
                case ::HIR::ExprNode_PathValue::CONSTANT: {
                    } break;
                }
                ),
            (UfcsUnknown,
                BUG(sp, "Encountered UfcsUnknown");
                ),
            (UfcsKnown,
                check_associated_type(sp, ::HIR::TypeRef(),  e.trait.m_path, e.trait.m_params, e.type->clone(), "");

                const auto& trait = this->m_resolve.m_crate.get_trait_by_path(sp, e.trait.m_path);
                auto it = trait.m_values.find( e.item );
                if( it == trait.m_values.end() ) {
                    ERROR(sp, E0000, "`" << e.item << "` is not a value member of trait " << e.trait.m_path);
                }
                TU_MATCH( ::HIR::TraitValueItem, (it->second), (ie),
                (Constant,
                    auto cb = monomorphise_type_get_cb(sp, &*e.type, &e.trait.m_params, nullptr);
                    ::HIR::TypeRef  tmp;
                    const ::HIR::TypeRef* typ;
                    if(monomorphise_type_needed(ie.m_type)) {
                        tmp = monomorphise_type_with(sp, ie.m_type, cb);
                        m_resolve.expand_associated_types(sp, tmp);
                        typ = &tmp;
                    }
                    else {
                        typ = &ie.m_type;
                    }
                    check_types_equal(sp, node.m_res_type, *typ);
                    ),
                (Static,
                    TODO(sp, "Monomorpise associated static type - " << ie.m_type);
                    ),
                (Function,
                    assert( node.m_res_type.m_data.is_Function() );
                    )
                )
                ),
            (UfcsInherent,
                )
            )
        }

        void visit(::HIR::ExprNode_Variable& node) override
        {
            // TODO: Check against variable slot? Nah.
        }

        void visit(::HIR::ExprNode_Closure& node) override
        {
            TRACE_FUNCTION_F(&node << " |...| ...");

            if( node.m_code )
            {
                check_types_equal(node.m_code->span(), node.m_return, node.m_code->m_res_type);
                this->closure_ret_types.push_back( &node.m_return );
                node.m_code->visit( *this );
                this->closure_ret_types.pop_back( );
            }
        }

    private:
        void check_types_equal(const ::HIR::TypeRef& l, const ::HIR::ExprNodeP& node) const
        {
            check_types_equal(node->span(), l, node->m_res_type);
        }
        void check_types_equal(const Span& sp, const ::HIR::TypeRef& l, const ::HIR::TypeRef& r) const
        {
            if( /*l.m_data.is_Diverge() ||*/ r.m_data.is_Diverge() ) {
                // Diverge, matches everything.
                // TODO: Is this always true?
            }
            else if( l != r ) {
                ERROR(sp, E0000, "Type mismatch - " << l << " != " << r);
            }
            else {
                // All good
            }
        }
        void check_associated_type(const Span& sp,
                const ::HIR::TypeRef& res,  // Expected result
                const ::HIR::SimplePath& trait, const ::HIR::PathParams& params, const ::HIR::TypeRef& ity, const char* name
            ) const
        {
            if( trait == m_lang_Index && ity.m_data.is_Array() ) {
                if(name)
                {
                    if( res != *ity.m_data.as_Array().inner ) {
                        ERROR(sp, E0000, "Associated type on " << trait << params << " for " << ity << " doesn't match - " << res << " != " << *ity.m_data.as_Array().inner);
                    }
                }
                return ;
            }
            bool found = m_resolve.find_impl(sp, trait, &params, ity, [&](auto impl, bool fuzzy) {
                if( name )
                {
                    auto atyv = impl.get_type(name);
                    m_resolve.expand_associated_types(sp, atyv);
                    if( atyv == ::HIR::TypeRef() )
                    {
                        // TODO: Check that `res` is <ity as trait>::name
                    }
                    else if( res != atyv )
                    {
                        ERROR(sp, E0000, "Associated type on " << trait << params << " for " << ity << " doesn't match - " << res << " != " << atyv);
                    }
                }

                return true;
                });
            if( !found )
            {
                ERROR(sp, E0000, "Cannot find an impl of " << trait << params << " for " << ity);
            }
        }

        const ::HIR::SimplePath& get_lang_item_path(const Span& sp, const char* name) const
        {
            return m_resolve.m_crate.get_lang_item_path(sp, name);
        }
    };


    class OuterVisitor:
        public ::HIR::Visitor
    {
        StaticTraitResolve  m_resolve;
    public:
        OuterVisitor(const ::HIR::Crate& crate):
            m_resolve(crate)
        {}

        // NOTE: This is left here to ensure that any expressions that aren't handled by higher code cause a failure
        void visit_expr(::HIR::ExprPtr& exp) override {
            BUG(Span(), "visit_expr hit in OuterVisitor");
        }

        void visit_type(::HIR::TypeRef& ty) override
        {
            TU_IFLET(::HIR::TypeRef::Data, ty.m_data, Array, e,
                this->visit_type( *e.inner );
                DEBUG("Array size " << ty);
                if( e.size ) {
                    t_args  tmp;
                    auto ty_usize = ::HIR::TypeRef(::HIR::CoreType::Usize);
                    ExprVisitor_Validate    ev(m_resolve, tmp, ty_usize);
                    ev.visit_root( *e.size );
                }
            )
            else {
                ::HIR::Visitor::visit_type(ty);
            }
        }
        // ------
        // Code-containing items
        // ------
        void visit_function(::HIR::ItemPath p, ::HIR::Function& item) override {
            auto _ = this->m_resolve.set_item_generics(item.m_params);
            if( item.m_code )
            {
                DEBUG("Function code " << p);
                ExprVisitor_Validate    ev(m_resolve, item.m_args, item.m_return);
                ev.visit_root( item.m_code );
            }
            else
            {
                DEBUG("Function code " << p << " (none)");
            }
        }
        void visit_static(::HIR::ItemPath p, ::HIR::Static& item) override {
            if( item.m_value )
            {
                t_args  tmp;
                ExprVisitor_Validate    ev(m_resolve, tmp, item.m_type);
                ev.visit_root(item.m_value);
            }
        }
        void visit_constant(::HIR::ItemPath p, ::HIR::Constant& item) override {
            if( item.m_value )
            {
                t_args  tmp;
                ExprVisitor_Validate    ev(m_resolve, tmp, item.m_type);
                ev.visit_root(item.m_value);
            }
        }
        void visit_enum(::HIR::ItemPath p, ::HIR::Enum& item) override {
            //auto _ = this->m_ms.set_item_generics(item.m_params);

            if( auto* e = item.m_data.opt_Value() )
            {
                // TODO: Use a different type depding on repr()
                auto enum_type = ::HIR::TypeRef(::HIR::CoreType::Isize);
                for(auto& var : e->variants)
                {
                    DEBUG("Enum value " << p << " - " << var.name);

                    if( var.expr )
                    {
                        t_args  tmp;
                        ExprVisitor_Validate    ev(m_resolve, tmp, enum_type);
                        ev.visit_root(var.expr);
                    }
                }
            }
        }

        void visit_trait(::HIR::ItemPath p, ::HIR::Trait& item) override
        {
            auto _ = this->m_resolve.set_impl_generics(item.m_params);
            ::HIR::Visitor::visit_trait(p, item);
        }
        void visit_type_impl(::HIR::TypeImpl& impl) override
        {
            TRACE_FUNCTION_F("impl " << impl.m_type);
            auto _ = this->m_resolve.set_impl_generics(impl.m_params);

            ::HIR::Visitor::visit_type_impl(impl);
        }
        void visit_trait_impl(const ::HIR::SimplePath& trait_path, ::HIR::TraitImpl& impl) override
        {
            TRACE_FUNCTION_F("impl" << impl.m_params.fmt_args() << " " << trait_path << " for " << impl.m_type);
            auto _ = this->m_resolve.set_impl_generics(impl.m_params);

            ::HIR::Visitor::visit_trait_impl(trait_path, impl);
        }
    };
}

void Typecheck_Expressions_Validate(::HIR::Crate& crate)
{
    OuterVisitor    ov(crate);
    ov.visit_crate( crate );
}

