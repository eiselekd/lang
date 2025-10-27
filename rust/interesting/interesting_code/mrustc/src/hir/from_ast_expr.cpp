/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * hir/from_ast_expr.cpp
 * - Constructs a HIR expression tree from an AST expression tree
 */
#include <hir/expr_ptr.hpp>
#include <hir/expr.hpp>
#include <ast/expr.hpp>
#include <ast/ast.hpp>
#include "from_ast.hpp"

::std::unique_ptr< ::HIR::ExprNode> LowerHIR_ExprNode_Inner(const ::AST::ExprNode& e);
::std::unique_ptr< ::HIR::ExprNode> LowerHIR_ExprNode_Inner_Opt(const ::AST::ExprNode* e) {
    if( e ) {
        return LowerHIR_ExprNode_Inner(*e);
    }
    else {
        return nullptr;
    }
}

struct LowerHIR_ExprNode_Visitor:
    public ::AST::NodeVisitor
{
    ::std::unique_ptr< ::HIR::ExprNode> m_rv;

    virtual void visit(::AST::ExprNode_Block& v) override {
        auto rv = new ::HIR::ExprNode_Block(v.span());
        for(const auto& n : v.m_nodes)
        {
            ASSERT_BUG(v.span(), n, "NULL node encountered in block");
            rv->m_nodes.push_back( LowerHIR_ExprNode_Inner( *n ) );
        }
        if( v.m_yields_final_value && ! rv->m_nodes.empty() )
        {
            rv->m_value_node = mv$(rv->m_nodes.back());
            rv->m_nodes.pop_back();
        }

        if( v.m_local_mod )
        {
            // TODO: Populate m_traits from the local module's import list
            rv->m_local_mod = LowerHIR_SimplePath(v.span(), v.m_local_mod->path());
        }

        m_rv.reset( static_cast< ::HIR::ExprNode*>(rv) );
    }
    virtual void visit(::AST::ExprNode_Macro& v) override {
        BUG(v.span(), "Hit ExprNode_Macro");
    }
    virtual void visit(::AST::ExprNode_Asm& v) override {
        ::std::vector< ::HIR::ExprNode_Asm::ValRef> outputs;
        ::std::vector< ::HIR::ExprNode_Asm::ValRef> inputs;
        for(auto& vr : v.m_output)
            outputs.push_back( ::HIR::ExprNode_Asm::ValRef { vr.name, LowerHIR_ExprNode_Inner(*vr.value) } );
        for(auto& vr : v.m_input)
            inputs.push_back( ::HIR::ExprNode_Asm::ValRef { vr.name, LowerHIR_ExprNode_Inner(*vr.value) } );

        m_rv.reset( new ::HIR::ExprNode_Asm( v.span(), v.m_text, mv$(outputs), mv$(inputs), v.m_clobbers, v.m_flags ) );
    }
    virtual void visit(::AST::ExprNode_Flow& v) override {
        switch( v.m_type )
        {
        case ::AST::ExprNode_Flow::RETURN:
            if( v.m_value )
                m_rv.reset( new ::HIR::ExprNode_Return( v.span(), LowerHIR_ExprNode_Inner(*v.m_value) ) );
            else
                m_rv.reset( new ::HIR::ExprNode_Return( v.span(), ::HIR::ExprNodeP(new ::HIR::ExprNode_Tuple(v.span(), {})) ) );
            break;
        case ::AST::ExprNode_Flow::CONTINUE:
        case ::AST::ExprNode_Flow::BREAK:
            auto val = v.m_value ? LowerHIR_ExprNode_Inner(*v.m_value) : ::HIR::ExprNodeP();
            ASSERT_BUG(v.span(), !(v.m_type == ::AST::ExprNode_Flow::CONTINUE && val), "Continue with a value isn't allowed");
            m_rv.reset( new ::HIR::ExprNode_LoopControl( v.span(), v.m_target, (v.m_type == ::AST::ExprNode_Flow::CONTINUE), mv$(val) ) );
            break;
        }
    }
    virtual void visit(::AST::ExprNode_LetBinding& v) override {
        m_rv.reset( new ::HIR::ExprNode_Let( v.span(),
            LowerHIR_Pattern( v.m_pat ),
            LowerHIR_Type( v.m_type ),
            LowerHIR_ExprNode_Inner_Opt( v.m_value.get() )
            ) );
    }
    virtual void visit(::AST::ExprNode_Assign& v) override {
        struct H {
            static ::HIR::ExprNode_Assign::Op get_op(::AST::ExprNode_Assign::Operation o) {
                switch(o)
                {
                case ::AST::ExprNode_Assign::NONE:  return ::HIR::ExprNode_Assign::Op::None;
                case ::AST::ExprNode_Assign::ADD:   return ::HIR::ExprNode_Assign::Op::Add;
                case ::AST::ExprNode_Assign::SUB:   return ::HIR::ExprNode_Assign::Op::Sub;

                case ::AST::ExprNode_Assign::MUL:   return ::HIR::ExprNode_Assign::Op::Mul;
                case ::AST::ExprNode_Assign::DIV:   return ::HIR::ExprNode_Assign::Op::Div;
                case ::AST::ExprNode_Assign::MOD:   return ::HIR::ExprNode_Assign::Op::Mod;

                case ::AST::ExprNode_Assign::AND:   return ::HIR::ExprNode_Assign::Op::And;
                case ::AST::ExprNode_Assign::OR :   return ::HIR::ExprNode_Assign::Op::Or ;
                case ::AST::ExprNode_Assign::XOR:   return ::HIR::ExprNode_Assign::Op::Xor;

                case ::AST::ExprNode_Assign::SHR:   return ::HIR::ExprNode_Assign::Op::Shr;
                case ::AST::ExprNode_Assign::SHL:   return ::HIR::ExprNode_Assign::Op::Shl;
                }
                throw "";
            }
        };
        m_rv.reset( new ::HIR::ExprNode_Assign( v.span(),
            H::get_op(v.m_op),
            LowerHIR_ExprNode_Inner( *v.m_slot ),
            LowerHIR_ExprNode_Inner( *v.m_value )
            ) );
    }
    virtual void visit(::AST::ExprNode_BinOp& v) override {
        ::HIR::ExprNode_BinOp::Op   op;
        switch(v.m_type)
        {
        case ::AST::ExprNode_BinOp::RANGE: {
            BUG(v.span(), "Unexpected RANGE binop");
            break; }
        case ::AST::ExprNode_BinOp::RANGE_INC: {
            BUG(v.span(), "Unexpected RANGE_INC binop");
            break; }
        case ::AST::ExprNode_BinOp::PLACE_IN:
            m_rv.reset(new ::HIR::ExprNode_Emplace(v.span(),
                ::HIR::ExprNode_Emplace::Type::Placer,
                LowerHIR_ExprNode_Inner(*v.m_left),
                LowerHIR_ExprNode_Inner(*v.m_right)
                ));
            break;

        case ::AST::ExprNode_BinOp::CMPEQU :    op = ::HIR::ExprNode_BinOp::Op::CmpEqu ; if(0)
        case ::AST::ExprNode_BinOp::CMPNEQU:    op = ::HIR::ExprNode_BinOp::Op::CmpNEqu; if(0)
        case ::AST::ExprNode_BinOp::CMPLT : op = ::HIR::ExprNode_BinOp::Op::CmpLt ; if(0)
        case ::AST::ExprNode_BinOp::CMPLTE: op = ::HIR::ExprNode_BinOp::Op::CmpLtE; if(0)
        case ::AST::ExprNode_BinOp::CMPGT : op = ::HIR::ExprNode_BinOp::Op::CmpGt ; if(0)
        case ::AST::ExprNode_BinOp::CMPGTE: op = ::HIR::ExprNode_BinOp::Op::CmpGtE; if(0)
        case ::AST::ExprNode_BinOp::BOOLAND:    op = ::HIR::ExprNode_BinOp::Op::BoolAnd; if(0)
        case ::AST::ExprNode_BinOp::BOOLOR :    op = ::HIR::ExprNode_BinOp::Op::BoolOr ; if(0)

        case ::AST::ExprNode_BinOp::BITAND: op = ::HIR::ExprNode_BinOp::Op::And; if(0)
        case ::AST::ExprNode_BinOp::BITOR : op = ::HIR::ExprNode_BinOp::Op::Or ; if(0)
        case ::AST::ExprNode_BinOp::BITXOR: op = ::HIR::ExprNode_BinOp::Op::Xor; if(0)
        case ::AST::ExprNode_BinOp::MULTIPLY:   op = ::HIR::ExprNode_BinOp::Op::Mul; if(0)
        case ::AST::ExprNode_BinOp::DIVIDE  :   op = ::HIR::ExprNode_BinOp::Op::Div; if(0)
        case ::AST::ExprNode_BinOp::MODULO  :   op = ::HIR::ExprNode_BinOp::Op::Mod; if(0)
        case ::AST::ExprNode_BinOp::ADD:    op = ::HIR::ExprNode_BinOp::Op::Add; if(0)
        case ::AST::ExprNode_BinOp::SUB:    op = ::HIR::ExprNode_BinOp::Op::Sub; if(0)
        case ::AST::ExprNode_BinOp::SHR:    op = ::HIR::ExprNode_BinOp::Op::Shr; if(0)
        case ::AST::ExprNode_BinOp::SHL:    op = ::HIR::ExprNode_BinOp::Op::Shl;

            m_rv.reset( new ::HIR::ExprNode_BinOp( v.span(),
                op,
                LowerHIR_ExprNode_Inner( *v.m_left ),
                LowerHIR_ExprNode_Inner( *v.m_right )
                ) );
            break;
        }
    }
    virtual void visit(::AST::ExprNode_UniOp& v) override {
        ::HIR::ExprNode_UniOp::Op   op;
        switch(v.m_type)
        {
        case ::AST::ExprNode_UniOp::BOX: {
            m_rv.reset(new ::HIR::ExprNode_Emplace(v.span(),
                ::HIR::ExprNode_Emplace::Type::Boxer,
                ::HIR::ExprNodeP(new ::HIR::ExprNode_Tuple(v.span(), {})),
                LowerHIR_ExprNode_Inner(*v.m_value)
                ));
            } break;
        case ::AST::ExprNode_UniOp::QMARK:
            BUG(v.span(), "Encounterd question mark operator (should have been expanded in AST)");
            break;

        case ::AST::ExprNode_UniOp::REF:
            m_rv.reset(new ::HIR::ExprNode_Borrow(v.span(), ::HIR::BorrowType::Shared, LowerHIR_ExprNode_Inner( *v.m_value ) ));
            break;
        case ::AST::ExprNode_UniOp::REFMUT:
            m_rv.reset(new ::HIR::ExprNode_Borrow(v.span(), ::HIR::BorrowType::Unique, LowerHIR_ExprNode_Inner( *v.m_value ) ));
            break;

        case ::AST::ExprNode_UniOp::INVERT: op = ::HIR::ExprNode_UniOp::Op::Invert; if(0)
        case ::AST::ExprNode_UniOp::NEGATE: op = ::HIR::ExprNode_UniOp::Op::Negate;
            m_rv.reset( new ::HIR::ExprNode_UniOp( v.span(),
                op,
                LowerHIR_ExprNode_Inner( *v.m_value )
                ) );
            break;
        }
    }
    virtual void visit(::AST::ExprNode_Cast& v) override {
        m_rv.reset( new ::HIR::ExprNode_Cast( v.span(),
            LowerHIR_ExprNode_Inner( *v.m_value ),
            LowerHIR_Type(v.m_type)
            ) );
    }
    virtual void visit(::AST::ExprNode_TypeAnnotation& v) override {
        // TODO: A proper node?
        m_rv.reset( new ::HIR::ExprNode_Unsize( v.span(),
            LowerHIR_ExprNode_Inner( *v.m_value ),
            LowerHIR_Type(v.m_type)
            ) );
    }

    virtual void visit(::AST::ExprNode_CallPath& v) override {
        ::std::vector< ::HIR::ExprNodeP> args;
        for(const auto& arg : v.m_args)
            args.push_back( LowerHIR_ExprNode_Inner(*arg) );

        TU_IFLET(::AST::Path::Class, v.m_path.m_class, Local, e,
            m_rv.reset( new ::HIR::ExprNode_CallValue( v.span(),
                ::HIR::ExprNodeP(new ::HIR::ExprNode_Variable( v.span(), e.name, v.m_path.binding().as_Variable().slot )),
                mv$(args)
                ) );
        )
        else
        {
            TU_MATCH_DEF(::AST::PathBinding, (v.m_path.binding()), (e),
            (
                m_rv.reset( new ::HIR::ExprNode_CallPath( v.span(),
                    LowerHIR_Path(v.span(), v.m_path),
                    mv$( args )
                    ) );
                ),
            (TypeAlias,
                TODO(v.span(), "CallPath -> TupleVariant TypeAlias");
                ),
            (EnumVar,
                m_rv.reset( new ::HIR::ExprNode_TupleVariant( v.span(),
                    LowerHIR_GenericPath(v.span(), v.m_path), false,
                    mv$( args )
                    ) );
                ),
            (Struct,
                m_rv.reset( new ::HIR::ExprNode_TupleVariant( v.span(),
                    LowerHIR_GenericPath(v.span(), v.m_path), true,
                    mv$( args )
                    ) );
                )
            )
        }
    }
    virtual void visit(::AST::ExprNode_CallMethod& v) override {
        ::std::vector< ::HIR::ExprNodeP> args;
        for(const auto& arg : v.m_args)
            args.push_back( LowerHIR_ExprNode_Inner(*arg) );

        // TODO: Should this be abstracted?
        ::HIR::PathParams   params;
        for(const auto& param : v.m_method.args().m_types)
            params.m_types.push_back( LowerHIR_Type(param) );

        m_rv.reset( new ::HIR::ExprNode_CallMethod( v.span(),
            LowerHIR_ExprNode_Inner(*v.m_val),
            v.m_method.name(),
            mv$(params),
            mv$(args)
            ) );
    }
    virtual void visit(::AST::ExprNode_CallObject& v) override {
        ::std::vector< ::HIR::ExprNodeP> args;
        for(const auto& arg : v.m_args)
            args.push_back( LowerHIR_ExprNode_Inner(*arg) );

        m_rv.reset( new ::HIR::ExprNode_CallValue( v.span(),
            LowerHIR_ExprNode_Inner(*v.m_val),
            mv$(args)
            ) );
    }
    virtual void visit(::AST::ExprNode_Loop& v) override {
        switch( v.m_type )
        {
        case ::AST::ExprNode_Loop::LOOP:
            m_rv.reset( new ::HIR::ExprNode_Loop( v.span(),
                v.m_label,
                LowerHIR_ExprNode_Inner(*v.m_code)
                ) );
            break;
        case ::AST::ExprNode_Loop::WHILE: {
            ::std::vector< ::HIR::ExprNodeP>    code;
            // - if `m_cond` { () } else { break `m_label` }
            code.push_back( ::HIR::ExprNodeP(new ::HIR::ExprNode_If( v.span(),
                LowerHIR_ExprNode_Inner(*v.m_cond),
                ::HIR::ExprNodeP( new ::HIR::ExprNode_Tuple(v.span(), {}) ),
                ::HIR::ExprNodeP( new ::HIR::ExprNode_LoopControl(v.span(), v.m_label, false) )
                )) );
            code.push_back( LowerHIR_ExprNode_Inner(*v.m_code) );

            m_rv.reset( new ::HIR::ExprNode_Loop( v.span(),
                v.m_label,
                ::HIR::ExprNodeP(new ::HIR::ExprNode_Block( v.span(), false, mv$(code), {} ))
                ) );
            break; }
        case ::AST::ExprNode_Loop::WHILELET: {
            ::std::vector< ::HIR::ExprNode_Match::Arm>  arms;

            // - Matches pattern - Run inner code
            arms.push_back(::HIR::ExprNode_Match::Arm {
                ::make_vec1( LowerHIR_Pattern(v.m_pattern) ),
                ::HIR::ExprNodeP(),
                LowerHIR_ExprNode_Inner(*v.m_code)
                });
            // - Matches anything else - break
            arms.push_back(::HIR::ExprNode_Match::Arm {
                ::make_vec1( ::HIR::Pattern() ),
                ::HIR::ExprNodeP(),
                ::HIR::ExprNodeP( new ::HIR::ExprNode_LoopControl( v.span(), v.m_label, false) )
                });

            m_rv.reset( new ::HIR::ExprNode_Loop( v.span(),
                v.m_label,
                ::HIR::ExprNodeP(new ::HIR::ExprNode_Match( v.span(),
                    LowerHIR_ExprNode_Inner(*v.m_cond),
                    mv$(arms)
                    ))
                ) );
            break; }
        case ::AST::ExprNode_Loop::FOR:
            // NOTE: This should already be desugared (as a pass before resolve)
            BUG(v.span(), "Encountered still-sugared for loop");
            break;
        }

        // TODO: Iterate the constructed loop and determine if there are any `break` statements pointing to it
        {
            struct LoopVisitor:
                public ::HIR::ExprVisitorDef
            {
                const ::std::string& top_label;
                bool    top_is_broken;
                ::std::vector< const ::std::string*>   name_stack;

                LoopVisitor(const ::std::string& top_label):
                    top_label(top_label),
                    top_is_broken(false),
                    name_stack()
                {}

                void visit(::HIR::ExprNode_Loop& node) override {
                    this->name_stack.push_back( &node.m_label );
                    ::HIR::ExprVisitorDef::visit(node);
                    this->name_stack.pop_back( );
                }
                void visit(::HIR::ExprNode_LoopControl& node) override {
                    ::HIR::ExprVisitorDef::visit(node);

                    if( node.m_continue ) {
                    }
                    else {
                        for( auto it = this->name_stack.rbegin(); it != this->name_stack.rend(); ++ it )
                        {
                            if( node.m_label == "" || node.m_label == **it )
                                return ;
                        }
                        if( node.m_label == "" || node.m_label == this->top_label ) {
                            this->top_is_broken = true;
                        }
                        else {
                            // break is for a higher loop
                        }
                    }
                }
            };

            auto& loop_node = dynamic_cast< ::HIR::ExprNode_Loop&>(*m_rv);;
            LoopVisitor lv { loop_node.m_label };
            loop_node.m_code->visit(lv);
            if( ! lv.top_is_broken ) {
                // If the loop never hit a 'break', the loop yields ! not ()
                loop_node.m_res_type.m_data = ::HIR::TypeRef::Data::make_Diverge({});
            }
        }
    }
    virtual void visit(::AST::ExprNode_Match& v) override {
        ::std::vector< ::HIR::ExprNode_Match::Arm>  arms;

        for(const auto& arm : v.m_arms)
        {
            ::HIR::ExprNode_Match::Arm  new_arm {
                {},
                LowerHIR_ExprNode_Inner_Opt(arm.m_cond.get()),
                LowerHIR_ExprNode_Inner(*arm.m_code)
                };

            for(const auto& pat : arm.m_patterns)
                new_arm.m_patterns.push_back( LowerHIR_Pattern(pat) );

            arms.push_back( mv$(new_arm) );
        }

        m_rv.reset( new ::HIR::ExprNode_Match( v.span(),
            LowerHIR_ExprNode_Inner(*v.m_val),
            mv$(arms)
            ));
    }
    virtual void visit(::AST::ExprNode_If& v) override {
        m_rv.reset( new ::HIR::ExprNode_If( v.span(),
            LowerHIR_ExprNode_Inner(*v.m_cond),
            LowerHIR_ExprNode_Inner(*v.m_true),
            LowerHIR_ExprNode_Inner_Opt(&*v.m_false)
            ));
    }
    virtual void visit(::AST::ExprNode_IfLet& v) override {
        ::std::vector< ::HIR::ExprNode_Match::Arm>  arms;

        // - Matches pattern - Take true branch
        arms.push_back(::HIR::ExprNode_Match::Arm {
            ::make_vec1( LowerHIR_Pattern(v.m_pattern) ),
            ::HIR::ExprNodeP(),
            LowerHIR_ExprNode_Inner(*v.m_true)
            });
        // - Matches anything else - take false branch
        arms.push_back(::HIR::ExprNode_Match::Arm {
            ::make_vec1( ::HIR::Pattern() ),
            ::HIR::ExprNodeP(),
            v.m_false ? LowerHIR_ExprNode_Inner(*v.m_false) : ::HIR::ExprNodeP(new ::HIR::ExprNode_Tuple(v.span(), {}))
            });
        m_rv.reset( new ::HIR::ExprNode_Match( v.span(),
            LowerHIR_ExprNode_Inner(*v.m_value),
            mv$(arms)
            ));
    }

    virtual void visit(::AST::ExprNode_Integer& v) override {
        struct H {
            static ::HIR::CoreType get_type(Span sp, ::eCoreType ct) {
                switch(ct)
                {
                case CORETYPE_ANY:  return ::HIR::CoreType::Str;

                case CORETYPE_I8 :  return ::HIR::CoreType::I8;
                case CORETYPE_U8 :  return ::HIR::CoreType::U8;
                case CORETYPE_I16:  return ::HIR::CoreType::I16;
                case CORETYPE_U16:  return ::HIR::CoreType::U16;
                case CORETYPE_I32:  return ::HIR::CoreType::I32;
                case CORETYPE_U32:  return ::HIR::CoreType::U32;
                case CORETYPE_I64:  return ::HIR::CoreType::I64;
                case CORETYPE_U64:  return ::HIR::CoreType::U64;
                case CORETYPE_I128: return ::HIR::CoreType::I128;
                case CORETYPE_U128: return ::HIR::CoreType::U128;

                case CORETYPE_INT:  return ::HIR::CoreType::Isize;
                case CORETYPE_UINT: return ::HIR::CoreType::Usize;

                case CORETYPE_CHAR: return ::HIR::CoreType::Char;

                default:
                    BUG(sp, "Unknown type for integer literal - " << coretype_name(ct));
                }
            }
        };
        if( v.m_datatype == CORETYPE_F32 || v.m_datatype == CORETYPE_F64 ) {
            DEBUG("Integer annotated as float, create float node");
            m_rv.reset( new ::HIR::ExprNode_Literal( v.span(),
                ::HIR::ExprNode_Literal::Data::make_Float({
                    (v.m_datatype == CORETYPE_F32 ? ::HIR::CoreType::F32 : ::HIR::CoreType::F64),
                    static_cast<double>(v.m_value)
                    })
                ) );
            return ;
        }
        m_rv.reset( new ::HIR::ExprNode_Literal( v.span(),
            ::HIR::ExprNode_Literal::Data::make_Integer({
                H::get_type( v.span(), v.m_datatype ),
                v.m_value
                })
            ) );
    }
    virtual void visit(::AST::ExprNode_Float& v) override {
        ::HIR::CoreType ct;
        switch(v.m_datatype)
        {
        case CORETYPE_ANY:  ct = ::HIR::CoreType::Str;  break;
        case CORETYPE_F32:  ct = ::HIR::CoreType::F32;  break;
        case CORETYPE_F64:  ct = ::HIR::CoreType::F64;  break;
        default:
            BUG(v.span(), "Unknown type for float literal");
        }
        m_rv.reset( new ::HIR::ExprNode_Literal( v.span(),
            ::HIR::ExprNode_Literal::Data::make_Float({ ct, v.m_value })
            ) );
    }
    virtual void visit(::AST::ExprNode_Bool& v) override {
        m_rv.reset( new ::HIR::ExprNode_Literal( v.span(), ::HIR::ExprNode_Literal::Data::make_Boolean( v.m_value ) ) );
    }
    virtual void visit(::AST::ExprNode_String& v) override {
        m_rv.reset( new ::HIR::ExprNode_Literal( v.span(), ::HIR::ExprNode_Literal::Data::make_String( v.m_value ) ) );
    }
    virtual void visit(::AST::ExprNode_ByteString& v) override {
        ::std::vector<char> dat { v.m_value.begin(), v.m_value.end() };
        m_rv.reset( new ::HIR::ExprNode_Literal( v.span(), ::HIR::ExprNode_Literal::Data::make_ByteString( mv$(dat) ) ) );
    }
    virtual void visit(::AST::ExprNode_Closure& v) override {
        ::HIR::ExprNode_Closure::args_t args;
        for(const auto& arg : v.m_args) {
            args.push_back( ::std::make_pair(
                LowerHIR_Pattern( arg.first ),
                LowerHIR_Type( arg.second )
                ) );
        }
        m_rv.reset( new ::HIR::ExprNode_Closure( v.span(),
            mv$(args),
            LowerHIR_Type(v.m_return),
            LowerHIR_ExprNode_Inner(*v.m_code),
            v.m_is_move
            ) );
    }
    virtual void visit(::AST::ExprNode_StructLiteral& v) override {
        if( v.m_path.binding().is_Union() )
        {
            if( v.m_values.size() != 1 )
                ERROR(v.span(), E0000, "Union constructors can only specify a single field");
            if( v.m_base_value )
                ERROR(v.span(), E0000, "Union constructors can't take a base value");

            m_rv.reset( new ::HIR::ExprNode_UnionLiteral( v.span(),
                LowerHIR_GenericPath(v.span(), v.m_path),
                v.m_values[0].name,
                LowerHIR_ExprNode_Inner(*v.m_values[0].value)
                ) );
        }
        else
        {
            ::HIR::ExprNode_StructLiteral::t_values values;
            for(const auto& val : v.m_values)
                values.push_back( ::std::make_pair(val.name, LowerHIR_ExprNode_Inner(*val.value)) );
            m_rv.reset( new ::HIR::ExprNode_StructLiteral( v.span(),
                LowerHIR_GenericPath(v.span(), v.m_path),
                ! v.m_path.binding().is_EnumVar(),
                LowerHIR_ExprNode_Inner_Opt(v.m_base_value.get()),
                mv$(values)
                ) );
        }
    }
    virtual void visit(::AST::ExprNode_Array& v) override {
        if( v.m_size )
        {
            m_rv.reset( new ::HIR::ExprNode_ArraySized( v.span(),
                LowerHIR_ExprNode_Inner( *v.m_values.at(0) ),
                // TODO: Should this size be a full expression on its own?
                LowerHIR_ExprNode_Inner( *v.m_size )
                ) );
        }
        else
        {
            ::std::vector< ::HIR::ExprNodeP>    vals;
            for(const auto& val : v.m_values)
                vals.push_back( LowerHIR_ExprNode_Inner(*val) );
            m_rv.reset( new ::HIR::ExprNode_ArrayList( v.span(), mv$(vals) ) );
        }
    }
    virtual void visit(::AST::ExprNode_Tuple& v) override {
        ::std::vector< ::HIR::ExprNodeP>    vals;
        for(const auto& val : v.m_values)
            vals.push_back( LowerHIR_ExprNode_Inner(*val ) );
        m_rv.reset( new ::HIR::ExprNode_Tuple( v.span(), mv$(vals) ) );
    }
    virtual void visit(::AST::ExprNode_NamedValue& v) override {
        TU_IFLET(::AST::Path::Class, v.m_path.m_class, Local, e,
            if( !v.m_path.binding().is_Variable() ) {
                BUG(v.span(), "Named value was a local, but wasn't bound - " << v.m_path);
            }
            auto slot = v.m_path.binding().as_Variable().slot;
            m_rv.reset( new ::HIR::ExprNode_Variable( v.span(), e.name, slot ) );
        )
        else {
            TU_MATCH_DEF(::AST::PathBinding, (v.m_path.binding()), (e),
            (
                auto p = LowerHIR_Path(v.span(), v.m_path);
                if( p.m_data.is_Generic() ) {
                    BUG(v.span(), "Unknown binding for PathValue but path is generic - " << v.m_path);
                }
                m_rv.reset( new ::HIR::ExprNode_PathValue( v.span(), mv$(p), ::HIR::ExprNode_PathValue::UNKNOWN ) );
                ),
            (Struct,
                ASSERT_BUG(v.span(), e.struct_ || e.hir, "PathValue bound to a struct but pointer not set - " << v.m_path);
                // Check the form and emit a PathValue if not a unit
                bool is_tuple_constructor = false;
                if( e.struct_ )
                {
                    if( e.struct_->m_data.is_Struct() ) {
                        ERROR(v.span(), E0000, "Named value referring to a struct that isn't tuple-like or unit-like - " << v.m_path);
                    }
                    is_tuple_constructor = e.struct_->m_data.is_Tuple();
                }
                else
                {
                    const auto& str = *e.hir;
                    if( str.m_data.is_Unit() ) {
                        is_tuple_constructor = false;
                    }
                    else if( str.m_data.is_Tuple() ) {
                        is_tuple_constructor = true;
                    }
                    else {
                        ERROR(v.span(), E0000, "Named value referring to a struct that isn't tuple-like or unit-like - " << v.m_path);
                    }
                }
                if( is_tuple_constructor ) {
                    m_rv.reset( new ::HIR::ExprNode_PathValue( v.span(), LowerHIR_Path(v.span(), v.m_path), ::HIR::ExprNode_PathValue::STRUCT_CONSTR ) );
                }
                else {
                    m_rv.reset( new ::HIR::ExprNode_UnitVariant( v.span(), LowerHIR_GenericPath(v.span(), v.m_path), true ) );
                }
                ),
            (EnumVar,
                ASSERT_BUG(v.span(), e.enum_ || e.hir, "PathValue bound to an enum but pointer not set - " << v.m_path);
                const auto& var_name = v.m_path.nodes().back().name();
                bool is_tuple_constructor = false;
                unsigned int var_idx;
                if( e.enum_ )
                {
                    const auto& enm = *e.enum_;
                    auto it = ::std::find_if(enm.variants().begin(), enm.variants().end(), [&](const auto& x){ return x.m_name == var_name; });
                    assert(it != enm.variants().end());

                    var_idx = static_cast<unsigned int>(it - enm.variants().begin());
                    if( it->m_data.is_Struct() ) {
                        ERROR(v.span(), E0000, "Named value referring to an enum that isn't tuple-like or unit-like - " << v.m_path);
                    }
                    is_tuple_constructor = it->m_data.is_Tuple() && it->m_data.as_Tuple().m_sub_types.size() > 0;
                }
                else
                {
                    const auto& enm = *e.hir;
                    auto idx = enm.find_variant(var_name);
                    assert(idx != SIZE_MAX);

                    var_idx = idx;
                    if( const auto* ee = enm.m_data.opt_Data() )
                    {
                        if( ee->at(idx).type == ::HIR::TypeRef::new_unit() ) {
                        }
                        // TODO: Assert that it's not a struct-like
                        else {
                            is_tuple_constructor = true;
                        }
                    }
                }
                (void)var_idx;  // TODO: Save time later by saving this.
                if( is_tuple_constructor ) {
                    m_rv.reset( new ::HIR::ExprNode_PathValue( v.span(), LowerHIR_Path(v.span(), v.m_path), ::HIR::ExprNode_PathValue::ENUM_VAR_CONSTR ) );
                }
                else {
                    m_rv.reset( new ::HIR::ExprNode_UnitVariant( v.span(), LowerHIR_GenericPath(v.span(), v.m_path), false ) );
                }
                ),
            (Function,
                m_rv.reset( new ::HIR::ExprNode_PathValue( v.span(), LowerHIR_Path(v.span(), v.m_path), ::HIR::ExprNode_PathValue::FUNCTION ) );
                ),
            (Static,
                if( e.static_ )
                {
                    if( e.static_->s_class() != ::AST::Static::CONST ) {
                        m_rv.reset( new ::HIR::ExprNode_PathValue( v.span(), LowerHIR_Path(v.span(), v.m_path), ::HIR::ExprNode_PathValue::STATIC ) );
                    }
                    else {
                        m_rv.reset( new ::HIR::ExprNode_PathValue( v.span(), LowerHIR_Path(v.span(), v.m_path), ::HIR::ExprNode_PathValue::CONSTANT ) );
                    }
                }
                else if( e.hir )
                {
                    m_rv.reset( new ::HIR::ExprNode_PathValue( v.span(), LowerHIR_Path(v.span(), v.m_path), ::HIR::ExprNode_PathValue::STATIC ) );
                }
                // HACK: If the HIR pointer is nullptr, then it refers to a `const
                else
                {
                    m_rv.reset( new ::HIR::ExprNode_PathValue( v.span(), LowerHIR_Path(v.span(), v.m_path), ::HIR::ExprNode_PathValue::CONSTANT ) );
                }
                )
            )
        }
    }

    virtual void visit(::AST::ExprNode_Field& v) override {
        m_rv.reset( new ::HIR::ExprNode_Field( v.span(),
            LowerHIR_ExprNode_Inner(*v.m_obj),
            v.m_name
            ));
    }
    virtual void visit(::AST::ExprNode_Index& v) override {
        m_rv.reset( new ::HIR::ExprNode_Index( v.span(),
            LowerHIR_ExprNode_Inner(*v.m_obj),
            LowerHIR_ExprNode_Inner(*v.m_idx)
            ));
    }
    virtual void visit(::AST::ExprNode_Deref& v) override {
        m_rv.reset( new ::HIR::ExprNode_Deref( v.span(),
            LowerHIR_ExprNode_Inner(*v.m_value)
            ));
    }
};

::std::unique_ptr< ::HIR::ExprNode> LowerHIR_ExprNode_Inner(const ::AST::ExprNode& e)
{
    LowerHIR_ExprNode_Visitor v;

    const_cast<::AST::ExprNode*>(&e)->visit( v );

    if( ! v.m_rv ) {
        BUG(e.span(), typeid(e).name() << " - Yielded a nullptr HIR node");
    }
    return mv$( v.m_rv );
}

::HIR::ExprPtr LowerHIR_ExprNode(const ::AST::ExprNode& e)
{
    return ::HIR::ExprPtr( LowerHIR_ExprNode_Inner(e) );
}
