/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * mir/mir_builder.cpp
 * - MIR Building Helper
 */
#include <algorithm>
#include "from_hir.hpp"

// --------------------------------------------------------------------
// MirBuilder
// --------------------------------------------------------------------
MirBuilder::MirBuilder(const Span& sp, const StaticTraitResolve& resolve, const ::HIR::TypeRef& ret_ty, const ::HIR::Function::args_t& args, ::MIR::Function& output):
    m_root_span(sp),
    m_resolve(resolve),
    m_ret_ty(ret_ty),
    m_args(args),
    m_output(output),
    m_lang_Box(nullptr),
    m_block_active(false),
    m_result_valid(false),
    m_fcn_scope(*this, 0)
{
    if( resolve.m_crate.m_lang_items.count("owned_box") > 0 ) {
        m_lang_Box = &resolve.m_crate.m_lang_items.at("owned_box");
    }

    set_cur_block( new_bb_unlinked() );
    m_scopes.push_back( ScopeDef { sp, ScopeType::make_Owning({ false, {} }) } );
    m_scope_stack.push_back( 0 );

    m_scopes.push_back( ScopeDef { sp, ScopeType::make_Owning({ true, {} }) } );
    m_scope_stack.push_back( 1 );

    m_arg_states.reserve( args.size() );
    for(size_t i = 0; i < args.size(); i ++)
        m_arg_states.push_back( VarState::make_Valid({}) );
    m_slot_states.resize( output.locals.size() );
    m_first_temp_idx = output.locals.size();
    DEBUG("First temporary will be " << m_first_temp_idx);

    m_if_cond_lval = this->new_temporary(::HIR::CoreType::Bool);

    // Determine which variables can be replaced by arguents
    for(size_t i = 0; i < args.size(); i ++)
    {
        const auto& pat = args[i].first;
        if( pat.m_binding.is_valid() && pat.m_binding.m_type == ::HIR::PatternBinding::Type::Move )
        {
            m_var_arg_mappings[pat.m_binding.m_slot] = i;
        }
    }
}
MirBuilder::~MirBuilder()
{
    const auto& sp = m_root_span;
    if( block_active() )
    {
        if( m_ret_ty.m_data.is_Diverge() )
        {
            terminate_scope_early(sp, fcn_scope());
            // Validation fails if this is reachable.
            //end_block( ::MIR::Terminator::make_Incomplete({}) );
            end_block( ::MIR::Terminator::make_Diverge({}) );
        }
        else
        {
            if( has_result() )
            {
                push_stmt_assign( sp, ::MIR::LValue::make_Return({}), get_result(sp) );
            }

            terminate_scope_early(sp, fcn_scope());

            end_block( ::MIR::Terminator::make_Return({}) );
        }
    }
    else
    {
        terminate_scope(sp, ScopeHandle(*this, 1), /*emit_cleanup=*/false);
        terminate_scope(sp, mv$(m_fcn_scope), /*emit_cleanup=*/false);
    }
}

const ::HIR::TypeRef* MirBuilder::is_type_owned_box(const ::HIR::TypeRef& ty) const
{
    if( m_lang_Box )
    {
        if( ! ty.m_data.is_Path() ) {
            return nullptr;
        }
        const auto& te = ty.m_data.as_Path();

        if( ! te.path.m_data.is_Generic() ) {
            return nullptr;
        }
        const auto& pe = te.path.m_data.as_Generic();

        if( pe.m_path != *m_lang_Box ) {
            return nullptr;
        }
        // TODO: Properly assert the size?
        return &pe.m_params.m_types.at(0);
    }
    else
    {
        return nullptr;
    }
}

void MirBuilder::define_variable(unsigned int idx)
{
    DEBUG("DEFINE (var) _" << idx  << ": " << m_output.locals.at(idx));
    for( auto scope_idx : ::reverse(m_scope_stack) )
    {
        auto& scope_def = m_scopes.at(scope_idx);
        TU_MATCH_DEF( ScopeType, (scope_def.data), (e),
        (
            ),
        (Owning,
            if( !e.is_temporary )
            {
                auto it = ::std::find(e.slots.begin(), e.slots.end(), idx);
                assert(it == e.slots.end());
                e.slots.push_back( idx );
                return ;
            }
            ),
        (Split,
            BUG(Span(), "Variable " << idx << " introduced within a Split");
            )
        )
    }
    BUG(Span(), "Variable " << idx << " introduced with no Variable scope");
}
::MIR::LValue MirBuilder::new_temporary(const ::HIR::TypeRef& ty)
{
    unsigned int rv = m_output.locals.size();
    DEBUG("DEFINE (temp) _" << rv << ": " << ty);

    assert(m_output.locals.size() == m_slot_states.size());
    m_output.locals.push_back( ty.clone() );
    m_slot_states.push_back( VarState::make_Invalid(InvalidType::Uninit) );
    assert(m_output.locals.size() == m_slot_states.size());

    ScopeDef* top_scope = nullptr;
    for(unsigned int i = m_scope_stack.size(); i --; )
    {
        auto idx = m_scope_stack[i];
        if( const auto* e = m_scopes.at( idx ).data.opt_Owning() ) {
            if( e->is_temporary )
            {
                top_scope = &m_scopes.at(idx);
                break ;
            }
        }
        else if( m_scopes.at(idx).data.is_Loop() )
        {
            // Newly created temporary within a loop, if there is a saved
            // state this temp needs a drop flag.
            // TODO: ^
        }
        else if( m_scopes.at(idx).data.is_Split() )
        {
            // Newly created temporary within a split, if there is a saved
            // state this temp needs a drop flag.
            // TODO: ^
        }
        else
        {
            // Nothign.
        }
    }
    assert( top_scope );
    auto& tmp_scope = top_scope->data.as_Owning();
    assert(tmp_scope.is_temporary);
    tmp_scope.slots.push_back( rv );
    return ::MIR::LValue::make_Local(rv);
}
::MIR::LValue MirBuilder::lvalue_or_temp(const Span& sp, const ::HIR::TypeRef& ty, ::MIR::RValue val)
{
    TU_IFLET(::MIR::RValue, val, Use, e,
        return mv$(e);
    )
    else {
        auto temp = new_temporary(ty);
        push_stmt_assign( sp, temp.clone(), mv$(val) );
        return temp;
    }
}

::MIR::RValue MirBuilder::get_result(const Span& sp)
{
    if(!m_result_valid) {
        BUG(sp, "No value avaliable");
    }
    auto rv = mv$(m_result);
    m_result_valid = false;
    return rv;
}

::MIR::LValue MirBuilder::get_result_unwrap_lvalue(const Span& sp)
{
    auto rv = get_result(sp);
    TU_IFLET(::MIR::RValue, rv, Use, e,
        return mv$(e);
    )
    else {
        BUG(sp, "LValue expected, got RValue");
    }
}
::MIR::LValue MirBuilder::get_result_in_lvalue(const Span& sp, const ::HIR::TypeRef& ty, bool allow_missing_value/*=false*/)
{
    if( allow_missing_value && !block_active() )
    {
        return new_temporary(ty);
    }
    auto rv = get_result(sp);
    TU_IFLET(::MIR::RValue, rv, Use, e,
        return mv$(e);
    )
    else {
        auto temp = new_temporary(ty);
        push_stmt_assign( sp, ::MIR::LValue(temp.clone()), mv$(rv) );
        return temp;
    }
}
::MIR::Param MirBuilder::get_result_in_param(const Span& sp, const ::HIR::TypeRef& ty, bool allow_missing_value)
{
    if( allow_missing_value && !block_active() )
    {
        return new_temporary(ty);
    }

    auto rv = get_result(sp);
    if( auto* e = rv.opt_Constant() )
    {
        return mv$(*e);
    }
    else if( auto* e = rv.opt_Use() )
    {
        return mv$(*e);
    }
    else
    {
        auto temp = new_temporary(ty);
        push_stmt_assign( sp, ::MIR::LValue(temp.clone()), mv$(rv) );
        return ::MIR::Param( mv$(temp) );
    }
}
void MirBuilder::set_result(const Span& sp, ::MIR::RValue val)
{
    if(m_result_valid) {
        BUG(sp, "Pushing a result over an existing result");
    }
    m_result = mv$(val);
    m_result_valid = true;
}

void MirBuilder::push_stmt_assign(const Span& sp, ::MIR::LValue dst, ::MIR::RValue val)
{
    DEBUG(dst << " = " << val);
    ASSERT_BUG(sp, m_block_active, "Pushing statement with no active block");
    ASSERT_BUG(sp, dst.tag() != ::MIR::LValue::TAGDEAD, "");
    ASSERT_BUG(sp, val.tag() != ::MIR::RValue::TAGDEAD, "");

    auto moved_param = [&](const ::MIR::Param& p) {
        if(const auto* e = p.opt_LValue()) {
            this->moved_lvalue(sp, *e);
        }
        };
    TU_MATCHA( (val), (e),
    (Use,
        this->moved_lvalue(sp, e);
        ),
    (Constant,
        ),
    (SizedArray,
        moved_param(e.val);
        ),
    (Borrow,
        if( e.type == ::HIR::BorrowType::Owned ) {
            TODO(sp, "Move using &move");
            // Likely would require a marker that ensures that the memory isn't reused.
            this->moved_lvalue(sp, e.val);
        }
        else {
            // Doesn't move
        }
        ),
    (Cast,
        this->moved_lvalue(sp, e.val);
        ),
    (BinOp,
        switch(e.op)
        {
        case ::MIR::eBinOp::EQ:
        case ::MIR::eBinOp::NE:
        case ::MIR::eBinOp::GT:
        case ::MIR::eBinOp::GE:
        case ::MIR::eBinOp::LT:
        case ::MIR::eBinOp::LE:
            // Takes an implicit borrow... and only works on copy, so why is this block here?
            break;
        default:
            moved_param(e.val_l);
            moved_param(e.val_r);
            break;
        }
        ),
    (UniOp,
        this->moved_lvalue(sp, e.val);
        ),
    (DstMeta,
        // Doesn't move
        ),
    (DstPtr,
        // Doesn't move
        ),
    (MakeDst,
        moved_param(e.ptr_val);
        moved_param(e.meta_val);
        ),
    (Tuple,
        for(const auto& val : e.vals)
            moved_param(val);
        ),
    (Array,
        for(const auto& val : e.vals)
            moved_param(val);
        ),
    (Variant,
        moved_param(e.val);
        ),
    (Struct,
        for(const auto& val : e.vals)
            moved_param(val);
        )
    )

    // Drop target if populated
    mark_value_assigned(sp, dst);
    this->push_stmt( sp, ::MIR::Statement::make_Assign({ mv$(dst), mv$(val) }) );
}
void MirBuilder::push_stmt_drop(const Span& sp, ::MIR::LValue val, unsigned int flag/*=~0u*/)
{
    ASSERT_BUG(sp, m_block_active, "Pushing statement with no active block");
    ASSERT_BUG(sp, val.tag() != ::MIR::LValue::TAGDEAD, "");

    if( lvalue_is_copy(sp, val) ) {
        // Don't emit a drop for Copy values
        return ;
    }

    this->push_stmt(sp, ::MIR::Statement::make_Drop({ ::MIR::eDropKind::DEEP, mv$(val), flag }));
}
void MirBuilder::push_stmt_drop_shallow(const Span& sp, ::MIR::LValue val, unsigned int flag/*=~0u*/)
{
    ASSERT_BUG(sp, m_block_active, "Pushing statement with no active block");
    ASSERT_BUG(sp, val.tag() != ::MIR::LValue::TAGDEAD, "");

    // TODO: Ensure that the type is a Box?

    this->push_stmt(sp, ::MIR::Statement::make_Drop({ ::MIR::eDropKind::SHALLOW, mv$(val), flag }));
}
void MirBuilder::push_stmt_asm(const Span& sp, ::MIR::Statement::Data_Asm data)
{
    ASSERT_BUG(sp, m_block_active, "Pushing statement with no active block");

    // 1. Mark outputs as valid
    for(const auto& v : data.outputs)
        mark_value_assigned(sp, v.second);

    // 2. Push
    this->push_stmt(sp, ::MIR::Statement::make_Asm( mv$(data) ));
}
void MirBuilder::push_stmt_set_dropflag_val(const Span& sp, unsigned int idx, bool value)
{
    this->push_stmt(sp, ::MIR::Statement::make_SetDropFlag({ idx, value, ~0u }));
}
void MirBuilder::push_stmt_set_dropflag_other(const Span& sp, unsigned int idx, unsigned int other)
{
    this->push_stmt(sp, ::MIR::Statement::make_SetDropFlag({ idx, false, other }));
}
void MirBuilder::push_stmt_set_dropflag_default(const Span& sp, unsigned int idx)
{
    this->push_stmt(sp, ::MIR::Statement::make_SetDropFlag({ idx, this->get_drop_flag_default(sp, idx), ~0u }));
}
void MirBuilder::push_stmt(const Span& sp, ::MIR::Statement stmt)
{
    ASSERT_BUG(sp, m_block_active, "Pushing statement with no active block");
    DEBUG("BB" << m_current_block << " += " << stmt);
    m_output.blocks.at(m_current_block).statements.push_back( mv$(stmt) );
}

void MirBuilder::mark_value_assigned(const Span& sp, const ::MIR::LValue& dst)
{
    VarState*   state_p = nullptr;
    TU_MATCH_DEF(::MIR::LValue, (dst), (e),
    (
        ),
    (Return,
        // Don't drop.
        // No state tracking for the return value
        ),
    (Argument,
        state_p = &get_slot_state_mut(sp, e.idx, SlotType::Argument);
        ),
    (Local,
        state_p = &get_slot_state_mut(sp, e, SlotType::Local);
        )
    )

    if( state_p )
    {
        TU_IFLET( VarState, (*state_p), Invalid, se,
            ASSERT_BUG(sp, se != InvalidType::Descoped, "Assining of descoped variable - " << dst);
        )
        drop_value_from_state(sp, *state_p, dst.clone());
        *state_p = VarState::make_Valid({});
    }
}

void MirBuilder::raise_temporaries(const Span& sp, const ::MIR::LValue& val, const ScopeHandle& scope, bool to_above/*=false*/)
{
    TRACE_FUNCTION_F(val);
    TU_MATCH_DEF(::MIR::LValue, (val), (e),
    (
        // No raising of these source values?
        return ;
        ),
    // TODO: This may not be correct, because it can change the drop points and ordering
    // HACK: Working around cases where values are dropped while the result is not yet used.
    (Index,
        raise_temporaries(sp, *e.val, scope, to_above);
        raise_temporaries(sp, *e.idx, scope, to_above);
        return ;
        ),
    (Deref,
        raise_temporaries(sp, *e.val, scope, to_above);
        return ;
        ),
    (Field,
        raise_temporaries(sp, *e.val, scope, to_above);
        return ;
        ),
    (Downcast,
        raise_temporaries(sp, *e.val, scope, to_above);
        return ;
        ),
    // Actual value types
    (Local,
        )
    )
    ASSERT_BUG(sp, val.is_Local(), "Hit value raising code with non-variable value - " << val);
    const auto idx = val.as_Local();
    bool is_temp = (idx >= m_first_temp_idx);
    /*
    if( !is_temp ) {
        return ;
    }
    */

    // Find controlling scope
    auto scope_it = m_scope_stack.rbegin();
    while( scope_it != m_scope_stack.rend() )
    {
        auto& scope_def = m_scopes.at(*scope_it);

        if( *scope_it == scope.idx && !to_above )
        {
            DEBUG(val << " defined in or above target (scope " << scope << ")");
        }

        TU_IFLET( ScopeType, scope_def.data, Owning, e,
            if( e.is_temporary == is_temp )
            {
                auto tmp_it = ::std::find(e.slots.begin(), e.slots.end(), idx);
                if( tmp_it != e.slots.end() )
                {
                    e.slots.erase( tmp_it );
                    DEBUG("Raise slot " << idx << " from " << *scope_it);
                    break ;
                }
            }
            else
            {
                // TODO: Should this care about variables?
            }
        )
        else
        {
            // TODO: Does this need to handle this value being set in the
            // split scopes?
        }
        // If the variable was defined above the desired scope (i.e. this didn't find it), return
        if( *scope_it == scope.idx )
        {
            DEBUG("Value " << val << " is defined above the target (scope " << scope << ")");
            return ;
        }
        ++scope_it;
    }
    if( scope_it == m_scope_stack.rend() )
    {
        // Temporary wasn't defined in a visible scope?
        BUG(sp, val << " wasn't defined in a visible scope");
        return ;
    }

    // If the definition scope was the target scope
    bool target_seen = false;
    if( *scope_it == scope.idx )
    {
        if( to_above ) {
            // Want to shift to any above (but not including) it
            ++ scope_it;
        }
        else {
            // Want to shift to it or above.
        }

        target_seen = true;
    }
    else
    {
        // Don't bother searching the original definition scope
        ++scope_it;
    }

    // Iterate stack until:
    // - The target scope is seen
    // - AND a scope was found for it
    for( ; scope_it != m_scope_stack.rend(); ++ scope_it )
    {
        auto& scope_def = m_scopes.at(*scope_it);
        DEBUG("> Cross " << *scope_it << " - " << scope_def.data.tag_str());

        if( *scope_it == scope.idx )
        {
            target_seen = true;
        }

        TU_IFLET( ScopeType, scope_def.data, Owning, e,
            if( target_seen && e.is_temporary == is_temp )
            {
                e.slots.push_back( idx );
                DEBUG("- to " << *scope_it);
                return ;
            }
        )
        else if( auto* sd_loop = scope_def.data.opt_Loop() )
        {
            // If there is an exit state present, ensure that this variable is
            // present in that state (as invalid, as it can't have been valid
            // externally)
            if( sd_loop->exit_state_valid )
            {
                DEBUG("Adding " << val << " as unset to loop exit state");
                auto v = sd_loop->exit_state.states.insert( ::std::make_pair(idx, VarState(InvalidType::Uninit)) );
                ASSERT_BUG(sp, v.second, "Raising " << val << " which already had a state entry");
            }
            else
            {
                DEBUG("Crossing loop with no existing exit state");
            }
        }
        else if( auto* sd_split = scope_def.data.opt_Split() )
        {
            // If the split has already registered an exit state, ensure that
            // this variable is present in it. (as invalid)
            if( sd_split->end_state_valid )
            {
                DEBUG("Adding " << val << " as unset to loop exit state");
                auto v = sd_split->end_state.states.insert( ::std::make_pair(idx, VarState(InvalidType::Uninit)) );
                ASSERT_BUG(sp, v.second, "Raising " << val << " which already had a state entry");
            }
            else
            {
                DEBUG("Crossing split with no existing end state");
            }

            // TODO: This should update the outer state to unset.
            auto& arm = sd_split->arms.back();
            arm.states.insert(::std::make_pair( idx, get_slot_state(sp, idx, SlotType::Local).clone() ));
            m_slot_states.at(idx) = VarState(InvalidType::Uninit);
        }
        else
        {
            BUG(sp, "Crossing unknown scope type - " << scope_def.data.tag_str());
        }
    }
    BUG(sp, "Couldn't find a scope to raise " << val << " into");
}
void MirBuilder::raise_temporaries(const Span& sp, const ::MIR::RValue& rval, const ScopeHandle& scope, bool to_above/*=false*/)
{
    auto raise_vars = [&](const ::MIR::Param& p) {
        if( const auto* e = p.opt_LValue() )
            this->raise_temporaries(sp, *e, scope, to_above);
        };
    TU_MATCHA( (rval), (e),
    (Use,
        this->raise_temporaries(sp, e, scope, to_above);
        ),
    (Constant,
        ),
    (SizedArray,
        raise_vars(e.val);
        ),
    (Borrow,
        // TODO: Wait, is this valid?
        this->raise_temporaries(sp, e.val, scope, to_above);
        ),
    (Cast,
        this->raise_temporaries(sp, e.val, scope, to_above);
        ),
    (BinOp,
        raise_vars(e.val_l);
        raise_vars(e.val_r);
        ),
    (UniOp,
        this->raise_temporaries(sp, e.val, scope, to_above);
        ),
    (DstMeta,
        this->raise_temporaries(sp, e.val, scope, to_above);
        ),
    (DstPtr,
        this->raise_temporaries(sp, e.val, scope, to_above);
        ),
    (MakeDst,
        raise_vars(e.ptr_val);
        raise_vars(e.meta_val);
        ),
    (Tuple,
        for(const auto& val : e.vals)
            raise_vars(val);
        ),
    (Array,
        for(const auto& val : e.vals)
            raise_vars(val);
        ),
    (Variant,
        raise_vars(e.val);
        ),
    (Struct,
        for(const auto& val : e.vals)
            raise_vars(val);
        )
    )
}

void MirBuilder::set_cur_block(unsigned int new_block)
{
    ASSERT_BUG(Span(), !m_block_active, "Updating block when previous is active");
    ASSERT_BUG(Span(), new_block < m_output.blocks.size(), "Invalid block ID being started - " << new_block);
    ASSERT_BUG(Span(), m_output.blocks[new_block].terminator.is_Incomplete(), "Attempting to resume a completed block - BB" << new_block);
    DEBUG("BB" << new_block << " START");
    m_current_block = new_block;
    m_block_active = true;
}
void MirBuilder::end_block(::MIR::Terminator term)
{
    if( !m_block_active ) {
        BUG(Span(), "Terminating block when none active");
    }
    DEBUG("BB" << m_current_block << " END -> " << term);
    m_output.blocks.at(m_current_block).terminator = mv$(term);
    m_block_active = false;
    m_current_block = 0;
}
::MIR::BasicBlockId MirBuilder::pause_cur_block()
{
    if( !m_block_active ) {
        BUG(Span(), "Pausing block when none active");
    }
    DEBUG("BB" << m_current_block << " PAUSE");
    m_block_active = false;
    auto rv = m_current_block;
    m_current_block = 0;
    return rv;
}
::MIR::BasicBlockId MirBuilder::new_bb_linked()
{
    auto rv = new_bb_unlinked();
    DEBUG("BB" << rv);
    end_block( ::MIR::Terminator::make_Goto(rv) );
    set_cur_block(rv);
    return rv;
}
::MIR::BasicBlockId MirBuilder::new_bb_unlinked()
{
    auto rv = m_output.blocks.size();
    DEBUG("BB" << rv);
    m_output.blocks.push_back({});
    return rv;
}


unsigned int MirBuilder::new_drop_flag(bool default_state)
{
    auto rv = m_output.drop_flags.size();
    m_output.drop_flags.push_back(default_state);
    for(size_t i = m_scope_stack.size(); i --;)
    {
        if( auto* e = m_scopes.at(m_scope_stack[i]).data.opt_Loop() )
        {
            e->drop_flags.push_back(rv);
            break;
        }
    }
    DEBUG("(" << default_state << ") = " << rv);
    return rv;
}
unsigned int MirBuilder::new_drop_flag_and_set(const Span& sp, bool set_state)
{
    auto rv = new_drop_flag(!set_state);
    push_stmt_set_dropflag_val(sp, rv, set_state);
    return rv;
}
bool MirBuilder::get_drop_flag_default(const Span& sp, unsigned int idx)
{
    return m_output.drop_flags.at(idx);
}

ScopeHandle MirBuilder::new_scope_var(const Span& sp)
{
    unsigned int idx = m_scopes.size();
    m_scopes.push_back( ScopeDef {sp, ScopeType::make_Owning({ false, {} })} );
    m_scope_stack.push_back( idx );
    DEBUG("START (var) scope " << idx);
    return ScopeHandle { *this, idx };
}
ScopeHandle MirBuilder::new_scope_temp(const Span& sp)
{
    unsigned int idx = m_scopes.size();

    m_scopes.push_back( ScopeDef {sp, ScopeType::make_Owning({ true, {} })} );
    m_scope_stack.push_back( idx );
    DEBUG("START (temp) scope " << idx);
    return ScopeHandle { *this, idx };
}
ScopeHandle MirBuilder::new_scope_split(const Span& sp)
{
    unsigned int idx = m_scopes.size();
    m_scopes.push_back( ScopeDef {sp, ScopeType::make_Split({})} );
    m_scopes.back().data.as_Split().arms.push_back( {} );
    m_scope_stack.push_back( idx );
    DEBUG("START (split) scope " << idx);
    return ScopeHandle { *this, idx };
}
ScopeHandle MirBuilder::new_scope_loop(const Span& sp)
{
    unsigned int idx = m_scopes.size();
    m_scopes.push_back( ScopeDef {sp, ScopeType::make_Loop({})} );
    m_scopes.back().data.as_Loop().entry_bb = m_current_block;
    m_scope_stack.push_back( idx );
    DEBUG("START (loop) scope " << idx);
    return ScopeHandle { *this, idx };
}
ScopeHandle MirBuilder::new_scope_freeze(const Span& sp)
{
    unsigned int idx = m_scopes.size();
    m_scopes.push_back( ScopeDef {sp, ScopeType::make_Freeze({})} );
    m_scope_stack.push_back( idx );
    DEBUG("START (freeze) scope " << idx);
    return ScopeHandle { *this, idx };
}
void MirBuilder::terminate_scope(const Span& sp, ScopeHandle scope, bool emit_cleanup/*=true*/)
{
    TRACE_FUNCTION_F("DONE scope " << scope.idx << " - " << (emit_cleanup ? "CLEANUP" : "NO CLEANUP"));
    // 1. Check that this is the current scope (at the top of the stack)
    if( m_scope_stack.empty() || m_scope_stack.back() != scope.idx )
    {
        DEBUG("- m_scope_stack = [" << m_scope_stack << "]");
        auto it = ::std::find( m_scope_stack.begin(), m_scope_stack.end(), scope.idx );
        if( it == m_scope_stack.end() )
            BUG(sp, "Terminating scope not on the stack - scope " << scope.idx);
        BUG(sp, "Terminating scope " << scope.idx << " when not at top of stack, " << (m_scope_stack.end() - it - 1) << " scopes in the way");
    }

    auto& scope_def = m_scopes.at(scope.idx);
    //if( emit_cleanup ) {
    //    ASSERT_BUG( sp, scope_def.complete == false, "Terminating scope which is already terminated" );
    //}

    if( emit_cleanup && scope_def.complete == false )
    {
        // 2. Emit drops for all non-moved variables (share with below)
        drop_scope_values(scope_def);

        // Emit ScopeEnd for all controlled values
        #if 0
        ::MIR::Statement::Data_ScopeEnd se;
        if(const auto* e = scope_def.data.opt_Variables() ) {
            se.vars = e->vars;
        }
        else if(const auto* e = scope_def.data.opt_Temporaries()) {
            se.tmps = e->temporaries;
        }
        else {
        }
        // Only push the ScopeEnd if there were variables to end
        if( !se.vars.empty() || !se.tmps.empty() ) {
            this->push_stmt(sp, ::MIR::Statement( mv$(se) ));
        }
        #endif
    }

    // 3. Pop scope (last because `drop_scope_values` uses the stack)
    m_scope_stack.pop_back();

    complete_scope(scope_def);
}
void MirBuilder::raise_all(const Span& sp, ScopeHandle source, const ScopeHandle& target)
{
    TRACE_FUNCTION_F("scope " << source.idx << " => " << target.idx);

    // 1. Check that this is the current scope (at the top of the stack)
    if( m_scope_stack.empty() || m_scope_stack.back() != source.idx )
    {
        DEBUG("- m_scope_stack = [" << m_scope_stack << "]");
        auto it = ::std::find( m_scope_stack.begin(), m_scope_stack.end(), source.idx );
        if( it == m_scope_stack.end() )
            BUG(sp, "Terminating scope not on the stack - scope " << source.idx);
        BUG(sp, "Terminating scope " << source.idx << " when not at top of stack, " << (m_scope_stack.end() - it - 1) << " scopes in the way");
    }
    auto& src_scope_def = m_scopes.at(source.idx);

#if 1
    ASSERT_BUG(sp, src_scope_def.data.is_Owning(), "Rasising scopes can only be done on temporaries (source)");
    ASSERT_BUG(sp, src_scope_def.data.as_Owning().is_temporary, "Rasising scopes can only be done on temporaries (source)");
    auto& src_list = src_scope_def.data.as_Owning().slots;
    for(auto idx : src_list)
    {
        DEBUG("> Raising " << ::MIR::LValue::make_Local(idx));
        assert(idx >= m_first_temp_idx);
    }

    // Seek up stack until the target scope is seen
    auto it = m_scope_stack.rbegin() + 1;
    for( ; it != m_scope_stack.rend() && *it != target.idx; ++it)
    {
        auto& scope_def = m_scopes.at(*it);

        if(auto* sd_loop = scope_def.data.opt_Loop())
        {
            if(sd_loop->exit_state_valid)
            {
                DEBUG("Crossing loop with existing end state");
                // Insert these values as Invalid, both in the existing exit state, and in the changed list
                for(auto idx : src_list)
                {
                    auto v = sd_loop->exit_state.states.insert(::std::make_pair( idx, VarState(InvalidType::Uninit) ));
                    ASSERT_BUG(sp, v.second, "");
                }
            }
            else
            {
                DEBUG("Crossing loop with no end state");
            }

            for(auto idx : src_list)
            {
                auto v2 = sd_loop->changed_slots.insert(::std::make_pair( idx, VarState(InvalidType::Uninit) ));
                ASSERT_BUG(sp, v2.second, "");
            }
        }
        else if(auto* sd_split = scope_def.data.opt_Split())
        {
            if(sd_split->end_state_valid)
            {
                DEBUG("Crossing split with existing end state");
                // Insert these indexes as Invalid
                for(auto idx : src_list)
                {
                    auto v = sd_split->end_state.states.insert(::std::make_pair( idx, VarState(InvalidType::Uninit) ));
                    ASSERT_BUG(sp, v.second, "");
                }
            }
            else
            {
                DEBUG("Crossing split with no end state");
            }

            // TODO: Insert current state in the current arm
            assert(!sd_split->arms.empty());
            auto& arm = sd_split->arms.back();
            for(auto idx : src_list)
            {
                arm.states.insert(::std::make_pair( idx, mv$(m_slot_states.at(idx)) ));
                m_slot_states.at(idx) = VarState(InvalidType::Uninit);
            }
        }
    }
    if(it == m_scope_stack.rend())
    {
        BUG(sp, "Moving values to a scope not on the stack - scope " << target.idx);
    }
    auto& tgt_scope_def = m_scopes.at(target.idx);
    ASSERT_BUG(sp, tgt_scope_def.data.is_Owning(), "Rasising scopes can only be done on temporaries (target)");
    ASSERT_BUG(sp, tgt_scope_def.data.as_Owning().is_temporary, "Rasising scopes can only be done on temporaries (target)");

    // Move all defined variables from one to the other
    auto& tgt_list = tgt_scope_def.data.as_Owning().slots;
    tgt_list.insert( tgt_list.end(), src_list.begin(), src_list.end() );
#else
    auto list = src_scope_def.data.as_Temporaries().temporaries;
    for(auto idx : list)
    {
        this->raise_temporaries(sp, ::MIR::LValue::make_Temporary({ idx }), target);
    }
#endif

    // Scope completed
    m_scope_stack.pop_back();
    src_scope_def.complete = true;
}

void MirBuilder::terminate_scope_early(const Span& sp, const ScopeHandle& scope, bool loop_exit/*=false*/)
{
    TRACE_FUNCTION_F("EARLY scope " << scope.idx);

    // 1. Ensure that this block is in the stack
    auto it = ::std::find( m_scope_stack.begin(), m_scope_stack.end(), scope.idx );
    if( it == m_scope_stack.end() ) {
        BUG(sp, "Early-terminating scope not on the stack");
    }
    unsigned int slot = it - m_scope_stack.begin();

    bool is_conditional = false;
    for(unsigned int i = m_scope_stack.size(); i-- > slot; )
    {
        auto idx = m_scope_stack[i];
        auto& scope_def = m_scopes.at( idx );

        if( idx == scope.idx )
        {
            // If this is exiting a loop, save the state so the variable state after the loop is known.
            if( loop_exit && scope_def.data.is_Loop() )
            {
                terminate_loop_early(sp, scope_def.data.as_Loop());
            }
        }

        // If a conditional block is hit, prevent full termination of the rest
        if( scope_def.data.is_Split() || scope_def.data.is_Loop() )
            is_conditional = true;

        if( !is_conditional ) {
            DEBUG("Complete scope " << idx);
            drop_scope_values(scope_def);
            complete_scope(scope_def);
        }
        else {
            // Mark patial within this scope?
            DEBUG("Drop part of scope " << idx);

            // Emit drops for dropped values within this scope
            drop_scope_values(scope_def);
            // Inform the scope that it's been early-exited
            TU_IFLET( ScopeType, scope_def.data, Split, e,
                e.arms.back().has_early_terminated = true;
            )
        }
    }


    // Index 0 is the function scope, this only happens when about to return/panic
    if( scope.idx == 0 )
    {
        // Ensure that all arguments are dropped if they were not moved
        for(size_t i = 0; i < m_arg_states.size(); i ++)
        {
            const auto& state = get_slot_state(sp, i, SlotType::Argument);
            this->drop_value_from_state(sp, state, ::MIR::LValue::make_Argument({ static_cast<unsigned>(i) }));
        }
    }
}

namespace
{
    static void merge_state(const Span& sp, MirBuilder& builder, const ::MIR::LValue& lv, VarState& old_state, const VarState& new_state)
    {
        TRACE_FUNCTION_FR(lv << " : " << old_state << " <= " << new_state, lv << " : " << old_state);
        switch(old_state.tag())
        {
        case VarState::TAGDEAD: throw "";
        case VarState::TAG_Invalid:
            switch( new_state.tag() )
            {
            case VarState::TAGDEAD: throw "";
            case VarState::TAG_Invalid:
                // Invalid->Invalid :: Choose the highest of the invalid types (TODO)
                return ;
            case VarState::TAG_Valid:
                // Allocate a drop flag
                old_state = VarState::make_Optional( builder.new_drop_flag_and_set(sp, true) );
                return ;
            case VarState::TAG_Optional: {
                // Was invalid, now optional.
                auto flag_idx = new_state.as_Optional();
                if( true || builder.get_drop_flag_default(sp, flag_idx) != false ) {
                    #if 1
                    auto new_flag = builder.new_drop_flag(false);
                    builder.push_stmt_set_dropflag_other(sp, new_flag, flag_idx);
                    builder.push_stmt_set_dropflag_default(sp, flag_idx);
                    old_state = VarState::make_Optional( new_flag );
                    #else
                    // TODO: Rewrite history. I.e. visit all previous branches and set this drop flag to `false` in all of them
                    for(auto pos : other_arms) {
                        builder.push_df_set_at(pos, flag_idx, false);
                    }
                    TODO(sp, "Drop flag default not false when going Invalid->Optional");
                    #endif
                }
                else {
                    old_state = VarState::make_Optional( flag_idx );
                }
                return ;
                }
            case VarState::TAG_MovedOut: {
                const auto& nse = new_state.as_MovedOut();

                // Create a new state that is internally valid and uses the same drop flag
                old_state = VarState::make_MovedOut({ box$(old_state.clone()), nse.outer_flag });
                auto& ose = old_state.as_MovedOut();
                if( ose.outer_flag != ~0u )
                {
                    // If the flag's default isn't false, then create a new flag that does have such a default
                    // - Other arm (old_state) uses default, this arm (new_state) can be manipulated
                    if( builder.get_drop_flag_default(sp, ose.outer_flag) != false )
                    {
                        auto new_flag = builder.new_drop_flag(false);
                        builder.push_stmt_set_dropflag_other(sp, new_flag, nse.outer_flag);
                        builder.push_stmt_set_dropflag_default(sp, nse.outer_flag);
                        ose.outer_flag = new_flag;
#if 0
                        for(auto pos : other_arms) {
                        }
#endif
                    }
                }
                else
                {
                    // In the old arm, the container isn't valid. Create a drop flag with a default of false and set it to true
                    ose.outer_flag = builder.new_drop_flag(false);
                    builder.push_stmt_set_dropflag_val(sp, ose.outer_flag, true);
                }

                bool is_box = false;
                builder.with_val_type(sp, lv, [&](const auto& ty){
                        is_box = builder.is_type_owned_box(ty);
                        });
                if( is_box )
                {
                    merge_state(sp, builder, ::MIR::LValue::make_Deref({ box$(lv.clone()) }), *ose.inner_state, *nse.inner_state);
                }
                else
                {
                    BUG(sp, "Handle MovedOut on non-Box");
                }
                return ;
                }
            case VarState::TAG_Partial: {
                const auto& nse = new_state.as_Partial();
                bool is_enum = false;
                builder.with_val_type(sp, lv, [&](const auto& ty){
                        is_enum = ty.m_data.is_Path() && ty.m_data.as_Path().binding.is_Enum();
                        });

                // Create a partial filled with Invalid
                {
                    ::std::vector<VarState> inner; inner.reserve( nse.inner_states.size() );
                    for(size_t i = 0; i < nse.inner_states.size(); i++)
                        inner.push_back( old_state.clone() );
                    old_state = VarState::make_Partial({ mv$(inner) });
                }
                auto& ose = old_state.as_Partial();
                if( is_enum ) {
                    for(size_t i = 0; i < ose.inner_states.size(); i ++)
                    {
                        merge_state(sp, builder, ::MIR::LValue::make_Downcast({ box$(lv.clone()), static_cast<unsigned int>(i) }), ose.inner_states[i], nse.inner_states[i]);
                    }
                }
                else {
                    for(unsigned int i = 0; i < ose.inner_states.size(); i ++ )
                    {
                        merge_state(sp, builder, ::MIR::LValue::make_Field({ box$(lv.clone()), i }), ose.inner_states[i], nse.inner_states[i]);
                    }
                }
                } return;
            }
            break;
        // Valid <= ...
        case VarState::TAG_Valid:
            switch( new_state.tag() )
            {
            case VarState::TAGDEAD: throw "";
            // Valid <= Invalid
            case VarState::TAG_Invalid:
                old_state = VarState::make_Optional( builder.new_drop_flag_and_set(sp, false) );
                return ;
            // Valid <= Valid
            case VarState::TAG_Valid:
                return ;
            // Valid <= Optional
            case VarState::TAG_Optional: {
                auto flag_idx = new_state.as_Optional();
                // Was valid, now optional.
                if( builder.get_drop_flag_default(sp, flag_idx) != true ) {
                    // Allocate a new drop flag with a default state of `true` and set it to this flag?
                    #if 1
                    auto new_flag = builder.new_drop_flag(true);
                    builder.push_stmt_set_dropflag_other(sp, new_flag, flag_idx);
                    builder.push_stmt_set_dropflag_default(sp, flag_idx);
                    old_state = VarState::make_Optional( new_flag );
                    #else
                    // OR: Push an assign of this flag to every other completed arm
                    // - Cleaner generated code, but can't be used for Optional->Optional
                    for(unsigned int i = 0; i < sd_split.arms.size()-1; i ++)
                    {
                        if( sd_split.arms[i].end_block != 0 ) {
                            m_output.blocks.at( sd_split.arms[i].end_block )
                                .statements.push_back(::MIR::Statement::make_SetDropFlag({ flag_idx, true }));
                        }
                    }
                    #endif
                }
                else {
                    old_state = VarState::make_Optional( new_state.as_Optional() );
                }
                return ;
                }
            // Valid <= MovedOut
            case VarState::TAG_MovedOut: {
                const auto& nse = new_state.as_MovedOut();

                // Create a new staet that is internally valid and uses the same drop flag
                old_state = VarState::make_MovedOut({ box$(VarState::make_Valid({})), nse.outer_flag });
                auto& ose = old_state.as_MovedOut();
                if( ose.outer_flag != ~0u )
                {
                    // If the flag's default isn't true, then create a new flag that does have such a default
                    // - Other arm (old_state) uses default, this arm (new_state) can be manipulated
                    if( builder.get_drop_flag_default(sp, ose.outer_flag) != true )
                    {
                        auto new_flag = builder.new_drop_flag(true);
                        builder.push_stmt_set_dropflag_other(sp, new_flag, nse.outer_flag);
                        builder.push_stmt_set_dropflag_default(sp, nse.outer_flag);
                        ose.outer_flag = new_flag;
                    }
                }
                else
                {
                    // In both arms, the container is valid. No need for a drop flag
                }

                bool is_box = false;
                builder.with_val_type(sp, lv, [&](const auto& ty){
                        is_box = builder.is_type_owned_box(ty);
                        });

                if( is_box ) {
                    merge_state(sp, builder, ::MIR::LValue::make_Deref({ box$(lv.clone()) }), *ose.inner_state, *nse.inner_state);
                }
                else {
                    BUG(sp, "MovedOut on non-Box");
                }
                return;
                }
            // Valid <= Partial
            case VarState::TAG_Partial: {
                const auto& nse = new_state.as_Partial();
                bool is_enum = false;
                builder.with_val_type(sp, lv, [&](const auto& ty){
                        is_enum = ty.m_data.is_Path() && ty.m_data.as_Path().binding.is_Enum();
                        });

                // Create a partial filled with Valid
                {
                    ::std::vector<VarState> inner; inner.reserve( nse.inner_states.size() );
                    for(size_t i = 0; i < nse.inner_states.size(); i++)
                        inner.push_back( VarState::make_Valid({}) );
                    old_state = VarState::make_Partial({ mv$(inner) });
                }
                auto& ose = old_state.as_Partial();
                if( is_enum ) {
                    auto ilv = ::MIR::LValue::make_Downcast({ box$(lv.clone()), 0 });
                    for(size_t i = 0; i < ose.inner_states.size(); i ++)
                    {
                        merge_state(sp, builder, ilv, ose.inner_states[i], nse.inner_states[i]);
                        ilv.as_Downcast().variant_index ++;
                    }
                }
                else {
                    auto ilv = ::MIR::LValue::make_Field({ box$(lv.clone()), 0 });
                    for(unsigned int i = 0; i < ose.inner_states.size(); i ++ )
                    {
                        merge_state(sp, builder, ilv, ose.inner_states[i], nse.inner_states[i]);
                        ilv.as_Field().field_index ++;
                    }
                }
                } return;
            }
            break;
        // Optional <= ...
        case VarState::TAG_Optional:
            switch( new_state.tag() )
            {
            case VarState::TAGDEAD: throw "";
            case VarState::TAG_Invalid:
                builder.push_stmt_set_dropflag_val(sp, old_state.as_Optional(), false);
                return ;
            case VarState::TAG_Valid:
                builder.push_stmt_set_dropflag_val(sp, old_state.as_Optional(), true);
                return ;
            case VarState::TAG_Optional:
                if( old_state.as_Optional() != new_state.as_Optional() ) {
                    #if 1
                    builder.push_stmt_set_dropflag_other(sp, old_state.as_Optional(), new_state.as_Optional());
                    builder.push_stmt_set_dropflag_default(sp, new_state.as_Optional());
                    #else
                    // TODO: Rewrite history replacing one flag with another (if they have the same default)
                    #endif
                }
                return ;
            case VarState::TAG_MovedOut:
                TODO(sp, "Handle Optional->MovedOut in split scope");
            case VarState::TAG_Partial: {
                const auto& nse = new_state.as_Partial();
                bool is_enum = false;
                builder.with_val_type(sp, lv, [&](const auto& ty){
                        assert( !builder.is_type_owned_box(ty) );
                        is_enum = ty.m_data.is_Path() && ty.m_data.as_Path().binding.is_Enum();
                        });
                // Create a Partial filled with copies of the Optional
                // TODO: This can lead to contradictions when one field is moved and another not.
                // - Need to allocate a new drop flag and handle the case where old_state is the state before the
                //   split (and hence the default state of this new drop flag has to be the original state)
                //  > Could store reference to start BB and assign into it?
                //  > Can't it not be from before the split, because that would be a move when not known-valid?
                //  > Re-assign and partial drop.
                {
                    ::std::vector<VarState> inner;
                    inner.reserve( nse.inner_states.size() );
                    for(size_t i = 0; i < nse.inner_states.size(); i ++)
                    {
#if 1
                        auto new_flag = builder.new_drop_flag(builder.get_drop_flag_default(sp, old_state.as_Optional()));
                        builder.push_stmt_set_dropflag_other(sp, new_flag,  old_state.as_Optional());
                        // TODO: Move this assignment to the source block.
                        inner.push_back(VarState::make_Optional( new_flag ));
#else
                        inner.push_back(old_state.clone());
#endif
                    }
                    old_state = VarState::make_Partial({ mv$(inner) });
                }
                auto& ose = old_state.as_Partial();
                // Propagate to inners
                if( is_enum ) {
                    for(size_t i = 0; i < ose.inner_states.size(); i ++)
                    {
                        merge_state(sp, builder, ::MIR::LValue::make_Downcast({ box$(lv.clone()), static_cast<unsigned int>(i) }), ose.inner_states[i], nse.inner_states[i]);
                    }
                }
                else {
                    for(unsigned int i = 0; i < ose.inner_states.size(); i ++ )
                    {
                        merge_state(sp, builder, ::MIR::LValue::make_Field({ box$(lv.clone()), i }), ose.inner_states[i], nse.inner_states[i]);
                    }
                }
                return; }
            }
            break;
        case VarState::TAG_MovedOut: {
            auto& ose = old_state.as_MovedOut();
            bool is_box = false;
            builder.with_val_type(sp, lv, [&](const auto& ty){
                    is_box = builder.is_type_owned_box(ty);
                    });
            if( !is_box ) {
                BUG(sp, "MovedOut on non-Box");
            }
            switch( new_state.tag() )
            {
            case VarState::TAGDEAD: throw "";
            case VarState::TAG_Invalid:
            case VarState::TAG_Valid: {
                bool is_valid = new_state.is_Valid();
                if( ose.outer_flag == ~0u )
                {
                    // If not valid in new arm, then the outer state is conditional
                    if( !is_valid )
                    {
                        ose.outer_flag = builder.new_drop_flag(true);
                        builder.push_stmt_set_dropflag_val(sp, ose.outer_flag, false);
                    }
                }
                else
                {
                    builder.push_stmt_set_dropflag_val(sp, ose.outer_flag, is_valid);
                }

                merge_state(sp, builder, ::MIR::LValue::make_Deref({ box$(lv.clone()) }), *ose.inner_state, new_state);
                return ; }
            case VarState::TAG_Optional: {
                const auto& nse = new_state.as_Optional();
                if( ose.outer_flag == ~0u )
                {
                    if( ! builder.get_drop_flag_default(sp, nse) )
                    {
                        // Default wasn't true, need to make a new flag that does have a default of true
                        auto new_flag = builder.new_drop_flag(true);
                        builder.push_stmt_set_dropflag_other(sp, new_flag, nse);
                        builder.push_stmt_set_dropflag_default(sp, nse);
                        ose.outer_flag = new_flag;
                    }
                    else
                    {
                        ose.outer_flag = nse;
                    }
                }
                else
                {
                    // In this arm, assign the outer state to this drop flag
                    builder.push_stmt_set_dropflag_other(sp, ose.outer_flag, nse);
                    builder.push_stmt_set_dropflag_default(sp, nse);
                }
                merge_state(sp, builder, ::MIR::LValue::make_Deref({ box$(lv.clone()) }), *ose.inner_state, new_state);
                return; }
            case VarState::TAG_MovedOut: {
                const auto& nse = new_state.as_MovedOut();
                if( ose.outer_flag != nse.outer_flag )
                {
                    TODO(sp, "Handle mismatched flags in MovedOut");
                }
                merge_state(sp, builder, ::MIR::LValue::make_Deref({ box$(lv.clone()) }), *ose.inner_state, *nse.inner_state);
                return; }
            case VarState::TAG_Partial:
                BUG(sp, "MovedOut->Partial not valid");
            }
            break; }
        case VarState::TAG_Partial: {
            auto& ose = old_state.as_Partial();
            bool is_enum = false;
            builder.with_val_type(sp, lv, [&](const auto& ty){
                    assert( !builder.is_type_owned_box(ty) );
                    is_enum = ty.m_data.is_Path() && ty.m_data.as_Path().binding.is_Enum();
                    });
            // Need to tag for conditional shallow drop? Or just do that at the end of the split?
            // - End of the split means that the only optional state is outer drop.
            switch( new_state.tag() )
            {
            case VarState::TAGDEAD: throw "";
            case VarState::TAG_Invalid:
            case VarState::TAG_Valid:
            case VarState::TAG_Optional:
                if( is_enum ) {
                    for(size_t i = 0; i < ose.inner_states.size(); i ++)
                    {
                        merge_state(sp, builder, ::MIR::LValue::make_Downcast({ box$(lv.clone()), static_cast<unsigned int>(i) }), ose.inner_states[i], new_state);
                    }
                }
                else {
                    for(unsigned int i = 0; i < ose.inner_states.size(); i ++ )
                    {
                        merge_state(sp, builder, ::MIR::LValue::make_Field({ box$(lv.clone()), i }), ose.inner_states[i], new_state);
                    }
                }
                return ;
            case VarState::TAG_MovedOut:
                BUG(sp, "Partial->MovedOut not valid");
            case VarState::TAG_Partial: {
                const auto& nse = new_state.as_Partial();
                ASSERT_BUG(sp, ose.inner_states.size() == nse.inner_states.size(), "Partial->Partial with mismatched sizes - " << old_state << " <= " << new_state);
                if( is_enum ) {
                    for(size_t i = 0; i < ose.inner_states.size(); i ++)
                    {
                        merge_state(sp, builder, ::MIR::LValue::make_Downcast({ box$(lv.clone()), static_cast<unsigned int>(i) }), ose.inner_states[i], nse.inner_states[i]);
                    }
                }
                else {
                    for(unsigned int i = 0; i < ose.inner_states.size(); i ++ )
                    {
                        merge_state(sp, builder, ::MIR::LValue::make_Field({ box$(lv.clone()), i }), ose.inner_states[i], nse.inner_states[i]);
                    }
                }
                } return ;
            }
            } break;
        }
        BUG(sp, "Unhandled combination - " << old_state.tag_str() << " and " << new_state.tag_str());
    }
}

void MirBuilder::terminate_loop_early(const Span& sp, ScopeType::Data_Loop& sd_loop)
{
    if( sd_loop.exit_state_valid )
    {
        // Insert copies of parent state for newly changed values
        // and Merge all changed values
        auto merge_list = [sp,this](const auto& changed, auto& exit_states, ::std::function<::MIR::LValue(unsigned)> val_cb, auto type) {
            for(const auto& ent : changed)
            {
                auto idx = ent.first;
                auto it = exit_states.find(idx);
                if( it == exit_states.end() ) {
                    it = exit_states.insert(::std::make_pair( idx, ent.second.clone() )).first;
                }
                auto& old_state = it->second;
                merge_state(sp, *this, val_cb(idx), old_state,  get_slot_state(sp, idx, type));
            }
            };
        merge_list(sd_loop.changed_slots, sd_loop.exit_state.states, ::MIR::LValue::make_Local, SlotType::Local);
        merge_list(sd_loop.changed_args, sd_loop.exit_state.arg_states, [](auto v){ return ::MIR::LValue::make_Argument({v}); }, SlotType::Argument);
    }
    else
    {
        auto init_list = [sp,this](const auto& changed, auto& exit_states, auto type) {
            for(const auto& ent : changed)
            {
                DEBUG("Slot(" << ent.first << ") = " << ent.second);
                auto idx = ent.first;
                exit_states.insert(::std::make_pair( idx, get_slot_state(sp, idx, type).clone() ));
            }
            };
        // Obtain states of changed variables/temporaries
        init_list(sd_loop.changed_slots, sd_loop.exit_state.states, SlotType::Local);
        init_list(sd_loop.changed_args, sd_loop.exit_state.arg_states, SlotType::Argument);
        sd_loop.exit_state_valid = true;
    }
}

void MirBuilder::end_split_arm(const Span& sp, const ScopeHandle& handle, bool reachable, bool early/*=false*/)
{
    ASSERT_BUG(sp, handle.idx < m_scopes.size(), "Handle passed to end_split_arm is invalid");
    auto& sd = m_scopes.at( handle.idx );
    ASSERT_BUG(sp, sd.data.is_Split(), "Ending split arm on non-Split arm - " << sd.data.tag_str());
    auto& sd_split = sd.data.as_Split();
    ASSERT_BUG(sp, !sd_split.arms.empty(), "Split arm list is empty (impossible)");

    TRACE_FUNCTION_F("end split scope " << handle.idx << " arm " << (sd_split.arms.size()-1));
    if( reachable )
        ASSERT_BUG(sp, m_block_active, "Block must be active when ending a reachable split arm");

    auto& this_arm_state = sd_split.arms.back();
    this_arm_state.always_early_terminated = /*sd_split.arms.back().has_early_terminated &&*/ !reachable;

    if( sd_split.end_state_valid )
    {
        if( reachable )
        {
            auto merge_list = [sp,this](const auto& states, auto& end_states, auto type) {
                // Insert copies of the parent state
                for(const auto& ent : states) {
                    if( end_states.count(ent.first) == 0 ) {
                        end_states.insert(::std::make_pair( ent.first, get_slot_state(sp, ent.first, type, 1).clone() ));
                    }
                }
                // Merge state
                for(auto& ent : end_states)
                {
                    auto idx = ent.first;
                    auto& out_state = ent.second;

                    // Merge the states
                    auto it = states.find(idx);
                    const auto& src_state = (it != states.end() ? it->second : get_slot_state(sp, idx, type, 1));

                    auto lv = (type == SlotType::Local ? ::MIR::LValue::make_Local(idx) : ::MIR::LValue::make_Argument({idx}));
                    merge_state(sp, *this, mv$(lv), out_state, src_state);
                }
                };
            merge_list(this_arm_state.states, sd_split.end_state.states, SlotType::Local);
            merge_list(this_arm_state.arg_states, sd_split.end_state.arg_states, SlotType::Argument);
        }
        else
        {
            DEBUG("Unreachable, not merging");
        }
    }
    else
    {
        // Clone this arm's state
        for(auto& ent : this_arm_state.states)
        {
            DEBUG("Slot(" << ent.first << ") = " << ent.second);
            sd_split.end_state.states.insert(::std::make_pair( ent.first, ent.second.clone() ));
        }
        for(auto& ent : this_arm_state.arg_states)
        {
            DEBUG("Argument(" << ent.first << ") = " << ent.second);
            sd_split.end_state.arg_states.insert(::std::make_pair( ent.first, ent.second.clone() ));
        }
        sd_split.end_state_valid = true;
    }

    if( reachable )
    {
        assert(m_block_active);
    }
    if( !early )
    {
        sd_split.arms.push_back( {} );
    }
}
void MirBuilder::end_split_arm_early(const Span& sp)
{
    TRACE_FUNCTION_F("");
    size_t i = m_scope_stack.size();
    // Terminate every sequence of owning scopes
    while( i -- && m_scopes.at(m_scope_stack[i]).data.is_Owning() )
    {
        auto& scope_def = m_scopes[m_scope_stack[i]];
        // Fully drop the scope
        DEBUG("Complete scope " << m_scope_stack[i]);
        drop_scope_values(scope_def);
        complete_scope(scope_def);
    }

    if( i < m_scope_stack.size() )
    {
        if( m_scopes.at( m_scope_stack[i] ).data.is_Split() )
        {
            DEBUG("Early terminate split scope " << m_scope_stack.back());
            auto& sd = m_scopes[ m_scope_stack[i] ];
            auto& sd_split = sd.data.as_Split();
            sd_split.arms.back().has_early_terminated = true;

            // TODO: Create drop flags if required?
        }
        // TODO: What if this is a loop?
    }
}
void MirBuilder::complete_scope(ScopeDef& sd)
{
    sd.complete = true;

    TU_MATCHA( (sd.data), (e),
    (Owning,
        DEBUG("Owning (" << (e.is_temporary ? "temps" : "vars") << ") - " << e.slots);
        ),
    (Loop,
        DEBUG("Loop");
        ),
    (Split,
        ),
    (Freeze,
        //DEBUG("Freeze");
        )
    )

    struct H {
        static void apply_end_state(const Span& sp, MirBuilder& builder, SplitEnd& end_state)
        {
            for(auto& ent : end_state.states)
            {
                auto& vs = builder.get_slot_state_mut(sp, ent.first, SlotType::Local);
                if( vs != ent.second )
                {
                    DEBUG(::MIR::LValue::make_Local(ent.first) << " " << vs << " => " << ent.second);
                    vs = ::std::move(ent.second);
                }
            }
            for(auto& ent : end_state.arg_states)
            {
                auto& vs = builder.get_slot_state_mut(sp, ent.first, SlotType::Argument);
                if( vs != ent.second )
                {
                    DEBUG(::MIR::LValue::make_Argument({ent.first}) << " " << vs << " => " << ent.second);
                    vs = ::std::move(ent.second);
                }
            }
        }
    };

    // No macro for better debug output.
    if( auto* e = sd.data.opt_Loop() )
    {
        TRACE_FUNCTION_F("Loop");
        if( e->exit_state_valid )
        {
            H::apply_end_state(sd.span, *this, e->exit_state);
        }

        // Insert sets of drop flags to the first block (at the start of that block)
        auto& stmts = m_output.blocks.at(e->entry_bb).statements;
        for(auto idx : e->drop_flags)
        {
            DEBUG("Reset df$" << idx);
            stmts.insert( stmts.begin(), ::MIR::Statement::make_SetDropFlag({ idx, m_output.drop_flags.at(idx), ~0u }) );
        }
    }
    else if( sd.data.is_Split() )
    {
        auto& e = sd.data.as_Split();
        TRACE_FUNCTION_F("Split - " << (e.arms.size() - 1) << " arms");

        ASSERT_BUG(sd.span, e.end_state_valid, "");
        H::apply_end_state(sd.span, *this, e.end_state);
    }
}

void MirBuilder::with_val_type(const Span& sp, const ::MIR::LValue& val, ::std::function<void(const ::HIR::TypeRef&)> cb) const
{
    TU_MATCH(::MIR::LValue, (val), (e),
    (Return,
        TODO(sp, "Return");
        ),
    (Argument,
        cb( m_args.at(e.idx).second );
        ),
    (Local,
        cb( m_output.locals.at(e) );
        ),
    (Static,
        TU_MATCHA( (e.m_data), (pe),
        (Generic,
            ASSERT_BUG(sp, pe.m_params.m_types.empty(), "Path params on static");
            const auto& s = m_resolve.m_crate.get_static_by_path(sp, pe.m_path);
            cb( s.m_type );
            ),
        (UfcsKnown,
            TODO(sp, "Static - UfcsKnown - " << e);
            ),
        (UfcsUnknown,
            BUG(sp, "Encountered UfcsUnknown in Static - " << e);
            ),
        (UfcsInherent,
            TODO(sp, "Static - UfcsInherent - " << e);
            )
        )
        ),
    (Field,
        with_val_type(sp, *e.val, [&](const auto& ty){
            TU_MATCH_DEF( ::HIR::TypeRef::Data, (ty.m_data), (te),
            (
                BUG(sp, "Field access on unexpected type - " << ty);
                ),
            (Array,
                cb( *te.inner );
                ),
            (Slice,
                cb( *te.inner );
                ),
            (Path,
                ::HIR::TypeRef  tmp;
                if( const auto* tep = te.binding.opt_Struct() )
                {
                    const auto& str = **tep;
                    auto maybe_monomorph = [&](const ::HIR::TypeRef& t)->const ::HIR::TypeRef& {
                        if( monomorphise_type_needed(t) ) {
                            tmp = monomorphise_type(sp, str.m_params, te.path.m_data.as_Generic().m_params, t);
                            m_resolve.expand_associated_types(sp, tmp);
                            return tmp;
                        }
                        else {
                            return t;
                        }
                        };
                    TU_MATCHA( (str.m_data), (se),
                    (Unit,
                        BUG(sp, "Field on unit-like struct - " << ty);
                        ),
                    (Tuple,
                        ASSERT_BUG(sp, e.field_index < se.size(),
                            "Field index out of range in tuple-struct " << ty << " - " << e.field_index << " > " << se.size());
                        const auto& fld = se[e.field_index];
                        cb( maybe_monomorph(fld.ent) );
                        ),
                    (Named,
                        ASSERT_BUG(sp, e.field_index < se.size(),
                            "Field index out of range in struct " << ty << " - " << e.field_index << " > " << se.size());
                        const auto& fld = se[e.field_index].second;
                        cb( maybe_monomorph(fld.ent) );
                        )
                    )
                }
                else if( const auto* tep = te.binding.opt_Union() )
                {
                    BUG(sp, "Field access on a union isn't valid, use Downcast instead - " << ty);
                    const auto& unm = **tep;
                    auto maybe_monomorph = [&](const ::HIR::TypeRef& t)->const ::HIR::TypeRef& {
                        if( monomorphise_type_needed(t) ) {
                            tmp = monomorphise_type(sp, unm.m_params, te.path.m_data.as_Generic().m_params, t);
                            m_resolve.expand_associated_types(sp, tmp);
                            return tmp;
                        }
                        else {
                            return t;
                        }
                        };
                    ASSERT_BUG(sp, e.field_index < unm.m_variants.size(), "Field index out of range for union");
                    cb( maybe_monomorph(unm.m_variants.at(e.field_index).second.ent) );
                }
                else
                {
                    BUG(sp, "Field acess on unexpected type - " << ty);
                }
                ),
            (Tuple,
                ASSERT_BUG(sp, e.field_index < te.size(), "Field index out of range in tuple " << e.field_index << " >= " << te.size());
                cb( te[e.field_index] );
                )
            )
            });
        ),
    (Deref,
        with_val_type(sp, *e.val, [&](const auto& ty){
            TU_MATCH_DEF( ::HIR::TypeRef::Data, (ty.m_data), (te),
            (
                BUG(sp, "Deref on unexpected type - " << ty);
                ),
            (Path,
                if( const auto* inner_ptr = this->is_type_owned_box(ty) )
                {
                    cb( *inner_ptr );
                }
                else {
                    BUG(sp, "Deref on unexpected type - " << ty);
                }
                ),
            (Pointer,
                cb(*te.inner);
                ),
            (Borrow,
                cb(*te.inner);
                )
            )
            });
        ),
    (Index,
        with_val_type(sp, *e.val, [&](const auto& ty){
            TU_MATCH_DEF( ::HIR::TypeRef::Data, (ty.m_data), (te),
            (
                BUG(sp, "Index on unexpected type - " << ty);
                ),
            (Slice,
                cb(*te.inner);
                ),
            (Array,
                cb(*te.inner);
                )
            )
            });
        ),
    (Downcast,
        with_val_type(sp, *e.val, [&](const auto& ty){
            TU_MATCH_DEF( ::HIR::TypeRef::Data, (ty.m_data), (te),
            (
                BUG(sp, "Downcast on unexpected type - " << ty);
                ),
            (Path,
                if( const auto* pbe = te.binding.opt_Enum() )
                {
                    const auto& enm = **pbe;
                    ASSERT_BUG(sp, enm.m_data.is_Data(), "Downcast on non-data enum");
                    const auto& variants = enm.m_data.as_Data();
                    ASSERT_BUG(sp, e.variant_index < variants.size(), "Variant index out of range");
                    const auto& variant = variants[e.variant_index];

                    if( monomorphise_type_needed(variant.type) ) {
                        auto tmp = monomorphise_type(sp, enm.m_params, te.path.m_data.as_Generic().m_params, variant.type);
                        m_resolve.expand_associated_types(sp, tmp);
                        cb(tmp);
                    }
                    else {
                        cb(variant.type);
                    }
                }
                else if( const auto* pbe = te.binding.opt_Union() )
                {
                    const auto& unm = **pbe;
                    ASSERT_BUG(sp, e.variant_index < unm.m_variants.size(), "Variant index out of range");
                    const auto& variant = unm.m_variants.at(e.variant_index);
                    const auto& fld = variant.second;

                    if( monomorphise_type_needed(fld.ent) ) {
                        auto sty = monomorphise_type(sp, unm.m_params, te.path.m_data.as_Generic().m_params, fld.ent);
                        m_resolve.expand_associated_types(sp, sty);
                        cb(sty);
                    }
                    else {
                        cb(fld.ent);
                    }
                }
                else
                {
                    BUG(sp, "Downcast on non-Enum/Union - " << ty << " for " << val);
                }
                )
            )
            });
        )
    )
}

bool MirBuilder::lvalue_is_copy(const Span& sp, const ::MIR::LValue& val) const
{
    int rv = 0;
    with_val_type(sp, val, [&](const auto& ty){
        DEBUG("[lvalue_is_copy] ty="<<ty);
        rv = (m_resolve.type_is_copy(sp, ty) ? 2 : 1);
        });
    ASSERT_BUG(sp, rv != 0, "Type for " << val << " can't be determined");
    return rv == 2;
}

const VarState& MirBuilder::get_slot_state(const Span& sp, unsigned int idx, SlotType type, unsigned int skip_count/*=0*/) const
{
    // 1. Find an applicable Split scope
    for( auto scope_idx : ::reverse(m_scope_stack) )
    {
        const auto& scope_def = m_scopes.at(scope_idx);
        TU_MATCH_DEF( ScopeType, (scope_def.data), (e),
        (
            ),
        (Owning,
            if( type == SlotType::Local )
            {
                auto it = ::std::find(e.slots.begin(), e.slots.end(), idx);
                if( it != e.slots.end() ) {
                    break ;
                }
            }
            ),
        (Split,
            const auto& cur_arm = e.arms.back();
            const auto& list = (type == SlotType::Local ? cur_arm.states : cur_arm.arg_states);
            auto it = list.find(idx);
            if( it != list.end() )
            {
                if( ! skip_count -- )
                {
                    return it->second;
                }
            }
            )
        )
    }
    switch(type)
    {
    case SlotType::Local:
        if( idx == ~0u )
        {
            return m_return_state;
        }
        else
        {
            ASSERT_BUG(sp, idx < m_slot_states.size(), "Slot " << idx << " out of range for state table");
            return m_slot_states.at(idx);
        }
        break;
    case SlotType::Argument:
        ASSERT_BUG(sp, idx < m_arg_states.size(), "Argument " << idx << " out of range for state table");
        return m_arg_states.at(idx);
    }
    throw "";
}
VarState& MirBuilder::get_slot_state_mut(const Span& sp, unsigned int idx, SlotType type)
{
    VarState* ret = nullptr;
    for( auto scope_idx : ::reverse(m_scope_stack) )
    {
        auto& scope_def = m_scopes.at(scope_idx);
        if( const auto* e = scope_def.data.opt_Owning() )
        {
            if( type == SlotType::Local )
            {
                auto it = ::std::find(e->slots.begin(), e->slots.end(), idx);
                if( it != e->slots.end() ) {
                    break ;
                }
            }
        }
        else if( auto* e = scope_def.data.opt_Split() )
        {
            auto& cur_arm = e->arms.back();
            if( ! ret )
            {
                if( idx == ~0u ) {
                }
                else {
                    auto& states = (type == SlotType::Local ? cur_arm.states : cur_arm.arg_states);
                    auto it = states.find(idx);
                    if( it == states.end() )
                    {
                        DEBUG("Split new (scope " << scope_idx << ")");
                        it = states.insert(::std::make_pair( idx, get_slot_state(sp, idx, type).clone() )).first;
                    }
                    else
                    {
                        DEBUG("Split existing (scope " << scope_idx << ")");
                    }
                    ret = &it->second;
                }
            }
        }
        else if( auto* e = scope_def.data.opt_Loop() )
        {
            if( idx == ~0u )
            {
            }
            else
            {
                auto& states = (type == SlotType::Local ? e->changed_slots : e->changed_args);
                if( states.count(idx) == 0 )
                {
                    auto state = e->exit_state_valid ? get_slot_state(sp, idx, type).clone() : VarState::make_Valid({});
                    states.insert(::std::make_pair( idx, mv$(state) ));
                }
            }
        }
        else if( scope_def.data.is_Freeze() )
        {
            if( type == SlotType::Local && idx == m_if_cond_lval.as_Local() )
            {
            }
            else
            {
                // NOTE: This is only used in match conditions
                DEBUG("Mutating state of ?" << idx);
                ERROR(sp, E0000, "Attempting to move/initialise a value where not allowed");
            }
        }
        else
        {
        }
    }
    if( ret )
    {
        return *ret;
    }
    else
    {
        switch(type)
        {
        case SlotType::Local:
            if( idx == ~0u )
            {
                return m_return_state;
            }
            else
            {
                return m_slot_states.at(idx);
            }
        case SlotType::Argument:
            return m_arg_states.at(idx);
        }
        throw "";
    }
}

const VarState& MirBuilder::get_val_state(const Span& sp, const ::MIR::LValue& lv, unsigned int skip_count)
{
    TODO(sp, "");
}
VarState& MirBuilder::get_val_state_mut(const Span& sp, const ::MIR::LValue& lv)
{
    TRACE_FUNCTION_F(lv);
    TU_MATCHA( (lv), (e),
    (Return,
        BUG(sp, "Move of return value");
        return get_slot_state_mut(sp, ~0u, SlotType::Local);
        ),
    (Argument,
        return get_slot_state_mut(sp, e.idx, SlotType::Argument);
        ),
    (Local,
        return get_slot_state_mut(sp, e, SlotType::Local);
        ),
    (Static,
        BUG(sp, "Attempting to mutate state of a static");
        ),
    (Field,
        auto& ivs = get_val_state_mut(sp, *e.val);
        VarState    tpl;
        TU_MATCHA( (ivs), (ivse),
        (Invalid,
            //BUG(sp, "Mutating inner state of an invalidated composite - " << lv);
            tpl = VarState::make_Valid({});
            ),
        (MovedOut,
            BUG(sp, "Field on value with MovedOut state - " << lv);
            ),
        (Partial,
            ),
        (Optional,
            tpl = ivs.clone();
            ),
        (Valid,
            tpl = VarState::make_Valid({});
            )
        )
        if( !ivs.is_Partial() )
        {
            size_t n_flds = 0;
            with_val_type(sp, *e.val, [&](const auto& ty) {
                DEBUG("ty = " << ty);
                if(const auto* e = ty.m_data.opt_Path()) {
                    ASSERT_BUG(sp, e->binding.is_Struct(), "");
                    const auto& str = *e->binding.as_Struct();
                    TU_MATCHA( (str.m_data), (se),
                    (Unit,
                        BUG(sp, "Field access of unit-like struct");
                        ),
                    (Tuple,
                        n_flds = se.size();
                        ),
                    (Named,
                        n_flds = se.size();
                        )
                    )
                }
                else if(const auto* e = ty.m_data.opt_Tuple()) {
                    n_flds = e->size();
                }
                else if(const auto* e = ty.m_data.opt_Array()) {
                    n_flds = e->size_val;
                }
                else {
                    TODO(sp, "Determine field count for " << ty);
                }
                });
            ::std::vector<VarState> inner_vs; inner_vs.reserve(n_flds);
            for(size_t i = 0; i < n_flds; i++)
                inner_vs.push_back( tpl.clone() );
            ivs = VarState::make_Partial({ mv$(inner_vs) });
        }
        return ivs.as_Partial().inner_states.at(e.field_index);
        ),
    (Deref,
        // HACK: If the dereferenced type is a Box ("owned_box") then hack in move and shallow drop
        bool is_box = false;
        if( this->m_lang_Box )
        {
            with_val_type(sp, *e.val, [&](const auto& ty){
                DEBUG("ty = " << ty);
                is_box = this->is_type_owned_box(ty);
                });
        }


        if( is_box )
        {
            auto& ivs = get_val_state_mut(sp, *e.val);
            if( ! ivs.is_MovedOut() )
            {
                ::std::vector<VarState> inner;
                inner.push_back(VarState::make_Valid({}));
                unsigned int drop_flag = (ivs.is_Optional() ? ivs.as_Optional() : ~0u);
                ivs = VarState::make_MovedOut({ box$(VarState::make_Valid({})), drop_flag });
            }
            return *ivs.as_MovedOut().inner_state;
        }
        else
        {
            BUG(sp, "Move out of deref with non-Copy values - &move? - " << lv << " : " << FMT_CB(ss, this->with_val_type(sp, lv, [&](const auto& ty){ss<<ty;});) );
        }
        ),
    (Index,
        BUG(sp, "Move out of index with non-Copy values - Partial move?");
        ),
    (Downcast,
        // TODO: What if the inner is Copy? What if the inner is a hidden pointer?
        auto& ivs = get_val_state_mut(sp, *e.val);
        //static VarState ivs; ivs = VarState::make_Valid({});

        if( !ivs.is_Partial() )
        {
            ASSERT_BUG(sp, !ivs.is_MovedOut(), "Downcast of a MovedOut value");

            size_t var_count = 0;
            with_val_type(sp, *e.val, [&](const auto& ty){
                DEBUG("ty = " << ty);
                ASSERT_BUG(sp, ty.m_data.is_Path(), "Downcast on non-Path type - " << ty);
                const auto& pb = ty.m_data.as_Path().binding;
                // TODO: What about unions?
                // - Iirc, you can't move out of them so they will never have state mutated
                if( pb.is_Enum() )
                {
                    const auto& enm = *pb.as_Enum();
                    var_count = enm.num_variants();
                }
                else if( const auto* pbe = pb.opt_Union() )
                {
                    const auto& unm = **pbe;
                    var_count = unm.m_variants.size();
                }
                else
                {
                    BUG(sp, "Downcast on non-Enum/Union - " << ty);
                }
                });

            ::std::vector<VarState> inner;
            for(size_t i = 0; i < var_count; i ++)
            {
                inner.push_back( VarState::make_Invalid(InvalidType::Uninit) );
            }
            inner[e.variant_index] = mv$(ivs);
            ivs = VarState::make_Partial({ mv$(inner) });
        }

        return ivs.as_Partial().inner_states.at(e.variant_index);
        )
    )
    BUG(sp, "Fell off send of get_val_state_mut");
}

void MirBuilder::drop_value_from_state(const Span& sp, const VarState& vs, ::MIR::LValue lv)
{
    TU_MATCHA( (vs), (vse),
    (Invalid,
        ),
    (Valid,
        push_stmt_drop(sp, mv$(lv));
        ),
    (MovedOut,
        bool is_box = false;
        with_val_type(sp, lv, [&](const auto& ty){
            is_box = this->is_type_owned_box(ty);
            });
        if( is_box )
        {
            drop_value_from_state(sp, *vse.inner_state, ::MIR::LValue::make_Deref({ box$(lv.clone()) }));
            push_stmt_drop_shallow(sp, mv$(lv), vse.outer_flag);
        }
        else
        {
            TODO(sp, "");
        }
        ),
    (Partial,
        bool is_enum = false;
        bool is_union = false;
        with_val_type(sp, lv, [&](const auto& ty){
            is_enum = ty.m_data.is_Path() && ty.m_data.as_Path().binding.is_Enum();
            is_union = ty.m_data.is_Path() && ty.m_data.as_Path().binding.is_Union();
            });
        if(is_enum)
        {
            DEBUG("TODO: Switch based on enum value");
            //for(size_t i = 0; i < vse.inner_states.size(); i ++)
            //{
            //    drop_value_from_state(sp, vse.inner_states[i], ::MIR::LValue::make_Downcast({ box$(lv.clone()), static_cast<unsigned int>(i) }));
            //}
        }
        else if( is_union )
        {
            // NOTE: Unions don't drop inner items.
        }
        else
        {
            for(size_t i = 0; i < vse.inner_states.size(); i ++)
            {
                drop_value_from_state(sp, vse.inner_states[i], ::MIR::LValue::make_Field({ box$(lv.clone()), static_cast<unsigned int>(i) }));
            }
        }
        ),
    (Optional,
        push_stmt_drop(sp, mv$(lv), vse);
        )
    )
}

void MirBuilder::drop_scope_values(const ScopeDef& sd)
{
    TU_MATCHA( (sd.data), (e),
    (Owning,
        for(auto idx : ::reverse(e.slots))
        {
            const auto& vs = get_slot_state(sd.span, idx, SlotType::Local);
            DEBUG("slot" << idx << " - " << vs);
            drop_value_from_state( sd.span, vs, ::MIR::LValue::make_Local(idx) );
        }
        ),
    (Split,
        // No values, controls parent
        ),
    (Loop,
        // No values
        ),
    (Freeze,
        )
    )
}


void MirBuilder::moved_lvalue(const Span& sp, const ::MIR::LValue& lv)
{
    if( !lvalue_is_copy(sp, lv) ) {
        auto& vs = get_val_state_mut(sp, lv);
        // TODO: If the current state is Optional, set the drop flag to 0
        vs = VarState::make_Invalid(InvalidType::Moved);
    }
}

const ::MIR::LValue& MirBuilder::get_ptr_to_dst(const Span& sp, const ::MIR::LValue& lv) const
{
    // Undo field accesses
    const auto* lvp = &lv;
    while(lvp->is_Field())
        lvp = &*lvp->as_Field().val;

    // TODO: Enum variants?

    ASSERT_BUG(sp, lvp->is_Deref(), "Access of an unsized field without a dereference - " << lv);

    return *lvp->as_Deref().val;
}

// --------------------------------------------------------------------

ScopeHandle::~ScopeHandle()
{
    if( idx != ~0u )
    {
        try {
            ASSERT_BUG(Span(), m_builder.m_scopes.size() > idx, "Scope invalid");
            ASSERT_BUG(Span(), m_builder.m_scopes.at(idx).complete, "Scope " << idx << " not completed");
        }
        catch(...) {
            abort();
        }
    }
}

VarState VarState::clone() const
{
    TU_MATCHA( (*this), (e),
    (Invalid,
        return VarState(e);
        ),
    (Valid,
        return VarState(e);
        ),
    (Optional,
        return VarState(e);
        ),
    (MovedOut,
        return VarState::make_MovedOut({ box$(e.inner_state->clone()), e.outer_flag });
        ),
    (Partial,
        ::std::vector<VarState> n;
        n.reserve(e.inner_states.size());
        for(const auto& a : e.inner_states)
            n.push_back( a.clone() );
        return VarState::make_Partial({ mv$(n) });
        )
    )
    throw "";
}
bool VarState::operator==(VarState& x) const
{
    if( this->tag() != x.tag() )
        return false;
    TU_MATCHA( (*this, x), (te, xe),
    (Invalid,
        return te == xe;
        ),
    (Valid,
        return true;
        ),
    (Optional,
        return te == xe;
        ),
    (MovedOut,
        if( te.outer_flag != xe.outer_flag )
            return false;
        return *te.inner_state == *xe.inner_state;
        ),
    (Partial,
        if( te.inner_states.size() != xe.inner_states.size() )
            return false;
        for(unsigned int i = 0; i < te.inner_states.size(); i ++)
        {
            if( te.inner_states[i] != xe.inner_states[i] )
                return false;
        }
        return true;
        )
    )
    throw "";
}
::std::ostream& operator<<(::std::ostream& os, const VarState& x)
{
    TU_MATCHA( (x), (e),
    (Invalid,
        switch(e)
        {
        case InvalidType::Uninit:   os << "Uninit"; break;
        case InvalidType::Moved:    os << "Moved";  break;
        case InvalidType::Descoped: os << "Descoped";   break;
        }
        ),
    (Valid,
        os << "Valid";
        ),
    (Optional,
        os << "Optional(" << e << ")";
        ),
    (MovedOut,
        os << "MovedOut(";
        if( e.outer_flag == ~0u )
            os << "-";
        else
            os << "df" << e.outer_flag;
        os << " " << *e.inner_state <<")";
        ),
    (Partial,
        os << "Partial(";
        os << ", [" << e.inner_states << "])";
        )
    )
    return os;
}
