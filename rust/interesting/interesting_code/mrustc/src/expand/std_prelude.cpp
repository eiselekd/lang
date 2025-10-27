/*
 * MRustC - Mutabah's Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * expand/std_prelude.cpp
 * - Handling of no_std/no_core/no_prelude
 */
#include <synext.hpp>
#include <ast/crate.hpp>

class Decorator_NoStd:
    public ExpandDecorator
{
public:
    AttrStage stage() const override { return AttrStage::Pre; }

    void handle(const Span& sp, const AST::Attribute& mi, AST::Crate& crate) const override {
        if( crate.m_load_std != AST::Crate::LOAD_STD && crate.m_load_std != AST::Crate::LOAD_CORE ) {
            ERROR(sp, E0000, "Invalid use of #![no_std] with itself or #![no_core]");
        }
        crate.m_load_std = AST::Crate::LOAD_CORE;
    }
};
class Decorator_NoCore:
    public ExpandDecorator
{
public:
    AttrStage stage() const override { return AttrStage::Pre; }

    void handle(const Span& sp, const AST::Attribute& mi, AST::Crate& crate) const override {
        if( crate.m_load_std != AST::Crate::LOAD_STD && crate.m_load_std != AST::Crate::LOAD_NONE ) {
            ERROR(sp, E0000, "Invalid use of #![no_core] with itself or #![no_std]");
        }
        crate.m_load_std = AST::Crate::LOAD_NONE;
    }
};
//class Decorator_Prelude:
//    public ExpandDecorator
//{
//public:
//    AttrStage stage() const override { return AttrStage::Pre; }
//};

class Decorator_NoPrelude:
    public ExpandDecorator
{
public:
    AttrStage stage() const override { return AttrStage::Pre; }

    void handle(const Span& sp, const AST::Attribute& mi, ::AST::Crate& crate, const AST::Path& path, AST::Module& mod, AST::Item&i) const override {
        if( i.is_Module() ) {
            i.as_Module().m_insert_prelude = false;
        }
        else {
            ERROR(sp, E0000, "Invalid use of #[no_prelude] on non-module");
        }
    }
};

class Decorator_PreludeImport:
    public ExpandDecorator
{
public:
    AttrStage stage() const override { return AttrStage::Post; }

    void handle(const Span& sp, const AST::Attribute& mi, ::AST::Crate& crate, const AST::Path& path, AST::Module& mod, AST::Item&i) const override {
        if( i.is_Use() ) {
            const auto& p = i.as_Use().path;
            // TODO: Ensure that this statement is a glob (has a name of "")
            crate.m_prelude_path = AST::Path(p);
        }
        else {
            ERROR(sp, E0000, "Invalid use of #[no_prelude] on non-module");
        }
    }
};


STATIC_DECORATOR("no_std", Decorator_NoStd)
STATIC_DECORATOR("no_core", Decorator_NoCore)
//STATIC_DECORATOR("prelude", Decorator_Prelude)    // mrustc
STATIC_DECORATOR("prelude_import", Decorator_PreludeImport)   // rustc

STATIC_DECORATOR("no_prelude", Decorator_NoPrelude)

