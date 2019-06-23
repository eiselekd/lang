/*
 * MRustC - Mutabah's Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * include/main_bindings.hpp
 * - General bindings of AST passes for main to call
 */
#ifndef _MAIN_BINDINGS_HPP_
#define _MAIN_BINDINGS_HPP_

#include <string>
#include <memory>

namespace AST {
    class Crate;
}

/// Parse a crate from the given file
extern AST::Crate Parse_Crate(::std::string mainfile);

extern void Expand(::AST::Crate& crate);
extern void Expand_TestHarness(::AST::Crate& crate);
extern void Expand_ProcMacro(::AST::Crate& crate);

/// Dump the crate AST as annotated rust
extern void Dump_Rust(const char *Filename, const AST::Crate& crate);

#endif

