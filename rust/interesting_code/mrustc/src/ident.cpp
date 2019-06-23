/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * include/ident.cpp
 * - Identifiers with hygiene
 */
#include <iostream>
#include <ident.hpp>
#include <debug.hpp>
#include <common.hpp>   // vector print

unsigned int Ident::Hygiene::g_next_scope = 0;

bool Ident::Hygiene::is_visible(const Hygiene& src) const
{
    // HACK: Disable hygiene for now
    //return true;

    if( this->contexts.size() == 0 ) {
        return src.contexts.size() == 0;
    }

    auto des = this->contexts.back();
    for(const auto& c : src.contexts)
        if( des == c )
            return true;
    return false;
}

::std::ostream& operator<<(::std::ostream& os, const Ident& x) {
    os << x.name << x.hygiene;
    return os;
}

::std::ostream& operator<<(::std::ostream& os, const Ident::Hygiene& x) {
    os << "{" << x.contexts << "}";
    return os;
}

