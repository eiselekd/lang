/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * expand/include.cpp
 * - include!/include_str!/include_bytes! support
 */
#include <synext_macro.hpp>
#include <synext.hpp>   // for Expand_BareExpr
#include <parse/common.hpp>
#include <parse/parseerror.hpp> // for GET_CHECK_TOK
#include <parse/ttstream.hpp>
#include <parse/lex.hpp>    // Lexer (new files)
#include <ast/expr.hpp>

namespace {

    ::std::string get_string(const Span& sp, TokenStream& lex, const ::AST::Crate& crate, AST::Module& mod)
    {
        auto n = Parse_ExprVal(lex);
        ASSERT_BUG(sp, n, "No expression returned");
        Expand_BareExpr(crate, mod, n);

        auto* string_np = dynamic_cast<AST::ExprNode_String*>(&*n);
        if( !string_np ) {
            ERROR(sp, E0000, "include! requires a string literal - got " << *n);
        }
        return mv$( string_np->m_value );
    }

    ::std::string get_path_relative_to(const ::std::string& base_path, ::std::string path)
    {
        DEBUG(base_path << ", " << path);
        if( path[0] == '/' || path[0] == '\\' ) {
            return path;
        }
        else if( base_path.size() == 0 ) {
            return path;
        }
        else if( base_path.back() == '/' || base_path.back() == '\\' ) {
            return base_path + path;
        }
        else {

            auto slash = ::std::min( base_path.find_last_of('/'), base_path.find_last_of('\\') );
            if( slash == ::std::string::npos )
            {
                return path;
            }
            else
            {
                slash += 1;
                ::std::string   rv;
                rv.reserve( slash + path.size() );
                rv.append( base_path.begin(), base_path.begin() + slash );
                rv.append( path.begin(), path.end() );
                return rv;
            }
        }
    }
};

class CIncludeExpander:
    public ExpandProcMacro
{
    ::std::unique_ptr<TokenStream> expand(const Span& sp, const AST::Crate& crate, const ::std::string& ident, const TokenTree& tt, AST::Module& mod) override
    {
        if( ident != "" )
            ERROR(sp, E0000, "include! doesn't take an ident");

        Token   tok;
        auto lex = TTStream(sp, tt);

        auto path = get_string(sp, lex, crate, mod);
        GET_CHECK_TOK(tok, lex, TOK_EOF);

        ::std::string file_path = get_path_relative_to(mod.m_file_info.path, mv$(path));

        try {
            return box$( Lexer(file_path) );
        }
        catch(::std::runtime_error& e)
        {
            ERROR(sp, E0000, e.what());
        }
    }
};

class CIncludeBytesExpander:
    public ExpandProcMacro
{
    ::std::unique_ptr<TokenStream> expand(const Span& sp, const AST::Crate& crate, const ::std::string& ident, const TokenTree& tt, AST::Module& mod) override
    {
        if( ident != "" )
            ERROR(sp, E0000, "include_bytes! doesn't take an ident");

        Token   tok;
        auto lex = TTStream(sp, tt);

        auto path = get_string(sp, lex, crate, mod);
        GET_CHECK_TOK(tok, lex, TOK_EOF);

        ::std::string file_path = get_path_relative_to(mod.m_file_info.path, mv$(path));

        ::std::ifstream is(file_path);
        if( !is.good() ) {
            ERROR(sp, E0000, "Cannot open file " << file_path << " for include_bytes!");
        }
        ::std::stringstream   ss;
        ss << is.rdbuf();

        ::std::vector<TokenTree>    toks;
        toks.push_back(Token(TOK_BYTESTRING, mv$(ss.str())));
        return box$( TTStreamO(sp, TokenTree(Ident::Hygiene::new_scope(), mv$(toks))) );
    }
};

class CIncludeStrExpander:
    public ExpandProcMacro
{
    ::std::unique_ptr<TokenStream> expand(const Span& sp, const AST::Crate& crate, const ::std::string& ident, const TokenTree& tt, AST::Module& mod) override
    {
        if( ident != "" )
            ERROR(sp, E0000, "include_str! doesn't take an ident");

        Token   tok;
        auto lex = TTStream(sp, tt);

        auto path = get_string(sp, lex, crate, mod);
        GET_CHECK_TOK(tok, lex, TOK_EOF);

        ::std::string file_path = get_path_relative_to(mod.m_file_info.path, mv$(path));

        ::std::ifstream is(file_path);
        if( !is.good() ) {
            ERROR(sp, E0000, "Cannot open file " << file_path << " for include_str!");
        }
        ::std::stringstream   ss;
        ss << is.rdbuf();

        ::std::vector<TokenTree>    toks;
        toks.push_back(Token(TOK_STRING, mv$(ss.str())));
        return box$( TTStreamO(sp, TokenTree(Ident::Hygiene::new_scope(), mv$(toks))) );
    }
};

// TODO: include_str! and include_bytes!

STATIC_MACRO("include", CIncludeExpander);
STATIC_MACRO("include_bytes", CIncludeBytesExpander);
STATIC_MACRO("include_str", CIncludeStrExpander);


