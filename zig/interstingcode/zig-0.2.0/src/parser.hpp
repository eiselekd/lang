/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of zig, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#ifndef ZIG_PARSER_HPP
#define ZIG_PARSER_HPP

#include "all_types.hpp"
#include "tokenizer.hpp"
#include "errmsg.hpp"

ATTRIBUTE_PRINTF(2, 3)
void ast_token_error(Token *token, const char *format, ...);


// This function is provided by generated code, generated by parsergen.cpp
AstNode * ast_parse(Buf *buf, ZigList<Token> *tokens, ImportTableEntry *owner, ErrColor err_color);

void ast_print(AstNode *node, int indent);

void ast_visit_node_children(AstNode *node, void (*visit)(AstNode **, void *context), void *context);

bool statement_terminates_without_semicolon(AstNode *node);

#endif