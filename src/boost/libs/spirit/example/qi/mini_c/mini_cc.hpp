/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_MINI_CC)
#define BOOST_SPIRIT_MINI_CC

#include "mini_c.hpp"

///////////////////////////////////////////////////////////////////////////////
//  Our statement grammar and compiler
///////////////////////////////////////////////////////////////////////////////
template <typename Iterator>
statement<Iterator>::statement(
    std::vector<int>& code, symbols<char, function_info>& functions)
  : statement::base_type(statement_list)
  , code(code)
  , functions(functions)
  , nvars(0)
  , expr(code, vars, functions)
  , add_var(var_adder(vars, nvars))
  , op(code)
{
    identifier %=
        raw[lexeme[alpha >> *(alnum | '_')]]
        ;

    var_ref =
        lexeme
        [
                vars        [_val = _1]
            >>  !(alnum | '_') // make sure we have whole words
        ]
        ;

    var_decl =
            "int"
        >   !var_ref        // make sure the variable isn't redeclared
        >   identifier      [add_var(_1)]
        >   (';' | '=' > assignment_rhs(ref(nvars)-1))
        ;

    assignment =
            var_ref         [_a = _1]
        >>  '='
        >   assignment_rhs(_a)
        ;

    assignment_rhs =
            expr
        >   char_(';')      [op(op_store, _r1)]
        ;

    if_statement =
            lit("if")
        >>  '('
        >   expr            [
                                op(op_jump_if, 0), // we shall fill this (0) in later
                                _a = size(ref(code))-1 // mark its position
                            ]
        >   ')'
        >   statement_      [
                                // now we know where to jump to (after the if branch)
                                ref(code)[_a] = size(ref(code))
                            ]
        >>
           -(
                lexeme[
                    "else"
                    >> !(alnum | '_') // make sure we have whole words
                ]                                [
                                ref(code)[_a] += 2, // adjust for the "else" jump
                                op(op_jump, 0), // we shall fill this (0) in later
                                _a = size(ref(code))-1 // mark its position
                            ]
            >   statement_  [
                                // now we know where to jump to (after the else branch)
                                ref(code)[_a] = size(ref(code))
                            ]
            )
        ;

    while_statement =
            lit("while")    [
                                _a = size(ref(code)) // mark our position
                            ]
        >>  '('
        >   expr            [
                                op(op_jump_if, 0), // we shall fill this (0) in later
                                _b = size(ref(code))-1 // mark its position
                            ]
        >   ')'
        >   statement_      [
                                op(op_jump, _a), // loop back
                                // now we know where to jump to (to exit the loop)
                                ref(code)[_b] = size(ref(code))
                            ]
        ;

    compound_statement =
        '{' >> -statement_list >> '}'
        ;

    return_statement =
            lexeme[
                "return"
                >> !(alnum | '_') // make sure we have whole words
            ]
        >> -(
                eps(ref(has_return)) > expr     [op(op_return)]
            )
        >   ';'
        ;

    statement_ =
            var_decl
        |   assignment
        |   compound_statement
        |   if_statement
        |   while_statement
        |   return_statement
        ;

    statement_list = +statement_;

    identifier.name("identifier");
    var_ref.name("variable-reference");
    var_decl.name("variable-declaration");
    assignment.name("assignment");
    assignment_rhs.name("assignment-rhs");
    if_statement.name("if-statement");
    while_statement.name("while-statement");
    compound_statement.name("compound-statement");
    return_statement.name("return-statement");
    statement_.name("statement");
    statement_list.name("statement-list");

    on_error<fail>(statement_list, error_handler(_4, _3, _2));
}

#endif

