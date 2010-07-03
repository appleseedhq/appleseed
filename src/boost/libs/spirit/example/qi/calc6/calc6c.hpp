/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_CALC6C)
#define BOOST_SPIRIT_CALC6C

#include "calc6.hpp"

///////////////////////////////////////////////////////////////////////////////
//  Our statement grammar and compiler
///////////////////////////////////////////////////////////////////////////////
template <typename Iterator>
statement<Iterator>::statement(std::vector<int>& code)
  : statement::base_type(start)
  , code(code)
  , nvars(0)
  , expr(code, vars)
  , add_var(vars)
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
            "var"
        >   !var_ref        // make sure the variable isn't redeclared
        >   identifier      [add_var(_1, ref(nvars))]
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

    start = +(var_decl | assignment);

    identifier.name("identifier");
    var_ref.name("variable-reference");
    var_decl.name("variable-declaration");
    assignment.name("assignment");
    assignment_rhs.name("assignment-rhs");

    on_error<fail>(start, error_handler(_4, _3, _2));
}

#endif