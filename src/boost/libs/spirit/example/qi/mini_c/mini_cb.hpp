/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_MINI_CB)
#define BOOST_SPIRIT_MINI_CB

#include "mini_c.hpp"

///////////////////////////////////////////////////////////////////////////////
//  Our expression grammar and compiler
///////////////////////////////////////////////////////////////////////////////
template <typename Iterator>
expression<Iterator>::expression(
    std::vector<int>& code
  , symbols<char, int>& vars
  , symbols<char, function_info>& functions)
  : expression::base_type(expr)
  , code(code)
  , vars(vars)
  , functions(functions)
  , op(code)
{
    expr =
        equality_expr.alias()
        ;

    equality_expr =
        relational_expr
        >> *(   ("==" > relational_expr     [op(op_eq)])
            |   ("!=" > relational_expr     [op(op_neq)])
            )
        ;

    relational_expr =
        logical_expr
        >> *(   ("<=" > logical_expr        [op(op_lte)])
            |   ('<' > logical_expr         [op(op_lt)])
            |   (">=" > logical_expr        [op(op_gte)])
            |   ('>' > logical_expr         [op(op_gt)])
            )
        ;

    logical_expr =
        additive_expr
        >> *(   ("&&" > additive_expr       [op(op_and)])
            |   ("||" > additive_expr       [op(op_or)])
            )
        ;

    additive_expr =
        multiplicative_expr
        >> *(   ('+' > multiplicative_expr  [op(op_add)])
            |   ('-' > multiplicative_expr  [op(op_sub)])
            )
        ;

    multiplicative_expr =
        unary_expr
        >> *(   ('*' > unary_expr           [op(op_mul)])
            |   ('/' > unary_expr           [op(op_div)])
            )
        ;

    unary_expr =
            primary_expr
        |   ('!' > primary_expr             [op(op_not)])
        |   ('-' > primary_expr             [op(op_neg)])
        |   ('+' > primary_expr)
        ;

    primary_expr =
        uint_                               [op(op_int, _1)]
        |   variable
        |   function_call
        |   lit("true")                     [op(op_true)]
        |   lit("false")                    [op(op_false)]
        |   '(' > expr > ')'
        ;

    variable =
        (
            lexeme[
                vars
                >> !(alnum | '_')           // make sure we have whole words
            ]
        )                                   [op(op_load, _1)]
        ;

    function_call =
        functions                           [_a = _1]
        >>  '('
        >> -(
                expr                        [++_b]
                >> *(',' > expr             [++_b])
            )
        >   char_(')')                      [op(_a, _b, pass)]
        ;

    expr.name("expression");
    equality_expr.name("equality-expression");
    relational_expr.name("relational-expression");
    logical_expr.name("logical-expression");
    additive_expr.name("additive-expression");
    multiplicative_expr.name("multiplicative-expression");
    unary_expr.name("unary-expression");
    primary_expr.name("primary-expression");
    variable.name("variable");
    function_call.name("function-call");

    on_error<fail>(expr, error_handler(_4, _3, _2));
}

#endif
