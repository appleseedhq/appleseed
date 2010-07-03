/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
///////////////////////////////////////////////////////////////////////////////
//
//  A Calculator example demonstrating generation of AST
//
//  [ JDG April 28, 2002 ]
//
///////////////////////////////////////////////////////////////////////////////

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/variant/recursive_variant.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_function.hpp>

#include <iostream>
#include <vector>
#include <string>

using namespace boost::spirit;
using namespace boost::spirit::qi;
using namespace boost::spirit::ascii;
using namespace boost::spirit::arg_names;

///////////////////////////////////////////////////////////////////////////////
//  Our AST
///////////////////////////////////////////////////////////////////////////////
struct binary_op;
struct unary_op;
struct nil {};

struct expression_ast
{
    typedef
        boost::variant<
            nil // can't happen!
          , unsigned int
          , boost::recursive_wrapper<expression_ast>
          , boost::recursive_wrapper<binary_op>
          , boost::recursive_wrapper<unary_op>
        >
    type;

    expression_ast()
      : expr(nil()) {}

    template <typename Expr>
    expression_ast(Expr const& expr)
      : expr(expr) {}

    expression_ast& operator+=(expression_ast const& rhs);
    expression_ast& operator-=(expression_ast const& rhs);
    expression_ast& operator*=(expression_ast const& rhs);
    expression_ast& operator/=(expression_ast const& rhs);

    type expr;
};

struct binary_op
{
    binary_op(
        char op
      , expression_ast const& left
      , expression_ast const& right)
    : op(op), left(left), right(right) {}

    char op;
    expression_ast left;
    expression_ast right;
};

struct unary_op
{
    unary_op(
        char op
      , expression_ast const& subject)
    : op(op), subject(subject) {}

    char op;
    expression_ast subject;
};

expression_ast& expression_ast::operator+=(expression_ast const& rhs)
{
    expr = binary_op('+', expr, rhs);
    return *this;
}

expression_ast& expression_ast::operator-=(expression_ast const& rhs)
{
    expr = binary_op('-', expr, rhs);
    return *this;
}

expression_ast& expression_ast::operator*=(expression_ast const& rhs)
{
    expr = binary_op('*', expr, rhs);
    return *this;
}

expression_ast& expression_ast::operator/=(expression_ast const& rhs)
{
    expr = binary_op('/', expr, rhs);
    return *this;
}

// We should be using expression_ast::operator-. There's a bug
// in phoenix type deduction mechanism that prevents us from
// doing so. Phoenix will be switching to BOOST_TYPEOF. In the
// meantime, we will use a phoenix::function below:
struct negate_expr
{
    template <typename T>
    struct result { typedef T type; };

    expression_ast operator()(expression_ast const& expr) const
    {
        return expression_ast(unary_op('-', expr));
    }
};

boost::phoenix::function<negate_expr> neg;

///////////////////////////////////////////////////////////////////////////////
//  Our calculator grammar
///////////////////////////////////////////////////////////////////////////////
template <typename Iterator>
struct calculator : grammar<Iterator, expression_ast(), space_type>
{
    calculator() : calculator::base_type(expression)
    {
        expression =
            term                            [_val = _1]
            >> *(   ('+' >> term            [_val += _1])
                |   ('-' >> term            [_val -= _1])
                )
            ;

        term =
            factor                          [_val = _1]
            >> *(   ('*' >> factor          [_val *= _1])
                |   ('/' >> factor          [_val /= _1])
                )
            ;

        factor =
            uint_                           [_val = _1]
            |   '(' >> expression           [_val = _1] >> ')'
            |   ('-' >> factor              [_val = neg(_1)])
            |   ('+' >> factor              [_val = _1])
            ;
    }

    rule<Iterator, expression_ast(), space_type> expression, term, factor;
};

///////////////////////////////////////////////////////////////////////////////
//  Main program
///////////////////////////////////////////////////////////////////////////////
int
main()
{
    std::cout << "/////////////////////////////////////////////////////////\n\n";
    std::cout << "Expression parser...\n\n";
    std::cout << "/////////////////////////////////////////////////////////\n\n";
    std::cout << "Type an expression...or [q or Q] to quit\n\n";

    typedef std::string::const_iterator iterator_type;
    typedef calculator<iterator_type> calculator;

    calculator calc; // Our grammar

    std::string str;
    while (std::getline(std::cin, str))
    {
        if (str.empty() || str[0] == 'q' || str[0] == 'Q')
            break;

        std::string::const_iterator iter = str.begin();
        std::string::const_iterator end = str.end();
        bool r = phrase_parse(iter, end, calc, space);

        if (r && iter == end)
        {
            std::cout << "-------------------------\n";
            std::cout << "Parsing succeeded\n";
            std::cout << "-------------------------\n";
        }
        else
        {
            std::string rest(iter, end);
            std::cout << "-------------------------\n";
            std::cout << "Parsing failed\n";
            std::cout << "stopped at: \": " << rest << "\"\n";
            std::cout << "-------------------------\n";
        }
    }

    std::cout << "Bye... :-) \n\n";
    return 0;
}


