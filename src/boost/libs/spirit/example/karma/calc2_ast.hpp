/*=============================================================================
    Copyright (c) 2001-2008 Joel de Guzman
    Copyright (c) 2001-2008 Hartmut Kaiser

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
///////////////////////////////////////////////////////////////////////////////
//
//  A Calculator example demonstrating generation of AST which gets dumped into
//  a human readable format afterwards.
//
//  [ JDG April 28, 2008 ]
//  [ HK April 28, 2008 ]
//
///////////////////////////////////////////////////////////////////////////////

#if !defined(SPIRIT_EXAMPLE_CALC2_AST_APR_30_2008_1011AM)
#define SPIRIT_EXAMPLE_CALC2_AST_APR_30_2008_1011AM

#include <boost/variant/recursive_variant.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_function.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>

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
          , int
          , boost::recursive_wrapper<binary_op>
          , boost::recursive_wrapper<unary_op>
        >
    type;

    // expose variant types 
    typedef type::types types;
    
    // expose variant functionality
    int which() const { return expr.which(); }
    
    // constructors
    expression_ast()
      : expr(nil()) {}

    expression_ast(unary_op const& expr)
      : expr(expr) {}

    expression_ast(binary_op const& expr)
      : expr(expr) {}

    expression_ast(unsigned int expr)
      : expr(expr) {}
      
    expression_ast(type const& expr)
      : expr(expr) {}
      
    expression_ast& operator+=(expression_ast const& rhs);
    expression_ast& operator-=(expression_ast const& rhs);
    expression_ast& operator*=(expression_ast const& rhs);
    expression_ast& operator/=(expression_ast const& rhs);

    type expr;
};

// expose variant functionality
template <typename T>
inline T get(expression_ast const& expr)
{
    return boost::get<T>(expr.expr);
}

///////////////////////////////////////////////////////////////////////////////
struct binary_op
{
    binary_op() {}
    
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
      , expression_ast const& right)
    : op(op), right(right) {}

    char op;
    expression_ast right;
};

inline expression_ast& expression_ast::operator+=(expression_ast const& rhs)
{
    expr = binary_op('+', expr, rhs);
    return *this;
}

inline expression_ast& expression_ast::operator-=(expression_ast const& rhs)
{
    expr = binary_op('-', expr, rhs);
    return *this;
}

inline expression_ast& expression_ast::operator*=(expression_ast const& rhs)
{
    expr = binary_op('*', expr, rhs);
    return *this;
}

inline expression_ast& expression_ast::operator/=(expression_ast const& rhs)
{
    expr = binary_op('/', expr, rhs);
    return *this;
}

// We should be using expression_ast::operator-. There's a bug
// in phoenix type deduction mechanism that prevents us from
// doing so. Phoenix will be switching to BOOST_TYPEOF. In the
// meantime, we will use a phoenix::function below:
template <char Op>
struct unary_expr
{
    template <typename T>
    struct result { typedef T type; };

    expression_ast operator()(expression_ast const& expr) const
    {
        return unary_op(Op, expr);
    }
};

boost::phoenix::function<unary_expr<'+'> > pos;
boost::phoenix::function<unary_expr<'-'> > neg;

///////////////////////////////////////////////////////////////////////////////
//  A couple of phoenix functions helping to access the elements of the 
//  generated AST
///////////////////////////////////////////////////////////////////////////////
template <typename T>
struct get_element
{
    template <typename T1>
    struct result { typedef T const& type; };

    T const& operator()(expression_ast const& expr) const
    {
        return boost::get<T>(expr.expr);
    }
};

boost::phoenix::function<get_element<int> > _int;
boost::phoenix::function<get_element<binary_op> > _bin_op;
boost::phoenix::function<get_element<unary_op> > _unary_op;

///////////////////////////////////////////////////////////////////////////////
struct get_left
{
    template <typename T1>
    struct result { typedef expression_ast const& type; };

    expression_ast const& operator()(binary_op const& bin_op) const
    {
        return bin_op.left;
    }
};

boost::phoenix::function<get_left> _left;

struct get_right
{
    template <typename T1>
    struct result { typedef expression_ast const& type; };

    template <typename Node>
    expression_ast const& operator()(Node const& op) const
    {
        return op.right;
    }
};

boost::phoenix::function<get_right> _right;

struct get_op
{
    template <typename T1>
    struct result { typedef char type; };

    template <typename Node>
    char operator()(Node const& bin_op) const
    {
        return bin_op.op;
    }
};

boost::phoenix::function<get_op> _op;

#endif
