/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_CALC7)
#define BOOST_SPIRIT_CALC7

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_container.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

#include <iostream>
#include <string>
#include <vector>

using namespace boost::spirit;
using namespace boost::spirit::qi;
using namespace boost::spirit::ascii;
using namespace boost::spirit::arg_names;

using boost::phoenix::function;
using boost::phoenix::ref;
using boost::phoenix::size;

///////////////////////////////////////////////////////////////////////////////
//  The Virtual Machine
///////////////////////////////////////////////////////////////////////////////
enum byte_code
{
    op_neg,         //  negate the top stack entry
    op_add,         //  add top two stack entries
    op_sub,         //  subtract top two stack entries
    op_mul,         //  multiply top two stack entries
    op_div,         //  divide top two stack entries

    op_not,         //  boolean negate the top stack entry
    op_eq,          //  compare the top two stack entries for ==
    op_neq,         //  compare the top two stack entries for !=
    op_lt,          //  compare the top two stack entries for <
    op_lte,         //  compare the top two stack entries for <=
    op_gt,          //  compare the top two stack entries for >
    op_gte,         //  compare the top two stack entries for >=

    op_and,         //  logical and top two stack entries
    op_or,          //  logical or top two stack entries

    op_load,        //  load a variable
    op_store,       //  store a variable

    op_int,         //  push constant integer into the stack
    op_true,        //  push constant 0 into the stack
    op_false,       //  push constant 1 into the stack

    op_jump_if,     //  jump to an absolute position in the code if top stack
                    //  evaluates to false
    op_jump         //  jump to an absolute position in the code
};

class vmachine
{
public:

    vmachine(unsigned stackSize = 4096)
      : stack(stackSize)
      , stack_ptr(stack.begin())
    {
    }

    std::vector<int> const& get_stack() const { return stack; };
    void execute(std::vector<int> const& code, int nvars);

private:

    std::vector<int> stack;
    std::vector<int>::iterator stack_ptr;
};

///////////////////////////////////////////////////////////////////////////////
//  A generic compiler that compiles 1 to 3 codes
///////////////////////////////////////////////////////////////////////////////
struct compile_op
{
    template <typename A, typename B = unused_type, typename C = unused_type>
    struct result { typedef void type; };

    compile_op(std::vector<int>& code)
      : code(code)
    {
    }

    void operator()(int a) const
    {
        code.push_back(a);
    }

    void operator()(int a, int b) const
    {
        code.push_back(a);
        code.push_back(b);
    }

    void operator()(int a, int b, int c) const
    {
        code.push_back(a);
        code.push_back(b);
        code.push_back(c);
    }

    std::vector<int>& code;
};

///////////////////////////////////////////////////////////////////////////////
//  Our error handler
///////////////////////////////////////////////////////////////////////////////
struct error_handler_
{
    template <typename, typename, typename>
    struct result { typedef void type; };

    template <typename Iterator>
    void operator()(
        std::string const& what
      , Iterator err_pos, Iterator last) const
    {
        std::cout
            << "Error! Expecting "
            << what                         // what failed?
            << " here: \""
            << std::string(err_pos, last)   // iterators to error-pos, end
            << "\""
            << std::endl
        ;
    }
};

function<error_handler_> const error_handler = error_handler_();

///////////////////////////////////////////////////////////////////////////////
//  Our expression grammar and compiler
///////////////////////////////////////////////////////////////////////////////
template <typename Iterator>
struct expression : grammar<Iterator, space_type>
{
    expression(std::vector<int>& code, symbols<char, int>& vars);

    rule<Iterator, space_type>
        expr, equality_expr, relational_expr
      , logical_expr, additive_expr, multiplicative_expr
      , unary_expr, primary_expr, variable
    ;

    std::vector<int>& code;
    symbols<char, int>& vars;
    function<compile_op> op;
};

///////////////////////////////////////////////////////////////////////////////
//  Our statement grammar and compiler
///////////////////////////////////////////////////////////////////////////////
struct var_adder
{
    template <typename, typename>
    struct result { typedef void type; };

    var_adder(symbols<char, int>& vars)
      : vars(vars)
    {
    }

    void operator()(std::string const& var, int& nvars) const
    {
        vars.add(var.begin(), var.end(), nvars++);
    };

    symbols<char, int>& vars;
};

template <typename Iterator>
struct statement : grammar<Iterator, space_type>
{
    statement(std::vector<int>& code);

    std::vector<int>& code;
    symbols<char, int> vars;
    int nvars;

    expression<Iterator> expr;
    rule<Iterator, space_type>
        statement_, statement_list, var_decl, compound_statement
    ;

    rule<Iterator, locals<int>, space_type> if_statement;
    rule<Iterator, locals<int, int>, space_type> while_statement;
    rule<Iterator, std::string(), space_type> identifier;
    rule<Iterator, int(), space_type> var_ref;
    rule<Iterator, locals<int>, space_type> assignment;
    rule<Iterator, void(int), space_type> assignment_rhs;

    function<var_adder> add_var;
    function<compile_op> op;
};

#endif
