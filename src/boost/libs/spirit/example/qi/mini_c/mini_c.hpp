/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_MINI_C)
#define BOOST_SPIRIT_MINI_C

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_container.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/lexical_cast.hpp>

#include <iostream>
#include <fstream>
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
    op_jump,        //  jump to an absolute position in the code

    op_stk_adj,     // adjust the stack (for args and locals)
    op_call,        // function call
    op_return       // return from function
};

class vmachine
{
public:

    vmachine(unsigned stackSize = 4096)
      : stack(stackSize)
    {
    }

    int execute(
        std::vector<int> const& code            // the program code
      , std::vector<int>::const_iterator pc     // program counter
      , std::vector<int>::iterator frame_ptr    // start of arguments and locals
    );

    std::vector<int> stack;
};

///////////////////////////////////////////////////////////////////////////////
//  A generic compiler that compiles 1 to 3 codes
///////////////////////////////////////////////////////////////////////////////
struct function_info
{
    int arity;
    int address;
};

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

    // special overload for function calls
    void operator()(function_info const& info, int got_nargs, bool& parse_result) const
    {
        if (got_nargs == info.arity)
        {
            code.push_back(op_call);
            code.push_back(info.arity);
            code.push_back(info.address);
        }
        else
        {
            parse_result = false; // fail the parse
            std::cerr << "wrong number of args" << std::endl;
        }
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
//  A functor that adds variables to our (variables) symbol-table
///////////////////////////////////////////////////////////////////////////////
struct var_adder
{
    template <typename>
    struct result { typedef void type; };

    var_adder(symbols<char, int>& vars, int& nvars)
      : vars(vars), nvars(nvars)
    {
    }

    void operator()(std::string const& var) const
    {
        vars.add(var.begin(), var.end(), nvars++);
    };

    symbols<char, int>& vars;
    int& nvars;
};

///////////////////////////////////////////////////////////////////////////////
//  A functor that adds functions to our (function) symbol-table
///////////////////////////////////////////////////////////////////////////////
struct function_adder
{
    template <typename, typename, typename>
    struct result { typedef void type; };

    function_adder(symbols<char, function_info>& functions)
      : functions(functions)
    {
    }

    void operator()(std::string const& function_id, int arity, int address) const
    {
        function_info info = {arity, address};
        functions.add(function_id.begin(), function_id.end(), info);
    };

    symbols<char, function_info>& functions;
};

///////////////////////////////////////////////////////////////////////////////
//  A functor that resets the function-related state variables
///////////////////////////////////////////////////////////////////////////////
struct function_state_reset
{
    template <typename>
    struct result { typedef void type; };

    function_state_reset(
        std::vector<int>& code
      , symbols<char, int>& vars
      , int& nvars)
      : code(code)
      , vars(vars)
      , nvars(nvars)
    {
    }

    void operator()(int address) const
    {
        code[address+1] = nvars;
        nvars = 0; // reset
        vars.clear(); // reset
    };

    std::vector<int>& code;
    symbols<char, int>& vars;
    int& nvars;
};

///////////////////////////////////////////////////////////////////////////////
//  White-space and comments grammar definition
///////////////////////////////////////////////////////////////////////////////
template <typename Iterator>
struct white_space : grammar<Iterator>
{
    white_space() : white_space::base_type(start)
    {
        start =
                space                               // tab/space/cr/lf
            |   "/*" >> *(char_ - "*/") >> "*/"     // C-style comments
            ;
    }

    rule<Iterator> start;
};

///////////////////////////////////////////////////////////////////////////////
//  Our expression grammar and compiler
///////////////////////////////////////////////////////////////////////////////
template <typename Iterator>
struct expression : grammar<Iterator, white_space<Iterator> >
{
    expression(
        std::vector<int>& code
      , symbols<char, int>& vars
      , symbols<char, function_info>& functions);

    typedef white_space<Iterator> white_space;

    rule<Iterator, white_space>
        expr, equality_expr, relational_expr
      , logical_expr, additive_expr, multiplicative_expr
      , unary_expr, primary_expr, variable
    ;

    rule<Iterator, locals<function_info, int>, white_space> function_call;

    std::vector<int>& code;
    symbols<char, int>& vars;
    symbols<char, function_info>& functions;
    function<compile_op> op;
};

///////////////////////////////////////////////////////////////////////////////
//  Our statement grammar and compiler
///////////////////////////////////////////////////////////////////////////////
template <typename Iterator>
struct statement : grammar<Iterator, white_space<Iterator> >
{
    statement(std::vector<int>& code, symbols<char, function_info>& functions);

    typedef white_space<Iterator> white_space;

    std::vector<int>& code;
    symbols<char, int> vars;
    symbols<char, function_info>& functions;
    int nvars;
    bool has_return;

    expression<Iterator> expr;
    rule<Iterator, white_space>
        statement_, statement_list, var_decl, compound_statement
      , return_statement;

    rule<Iterator, locals<int>, white_space> if_statement;
    rule<Iterator, locals<int, int>, white_space> while_statement;
    rule<Iterator, std::string(), white_space> identifier;
    rule<Iterator, int(), white_space> var_ref;
    rule<Iterator, locals<int>, white_space> assignment;
    rule<Iterator, void(int), white_space> assignment_rhs;

    function<var_adder> add_var;
    function<compile_op> op;
};

///////////////////////////////////////////////////////////////////////////////
//  Our program grammar and compiler
///////////////////////////////////////////////////////////////////////////////
template <typename Iterator>
struct program : grammar<Iterator, white_space<Iterator> >
{
    program(std::vector<int>& code);

    typedef white_space<Iterator> white_space;

    std::vector<int>& code;
    rule<Iterator, std::string(), white_space> identifier;
    rule<Iterator, white_space> start;

    typedef
        locals<
            std::string // function name
          , int         // address
        >
    function_locals;

    symbols<char, function_info> functions;
    statement<Iterator> statement;

    rule<Iterator, function_locals, white_space> function;
    boost::phoenix::function<function_adder> add_function;
    boost::phoenix::function<function_state_reset> state_reset;
    boost::phoenix::function<compile_op> op;
};

#endif


