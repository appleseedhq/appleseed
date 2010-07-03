/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
///////////////////////////////////////////////////////////////////////////////
//
//  Yet another calculator example! This time, we will compile to a simple
//  virtual machine. This is actually one of the very first Spirit example
//  circa 2000. Now, it's ported to Spirit2.
//
//  [ JDG Sometime 2000 ]       pre-boost
//  [ JDG September 18, 2002 ]  spirit1
//  [ JDG April 8, 2007 ]       spirit2
//
///////////////////////////////////////////////////////////////////////////////

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_container.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>
#include <boost/spirit/include/phoenix_object.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

#include <iostream>
#include <string>
#include <vector>

using namespace boost::spirit;
using namespace boost::spirit::qi;
using namespace boost::spirit::ascii;
using namespace boost::spirit::arg_names;

using boost::phoenix::ref;
using boost::phoenix::push_back;
using boost::phoenix::val;
using boost::phoenix::construct;

///////////////////////////////////////////////////////////////////////////////
//  The Virtual Machine
///////////////////////////////////////////////////////////////////////////////
enum byte_code
{
    op_neg,     //  negate the top stack entry
    op_add,     //  add top two stack entries
    op_sub,     //  subtract top two stack entries
    op_mul,     //  multiply top two stack entries
    op_div,     //  divide top two stack entries
    op_int,     //  push constant integer into the stack
};

class vmachine
{
public:

    vmachine(unsigned stackSize = 4096)
      : stack(stackSize)
      , stack_ptr(stack.begin())
    {
    }

    int top() const { return stack_ptr[-1]; };
    void execute(std::vector<int> const& code);

private:

    std::vector<int> stack;
    std::vector<int>::iterator stack_ptr;
};

void vmachine::execute(std::vector<int> const& code)
{
    std::vector<int>::const_iterator pc = code.begin();
    stack_ptr = stack.begin();

    while (pc != code.end())
    {
        switch (*pc++)
        {
            case op_neg:
                stack_ptr[-1] = -stack_ptr[-1];
                break;

            case op_add:
                --stack_ptr;
                stack_ptr[-1] += stack_ptr[0];
                break;

            case op_sub:
                --stack_ptr;
                stack_ptr[-1] -= stack_ptr[0];
                break;

            case op_mul:
                --stack_ptr;
                stack_ptr[-1] *= stack_ptr[0];
                break;

            case op_div:
                --stack_ptr;
                stack_ptr[-1] /= stack_ptr[0];
                break;

            case op_int:
                *stack_ptr++ = *pc++;
                break;
        }
    }
}

///////////////////////////////////////////////////////////////////////////////
//  Our calculator grammar and compiler
///////////////////////////////////////////////////////////////////////////////
template <typename Iterator>
struct calculator : grammar<Iterator, space_type>
{
    calculator(std::vector<int>& code)
      : calculator::base_type(expression)
      , code(code)
    {
        expression =
            term
            >> *(   ('+' > term             [push_back(ref(code), op_add)])
                |   ('-' > term             [push_back(ref(code), op_sub)])
                )
            ;

        term =
            factor
            >> *(   ('*' > factor           [push_back(ref(code), op_mul)])
                |   ('/' > factor           [push_back(ref(code), op_div)])
                )
            ;

        factor =
            uint_                           [
                                                push_back(ref(code), op_int),
                                                push_back(ref(code), _1)
                                            ]
            |   '(' > expression > ')'
            |   ('-' > factor               [push_back(ref(code), op_neg)])
            |   ('+' > factor)
            ;

        expression.name("expression");
        term.name("term");
        factor.name("factor");

        on_error<fail>
        (
            expression
          , std::cout
                << val("Error! Expecting ")
                << _4                               // what failed?
                << val(" here: \"")
                << construct<std::string>(_3, _2)   // iterators to error-pos, end
                << val("\"")
                << std::endl
        );
    }

    rule<Iterator, space_type> expression, term, factor;
    std::vector<int>& code;
};

template <typename Grammar>
bool compile(Grammar const& calc, std::string const& expr)
{
    std::string::const_iterator iter = expr.begin();
    std::string::const_iterator end = expr.end();
    bool r = phrase_parse(iter, end, calc, space);

    if (r && iter == end)
    {
        std::cout << "-------------------------\n";
        std::cout << "Parsing succeeded\n";
        std::cout << "-------------------------\n";
        return true;
    }
    else
    {
        std::cout << "-------------------------\n";
        std::cout << "Parsing failed\n";
        std::cout << "-------------------------\n";
        return false;
    }
}

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

    vmachine mach;                  //  Our virtual machine
    std::vector<int> code;          //  Our VM code
    calculator calc(code);          //  Our grammar

    std::string str;
    while (std::getline(std::cin, str))
    {
        if (str.empty() || str[0] == 'q' || str[0] == 'Q')
            break;

        code.clear();
        if (::compile(calc, str))
        {
            mach.execute(code);
            std::cout << "\n\nresult = " << mach.top() << std::endl;
            std::cout << "-------------------------\n\n";
        }
    }

    std::cout << "Bye... :-) \n\n";
    return 0;
}


