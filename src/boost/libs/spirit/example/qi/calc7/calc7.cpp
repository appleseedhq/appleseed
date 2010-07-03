/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
///////////////////////////////////////////////////////////////////////////////
//
//  Now we'll introduce boolean expressions and control structures.
//  Is it obvious now what we are up to? ;-)
//
//  [ JDG April 9, 2007 ]       spirit2
//
///////////////////////////////////////////////////////////////////////////////
#include "calc7.hpp"

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
struct var_printer
{
    var_printer(std::vector<int> const& stack)
      : stack(stack)
    {
    }

    template <typename String, typename Data>
    void operator()(String const& s, Data const& data)
    {
        std::cout << "    " << s << ": " << stack[data] << std::endl;
    }

    std::vector<int> const& stack;
};

int
main()
{
    std::cout << "/////////////////////////////////////////////////////////\n\n";
    std::cout << "Expression parser...\n\n";
    std::cout << "/////////////////////////////////////////////////////////\n\n";
    std::cout << "Type some statements... ";
    std::cout << "Then type period ('.') to compile, run and print results\n\n";

    typedef std::string::const_iterator iterator_type;
    typedef statement<iterator_type> statement;

    vmachine mach;                      //  Our virtual machine
    std::vector<int> code;              //  Our VM code
    statement calc(code);               //  Our grammar

    std::string str;
    std::string program;
    while (std::getline(std::cin, str))
    {
        if (str.empty() || str[0] == '.')
            break;
        program += str;
    }

    if (::compile(calc, program))
    {
        mach.execute(code, calc.nvars);

        std::cout << "Results------------------\n\n";
        calc.vars.for_each(var_printer(mach.get_stack()));
        std::cout << "-------------------------\n\n";
    }

    std::cout << "Bye... :-) \n\n";
    return 0;
}


