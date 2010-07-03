/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
///////////////////////////////////////////////////////////////////////////////
//
//  Not a calculator anymore, right? :-)
//
//  [ JDG April 10, 2007 ]       spirit2
//
///////////////////////////////////////////////////////////////////////////////
#include "mini_c.hpp"

///////////////////////////////////////////////////////////////////////////////
//  Our main compiler
///////////////////////////////////////////////////////////////////////////////
template <typename Grammar>
bool compile(Grammar const& prog, std::string const& expr)
{
    typedef white_space<std::string::const_iterator> white_space;
    white_space ws; //  Our skipper

    std::string::const_iterator iter = expr.begin();
    std::string::const_iterator end = expr.end();
    bool r = phrase_parse(iter, end, prog, ws);

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
int main(int argc, char **argv)
{
    char const* filename;
    if (argc > 1)
    {
        filename = argv[1];
    }
    else
    {
        std::cerr << "Error: No input file provided." << std::endl;
        return 1;
    }

    std::ifstream in(filename, std::ios_base::in);

    if (!in)
    {
        std::cerr << "Error: Could not open input file: "
            << filename << std::endl;
        return 1;
    }

    std::string source_code; // We will read the contents here.
    in.unsetf(std::ios::skipws); // No white space skipping!
    std::copy(
        std::istream_iterator<char>(in),
        std::istream_iterator<char>(),
        std::back_inserter(source_code));

    typedef std::string::const_iterator iterator_type;
    typedef program<iterator_type> program;

    vmachine mach;              //  Our virtual machine
    std::vector<int> code;      //  Our VM code
    program prog(code);         //  Our grammar definition

    if (::compile(prog, source_code))
    {
        std::string fmain("main");
        std::string::iterator fbegin = fmain.begin();
        function_info* f = prog.functions.lookup()->find(fbegin, fmain.end());
        if (f == 0)
        {
            std::cerr << "Error: main function not defined" << std::endl;
            return 1;
        }

        int nargs = argc-2;
        if (f->arity != nargs)
        {
            std::cerr << "Error: main function requires " << f->arity << " arguments." << std::endl;
            std::cerr << nargs << "supplied." << std::endl;
            return 1;
        }

        for (int i = 0; i < nargs; ++i)
            mach.stack[i] = boost::lexical_cast<int>(argv[i+2]);

        int r = mach.execute(
            code                                // code
          , code.begin() + f->address           // pc
          , mach.stack.begin()                  // frame_ptr
        );

        std::cout << "-------------------------\n";
        std::cout << "Result: " << r << std::endl;
        std::cout << "-------------------------\n\n";
    }

    std::cout << "Bye... :-) \n\n";
    return 0;
}


