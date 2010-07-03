/*=============================================================================
    Copyright (c) 2001-2008 Joel de Guzman
    Copyright (c) 2001-2008 Hartmut Kaiser

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
///////////////////////////////////////////////////////////////////////////////
//
//  A Calculator example demonstrating generation of AST which gets dumped into
//  a reverse polish notation afterwards.
//
//  [ JDG April 28, 2008 ]
//  [ HK April 28, 2008 ]
//
///////////////////////////////////////////////////////////////////////////////
#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/karma.hpp>

#include <iostream>
#include <vector>
#include <string>

#include "calc2_ast.hpp"

using namespace boost::spirit;
using namespace boost::spirit::ascii;
using namespace boost::spirit::arg_names;

///////////////////////////////////////////////////////////////////////////////
//  Our calculator parser grammar
///////////////////////////////////////////////////////////////////////////////
template <typename Iterator>
struct calculator : qi::grammar<Iterator, expression_ast(), space_type>
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
            |   ('+' >> factor              [_val = pos(_1)])
            ;
    }

    qi::rule<Iterator, expression_ast(), space_type> expression, term, factor;
};

///////////////////////////////////////////////////////////////////////////////
//  Our AST grammar for the generator, this prints the AST in reverse polish 
//  notation
///////////////////////////////////////////////////////////////////////////////
template <typename OuputIterator>
struct ast_rpn
  : karma::grammar<OuputIterator, expression_ast(), space_type>
{
    ast_rpn() : ast_rpn::base_type(ast_node)
    {
        ast_node %= 
                int_        [_1 = _int(_val)]
            |   binary_node [_1 = _bin_op(_val)]
            |   unary_node  [_1 = _unary_op(_val)]
            ;
            
        binary_node = 
                (ast_node << ast_node << char_)
                [ 
                    _1 = _left(_val), _2 = _right(_val), _3 = _op(_val)
                ]
            ;

        unary_node =
                verbatim [
                    ('(' << ast_node << char_ << ')')
                    [
                        _1 = _right(_val), _2 = _op(_val)
                    ]
                ]
            ;
    }

    karma::rule<OuputIterator, expression_ast(), space_type> ast_node;
    karma::rule<OuputIterator, binary_op(), space_type> binary_node;
    karma::rule<OuputIterator, unary_op(), space_type> unary_node;
};

///////////////////////////////////////////////////////////////////////////////
//  Main program
///////////////////////////////////////////////////////////////////////////////
int
main()
{
    std::cout << "/////////////////////////////////////////////////////////\n\n";
    std::cout << "RPN generator for simple expressions...\n\n";
    std::cout << "/////////////////////////////////////////////////////////\n\n";
    std::cout << "Type an expression...or [q or Q] to quit\n\n";

    //  Our parser grammar definitions
    typedef std::string::const_iterator iterator_type;
    typedef calculator<iterator_type> calculator;

    calculator calc; 

    // Our generator grammar definitions
    typedef std::back_insert_iterator<std::string> output_iterator_type;
    typedef ast_rpn<output_iterator_type> ast_rpn;
    
    ast_rpn ast_grammar;

    std::string str;
    while (std::getline(std::cin, str))
    {
        if (str.empty() || str[0] == 'q' || str[0] == 'Q')
            break;

        expression_ast ast;   // this will hold the generated AST
        
        std::string::const_iterator iter = str.begin();
        std::string::const_iterator end = str.end();
        bool r = qi::phrase_parse(iter, end, calc, ast, space);

        if (r && iter == end)
        {
            std::string generated;
            r = karma::generate_delimited(
                std::back_inserter(generated), ast_grammar, ast, space);
            
            if (r)
            {
                std::cout << "RPN for '" << str << "': " << generated 
                          << std::endl;
            }
            else
            {
                std::cout << "-------------------------\n";
                std::cout << "Generating failed\n";
                std::cout << "-------------------------\n";
            }
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


