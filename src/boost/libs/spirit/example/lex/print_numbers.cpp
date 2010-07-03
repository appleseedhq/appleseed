//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  This example is the equivalent to the following lex program:
//
//     %{
//     #include <stdio.h>
//     %}
//     %%
//     [0-9]+   { printf("%s\n", yytext); }
//     .|\n     ;
//     %%
//     main()
//     {
//             yylex();
//     }
//
//  Its purpose is to print all the (integer) numbers found in a file

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/lex_lexer_lexertl.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

#include <iostream>
#include <string>

#include "example.hpp"

using namespace boost::spirit;
using namespace boost::spirit::qi;
using namespace boost::spirit::lex;
using namespace boost::spirit::arg_names;

///////////////////////////////////////////////////////////////////////////////
//  Token definition: We use the lexertl based lexer engine as the underlying 
//                    lexer type.
///////////////////////////////////////////////////////////////////////////////
template <typename Lexer>
struct print_numbers_tokens : lexer_def<Lexer>
{
    // define tokens and associate it with the lexer
    template <typename Self>
    void def (Self& self)
    {
        self = token_def<int>("[0-9]*") | ".|\n";
    }
};

///////////////////////////////////////////////////////////////////////////////
//  Grammar definition
///////////////////////////////////////////////////////////////////////////////
template <typename Iterator>
struct print_numbers_grammar : grammar<Iterator>
{
    print_numbers_grammar()
      : grammar<Iterator>(start)
    {
        start =  *(   token(lex::min_token_id)  [ std::cout << _1  << "\n" ] 
                  |   token(lex::min_token_id+1)
                  )
              ;
    }

    rule<Iterator> start;
};

///////////////////////////////////////////////////////////////////////////////
int main(int argc, char* argv[])
{
    // iterator type used to expose the underlying input stream
    typedef std::string::iterator base_iterator_type;

    // the token type to be used, 'int' is available as the type of the token 
    // value and no lexer state is supported
    typedef lexertl_token<
        base_iterator_type, boost::mpl::vector<int>, boost::mpl::false_
    > token_type;
    
    // lexer type
    typedef lexertl_lexer<token_type> lexer_type;
    
    // iterator type exposed by the lexer 
    typedef 
        lexer_iterator<print_numbers_tokens<lexer_type> >::type 
    iterator_type;

    // now we use the types defined above to create the lexer and grammar
    // object instances needed to invoke the parsing process
    print_numbers_tokens<lexer_type> print_tokens;    // Our token definition
    print_numbers_grammar<iterator_type> print;       // Our grammar definition

    // Parsing is done based on the the token stream, not the character 
    // stream read from the input.
    std::string str (read_from_file(1 == argc ? "print_numbers.input" : argv[1]));
    base_iterator_type first = str.begin();
    bool r = tokenize_and_parse(first, str.end(), make_lexer(print_tokens), 
        print);

    if (r) {
        std::cout << "-------------------------\n";
        std::cout << "Parsing succeeded\n";
        std::cout << "-------------------------\n";
    }
    else {
        std::string rest(first, str.end());
        std::cout << "-------------------------\n";
        std::cout << "Parsing failed\n";
        std::cout << "stopped at: \"" << rest << "\"\n";
        std::cout << "-------------------------\n";
    }

    std::cout << "Bye... :-) \n\n";
    return 0;
}



