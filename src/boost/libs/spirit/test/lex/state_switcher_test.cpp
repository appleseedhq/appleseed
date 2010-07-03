//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/qi_operator.hpp>
#include <boost/spirit/include/lex_lexer_lexertl.hpp>
#include "test_parser.hpp"

///////////////////////////////////////////////////////////////////////////////
//  Token definition
///////////////////////////////////////////////////////////////////////////////
template <typename Lexer>
struct switch_state_tokens : boost::spirit::lex::lexer_def<Lexer>
{
    template <typename Self>
    void def (Self& self)
    {
        // define tokens and associate them with the lexer
        identifier = "[a-zA-Z_][a-zA-Z0-9_]*";
        self = identifier;
        
        // any token definition to be used as the skip parser during parsing 
        // has to be associated with a separate lexer state (here 'WS') 
        white_space = "[ \\t\\n]+";
        self("WS") = white_space;

        separators = "[,;]";
        self("SEP") = separators;
    }
    
    boost::spirit::lex::token_def<> identifier, white_space, separators;
};

///////////////////////////////////////////////////////////////////////////////
int 
main()
{
    using namespace boost::spirit;
    using namespace boost::spirit::qi;
    using namespace boost::spirit::lex;
    using namespace spirit_test;

    typedef std::string::iterator base_iterator_type;
    typedef boost::spirit::lex::lexertl_token<base_iterator_type> token_type;
    typedef boost::spirit::lex::lexertl_lexer<token_type> lexer_type;

    {
        // the tokens class will be initialized inside the test_parser function
        switch_state_tokens<lexer_type> tokens;
        
        BOOST_TEST(test_parser("ident", tokens.identifier, tokens));
        BOOST_TEST(!test_parser("ident", set_state("WS") >> tokens.identifier, tokens));
        BOOST_TEST(!test_parser("ident", in_state("WS")[tokens.identifier], tokens));

        BOOST_TEST(test_parser("\t \n", set_state("WS") >> tokens.white_space, tokens));
        BOOST_TEST(test_parser("\t \n", in_state("WS")[tokens.white_space], tokens));
        BOOST_TEST(!test_parser("\t \n", tokens.white_space, tokens));
    }
    
    {
        // the tokens class will be initialized inside the test_parser function
        switch_state_tokens<lexer_type> tokens;
        
        BOOST_TEST(test_parser(",ident", tokens.identifier, tokens, 
            in_state("SEP")[tokens.separators]));
        BOOST_TEST(!test_parser(";ident", set_state("WS") >> tokens.identifier, 
            tokens, in_state("SEP")[tokens.separators]));
        BOOST_TEST(!test_parser(",ident", in_state("WS")[tokens.identifier], 
            tokens, in_state("SEP")[tokens.separators]));

        BOOST_TEST(test_parser(",\t \n", set_state("WS") >> tokens.white_space, 
            tokens, in_state("SEP")[tokens.separators]));
        BOOST_TEST(test_parser(";\t \n", in_state("WS")[tokens.white_space], 
            tokens, in_state("SEP")[tokens.separators]));
        BOOST_TEST(!test_parser(",\t \n", tokens.white_space, tokens, 
            in_state("SEP")[tokens.separators]));
    }
    
    {
        // the tokens class will be initialized inside the test_parser function
        switch_state_tokens<lexer_type> tokens;
        
        BOOST_TEST(test_parser("ident\t \n", 
            tokens.identifier >> set_state("WS") >> tokens.white_space, tokens));
        BOOST_TEST(test_parser("\t \nident", 
            in_state("WS")[tokens.white_space] >> tokens.identifier, tokens));
    }

    return boost::report_errors();
}
