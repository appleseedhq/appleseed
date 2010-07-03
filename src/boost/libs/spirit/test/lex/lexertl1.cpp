//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/lex_lexer_lexertl.hpp>
#include "test.hpp"

///////////////////////////////////////////////////////////////////////////////
int 
main()
{
    using namespace boost::spirit;
    using namespace spirit_test;

    // the following test aims at the low level lexer and token_def objects, 
    // normally not visible to/directly used by the user

    // initialize tokens
    typedef lex::token_def<std::string> token_def;
    
    std::size_t const CCOMMENT = 1;
    std::size_t const CPPCOMMENT = 2;
    token_def c_comment ("\\/\\*[^*]*\\*+([^/*][^*]*\\*+)*\\/", CCOMMENT);
    token_def cpp_comment ("\\/\\/[^\\n\\r]*(\\n|\\r|\\r\\n)", CPPCOMMENT);
        
    typedef std::string::iterator base_iterator_type;
    typedef lex::lexertl_token<base_iterator_type> token_type;
    typedef lex::lexertl_lexer<token_type> lexer_type;
    typedef lex::lexer_def<lexer_type> lexer_def;

    {
        // initialize lexer
        lexer_def def;
        def.self = c_comment;
        def.self += cpp_comment;
        
        // test lexer for two different input strings
        lex::lexer<lexer_def> lex(def);
        BOOST_TEST(test (lex, "/* this is a comment */", CCOMMENT));
        BOOST_TEST(test (lex, "// this is a comment as well\n", CPPCOMMENT));
    }
    
    {
        // initialize lexer
        lexer_def def;
        def.self = c_comment | cpp_comment;
        
        // test lexer for two different input strings
        lex::lexer<lexer_def> lex(def);
        BOOST_TEST(test (lex, "/* this is a comment */", CCOMMENT));
        BOOST_TEST(test (lex, "// this is a comment as well\n", CPPCOMMENT));
    }
    
    {
        // initialize lexer
        lexer_def def;
        def.self = token_def('+') | '-' | c_comment;
        def.self += '*' | cpp_comment;
        
        // test lexer for two different input strings
        lex::lexer<lexer_def> lex(def);
        BOOST_TEST(test (lex, "/* this is a comment */", CCOMMENT));
        BOOST_TEST(test (lex, "// this is a comment as well\n", CPPCOMMENT));
        BOOST_TEST(test (lex, "+", '+'));
        BOOST_TEST(test (lex, "-", '-'));
        BOOST_TEST(test (lex, "*", '*'));
    }

    return boost::report_errors();
}
