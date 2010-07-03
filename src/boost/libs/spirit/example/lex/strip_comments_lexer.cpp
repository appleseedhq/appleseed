//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  This example is the equivalent to the following lex program:
//
//       %{
//       /* INITIAL is the default start state.  COMMENT is our new  */
//       /* state where we remove comments.                          */
//       %}
// 
//       %s COMMENT
//       %%
//       <INITIAL>"//".*    ;
//       <INITIAL>"/*"      BEGIN COMMENT; 
//       <INITIAL>.         ECHO;
//       <INITIAL>[\n]      ECHO;
//       <COMMENT>"*/"      BEGIN INITIAL;
//       <COMMENT>.         ;
//       <COMMENT>[\n]      ;
//       %%
// 
//       main() 
//       {
//         yylex();
//       }
//
//  Its purpose is to strip comments out of C code.
//
//  Additionally this example demonstrates the use of lexer states to structure
//  the lexer definition.

// #define BOOST_SPIRIT_LEXERTL_DEBUG

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/lex_lexer_lexertl.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>
#include <boost/spirit/include/phoenix_core.hpp>

#include <iostream>
#include <string>

#include "example.hpp"

using namespace boost::spirit;  
using namespace boost::spirit::lex;
  
///////////////////////////////////////////////////////////////////////////////
//  Token definition: We use the lexertl based lexer engine as the underlying 
//                    lexer type.
///////////////////////////////////////////////////////////////////////////////
enum tokenids 
{
    IDANY = lex::min_token_id + 10,
    IDEOL = lex::min_token_id + 11
};

template <typename Lexer>
struct strip_comments_tokens : lexer_def<Lexer>
{
    template <typename Self>
    void def (Self& self)
    {
        // define tokens and associate them with the lexer
        cppcomment = "//[^\n]*";
        ccomment = "/\\*";
        endcomment = "\\*/";
        any = ".";
        eol = "\n";
        
        // The following tokens are associated with the default lexer state 
        // (the "INITIAL" state). Specifying 'INITIAL' as a lexer state is 
        // strictly optional.
        self =  cppcomment
            |   ccomment    [ set_state("COMMENT") ]
            |   eol         [ echo_input(std::cout) ]
            |   any         [ echo_input(std::cout) ]
            ;
        
        // The following tokens are associated with the lexer state 'COMMENT'.
        self("COMMENT") 
            =   endcomment  [ set_state("INITIAL") ]
            |   eol
            |   any 
            ;
    }
    
    token_def<> cppcomment, ccomment, endcomment, any, eol;
};

  ///////////////////////////////////////////////////////////////////////////////
int main(int argc, char* argv[])
{
    // iterator type used to expose the underlying input stream
    typedef std::string::iterator base_iterator_type;
    
    // lexer type
    typedef lexertl_actor_lexer<lexertl_token<base_iterator_type> > lexer_type;
    
    // now we use the types defined above to create the lexer and grammar
    // object instances needed to invoke the parsing process
    strip_comments_tokens<lexer_type> strip_comments;             // Our token definition

    // Parsing is done based on the the token stream, not the character 
    // stream read from the input.
    std::string str (read_from_file(1 == argc ? "strip_comments.input" : argv[1]));
    base_iterator_type first = str.begin();
    bool r = tokenize(first, str.end(), make_lexer(strip_comments));

    if (!r) {
        std::string rest(first, str.end());
        std::cerr << "Lexical analysis failed\n" << "stopped at: \"" 
                  << rest << "\"\n";
    }
    return 0;
}



