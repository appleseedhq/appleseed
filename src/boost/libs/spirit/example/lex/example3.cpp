//  Copyright (c) 2001-2009 Hartmut Kaiser
//  Copyright (c) 2001-2007 Joel de Guzman
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  This example shows how to create a simple lexer recognizing a couple of 
//  different tokens and how to use this with a grammar. This example has a 
//  heavily backtracking grammar which makes it a candidate for lexer based 
//  parsing (all tokens are scanned and generated only once, even if 
//  backtracking is required) which speeds up the overall parsing process 
//  considerably, out-weighting the overhead needed for setting up the lexer.
//
//  Additionally, this example demonstrates, how to define a token set usable 
//  as the skip parser during parsing, allowing to define several tokens to be 
//  ignored.
//
//  This example recognizes couplets, which are sequences of numbers enclosed 
//  in matching pairs of parenthesis. See the comments below to for details
//  and examples.

// #define BOOST_SPIRIT_LEXERTL_DEBUG
// #define BOOST_SPIRIT_DEBUG

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/lex_lexer_lexertl.hpp>

#include <iostream>
#include <fstream>
#include <string>

#include "example.hpp"

using namespace boost::spirit;
using namespace boost::spirit::qi;
using namespace boost::spirit::lex;

///////////////////////////////////////////////////////////////////////////////
//  Token definition
///////////////////////////////////////////////////////////////////////////////
template <typename Lexer>
struct example3_tokens : lexer_def<Lexer>
{
    typedef typename Lexer::token_set token_set;
    
    template <typename Self>
    void def (Self& self)
    {
        // define the tokens to match
        ellipses = "\\.\\.\\.";
        number = "[0-9]+";
        
        // define the whitespace to ignore (spaces, tabs, newlines and C-style 
        // comments)
        white_space 
            =   token_def<>("[ \\t\\n]+")               // whitespace
            |   "\\/\\*[^*]*\\*+([^/*][^*]*\\*+)*\\/"   // C style comments
            ;
        
        // associate the tokens and the token set with the lexer
        self = ellipses | '(' | ')' | number;
        self("WS") = white_space;
    }
    
    // these tokens expose the iterator_range of the matched input sequence
    token_def<> ellipses, identifier, number;
    token_set white_space;
};

///////////////////////////////////////////////////////////////////////////////
//  Grammar definition
///////////////////////////////////////////////////////////////////////////////
template <typename Iterator, typename Lexer>
struct example3_grammar 
  : grammar<Iterator, in_state_skipper<typename Lexer::token_set> >
{
    template <typename TokenDef>
    example3_grammar(TokenDef const& tok)
      : example3_grammar::base_type(start)
    {
        start 
            =  +(couplet | tok.ellipses)
            ;

        //  A couplet matches nested left and right parenthesis.
        //  For example:
        //    (1) (1 2) (1 2 3) ...
        //    ((1)) ((1 2)(3 4)) (((1) (2 3) (1 2 (3) 4))) ...
        //    (((1))) ...
        couplet
            =   tok.number
            |   '(' >> +couplet >> ')'
            ;

        BOOST_SPIRIT_DEBUG_NODE(start);
        BOOST_SPIRIT_DEBUG_NODE(couplet);
    }

    typedef typename Lexer::token_set token_set;
    rule<Iterator, in_state_skipper<token_set> > start, couplet;
};

///////////////////////////////////////////////////////////////////////////////
int main()
{
    // iterator type used to expose the underlying input stream
    typedef std::string::iterator base_iterator_type;

    // This is the token type to return from the lexer iterator
    typedef lexertl_token<base_iterator_type> token_type;

    // This is the lexer type to use to tokenize the input.
    // Here we use the lexertl based lexer engine.
    typedef lexertl_lexer<token_type> lexer_type;

    // This is the token definition type (derived from the given lexer type).
    typedef example3_tokens<lexer_type> example3_tokens;

    // this is the iterator type exposed by the lexer 
    typedef lexer<example3_tokens>::iterator_type iterator_type;

    // this is the type of the grammar to parse
    typedef example3_grammar<iterator_type, lexer_type> example3_grammar;

    // now we use the types defined above to create the lexer and grammar
    // object instances needed to invoke the parsing process
    example3_tokens tokens;                         // Our token definition
    example3_grammar calc(tokens);                  // Our grammar definition

    lexer<example3_tokens> lex(tokens);             // Our lexer

    std::string str (read_from_file("example3.input"));

    // At this point we generate the iterator pair used to expose the
    // tokenized input stream.
    std::string::iterator it = str.begin();
    iterator_type iter = lex.begin(it, str.end());
    iterator_type end = lex.end();

    // Parsing is done based on the the token stream, not the character 
    // stream read from the input.
    // Note, how we use the token_set defined above as the skip parser.
    std::string ws("WS");
    bool r = phrase_parse(iter, end, calc, in_state(ws)[tokens.white_space]);

    if (r && iter == end)
    {
        std::cout << "-------------------------\n";
        std::cout << "Parsing succeeded\n";
        std::cout << "-------------------------\n";
    }
    else
    {
        std::cout << "-------------------------\n";
        std::cout << "Parsing failed\n";
        std::cout << "-------------------------\n";
    }

    std::cout << "Bye... :-) \n\n";
    return 0;
}
