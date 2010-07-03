//  Copyright (c) 2001-2009 Hartmut Kaiser
//  Copyright (c) 2001-2007 Joel de Guzman
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  This example shows how to create a simple lexer recognizing a couple of 
//  different tokens aimed at a simple language and how to use this lexer with 
//  a grammar. It shows how to associate values to tokens and how to access the 
//  token values from inside the grammar.
//
//  We use explicit token value types, making the corresponding token instances
//  carry convert the matched input into an instance of that type. The token 
//  value is exposed as the parser attribute if this token is used as a 
//  parser component somewhere in a grammar.
//
//  Additionally, this example demonstrates, how to define a token set usable 
//  as the skip parser during parsing, allowing to define several tokens to be 
//  ignored.
//
//  This example recognizes a very simple programming language having 
//  assignment statements and if and while control structures. Look at the file
//  example4.input for an example.

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/lex_lexer_lexertl.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

#include <iostream>
#include <fstream>
#include <string>

#include "example.hpp"

using namespace boost::spirit;
using namespace boost::spirit::qi;
using namespace boost::spirit::lex;
using namespace boost::spirit::arg_names;

using boost::phoenix::val;

///////////////////////////////////////////////////////////////////////////////
//  Token definition
///////////////////////////////////////////////////////////////////////////////
template <typename Lexer>
struct example4_tokens : lexer_def<Lexer>
{
    typedef typename Lexer::token_set token_set;
    
    template <typename Self>
    void def (Self& self)
    {
        // define the tokens to match
        identifier = "[a-zA-Z_][a-zA-Z0-9_]*";
        constant = "[0-9]+";
        if_ = "if";
        else_ = "else";
        while_ = "while";
        
        // define the whitespace to ignore (spaces, tabs, newlines and C-style 
        // comments)
        white_space 
            =   token_def<>("[ \\t\\n]+") 
            |   "\\/\\*[^*]*\\*+([^/*][^*]*\\*+)*\\/"
            ;
        
        // associate the tokens and the token set with the lexer
        self = token_def<>('(') | ')' | '{' | '}' | '=' | ';' | constant;
        self += if_ | else_ | while_ | identifier;
        self("WS") = white_space;
    }

//[example4_token_def
    // these tokens expose the iterator_range of the matched input sequence
    token_def<> if_, else_, while_;
    
    // The following two tokens have an associated value type, 'identifier'
    // carries a string (the identifier name) and 'constant' carries the 
    // matched integer value.
    //
    // Note: any token value type specified explicitly during a token_def<>
    //       declaration needs to be listed during token type definition as 
    //       well (see the typedef for the token_type below).
    //
    // The conversion of the matched input to an instance of this type occurs
    // once (on first access), which makes token values as efficient as 
    // possible. Moreover, token instances are constructed once by the lexer
    // library. From this point on tokens are passed by reference only, 
    // avoiding tokens being copied around.
    token_def<std::string> identifier;
    token_def<unsigned int> constant;
//]

    // token set to be used as the skip parser
    token_set white_space;
};

///////////////////////////////////////////////////////////////////////////////
//  Grammar definition
///////////////////////////////////////////////////////////////////////////////
template <typename Iterator, typename Lexer>
struct example4_grammar 
  : grammar<Iterator, in_state_skipper<typename Lexer::token_set> >
{
    template <typename TokenDef>
    example4_grammar(TokenDef const& tok)
      : example4_grammar::base_type(program)
    {
        program 
            =  +block
            ;

        block
            =   '{' >> *statement >> '}'
            ;

        statement 
            =   assignment
            |   if_stmt
            |   while_stmt
            ;

        assignment 
            =   (tok.identifier >> '=' >> expression >> ';')
                [
                    std::cout << val("assignment statement to: ") << _1 << "\n"
                ]
            ;

        if_stmt
            =   (   tok.if_ >> '(' >> expression >> ')' >> block 
                    >> -(tok.else_ >> block) 
                )
                [
                    std::cout << val("if expression: ") << _2 << "\n"
                ]
            ;

        while_stmt 
            =   (tok.while_ >> '(' >> expression >> ')' >> block)
                [
                    std::cout << val("while expression: ") << _2 << "\n"
                ]
            ;

        //  since expression has a variant return type accommodating for 
        //  std::string and unsigned integer, both possible values may be 
        //  returned to the calling rule
        expression 
            =   tok.identifier [ _val = _1 ]
            |   tok.constant   [ _val = _1 ]
            ;
    }

    typedef typename Lexer::token_set token_set;
    typedef boost::variant<unsigned int, std::string> expression_type;
    
    rule<Iterator, in_state_skipper<token_set> > program, block, statement;
    rule<Iterator, in_state_skipper<token_set> > assignment, if_stmt;
    rule<Iterator, in_state_skipper<token_set> > while_stmt;
    
    //  the expression is the only rule having a return value
    rule<Iterator, expression_type(), in_state_skipper<token_set> >  expression;
};

///////////////////////////////////////////////////////////////////////////////
int main()
{
    // iterator type used to expose the underlying input stream
    typedef std::string::iterator base_iterator_type;
    
//[example4_token
    // This is the lexer token type to use. The second template parameter lists 
    // all attribute types used for token_def's during token definition (see 
    // calculator_tokens<> above). Here we use the predefined lexertl token 
    // type, but any compatible token type may be used instead.
    //
    // If you don't list any token value types in the following declaration 
    // (or just use the default token type: lexertl_token<base_iterator_type>)  
    // it will compile and work just fine, just a bit less efficient. This is  
    // because the token value will be generated from the matched input  
    // sequence every time it is requested. But as soon as you specify at 
    // least one token value type you'll have to list all value types used  
    // for token_def<> declarations in the token definition class above,  
    // otherwise compilation errors will occur.
    typedef lexertl_token<
        base_iterator_type, boost::mpl::vector<unsigned int, std::string> 
    > token_type;
//]
    // Here we use the lexertl based lexer engine.
    typedef lexertl_lexer<token_type> lexer_type;
    
    // This is the token definition type (derived from the given lexer type).
    typedef example4_tokens<lexer_type> example4_tokens;
    
    // this is the iterator type exposed by the lexer 
    typedef lexer<example4_tokens>::iterator_type iterator_type;

    // this is the type of the grammar to parse
    typedef example4_grammar<iterator_type, lexer_type> example4_grammar;

    // now we use the types defined above to create the lexer and grammar
    // object instances needed to invoke the parsing process
    example4_tokens tokens;                         // Our token definition
    example4_grammar calc(tokens);                  // Our grammar definition

    lexer<example4_tokens> lex(tokens);                 // Our lexer

    std::string str (read_from_file("example4.input"));

    // At this point we generate the iterator pair used to expose the
    // tokenized input stream.
    std::string::iterator it = str.begin();
    iterator_type iter = lex.begin(it, str.end());
    iterator_type end = lex.end();
        
    // Parsing is done based on the the token stream, not the character 
    // stream read from the input.
    // Note, how we use the token_set defined above as the skip parser. It must
    // be explicitly wrapped inside a state directive, switching the lexer 
    // state for the duration of skipping whitespace.
    bool r = phrase_parse(iter, end, calc, in_state("WS")[tokens.white_space]);

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
