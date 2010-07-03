//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#if !defined(SPIRIT_LEXER_EXAMPLE_WORD_COUNT_TOKENS_FEB_10_2008_0739PM)
#define SPIRIT_LEXER_EXAMPLE_WORD_COUNT_TOKENS_FEB_10_2008_0739PM

///////////////////////////////////////////////////////////////////////////////
//  Token definition: We keep the base class for the token definition as a 
//                    template parameter to allow this class to be used for
//                    both: the code generation and the lexical analysis
///////////////////////////////////////////////////////////////////////////////
//[wc_static_tokenids
enum tokenids 
{
    IDANY = boost::spirit::lex::min_token_id + 1,
};
//]

//[wc_static_tokendef
// This token definition class can be used without any change for all three
// possible use cases: a dynamic lexical analyzer, a code generator, and a
// static lexical analyzer.
template <typename BaseLexer>
struct word_count_tokens : boost::spirit::lex::lexer_def<BaseLexer> 
{
    template <typename Self>
    void def (Self& self)
    {
        // define tokens and associate them with the lexer
        word = "[^ \t\n]+";
        self = word | '\n' | token_def<>(".", IDANY);
    }
    
    boost::spirit::lex::token_def<std::string> word;
};
//]

#endif
