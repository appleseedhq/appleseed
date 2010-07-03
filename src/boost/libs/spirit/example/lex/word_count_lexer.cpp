//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  This example is the equivalent to the following lex program:
/*
//[wcl_flex_version
    %{
        int c = 0, w = 0, l = 0;
    %}
    %%
    [^ \t\n]+  { ++w; c += yyleng; }
    \n         { ++c; ++l; }
    .          { ++c; }
    %%
    main()
    {
        yylex();
        printf("%d %d %d\n", l, w, c);
    }
//]
*/
//  Its purpose is to do the word count function of the wc command in UNIX. It 
//  prints the number of lines, words and characters in a file. 
//
//  This examples shows how to use semantic actions associated with token 
//  definitions to directly attach actions to tokens. These get executed 
//  whenever the corresponding token got matched in the input sequence. Note,
//  how this example implements all functionality directly in the lexer 
//  definition without any need for a parser.

// #define BOOST_SPIRIT_LEXERTL_DEBUG

#include <boost/config/warning_disable.hpp>
//[wcl_includes
#include <boost/spirit/include/support_argument.hpp>
#include <boost/spirit/include/lex_lexer_lexertl.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>
#include <boost/spirit/include/phoenix_algorithm.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
//]

#include <iostream>
#include <string>

#include "example.hpp"

//[wcl_namespaces
using namespace boost::spirit;
using namespace boost::spirit::lex;
//]

///////////////////////////////////////////////////////////////////////////////
//  Token definition: We use the lexertl based lexer engine as the underlying 
//                    lexer type.
//
//  Note, the token definition type is derived from the 'lexertl_actor_lexer'
//  template, which is a necessary to being able to use lexer semantic actions.
///////////////////////////////////////////////////////////////////////////////
//[wcl_token_definition
template <typename Lexer>
struct word_count_tokens : lexer_def<Lexer>
{
    word_count_tokens()
      : c(0), w(0), l(0),
        word("[^ \t\n]+"), eol("\n"), any(".")  // define tokens
    {}
    
    template <typename Self>
    void def (Self& self)
    {
        using boost::phoenix::ref;
        using boost::phoenix::distance;
        using boost::spirit::arg_names::_1;

        // associate tokens with the lexer
        self =  word  [++ref(w), ref(c) += distance(_1)]
            |   eol   [++ref(c), ++ref(l)] 
            |   any   [++ref(c)]
            ;
    }
    
    std::size_t c, w, l;
    token_def<> word, eol, any;
};
//]

///////////////////////////////////////////////////////////////////////////////
//[wcl_main
int main(int argc, char* argv[])
{
    // read input from the given file
    std::string str (read_from_file(1 == argc ? "word_count.input" : argv[1]));

    // Specifying 'omitted' as the token value type generates a token class not
    // holding any token value at all (not even the iterator_range of the 
    // matched input sequence), therefor optimizing the token, the lexer, and 
    // possibly the parser implementation as much as possible. 
    //
    // Specifying mpl::false_ as the 3rd template parameter generates a token
    // type and an iterator, both holding no lexer state, allowing for even more 
    // aggressive optimizations.
    //
    // As a result the token instances contain the token ids as the only data 
    // member.
    typedef lexertl_token<char const*, omitted, boost::mpl::false_> token_type;

    // lexer type
    typedef lexertl_actor_lexer<token_type> lexer_type;
    
    // create the lexer object instance needed to invoke the lexical analysis 
    word_count_tokens<lexer_type> word_count_lexer;

    // tokenize the given string, all generated tokens are discarded
    char const* first = str.c_str();
    char const* last = &first[str.size()];
    bool r = tokenize(first, last, make_lexer(word_count_lexer));

    if (r) {
        std::cout << "lines: " << word_count_lexer.l 
                  << ", words: " << word_count_lexer.w 
                  << ", characters: " << word_count_lexer.c 
                  << "\n";
    }
    else {
        std::string rest(first, last);
        std::cout << "Lexical analysis failed\n" << "stopped at: \"" 
                  << rest << "\"\n";
    }
    return 0;
}
//]
