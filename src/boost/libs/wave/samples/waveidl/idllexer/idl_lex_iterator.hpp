/*=============================================================================
    Boost.Wave: A Standard compliant C++ preprocessor library

    Sample: Re2C based IDL lexer
            Definition of the lexer iterator 
    
    http://www.boost.org/

    Copyright (c) 2001-2009 Hartmut Kaiser. Distributed under the Boost
    Software License, Version 1.0. (See accompanying file
    LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#if !defined(IDL_LEX_ITERATOR_HPP_7926F865_E02F_4950_9EB5_5F453C9FF953_INCLUDED)
#define IDL_LEX_ITERATOR_HPP_7926F865_E02F_4950_9EB5_5F453C9FF953_INCLUDED

#include <string>
#include <iostream>

#include <boost/assert.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/spirit/include/classic_multi_pass.hpp>

#include <boost/wave/language_support.hpp>
#include <boost/wave/util/file_position.hpp>
#include <boost/wave/util/functor_input.hpp>

#include "idl_lex_interface.hpp"

#if 0 != __COMO_VERSION__
#define BOOST_WAVE_EOF_PREFIX static
#else
#define BOOST_WAVE_EOF_PREFIX 
#endif

///////////////////////////////////////////////////////////////////////////////
namespace boost {
namespace wave {
namespace idllexer {
namespace impl {

///////////////////////////////////////////////////////////////////////////////
//  
//  lex_iterator_functor_shim
//
///////////////////////////////////////////////////////////////////////////////

template <typename TokenT> 
class lex_iterator_functor_shim 
{
    typedef lex_input_interface_generator<TokenT> lex_input_interface_type;
        
public:
    template <typename IteratorT>
    lex_iterator_functor_shim(IteratorT const &first, IteratorT const &last, 
            typename TokenT::position_type const &pos, 
            boost::wave::language_support language)
    :   functor_ptr(lex_input_interface_type::new_lexer(first, last, pos, language)) 
    {}

// interface to the boost::spirit::classic::multi_pass_policies::functor_input 
// policy
    typedef TokenT result_type;

    BOOST_WAVE_EOF_PREFIX result_type const eof;
    
    result_type operator()() 
    { 
        BOOST_ASSERT(0 != functor_ptr.get());
        return functor_ptr->get(); 
    }
    void set_position(typename TokenT::position_type const &pos)
    {
        BOOST_ASSERT(0 != functor_ptr.get());
        functor_ptr->set_position(pos);
    }
    
private:
    boost::shared_ptr<cpplexer::lex_input_interface<TokenT> > functor_ptr;
};

#if 0 != __COMO_VERSION__
///////////////////////////////////////////////////////////////////////////////
//  eof token
template <typename LexT>
typename lex_iterator_functor_shim<LexT>::result_type const
    lex_iterator_functor_shim<LexT>::eof;
#endif // 0 != __COMO_VERSION__

///////////////////////////////////////////////////////////////////////////////
}   // namespace impl

///////////////////////////////////////////////////////////////////////////////
//  
//  lex_iterator
//
//      A generic C++ lexer interface class, which allows to plug in different
//      lexer implementations (template parameter LexT). The following 
//      requirement apply:
//
//          - the lexer type should have a function implemented, which returnes
//            the next lexed token from the input stream:
//                typename LexT::token_type get();
//          - at the end of the input stream this function should return the
//            eof token equivalent
//          - the lexer should implement a constructor taking two iterators
//            pointing to the beginning and the end of the input stream and
//            a third parameter containing the name of the parsed input file 
//
///////////////////////////////////////////////////////////////////////////////

template <typename TokenT>
class lex_iterator 
:   public boost::spirit::classic::multi_pass<
        impl::lex_iterator_functor_shim<TokenT>,
        boost::wave::util::functor_input
    >
{
    typedef impl::lex_iterator_functor_shim<TokenT> input_policy_type;
    typedef 
        boost::spirit::classic::multi_pass<input_policy_type, 
                boost::wave::util::functor_input>
        base_type;
    typedef lex_iterator<TokenT>                    self_type;
    
public:
    typedef TokenT token_type;
    
    lex_iterator()
    {}
    
    template <typename IteratorT>
    lex_iterator(IteratorT const &first, IteratorT const &last, 
            typename TokenT::position_type const &pos, 
            boost::wave::language_support language)
    :   base_type(input_policy_type(first, last, pos, language))
    {}

    void set_position(typename TokenT::position_type const &pos)
    {
        typedef typename TokenT::position_type position_type;
        
        // set the new position in the current token
        position_type currpos = base_type::get_input().get_position();
        currpos.set_file(pos.get_file());
        currpos.set_line(pos.get_line());
        base_type::get_input().set_position(currpos);
        
        // set the new position for future tokens as well
        base_type::get_functor().set_position(pos);
    }

#if BOOST_WAVE_SUPPORT_PRAGMA_ONCE != 0
    // this sample does no include guard detection
    bool has_include_guards(std::string&) const { return false; }
#endif    
};

///////////////////////////////////////////////////////////////////////////////
}   // namespace idllexer
}   // namespace wave
}   // namespace boost

#endif // !defined(IDL_LEX_ITERATOR_HPP_7926F865_E02F_4950_9EB5_5F453C9FF953_INCLUDED)
