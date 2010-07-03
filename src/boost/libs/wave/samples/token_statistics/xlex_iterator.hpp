/*=============================================================================
    Boost.Wave: A Standard compliant C++ preprocessor library

    Definition of the lexer iterator for the xpressive lexer
    
    http://www.boost.org/

    Copyright (c) 2001-2009 Hartmut Kaiser. Distributed under the Boost 
    Software License, Version 1.0. (See accompanying file 
    LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#if !defined(XLEX_ITERATOR_HPP)
#define XLEX_ITERATOR_HPP

#include <string>
#include <iostream>

#include <boost/assert.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/spirit/include/classic_multi_pass.hpp>

#include <boost/wave/language_support.hpp>
#include <boost/wave/util/file_position.hpp>
#include <boost/wave/util/functor_input.hpp>

#include "xlex_interface.hpp"

///////////////////////////////////////////////////////////////////////////////
namespace boost {
namespace wave {
namespace cpplexer {
namespace xlex {
namespace impl {

///////////////////////////////////////////////////////////////////////////////
//  
//  lex_iterator_functor_shim
//
///////////////////////////////////////////////////////////////////////////////

template <typename TokenT> 
class xlex_iterator_functor_shim 
{
public:
    template <typename IteratorT>
    xlex_iterator_functor_shim(IteratorT const &first, IteratorT const &last, 
            typename TokenT::position_type const &pos, 
            boost::wave::language_support language)
    :   functor_ptr(xlex_input_interface<TokenT>
            ::new_lexer(first, last, pos, language)) 
    {}

// interface to the boost::spirit::classic::multi_pass_policies::functor_input 
// policy
    typedef TokenT result_type;

    /*static*/ result_type const eof;
    
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
    
#if BOOST_WAVE_SUPPORT_PRAGMA_ONCE != 0
    bool has_include_guards(std::string& guard_name) const
    {
        return functor_ptr->has_include_guards(guard_name);
    }
#endif    

private:
    boost::shared_ptr<lex_input_interface<TokenT> > functor_ptr;
};

///////////////////////////////////////////////////////////////////////////////
//  eof token
//template <typename TokenT>
//typename xlex_iterator_functor_shim<TokenT>::result_type const
//    xlex_iterator_functor_shim<TokenT>::eof = 
//        typename xlex_iterator_functor_shim<TokenT>::result_type();

///////////////////////////////////////////////////////////////////////////////
}   // namespace impl

///////////////////////////////////////////////////////////////////////////////
//  
//  xlex_iterator
//
//      A generic C++ lexer interface class, which allows to plug in different
//      lexer implementations (template parameter LexT). The following 
//      requirement apply:
//
//          - the lexer type should have a function implemented, which returns
//            the next lexed token from the input stream:
//                typename LexT::token_type get();
//          - at the end of the input stream this function should return the
//            eof token equivalent
//          - the lexer should implement a constructor taking two iterators
//            pointing to the beginning and the end of the input stream and
//            a third parameter containing the name of the parsed input file,
//            the 4th parameter contains the information about the mode the 
//            preprocessor is used in (C99/C++ mode etc.)
//
///////////////////////////////////////////////////////////////////////////////

template <typename TokenT>
class xlex_iterator 
:   public boost::spirit::classic::multi_pass<
        impl::xlex_iterator_functor_shim<TokenT>,
        boost::wave::util::functor_input
    >
{
    typedef impl::xlex_iterator_functor_shim<TokenT> input_policy_type;
    typedef 
        boost::spirit::classic::multi_pass<input_policy_type, 
                boost::wave::util::functor_input>
        base_type;
    
public:
    typedef TokenT token_type;
    
    xlex_iterator()
    {}
    
    template <typename IteratorT>
    xlex_iterator(IteratorT const &first, IteratorT const &last, 
            typename TokenT::position_type const &pos, 
            boost::wave::language_support language)
    :   base_type(input_policy_type(first, last, pos, language))
    {}

    void set_position(typename TokenT::position_type const &pos)
    {
        typedef typename token_type::position_type position_type;
        
    // set the new position in the current token
    token_type const& currtoken = base_type::get_input();
    position_type currpos = currtoken.get_position();
    
        currpos.set_file(pos.get_file());
        currpos.set_line(pos.get_line());
        base_type::get_input().set_position(currpos);
        
    // set the new position for future tokens as well
        if (token_type::string_type::npos != 
            currtoken.get_value().find_first_of('\n'))
        {
            currpos.set_line(pos.get_line() + 1);
        }
        base_type::get_functor().set_position(currpos);
    }

#if BOOST_WAVE_SUPPORT_PRAGMA_ONCE != 0
    // return, whether the current file has include guards
    // this function returns meaningful results only if the file was scanned 
    // completely
    bool has_include_guards(std::string& guard_name) const
    {
        return base_type::get_functor().has_include_guards(guard_name);
    }
#endif    
};

///////////////////////////////////////////////////////////////////////////////
}   // namespace xlex
}   // namespace cpplexer
}   // namespace wave
}   // namespace boost

#endif // !defined(XLEX_ITERATOR_HPP)
