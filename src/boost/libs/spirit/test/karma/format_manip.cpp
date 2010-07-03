//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/karma.hpp>
#include <boost/spirit/include/karma_stream.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>

#include <string>
#include <sstream>
#include <vector>
#include <list>

#include <boost/static_assert.hpp>
#include <boost/detail/lightweight_test.hpp>
#include <boost/assign/std/vector.hpp>
#include <boost/assign/std/list.hpp>

///////////////////////////////////////////////////////////////////////////////
template <typename Char, typename Expr>
bool test(Char const *expected, Expr const& xpr)
{
    namespace spirit = boost::spirit;
    typedef 
        spirit::traits::is_component<spirit::karma::domain, Expr> 
    is_component;

    // report invalid expression error as early as possible
    BOOST_MPL_ASSERT_MSG(is_component::value,
        xpr_is_not_convertible_to_a_generator, ());

    typedef
        typename spirit::result_of::as_component<spirit::karma::domain, Expr>::type
    component;
    typedef typename component::director director;

    component c = spirit::as_component(spirit::karma::domain(), xpr);

    std::ostringstream ostrm;
    ostrm << c;
    return ostrm.good() && ostrm.str() == expected;
}

template <typename Char, typename Expr, typename Parameter, typename Delimiter>
bool test(Char const *expected, 
    boost::spirit::karma::detail::format_manip<Expr, Parameter, Delimiter> const& fm)
{
    std::ostringstream ostrm;
    ostrm << fm;
    return ostrm.good() && ostrm.str() == expected;
}

///////////////////////////////////////////////////////////////////////////////
int
main()
{
    using namespace boost::spirit;
    using namespace boost::spirit::ascii;
    using namespace boost::spirit::arg_names;
    using namespace boost::spirit::karma;

    namespace fusion = boost::fusion;
    using namespace boost::phoenix;

    {
        BOOST_TEST(test( "a", 
            char_[_1 = val('a')]
        ));
        BOOST_TEST(test( "a", 
            format(char_[_1 = val('a')]) 
        ));
        BOOST_TEST(test( "a ", 
            format_delimited(char_[_1 = val('a')], space) 
        ));
        BOOST_TEST(test( "a", 
            format(char_, 'a') 
        ));
        BOOST_TEST(test( "a ", 
            format_delimited(char_, 'a', space) 
        ));
    }
    
    {
        BOOST_TEST(test( "ab", 
            char_[_1 = val('a')] << char_[_1 = val('b')] 
        ));
        BOOST_TEST(test( "ab", 
            format(char_[_1 = val('a')] << char_[_1 = val('b')]) 
        ));
        BOOST_TEST(test( "a b ", 
            format_delimited(char_[_1 = val('a')] << char_[_1 = val('b')], space) 
        ));
        
        fusion::vector<char, char> t('a', 'b');

        BOOST_TEST(test( "ab", 
            format(char_ << char_, t) 
        ));
        BOOST_TEST(test( "a b ", 
            format_delimited(char_ << char_, t, space) 
        ));
    }
    
    {
        BOOST_TEST(test( "abc", 
            char_[_1 = 'a'] << char_[_1 = 'b'] << char_[_1 = 'c']
        ));
        BOOST_TEST(test( "abc", 
            format(char_('a') << char_('b') << char_('c')) 
        ));
        BOOST_TEST(test( "a b c ", 
            format_delimited(char_('a') << char_('b') << char_('c'), space) 
        ));

        fusion::vector<char, char, char> t('a', 'b', 'c');

        BOOST_TEST(test( "abc", 
            format(char_ << char_ << char_, t) 
        ));
        BOOST_TEST(test( "a b c ", 
            format_delimited(char_ << char_ << char_, t, space) 
        ));
    }

    {
        BOOST_TEST(test( "a2", 
            (char_ << int_)[_1 = 'a', _2 = 2] 
        ));

        fusion::vector<char, int> t('a', 2);

        BOOST_TEST(test( "a2", 
            format(char_ << int_, t) 
        ));
        BOOST_TEST(test( "a 2 ", 
            format_delimited(char_ << int_, t, space) 
        ));
    }
    
    using namespace boost::assign;

    {
        // output all elements of a vector
        std::vector<char> v;
        v += 'a', 'b', 'c';
        
        BOOST_TEST(test( "abc", 
            (*char_)[_1 = v] 
        ));
        BOOST_TEST(test( "abc", 
            format(*char_, v)
        ));
        BOOST_TEST(test( "a b c ", 
            format_delimited(*char_, v, space)
        ));

        // output a comma separated list of vector elements
        BOOST_TEST(test( "a, b, c", 
            (char_ % lit(", "))[_0 = fusion::make_single_view(v)] 
        ));
        BOOST_TEST(test( "a, b, c", 
            format((char_ % lit(", "))[_0 = fusion::make_single_view(v)])
        ));
        BOOST_TEST(test( "a , b , c ", 
            format_delimited((char_ % ',')[_0 = fusion::make_single_view(v)], space)
        ));
        BOOST_TEST(test( "a,b,c", 
            format(char_ % ',', v)
        ));
        BOOST_TEST(test( "a , b , c ", 
            format_delimited(char_ % ',', v, space)
        ));

        // output all elements of a list
        std::list<char> l;
        l += 'a', 'b', 'c';
        
//         BOOST_TEST(test( "abc", 
//             (*char_)[_1 = l] 
//         ));
//         BOOST_TEST(test( "abc", 
//             format((*char_)[_1 = l])
//         ));
//         BOOST_TEST(test( "a b c ", 
//             format_delimited((*char_)[_1 = l], space)
//         ));
        BOOST_TEST(test( "abc", 
            format(*char_, l)
        ));
        BOOST_TEST(test( "a b c ", 
            format_delimited(*char_, l, space)
        ));
    }

    return boost::report_errors();
}

