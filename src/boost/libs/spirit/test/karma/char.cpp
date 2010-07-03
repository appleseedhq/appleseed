//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//#define KARMA_FAIL_COMPILATION

#include <boost/config/warning_disable.hpp>
#include <boost/detail/lightweight_test.hpp>

#include <boost/spirit/include/support_argument.hpp>
#include <boost/spirit/include/karma_char.hpp>
#include <boost/spirit/include/karma_generate.hpp>
#include <boost/spirit/include/karma_action.hpp>

#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>

#include "test.hpp"

using namespace spirit_test;

///////////////////////////////////////////////////////////////////////////////
int
main()
{
    using namespace boost::spirit;
    using namespace boost::phoenix;
    using namespace boost::spirit::arg_names;
            
    {
        using namespace boost::spirit::ascii;
        
        BOOST_TEST(test(" ", space));
        BOOST_TEST(test(L" ", space));
        BOOST_TEST(!test("\t", space));
        BOOST_TEST(!test(L"\t", space));
        
        BOOST_TEST(test(" ", space(' ')));
        BOOST_TEST(test(L" ", space(L' ')));
        BOOST_TEST(test("\t", space('\t')));
        BOOST_TEST(test(L"\t", space(L'\t')));
        
        BOOST_TEST(test(" ", space(' '), '\t'));
        BOOST_TEST(test(L" ", space(' '), L'\t'));
        BOOST_TEST(test("\t", space('\t'), ' '));
        BOOST_TEST(test(L"\t", space('\t'), L' '));
        
        BOOST_TEST(test(" ", space, ' '));
        BOOST_TEST(test(L" ", space, L' '));
        BOOST_TEST(test("\t", space, '\t'));
        BOOST_TEST(test(L"\t", space, L'\t'));
    }
    
    {
        BOOST_TEST(test("x", 'x'));
        BOOST_TEST(test(L"x", L'x'));
        BOOST_TEST(!test("x", 'y'));
        BOOST_TEST(!test(L"x", L'y'));
        
        BOOST_TEST(test("x", char_, 'x'));
        BOOST_TEST(test(L"x", char_, L'x'));
        BOOST_TEST(!test("x", char_, 'y'));
        BOOST_TEST(!test(L"x", char_, L'y'));
        
        BOOST_TEST(test("x", char_('x')));
        BOOST_TEST(!test("x", char_('y')));
        BOOST_TEST(test(L"x", char_(L'x')));
        BOOST_TEST(!test(L"x", char_(L'y')));

//         BOOST_TEST(test("x", char_("x")));
//         BOOST_TEST(test(L"x", char_(L"x")));

#if defined(KARMA_FAIL_COMPILATION)
        BOOST_TEST(test("x", char_));           // anychar without a parameter doesn't make any sense
#endif
    }

    {
        BOOST_TEST(test(L"x", L'x'));
        BOOST_TEST(test(L"x", 'x'));

        BOOST_TEST(test(L"x", wchar, L'x'));
        BOOST_TEST(test(L"x", wchar, 'x'));

        BOOST_TEST(test(L"x", wchar(L'x')));
        BOOST_TEST(test(L"x", wchar('x')));
    }
    
    {
        using namespace boost::spirit::ascii;
        
        BOOST_TEST(test_delimited("x ", 'x', ' '));
        BOOST_TEST(test_delimited(L"x ", L'x', L' '));
        BOOST_TEST(!test_delimited("x ", 'y', ' '));
        BOOST_TEST(!test_delimited(L"x ", L'y', L' '));

        BOOST_TEST(test_delimited("x ", 'x', space));
        BOOST_TEST(test_delimited(L"x ", L'x', space(L' ')));
        BOOST_TEST(!test_delimited("x ", 'y', space));
        BOOST_TEST(!test_delimited(L"x ", L'y', space(L' ')));

        BOOST_TEST(test_delimited("x ", char_, 'x', space));
        BOOST_TEST(test_delimited(L"x ", char_, L'x', space(L' ')));
        BOOST_TEST(!test_delimited("x ", char_, 'y', space));
        BOOST_TEST(!test_delimited(L"x ", char_, L'y', space(L' ')));

        BOOST_TEST(test_delimited("x ", char_('x'), space));
        BOOST_TEST(!test_delimited("x ", char_('y'), space(L' ')));
        BOOST_TEST(test_delimited(L"x ", char_(L'x'), space));
        BOOST_TEST(!test_delimited(L"x ", char_(L'y'), space(L' ')));
        
//         BOOST_TEST(test_delimited("x ", char_("x"), space));

#if defined(KARMA_FAIL_COMPILATION)
        BOOST_TEST(test_delimited("x ", char_, space));   // anychar without a parameter doesn't make any sense
#endif
    }
    
    {
        BOOST_TEST(test_delimited(L"x ", L'x', wchar(' ')));
        BOOST_TEST(test_delimited(L"x ", 'x', wchar(' ')));

        BOOST_TEST(test_delimited(L"x ", wchar, L'x', wchar(' ')));
        BOOST_TEST(test_delimited(L"x ", wchar, 'x', wchar(' ')));

        BOOST_TEST(test_delimited(L"x ", wchar(L'x'), wchar(' ')));
        BOOST_TEST(test_delimited(L"x ", wchar('x'), wchar(' ')));
        
#if defined(KARMA_FAIL_COMPILATION)
        BOOST_TEST(test_delimited("x ", char_, space));   // anychar without a parameter doesn't make any sense
#endif
    }
    
    // action tests
    {
        BOOST_TEST(test("x", char_[_1 = val('x')]));
        BOOST_TEST(test(L"x", wchar[_1 = val(L'x')]));
        BOOST_TEST(!test("x", char_[_1 = val('y')]));
        BOOST_TEST(!test(L"x", wchar[_1 = val(L'y')]));
    }

    {   // lazy chars

        using namespace boost::phoenix;
        BOOST_TEST((test("x", char_(val('x')))));
        BOOST_TEST((test(L"x", char_(val(L'x')))));
    }

    return boost::report_errors();
}
