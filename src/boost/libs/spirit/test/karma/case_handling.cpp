//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/config/warning_disable.hpp>
#include <boost/detail/lightweight_test.hpp>

#include <boost/spirit/include/karma_char.hpp>
#include <boost/spirit/include/karma_string.hpp>
#include <boost/spirit/include/karma_generate.hpp>
#include <boost/spirit/include/karma_directive.hpp>

#include "test.hpp"

using namespace spirit_test;

///////////////////////////////////////////////////////////////////////////////
int
main()
{
    using namespace boost::spirit;

    {
        using namespace boost::spirit::ascii;
        
        BOOST_TEST(test("x", lower['X']));
        BOOST_TEST(test("x", lower['x']));
        BOOST_TEST(test(L"x", lower[L'X']));
        BOOST_TEST(test(L"x", lower[L'x']));

        BOOST_TEST(test("x", lower[char_], 'X'));
        BOOST_TEST(test("x", lower[char_], 'x'));
        BOOST_TEST(test("x", lower[char_('X')]));
        BOOST_TEST(test("x", lower[char_('x')]));

        BOOST_TEST(test(" ", lower[space]));
        BOOST_TEST(test("\t", lower[space], '\t'));
        BOOST_TEST(test("\t", lower[space('\t')]));
        
        BOOST_TEST(test("x", lower[lower['X']]));
        BOOST_TEST(test("x", lower[lower['x']]));
        BOOST_TEST(test(L"x", lower[lower[L'X']]));
        BOOST_TEST(test(L"x", lower[lower[L'x']]));

        BOOST_TEST(test("x", lower[lower[char_]], 'X'));
        BOOST_TEST(test("x", lower[lower[char_]], 'x'));
        BOOST_TEST(test("x", lower[lower[char_('X')]]));
        BOOST_TEST(test("x", lower[lower[char_('x')]]));
        
        BOOST_TEST(test(" ", lower[lower[space]]));
        BOOST_TEST(test("\t", lower[lower[space]], '\t'));
        BOOST_TEST(test("\t", lower[lower[space('\t')]]));

        BOOST_TEST(test("X", upper[lower['X']]));
        BOOST_TEST(test("X", upper[lower['x']]));
        BOOST_TEST(test(L"X", upper[lower[L'X']]));
        BOOST_TEST(test(L"X", upper[lower[L'x']]));

        BOOST_TEST(test("X", upper[lower[char_]], 'X'));
        BOOST_TEST(test("X", upper[lower[char_]], 'x'));
        BOOST_TEST(test("X", upper[lower[char_('X')]]));
        BOOST_TEST(test("X", upper[lower[char_('x')]]));
        
        BOOST_TEST(test(" ", upper[lower[space]]));
        BOOST_TEST(test("\t", upper[lower[space]], '\t'));
        BOOST_TEST(test("\t", upper[lower[space('\t')]]));

        BOOST_TEST(test("X", upper['X']));
        BOOST_TEST(test("X", upper['x']));
        BOOST_TEST(test(L"X", upper[L'X']));
        BOOST_TEST(test(L"X", upper[L'x']));

        BOOST_TEST(test("X", upper[char_], 'X'));
        BOOST_TEST(test("X", upper[char_], 'x'));
        BOOST_TEST(test("X", upper[char_('X')]));
        BOOST_TEST(test("X", upper[char_('x')]));
        
        BOOST_TEST(test(" ", upper[space]));
        BOOST_TEST(test("\t", upper[space], '\t'));
        BOOST_TEST(test("\t", upper[space('\t')]));

        BOOST_TEST(test("x", lower[upper['X']]));
        BOOST_TEST(test("x", lower[upper['x']]));
        BOOST_TEST(test(L"x", lower[upper[L'X']]));
        BOOST_TEST(test(L"x", lower[upper[L'x']]));

        BOOST_TEST(test("x", lower[upper[char_]], 'X'));
        BOOST_TEST(test("x", lower[upper[char_]], 'x'));
        BOOST_TEST(test("x", lower[upper[char_('X')]]));
        BOOST_TEST(test("x", lower[upper[char_('x')]]));
        
        BOOST_TEST(test(" ", lower[upper[space]]));
        BOOST_TEST(test("\t", lower[upper[space]], '\t'));
        BOOST_TEST(test("\t", lower[upper[space('\t')]]));

        BOOST_TEST(test("X", upper[upper['X']]));
        BOOST_TEST(test("X", upper[upper['x']]));
        BOOST_TEST(test(L"X", upper[upper[L'X']]));
        BOOST_TEST(test(L"X", upper[upper[L'x']]));

        BOOST_TEST(test("X", upper[upper[char_]], 'X'));
        BOOST_TEST(test("X", upper[upper[char_]], 'x'));
        BOOST_TEST(test("X", upper[upper[char_('X')]]));
        BOOST_TEST(test("X", upper[upper[char_('x')]]));
        
        BOOST_TEST(test(" ", upper[upper[space]]));
        BOOST_TEST(test("\t", upper[upper[space]], '\t'));
        BOOST_TEST(test("\t", upper[upper[space('\t')]]));
    }

    {
        using namespace boost::spirit::ascii;
        
        BOOST_TEST(test("a1- ", lower["a1- "]));
        BOOST_TEST(test("a1- ", lower["a1- "]));
        BOOST_TEST(test("a1- ", lower["a1- "]));
        BOOST_TEST(test("a1- ", lower["A1- "]));
        
        BOOST_TEST(test("a1- ", lower[lit], "a1- "));
        BOOST_TEST(test("a1- ", lower[lit], "A1- "));
        BOOST_TEST(test("a1- ", lower[lit("a1- ")]));
        BOOST_TEST(test("a1- ", lower[lit("A1- ")]));

        BOOST_TEST(test("a1- ", lower[lower["a1- "]]));
        BOOST_TEST(test("a1- ", lower[lower["a1- "]]));
        BOOST_TEST(test("a1- ", lower[lower["a1- "]]));
        BOOST_TEST(test("a1- ", lower[lower["A1- "]]));
        
        BOOST_TEST(test("a1- ", lower[lower[lit]], "a1- "));
        BOOST_TEST(test("a1- ", lower[lower[lit]], "A1- "));
        BOOST_TEST(test("a1- ", lower[lower[lit("a1- ")]]));
        BOOST_TEST(test("a1- ", lower[lower[lit("A1- ")]]));
        
        BOOST_TEST(test("A1- ", upper[lower["a1- "]]));
        BOOST_TEST(test("A1- ", upper[lower["a1- "]]));
        BOOST_TEST(test("A1- ", upper[lower["a1- "]]));
        BOOST_TEST(test("A1- ", upper[lower["A1- "]]));
        
        BOOST_TEST(test("A1- ", upper[lower[lit]], "a1- "));
        BOOST_TEST(test("A1- ", upper[lower[lit]], "A1- "));
        BOOST_TEST(test("A1- ", upper[lower[lit("a1- ")]]));
        BOOST_TEST(test("A1- ", upper[lower[lit("A1- ")]]));
        
        BOOST_TEST(test("A1- ", upper["a1- "]));
        BOOST_TEST(test("A1- ", upper["a1- "]));
        BOOST_TEST(test("A1- ", upper["a1- "]));
        BOOST_TEST(test("A1- ", upper["A1- "]));
        
        BOOST_TEST(test("A1- ", upper[lit], "a1- "));
        BOOST_TEST(test("A1- ", upper[lit], "A1- "));
        BOOST_TEST(test("A1- ", upper[lit("a1- ")]));
        BOOST_TEST(test("A1- ", upper[lit("A1- ")]));
        
        BOOST_TEST(test("a1- ", lower[upper["a1- "]]));
        BOOST_TEST(test("a1- ", lower[upper["a1- "]]));
        BOOST_TEST(test("a1- ", lower[upper["a1- "]]));
        BOOST_TEST(test("a1- ", lower[upper["A1- "]]));
        
        BOOST_TEST(test("a1- ", lower[upper[lit]], "a1- "));
        BOOST_TEST(test("a1- ", lower[upper[lit]], "A1- "));
        BOOST_TEST(test("a1- ", lower[upper[lit("a1- ")]]));
        BOOST_TEST(test("a1- ", lower[upper[lit("A1- ")]]));
        
        BOOST_TEST(test("A1- ", upper[upper["a1- "]]));
        BOOST_TEST(test("A1- ", upper[upper["a1- "]]));
        BOOST_TEST(test("A1- ", upper[upper["a1- "]]));
        BOOST_TEST(test("A1- ", upper[upper["A1- "]]));
        
        BOOST_TEST(test("A1- ", upper[upper[lit]], "a1- "));
        BOOST_TEST(test("A1- ", upper[upper[lit]], "A1- "));
        BOOST_TEST(test("A1- ", upper[upper[lit("a1- ")]]));
        BOOST_TEST(test("A1- ", upper[upper[lit("A1- ")]]));
    }

    return boost::report_errors();
}
