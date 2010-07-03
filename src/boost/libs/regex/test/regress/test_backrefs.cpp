/*
 *
 * Copyright (c) 2004
 * John Maddock
 *
 * Use, modification and distribution are subject to the 
 * Boost Software License, Version 1.0. (See accompanying file 
 * LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 *
 */

#include "test.hpp"

#ifdef BOOST_MSVC
#pragma warning(disable:4127)
#endif

void test_backrefs()
{
   using namespace boost::regex_constants;
   TEST_INVALID_REGEX("a(b)\\2c", perl);
   TEST_INVALID_REGEX("a(b\\1)c", perl);
   TEST_REGEX_SEARCH("a(b*)c\\1d", perl, "abbcbbd", match_default, make_array(0, 7, 1, 3, -2, -2));
   TEST_REGEX_SEARCH("a(b*)c\\1d", perl, "abbcbd", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("a(b*)c\\1d", perl, "abbcbbbd", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("^(.)\\1", perl, "abc", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("a([bc])\\1d", perl, "abcdabbd", match_default, make_array(4, 8, 5, 6, -2, -2));
   TEST_REGEX_SEARCH("a\\([bc]\\)\\1d", basic, "abcdabbd", match_default, make_array(4, 8, 5, 6, -2, -2));
   // strictly speaking this is at best ambiguous, at worst wrong, this is what most
   // re implimentations will match though.
   TEST_REGEX_SEARCH("a(([bc])\\2)*d", perl, "abbccd", match_default, make_array(0, 6, 3, 5, 3, 4, -2, -2));
   TEST_REGEX_SEARCH("a(([bc])\\2)*d", perl, "abbcbd", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("a((b)*\\2)*d", perl, "abbbd", match_default, make_array(0, 5, 1, 4, 2, 3, -2, -2));
   TEST_REGEX_SEARCH("(ab*)[ab]*\\1", perl, "ababaaa", match_default, make_array(0, 4, 0, 2, -2, 4, 7, 4, 5, -2, -2));
   TEST_REGEX_SEARCH("(a)\\1bcd", perl, "aabcd", match_default, make_array(0, 5, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("(a)\\1bc*d", perl, "aabcd", match_default, make_array(0, 5, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("(a)\\1bc*d", perl, "aabd", match_default, make_array(0, 4, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("(a)\\1bc*d", perl, "aabcccd", match_default, make_array(0, 7, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("(a)\\1bc*[ce]d", perl, "aabcccd", match_default, make_array(0, 7, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("^(a)\\1b(c)*cd$", perl, "aabcccd", match_default, make_array(0, 7, 0, 1, 4, 5, -2, -2));
   TEST_REGEX_SEARCH("a\\(b*\\)c\\1d", basic, "abbcbbd", match_default, make_array(0, 7, 1, 3, -2, -2));
   TEST_REGEX_SEARCH("a\\(b*\\)c\\1d", basic, "abbcbd", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("a\\(b*\\)c\\1d", basic, "abbcbbbd", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("^\\(.\\)\\1", basic, "abc", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("a\\([bc]\\)\\1d", basic, "abcdabbd", match_default, make_array(4, 8, 5, 6, -2, -2));
   // strictly speaking this is at best ambiguous, at worst wrong, this is what most
   // re implimentations will match though.
   TEST_REGEX_SEARCH("a\\(\\([bc]\\)\\2\\)*d", basic, "abbccd", match_default, make_array(0, 6, 3, 5, 3, 4, -2, -2));
   TEST_REGEX_SEARCH("a\\(\\([bc]\\)\\2\\)*d", basic, "abbcbd", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("a\\(\\(b\\)*\\2\\)*d", basic, "abbbd", match_default, make_array(0, 5, 1, 4, 2, 3, -2, -2));
   TEST_REGEX_SEARCH("\\(a\\)\\1bcd", basic, "aabcd", match_default, make_array(0, 5, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("\\(a\\)\\1bc*d", basic, "aabcd", match_default, make_array(0, 5, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("\\(a\\)\\1bc*d", basic, "aabd", match_default, make_array(0, 4, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("\\(a\\)\\1bc*d", basic, "aabcccd", match_default, make_array(0, 7, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("\\(a\\)\\1bc*[ce]d", basic, "aabcccd", match_default, make_array(0, 7, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("^\\(a\\)\\1b\\(c\\)*cd$", basic, "aabcccd", match_default, make_array(0, 7, 0, 1, 4, 5, -2, -2));
   TEST_REGEX_SEARCH("\\(ab*\\)[ab]*\\1", basic, "ababaaa", match_default, make_array(0, 7, 0, 1, -2, -2));
}

