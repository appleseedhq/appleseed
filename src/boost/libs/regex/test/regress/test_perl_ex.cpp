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

void test_options3();

void test_independent_subs()
{
   using namespace boost::regex_constants;
   TEST_REGEX_SEARCH("(?>^abc)", perl, "abc", match_default, make_array(0, 3, -2, -2));
   TEST_REGEX_SEARCH("(?>^abc)", perl, "def\nabc", match_default, make_array(4, 7, -2, -2));
   TEST_REGEX_SEARCH("(?>^abc)", perl, "defabc", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?>.*/)foo", perl, "/this/is/a/very/long/line/in/deed/with/very/many/slashes/in/it/you/see/", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?>.*/)foo", perl, "/this/is/a/very/long/line/in/deed/with/very/many/slashes/in/and/foo", match_default, make_array(0, 67, -2, -2));
   TEST_REGEX_SEARCH("(?>(\\.\\d\\d[1-9]?))\\d+", perl, "1.230003938", match_default, make_array(1, 11, 1, 4, -2, -2));
   TEST_REGEX_SEARCH("(?>(\\.\\d\\d[1-9]?))\\d+", perl, "1.875000282", match_default, make_array(1, 11, 1, 5, -2, -2));
   TEST_REGEX_SEARCH("(?>(\\.\\d\\d[1-9]?))\\d+", perl, "1.235", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("^((?>\\w+)|(?>\\s+))*$", perl, "now is the time for all good men to come to the aid of the party", match_default, make_array(0, 64, 59, 64, -2, -2));
   TEST_REGEX_SEARCH("^((?>\\w+)|(?>\\s+))*$", perl, "this is not a line with only words and spaces!", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("((?>\\d+))(\\w)", perl, "12345a", match_default, make_array(0, 6, 0, 5, 5, 6, -2, -2));
   TEST_REGEX_SEARCH("((?>\\d+))(\\w)", perl, "12345+", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("((?>\\d+))(\\d)", perl, "12345", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?>a+)b", perl, "aaab", match_default, make_array(0, 4, -2, -2));
   TEST_REGEX_SEARCH("((?>a+)b)", perl, "aaab", match_default, make_array(0, 4, 0, 4, -2, -2));
   TEST_REGEX_SEARCH("(?>(a+))b", perl, "aaab", match_default, make_array(0, 4, 0, 3, -2, -2));
   TEST_REGEX_SEARCH("(?>b)+", perl, "aaabbbccc", match_default, make_array(3, 6, -2, -2));
   TEST_REGEX_SEARCH("(?>a+|b+|c+)*c", perl, "aaabbbbccccd", match_default, make_array(0, 8, -2, 8, 9, -2, 9, 10, -2, 10, 11, -2, -2));
   TEST_REGEX_SEARCH("((?>[^()]+)|\\([^()]*\\))+", perl, "((abc(ade)ufh()()x", match_default, make_array(2, 18, 17, 18, -2, -2));
   TEST_REGEX_SEARCH("\\(((?>[^()]+)|\\([^()]+\\))+\\)", perl, "(abc)", match_default, make_array(0, 5, 1, 4, -2, -2));
   TEST_REGEX_SEARCH("\\(((?>[^()]+)|\\([^()]+\\))+\\)", perl, "(abc(def)xyz)", match_default, make_array(0, 13, 9, 12, -2, -2));
   TEST_REGEX_SEARCH("\\(((?>[^()]+)|\\([^()]+\\))+\\)", perl, "((()aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("(?>a*)*", perl, "a", match_default, make_array(0, 1, -2, 1, 1, -2, -2));
   TEST_REGEX_SEARCH("(?>a*)*", perl, "aa", match_default, make_array(0, 2, -2, 2, 2, -2, -2));
   TEST_REGEX_SEARCH("(?>a*)*", perl, "aaaa", match_default, make_array(0, 4, -2, 4, 4, -2, -2));
   TEST_REGEX_SEARCH("(?>a*)*", perl, "a", match_default, make_array(0, 1, -2, 1, 1, -2, -2));
   TEST_REGEX_SEARCH("(?>a*)*", perl, "aaabcde", match_default, make_array(0, 3, -2, 3, 3, -2, 4, 4, -2, 5, 5, -2, 6, 6, -2, 7, 7, -2, -2));
   TEST_REGEX_SEARCH("((?>a*))*", perl, "aaaaa", match_default, make_array(0, 5, 5, 5, -2, 5, 5, 5, 5, -2, -2));
   TEST_REGEX_SEARCH("((?>a*))*", perl, "aabbaa", match_default, make_array(0, 2, 2, 2, -2, 2, 2, 2, 2, -2, 3, 3, 3, 3, -2, 4, 6, 6, 6, -2, 6, 6, 6, 6, -2, -2));
   TEST_REGEX_SEARCH("((?>a*?))*", perl, "aaaaa", match_default, make_array(0, 0, 0, 0, -2, 1, 1, 1, 1, -2, 2, 2, 2, 2, -2, 3, 3, 3, 3, -2, 4, 4, 4, 4, -2, 5, 5, 5, 5, -2, -2));
   TEST_REGEX_SEARCH("((?>a*?))*", perl, "aabbaa", match_default, make_array(0, 0, 0, 0, -2, 1, 1, 1, 1, -2, 2, 2, 2, 2, -2, 3, 3, 3, 3, -2, 4, 4, 4, 4, -2, 5, 5, 5, 5, -2, 6, 6, 6, 6, -2, -2));
   TEST_REGEX_SEARCH("word (?>(?:(?!otherword)[a-zA-Z0-9]+ ){0,30})otherword", perl, "word cat dog elephant mussel cow horse canary baboon snake shark otherword", match_default, make_array(0, 74, -2, -2));
   TEST_REGEX_SEARCH("word (?>(?:(?!otherword)[a-zA-Z0-9]+ ){0,30})otherword", perl, "word cat dog elephant mussel cow horse canary baboon snake shark", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("word (?>[a-zA-Z0-9]+ ){0,30}otherword", perl, "word cat dog elephant mussel cow horse canary baboon snake shark the quick brown fox and the lazy dog and several other words getting close to thirty by now I hope", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("word (?>[a-zA-Z0-9]+ ){0,30}otherword", perl, "word cat dog elephant mussel cow horse canary baboon snake shark the quick brown fox and the lazy dog and several other words getting close to thirty by now I really really hope otherword", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("((?>Z)+|A)+", perl, "ZABCDEFG", match_default, make_array(0, 2, 1, 2, -2, -2));
   TEST_INVALID_REGEX("((?>)+|A)+", perl);
}

void test_conditionals()
{
   using namespace boost::regex_constants;
   TEST_REGEX_SEARCH("(?:(a)|b)(?(1)A|B)", perl, "aA", match_default, make_array(0, 2, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("(?:(a)|b)(?(1)A|B)", perl, "bB", match_default, make_array(0, 2, -2, -2));
   TEST_REGEX_SEARCH("(?:(a)|b)(?(1)A|B)", perl, "aB", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?:(a)|b)(?(1)A|B)", perl, "bA", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("(a)?(?(1)A)B", perl, "aAB", match_default, make_array(0, 3, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("(a)?(?(1)A)B", perl, "B", match_default, make_array(0, 1, -1, -1, -2, -2));
   TEST_REGEX_SEARCH("(a)?(?(1)|A)B", perl, "aB", match_default, make_array(0, 2, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("(a)?(?(1)|A)B", perl, "AB", match_default, make_array(0, 2, -1, -1, -2, -2));

   TEST_REGEX_SEARCH("^(a)?(?(1)a|b)+$", perl, "aa", match_default, make_array(0, 2, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("^(a)?(?(1)a|b)+$", perl, "b", match_default, make_array(0, 1, -2, -2));
   TEST_REGEX_SEARCH("^(a)?(?(1)a|b)+$", perl, "bb", match_default, make_array(0, 2, -2, -2));
   TEST_REGEX_SEARCH("^(a)?(?(1)a|b)+$", perl, "ab", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("^(?(?=abc)\\w{3}:|\\d\\d)$", perl, "abc:", match_default, make_array(0, 4, -2, -2));
   TEST_REGEX_SEARCH("^(?(?=abc)\\w{3}:|\\d\\d)$", perl, "12", match_default, make_array(0, 2, -2, -2));
   TEST_REGEX_SEARCH("^(?(?=abc)\\w{3}:|\\d\\d)$", perl, "123", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("^(?(?=abc)\\w{3}:|\\d\\d)$", perl, "xyz", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("^(?(?!abc)\\d\\d|\\w{3}:)$", perl, "abc:", match_default, make_array(0, 4, -2, -2));
   TEST_REGEX_SEARCH("^(?(?!abc)\\d\\d|\\w{3}:)$", perl, "12", match_default, make_array(0, 2, -2, -2));
   TEST_REGEX_SEARCH("^(?(?!abc)\\d\\d|\\w{3}:)$", perl, "123", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("^(?(?!abc)\\d\\d|\\w{3}:)$", perl, "xyz", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("(?(?<=foo)bar|cat)", perl, "foobar", match_default, make_array(3, 6, -2, -2));
   TEST_REGEX_SEARCH("(?(?<=foo)bar|cat)", perl, "cat", match_default, make_array(0, 3, -2, -2));
   TEST_REGEX_SEARCH("(?(?<=foo)bar|cat)", perl, "fcat", match_default, make_array(1, 4, -2, -2));
   TEST_REGEX_SEARCH("(?(?<=foo)bar|cat)", perl, "focat", match_default, make_array(2, 5, -2, -2));
   TEST_REGEX_SEARCH("(?(?<=foo)bar|cat)", perl, "foocat", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("(?(?<!foo)cat|bar)", perl, "foobar", match_default, make_array(3, 6, -2, -2));
   TEST_REGEX_SEARCH("(?(?<!foo)cat|bar)", perl, "cat", match_default, make_array(0, 3, -2, -2));
   TEST_REGEX_SEARCH("(?(?<!foo)cat|bar)", perl, "fcat", match_default, make_array(1, 4, -2, -2));
   TEST_REGEX_SEARCH("(?(?<!foo)cat|bar)", perl, "focat", match_default, make_array(2, 5, -2, -2));
   TEST_REGEX_SEARCH("(?(?<!foo)cat|bar)", perl, "foocat", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("(\\()?[^()]+(?(1)\\))", perl, "abcd", match_default, make_array(0, 4, -1, -1, -2, -2));
   TEST_REGEX_SEARCH("(\\()?[^()]+(?(1)\\))", perl, "(abcd)", match_default, make_array(0, 6, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("(\\()?[^()]+(?(1)\\))", perl, "the quick (abcd) fox", match_default, make_array(0, 10, -1, -1, -2, 10, 16, 10, 11, -2, 16, 20, -1, -1, -2, -2));
   TEST_REGEX_SEARCH("(\\()?[^()]+(?(1)\\))", perl, "(abcd", match_default, make_array(1, 5, -1, -1, -2, -2));

   TEST_REGEX_SEARCH("^(?(2)a|(1)(2))+$", perl, "12", match_default, make_array(0, 2, 0, 1, 1, 2, -2, -2));
   TEST_REGEX_SEARCH("^(?(2)a|(1)(2))+$", perl, "12a", match_default, make_array(0, 3, 0, 1, 1, 2, -2, -2));
   TEST_REGEX_SEARCH("^(?(2)a|(1)(2))+$", perl, "12aa", match_default, make_array(0, 4, 0, 1, 1, 2, -2, -2));
   TEST_REGEX_SEARCH("^(?(2)a|(1)(2))+$", perl, "1234", match_default, make_array(-2, -2));

   TEST_INVALID_REGEX("(a)(?(1)a|b|c)", perl);
   TEST_INVALID_REGEX("(?(?=a)a|b|c)", perl);
   TEST_INVALID_REGEX("(?(1a)", perl);
   TEST_INVALID_REGEX("(?:(a)|b)(?(", perl);
   TEST_INVALID_REGEX("(?:(a)|b)(?(1", perl);
   TEST_INVALID_REGEX("(?:(a)|b)(?(1)", perl);
   TEST_INVALID_REGEX("(?:(a)|b)(?(a", perl);
   TEST_INVALID_REGEX("(?:(a)|b)(?(?", perl);
   TEST_INVALID_REGEX("(?:(a)|b)(?(?:", perl);
   TEST_INVALID_REGEX("(?:(a)|b)(?(?<", perl);
   TEST_INVALID_REGEX("(?:(a)|b)(?(?<a", perl);

   TEST_INVALID_REGEX("(?(?!#?)+)", perl);
   TEST_INVALID_REGEX("(?(?=:-){0})", perl);
   TEST_INVALID_REGEX("(?(123){1})", perl);
   TEST_INVALID_REGEX("(?(?<=A)*)", perl);
   TEST_INVALID_REGEX("(?(?<=A)+)", perl);

   TEST_INVALID_REGEX("(?<!*|^)", perl);
   TEST_INVALID_REGEX("(?<!*|A)", perl);
   TEST_INVALID_REGEX("(?<=?|A)", perl);
   TEST_INVALID_REGEX("(?<=*|\\B)", perl);
}

void test_options()
{
   // test the (?imsx) construct:
   using namespace boost::regex_constants;
   TEST_INVALID_REGEX("(?imsx", perl);
   TEST_INVALID_REGEX("(?g", perl);
   TEST_INVALID_REGEX("(?im-sx", perl);
   TEST_INVALID_REGEX("(?im-sx:", perl);
   TEST_INVALID_REGEX("(?-g)", perl);
   TEST_REGEX_SEARCH("(?-m)^abc", perl, "abc\nabc", match_default, make_array(0, 3, -2, -2));
   TEST_REGEX_SEARCH("(?m)^abc", perl|no_mod_m, "abc\nabc", match_default, make_array(0, 3, -2, 4, 7, -2, -2));
   TEST_REGEX_SEARCH("(?-m)^abc", perl, "abc\nabc", match_default, make_array(0, 3, -2, -2));
   TEST_REGEX_SEARCH("(?m)^abc", perl|no_mod_m, "abc\nabc", match_default, make_array(0, 3, -2, 4, 7, -2, -2));

   TEST_REGEX_SEARCH("   ^    a   (?# begins with a)  b\\sc (?# then b c) $ (?# then end)", perl|mod_x, "ab c", match_default, make_array(0, 4, -2, -2));
   TEST_REGEX_SEARCH("   ^    a   (?# begins with a)  b\\sc (?# then b c) $ (?# then end)", perl|mod_x, "abc", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("   ^    a   (?# begins with a)  b\\sc (?# then b c) $ (?# then end)", perl|mod_x, "ab cde", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?x)   ^    a   (?# begins with a)  b\\sc (?# then b c) $ (?# then end)", perl, "ab c", match_default, make_array(0, 4, -2, -2));
   TEST_REGEX_SEARCH("(?x)   ^    a   (?# begins with a)  b\\sc (?# then b c) $ (?# then end)", perl, "abc", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?x)   ^    a   (?# begins with a)  b\\sc (?# then b c) $ (?# then end)", perl, "ab cde", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("^   a\\ b[c ]d       $", perl|mod_x, "a bcd", match_default, make_array(0, 5, -2, -2));
   TEST_REGEX_SEARCH("^   a\\ b[c ]d       $", perl|mod_x, "a b d", match_default, make_array(0, 5, -2, -2));
   TEST_REGEX_SEARCH("^   a\\ b[c ]d       $", perl|mod_x, "abcd", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("^   a\\ b[c ]d       $", perl|mod_x, "ab d", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("^1234(?# test newlines\n  inside)", perl|mod_x, "1234", match_default, make_array(0, 4, -2, -2));
   TEST_REGEX_SEARCH("^1234 #comment in extended re\n", perl|mod_x, "1234", match_default, make_array(0, 4, -2, -2));
   TEST_REGEX_SEARCH("#rhubarb\n  abcd", perl|mod_x, "abcd", match_default, make_array(0, 4, -2, -2));
   TEST_REGEX_SEARCH("^1234 #comment in extended re\r\n", perl|mod_x, "1234", match_default, make_array(0, 4, -2, -2));
   TEST_REGEX_SEARCH("#rhubarb\r\n  abcd", perl|mod_x, "abcd", match_default, make_array(0, 4, -2, -2));
   TEST_REGEX_SEARCH("^abcd#rhubarb", perl|mod_x, "abcd", match_default, make_array(0, 4, -2, -2));
   TEST_REGEX_SEARCH("^abcd#rhubarb", perl, "abcd#rhubarb", match_default, make_array(0, 12, -2, -2));
   TEST_REGEX_SEARCH("^a   b\n\n    c", perl|mod_x, "abc", match_default, make_array(0, 3, -2, -2));

   TEST_REGEX_SEARCH("(?(?=[^a-z]+[a-z])  \\d{2}-[a-z]{3}-\\d{2}  |  \\d{2}-\\d{2}-\\d{2} ) ", perl|mod_x, "12-sep-98", match_default, make_array(0, 9, -2, -2));
   TEST_REGEX_SEARCH("(?(?=[^a-z]+[a-z])  \\d{2}-[a-z]{3}-\\d{2}  |  \\d{2}-\\d{2}-\\d{2} ) ", perl|mod_x, "12-09-98", match_default, make_array(0, 8, -2, -2));
   TEST_REGEX_SEARCH("(?(?=[^a-z]+[a-z])  \\d{2}-[a-z]{3}-\\d{2}  |  \\d{2}-\\d{2}-\\d{2} ) ", perl|mod_x, "sep-12-98", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("^a (?#xxx) (?#yyy) {3}c", perl|mod_x, "aaac", match_default, make_array(0, 4, -2, -2));
   TEST_REGEX_SEARCH("ab", perl|mod_x, "ab", match_default, make_array(0, 2, -2, -2));
   TEST_REGEX_SEARCH("   abc\\Q abc\\Eabc", perl|mod_x, "abc abcabc", match_default, make_array(0, 10, -2, -2));
   TEST_REGEX_SEARCH("   abc\\Q abc\\Eabc", perl|mod_x, "abcabcabc", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("abc#comment\n    \\Q#not comment\n    literal\\E", perl|mod_x, "abc#not comment\n    literal", match_default, make_array(0, 27, -2, -2));
   TEST_REGEX_SEARCH("abc#comment\n    \\Q#not comment\n    literal", perl|mod_x, "abc#not comment\n    literal", match_default, make_array(0, 27, -2, -2));
   TEST_REGEX_SEARCH("abc#comment\n    \\Q#not comment\n    literal\\E #more comment\n    ", perl|mod_x, "abc#not comment\n    literal", match_default, make_array(0, 27, -2, -2));
   TEST_REGEX_SEARCH("abc#comment\n    \\Q#not comment\n    literal\\E #more comment", perl|mod_x, "abc#not comment\n    literal", match_default, make_array(0, 27, -2, -2));
   
   TEST_REGEX_SEARCH("(a (?x)b c)d e", perl, "a bcd e", match_default, make_array(0, 7, 0, 4, -2, -2));
   TEST_REGEX_SEARCH("(a (?x)b c)d e", perl, "a b cd e", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(a (?x)b c)d e", perl, "abcd e", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(a (?x)b c)d e", perl, "a bcde", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(a b(?x)c d (?-x)e f)", perl, "a bcde f", match_default, make_array(0, 8, 0, 8, -2, -2));
   TEST_REGEX_SEARCH("(a b(?x)c d (?-x)e f)", perl, "abcdef", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("a(?x: b c )d", perl, "XabcdY", match_default, make_array(1, 5, -2, -2));
   TEST_REGEX_SEARCH("a(?x: b c )d", perl, "Xa b c d Y", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("((?x)x y z | a b c)", perl, "XabcY", match_default, make_array(1, 4, 1, 4, -2, -2));
   TEST_REGEX_SEARCH("((?x)x y z | a b c)", perl, "AxyzB", match_default, make_array(1, 4, 1, 4, -2, -2));

   TEST_REGEX_SEARCH("(a (?x)b c)d e", perl, "a bcd e", match_default, make_array(0, 7, 0, 4, -2, -2));
   TEST_REGEX_SEARCH("(a (?x)b c)d e", perl, "a b cd e", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(a (?x)b c)d e", perl, "abcd e", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(a (?x)b c)d e", perl, "a bcde", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(a b(?x)c d (?-x)e f)", perl, "a bcde f", match_default, make_array(0, 8, 0, 8, -2, -2));
   TEST_REGEX_SEARCH("(a b(?x)c d (?-x)e f)", perl, "abcdef", match_default, make_array(-2, -2));
}

void test_options2()
{
   using namespace boost::regex_constants;
   TEST_INVALID_REGEX("(?i-", perl);
   TEST_INVALID_REGEX("(?i-s", perl);
   TEST_INVALID_REGEX("(?i-sd)", perl);
   TEST_REGEX_SEARCH("(a(?i)b)c", perl, "abc", match_default, make_array(0, 3, 0, 2, -2, -2));
   TEST_REGEX_SEARCH("(a(?i)b)c", perl, "aBc", match_default, make_array(0, 3, 0, 2, -2, -2));
   TEST_REGEX_SEARCH("(a(?i)b)c", perl, "abC", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(a(?i)b)c", perl, "aBC", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(a(?i)b)c", perl, "Abc", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(a(?i)b)c", perl, "ABc", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(a(?i)b)c", perl, "ABC", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(a(?i)b)c", perl, "AbC", match_default, make_array(-2, -2));
   
   TEST_REGEX_SEARCH("(a(?i)B)c", perl, "abc", match_default, make_array(0, 3, 0, 2, -2, -2));
   TEST_REGEX_SEARCH("(a(?i)B)c", perl, "aBc", match_default, make_array(0, 3, 0, 2, -2, -2));
   TEST_REGEX_SEARCH("(a(?i)B)c", perl, "abC", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(a(?i)B)c", perl, "aBC", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(a(?i)B)c", perl, "Abc", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(a(?i)B)c", perl, "ABc", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(a(?i)B)c", perl, "ABC", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(a(?i)B)c", perl, "AbC", match_default, make_array(-2, -2));
   
   TEST_REGEX_SEARCH("a(?i:b)c", perl, "abc", match_default, make_array(0, 3, -2, -2));
   TEST_REGEX_SEARCH("a(?i:b)c", perl, "aBc", match_default, make_array(0, 3, -2, -2));
   TEST_REGEX_SEARCH("a(?i:b)c", perl, "ABC", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("a(?i:b)c", perl, "abC", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("a(?i:b)c", perl, "aBC", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("a(?i:b)*c", perl, "aBc", match_default, make_array(0, 3, -2, -2));
   TEST_REGEX_SEARCH("a(?i:b)*c", perl, "aBBc", match_default, make_array(0, 4, -2, -2));
   TEST_REGEX_SEARCH("a(?i:b)*c", perl, "aBC", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("a(?i:b)*c", perl, "aBBC", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("a(?=b(?i)c)\\w\\wd", perl, "abcd", match_default, make_array(0, 4, -2, -2));
   TEST_REGEX_SEARCH("a(?=b(?i)c)\\w\\wd", perl, "abCd", match_default, make_array(0, 4, -2, -2));
   TEST_REGEX_SEARCH("a(?=b(?i)c)\\w\\wd", perl, "aBCd", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("a(?=b(?i)c)\\w\\wd", perl, "abcD", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("(?s-i:more.*than).*million", perl|icase, "more than million", match_default, make_array(0, 17, -2, -2));
   TEST_REGEX_SEARCH("(?s-i:more.*than).*million", perl|icase, "more than MILLION", match_default, make_array(0, 17, -2, -2));
   TEST_REGEX_SEARCH("(?s-i:more.*than).*million", perl|icase, "more \n than Million", match_default, make_array(0, 19, -2, -2));
   TEST_REGEX_SEARCH("(?s-i:more.*than).*million", perl|icase, "MORE THAN MILLION", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?s-i:more.*than).*million", perl|icase|no_mod_s|no_mod_m, "more \n than \n million", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("(?:(?s-i)more.*than).*million", perl|icase, "more than million", match_default, make_array(0, 17, -2, -2));
   TEST_REGEX_SEARCH("(?:(?s-i)more.*than).*million", perl|icase, "more than MILLION", match_default, make_array(0, 17, -2, -2));
   TEST_REGEX_SEARCH("(?:(?s-i)more.*than).*million", perl|icase, "more \n than Million", match_default, make_array(0, 19, -2, -2));
   TEST_REGEX_SEARCH("(?:(?s-i)more.*than).*million", perl|icase, "MORE THAN MILLION", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?:(?s-i)more.*than).*million", perl|icase|no_mod_s|no_mod_m, "more \n than \n million", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("(?>a(?i)b+)+c", perl, "abc", match_default, make_array(0, 3, -2, -2));
   TEST_REGEX_SEARCH("(?>a(?i)b+)+c", perl, "aBbc", match_default, make_array(0, 4, -2, -2));
   TEST_REGEX_SEARCH("(?>a(?i)b+)+c", perl, "aBBc", match_default, make_array(0, 4, -2, -2));
   TEST_REGEX_SEARCH("(?>a(?i)b+)+c", perl, "Abc", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?>a(?i)b+)+c", perl, "abAb", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?>a(?i)b+)+c", perl, "abbC", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("(?=a(?i)b)\\w\\wc", perl, "abc", match_default, make_array(0, 3, -2, -2));
   TEST_REGEX_SEARCH("(?=a(?i)b)\\w\\wc", perl, "aBc", match_default, make_array(0, 3, -2, -2));
   TEST_REGEX_SEARCH("(?=a(?i)b)\\w\\wc", perl, "Ab", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?=a(?i)b)\\w\\wc", perl, "abC", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?=a(?i)b)\\w\\wc", perl, "aBC", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("(?<=a(?i)b)(\\w\\w)c", perl, "abxxc", match_default, make_array(2, 5, 2, 4, -2, -2));
   TEST_REGEX_SEARCH("(?<=a(?i)b)(\\w\\w)c", perl, "aBxxc", match_default, make_array(2, 5, 2, 4, -2, -2));
   TEST_REGEX_SEARCH("(?<=a(?i)b)(\\w\\w)c", perl, "Abxxc", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?<=a(?i)b)(\\w\\w)c", perl, "ABxxc", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?<=a(?i)b)(\\w\\w)c", perl, "abxxC", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("(?<=^.{4})(?:bar|cat)", perl, "fooocat", match_default, make_array(4, 7, -2, -2));
   TEST_REGEX_SEARCH("(?<=^.{4})(?:bar|cat)", perl, "foocat", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?<=^a{4})(?:bar|cat)", perl, "aaaacat", match_default, make_array(4, 7, -2, -2));
   TEST_REGEX_SEARCH("(?<=^a{4})(?:bar|cat)", perl, "aaacat", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?<=^[[:alpha:]]{4})(?:bar|cat)", perl, "aaaacat", match_default, make_array(4, 7, -2, -2));
   TEST_REGEX_SEARCH("(?<=^[[:alpha:]]{4})(?:bar|cat)", perl, "aaacat", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("(?<=ab(?i)x(?-i)y|(?i)z|b)ZZ", perl, "abxyZZ", match_default, make_array(4, 6, -2, -2));
   TEST_REGEX_SEARCH("(?<=ab(?i)x(?-i)y|(?i)z|b)ZZ", perl, "abXyZZ", match_default, make_array(4, 6, -2, -2));
   TEST_REGEX_SEARCH("(?:ab(?i)x(?-i)y|(?i)z|b)ZZ", perl, "ZZZ", match_default, make_array(0, 3, -2, -2));
   TEST_REGEX_SEARCH("(?:ab(?i)x(?-i)y|(?i)z|b)ZZ", perl, "zZZ", match_default, make_array(0, 3, -2, -2));
   TEST_REGEX_SEARCH("(?:ab(?i)x(?-i)y|(?i)z|b)ZZ", perl, "bZZ", match_default, make_array(0, 3, -2, -2));
   TEST_REGEX_SEARCH("(?:ab(?i)x(?-i)y|(?i)z|b)ZZ", perl, "BZZ", match_default, make_array(0, 3, -2, -2));
   TEST_REGEX_SEARCH("(?:ab(?i)x(?-i)y|(?i)z|b)ZZ", perl, "ZZ", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?:ab(?i)x(?-i)y|(?i)z|b)ZZ", perl, "abXYZZ", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?:ab(?i)x(?-i)y|(?i)z|b)ZZ", perl, "zzz", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?:ab(?i)x(?-i)y|(?i)z|b)ZZ", perl, "bzz", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("((?-i)[[:lower:]])[[:lower:]]", perl|icase, "ab", match_default, make_array(0, 2, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("((?-i)[[:lower:]])[[:lower:]]", perl|icase, "aB", match_default, make_array(0, 2, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("((?-i)[[:lower:]])[[:lower:]]", perl|icase, "Ab", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("((?-i)[[:lower:]])[[:lower:]]", perl|icase, "AB", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("a(?-i)b", perl|icase, "ab", match_default, make_array(0, 2, -2, -2));
   TEST_REGEX_SEARCH("a(?-i)b", perl|icase, "Ab", match_default, make_array(0, 2, -2, -2));
   TEST_REGEX_SEARCH("a(?-i)b", perl|icase, "aB", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("a(?-i)b", perl|icase, "AB", match_default, make_array(-2, -2));

   TEST_REGEX_SEARCH("(?:(?-i)a)b", perl|icase, "ab", match_default, make_array(0, 2, -2, -2));
   TEST_REGEX_SEARCH("((?-i)a)b", perl|icase, "ab", match_default, make_array(0, 2, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("(?:(?-i)a)b", perl|icase, "aB", match_default, make_array(0, 2, -2, -2));
   TEST_REGEX_SEARCH("((?-i)a)b", perl|icase, "aB", match_default, make_array(0, 2, 0, 1, -2, -2));

   TEST_REGEX_SEARCH("(?:(?-i)a)b", perl|icase, "Ab", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?:(?-i)a)b", perl|icase, "aB", match_default, make_array(0, 2, -2, -2));
   TEST_REGEX_SEARCH("((?-i)a)b", perl|icase, "aB", match_default, make_array(0, 2, 0, 1, -2, -2));

   TEST_REGEX_SEARCH("(?:(?-i)a)b", perl|icase, "Ab", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?:(?-i)a)b", perl|icase, "AB", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?-i:a)b", perl|icase, "ab", match_default, make_array(0, 2, -2, -2));
   TEST_REGEX_SEARCH("((?-i:a))b", perl|icase, "ab", match_default, make_array(0, 2, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("(?-i:a)b", perl|icase, "aB", match_default, make_array(0, 2, -2, -2));

   TEST_REGEX_SEARCH("((?-i:a))b", perl|icase, "aB", match_default, make_array(0, 2, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("(?-i:a)b", perl|icase, "AB", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?-i:a)b", perl|icase, "Ab", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?-i:a)b", perl|icase, "aB", match_default, make_array(0, 2, -2, -2));
   TEST_REGEX_SEARCH("((?-i:a))b", perl|icase, "aB", match_default, make_array(0, 2, 0, 1, -2, -2));
   TEST_REGEX_SEARCH("(?-i:a)b", perl|icase, "Ab", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?-i:a)b", perl|icase, "AB", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("((?-i:a.))b", perl|icase, "AB", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("((?-i:a.))b", perl|icase, "A\nB", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("((?s-i:a.))b", perl|icase, "a\nB", match_default, make_array(0, 3, 0, 2, -2, -2));

   TEST_REGEX_SEARCH(".", perl, "\n", match_default, make_array(0, 1, -2, -2));
   TEST_REGEX_SEARCH(".", perl, "\n", match_default|match_not_dot_newline, make_array(-2, -2));
   TEST_REGEX_SEARCH(".", perl|mod_s, "\n", match_default, make_array(0, 1, -2, -2));
   TEST_REGEX_SEARCH(".", perl|mod_s, "\n", match_default|match_not_dot_newline, make_array(0, 1, -2, -2));
   TEST_REGEX_SEARCH(".", perl|no_mod_s, "\n", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH(".", perl|no_mod_s, "\n", match_default|match_not_dot_newline, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?s).", perl, "\n", match_default, make_array(0, 1, -2, -2));
   TEST_REGEX_SEARCH("(?s).", perl, "\n", match_default|match_not_dot_newline, make_array(0, 1, -2, -2));
   TEST_REGEX_SEARCH("(?-s).", perl, "\n", match_default, make_array(-2, -2));
   TEST_REGEX_SEARCH("(?-s).", perl, "\n", match_default|match_not_dot_newline, make_array(-2, -2));
   test_options3();
}

void test_options3()
{
   using namespace boost::regex_constants;

   TEST_REGEX_SEARCH(".+", perl, "  \n  ", match_default, make_array(0, 5, -2, -2));
   TEST_REGEX_SEARCH(".+", perl, "  \n  ", match_default|match_not_dot_newline, make_array(0, 2, -2, 3, 5, -2, -2));
   TEST_REGEX_SEARCH(".+", perl|mod_s, "  \n  ", match_default, make_array(0, 5, -2, -2));
   TEST_REGEX_SEARCH(".+", perl|mod_s, "  \n  ", match_default|match_not_dot_newline, make_array(0, 5, -2, -2));
   TEST_REGEX_SEARCH(".+", perl|no_mod_s, "  \n  ", match_default, make_array(0, 2, -2, 3, 5, -2, -2));
   TEST_REGEX_SEARCH(".+", perl|no_mod_s, "  \n  ", match_default|match_not_dot_newline, make_array(0, 2, -2, 3, 5, -2, -2));
   TEST_REGEX_SEARCH("(?s).+", perl, "  \n  ", match_default, make_array(0, 5, -2, -2));
   TEST_REGEX_SEARCH("(?s).+", perl, "  \n  ", match_default|match_not_dot_newline, make_array(0, 5, -2, -2));
   TEST_REGEX_SEARCH("(?-s).+", perl, "  \n  ", match_default, make_array(0, 2, -2, 3, 5, -2, -2));
   TEST_REGEX_SEARCH("(?-s).+", perl, "  \n  ", match_default|match_not_dot_newline, make_array(0, 2, -2, 3, 5, -2, -2));

   const char* big_expression = 
"  (?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*                          # optional leading comment\n"
"(?:    (?:\n"
"[^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]+    # some number of atom characters...\n"
"(?![^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]) # ..not followed by something that could be part of an atom\n"
"|\n"
"\" (?:                      # opening quote...\n"
"[^\\\\\\x80-\\xff\\n\\015\"]                #   Anything except backslash and quote\n"
"|                     #    or\n"
"\\\\ [^\\x80-\\xff]           #   Escaped something (something != CR)\n"
")* \"  # closing quote\n"
")                    # initial word\n"
"(?:  (?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*  \\.  (?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*   (?:\n"
"[^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]+    # some number of atom characters...\n"
"(?![^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]) # ..not followed by something that could be part of an atom\n"
"|\n"
"\" (?:                      # opening quote...\n"
"[^\\\\\\x80-\\xff\\n\\015\"]                #   Anything except backslash and quote\n"
"|                     #    or\n"
"\\\\ [^\\x80-\\xff]           #   Escaped something (something != CR)\n"
")* \"  # closing quote\n"
")  )* # further okay, if led by a period\n"
"(?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*  @  (?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*    (?:\n"
"[^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]+    # some number of atom characters...\n"
"(?![^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]) # ..not followed by something that could be part of an atom\n"
"|   \\[                         # [\n"
"(?: [^\\\\\\x80-\\xff\\n\\015\\[\\]] |  \\\\ [^\\x80-\\xff]  )*    #    stuff\n"
"\\]                        #           ]\n"
")                           # initial subdomain\n"
"(?:                                  #\n"
"(?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*  \\.                        # if led by a period...\n"
"(?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*   (?:\n"
"[^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]+    # some number of atom characters...\n"
"(?![^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]) # ..not followed by something that could be part of an atom\n"
"|   \\[                         # [\n"
"(?: [^\\\\\\x80-\\xff\\n\\015\\[\\]] |  \\\\ [^\\x80-\\xff]  )*    #    stuff\n"
"\\]                        #           ]\n"
")                     #   ...further okay\n"
")*\n"
"# address\n"
"|                     #  or\n"
"(?:\n"
"[^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]+    # some number of atom characters...\n"
"(?![^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]) # ..not followed by something that could be part of an atom\n"
"|\n"
"\" (?:                      # opening quote...\n"
"[^\\\\\\x80-\\xff\\n\\015\"]                #   Anything except backslash and quote\n"
"|                     #    or\n"
"\\\\ [^\\x80-\\xff]           #   Escaped something (something != CR)\n"
")* \"  # closing quote\n"
")             # one word, optionally followed by....\n"
"(?:\n"
"[^()<>@,;:\".\\\\\\[\\]\\x80-\\xff\\000-\\010\\012-\\037]  |  # atom and space parts, or...\n"
"\\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)       |  # comments, or...\n"
"\" (?:                      # opening quote...\n"
"[^\\\\\\x80-\\xff\\n\\015\"]                #   Anything except backslash and quote\n"
"|                     #    or\n"
"\\\\ [^\\x80-\\xff]           #   Escaped something (something != CR)\n"
")* \"  # closing quote\n"
"# quoted strings\n"
")*\n"
"<  (?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*                     # leading <\n"
"(?:  @  (?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*    (?:\n"
"[^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]+    # some number of atom characters...\n"
"(?![^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]) # ..not followed by something that could be part of an atom\n"
"|   \\[                         # [\n"
"(?: [^\\\\\\x80-\\xff\\n\\015\\[\\]] |  \\\\ [^\\x80-\\xff]  )*    #    stuff\n"
"\\]                        #           ]\n"
")                           # initial subdomain\n"
"(?:                                  #\n"
"(?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*  \\.                        # if led by a period...\n"
"(?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*   (?:\n"
"[^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]+    # some number of atom characters...\n"
"(?![^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]) # ..not followed by something that could be part of an atom\n"
"|   \\[                         # [\n"
"(?: [^\\\\\\x80-\\xff\\n\\015\\[\\]] |  \\\\ [^\\x80-\\xff]  )*    #    stuff\n"
"\\]                        #           ]\n"
")                     #   ...further okay\n"
")*\n"
"(?:  (?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*  ,  (?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*  @  (?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*    (?:\n"
"[^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]+    # some number of atom characters...\n"
"(?![^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]) # ..not followed by something that could be part of an atom\n"
"|   \\[                         # [\n"
"(?: [^\\\\\\x80-\\xff\\n\\015\\[\\]] |  \\\\ [^\\x80-\\xff]  )*    #    stuff\n"
"\\]                        #           ]\n"
")                           # initial subdomain\n"
"(?:                                  #\n"
"(?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*  \\.                        # if led by a period...\n"
"(?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*   (?:\n"
"[^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]+    # some number of atom characters...\n"
"(?![^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]) # ..not followed by something that could be part of an atom\n"
"|   \\[                         # [\n"
"(?: [^\\\\\\x80-\\xff\\n\\015\\[\\]] |  \\\\ [^\\x80-\\xff]  )*    #    stuff\n"
"\\]                        #           ]\n"
")                     #   ...further okay\n"
")*\n"
")* # further okay, if led by comma\n"
":                                # closing colon\n"
"(?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*  )? #       optional route\n"
"(?:\n"
"[^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]+    # some number of atom characters...\n"
"(?![^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]) # ..not followed by something that could be part of an atom\n"
"|\n"
"\" (?:                      # opening quote...\n"
"[^\\\\\\x80-\\xff\\n\\015\"]                #   Anything except backslash and quote\n"
"|                     #    or\n"
"\\\\ [^\\x80-\\xff]           #   Escaped something (something != CR)\n"
")* \"  # closing quote\n"
")                    # initial word\n"
"(?:  (?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*  \\.  (?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*   (?:\n"
"[^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]+    # some number of atom characters...\n"
"(?![^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]) # ..not followed by something that could be part of an atom\n"
"|\n"
"\" (?:                      # opening quote...\n"
"[^\\\\\\x80-\\xff\\n\\015\"]                #   Anything except backslash and quote\n"
"|                     #    or\n"
"\\\\ [^\\x80-\\xff]           #   Escaped something (something != CR)\n"
")* \"  # closing quote\n"
")  )* # further okay, if led by a period\n"
"(?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*  @  (?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*    (?:\n"
"[^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]+    # some number of atom characters...\n"
"(?![^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]) # ..not followed by something that could be part of an atom\n"
"|   \\[                         # [\n"
"(?: [^\\\\\\x80-\\xff\\n\\015\\[\\]] |  \\\\ [^\\x80-\\xff]  )*    #    stuff\n"
"\\]                        #           ]\n"
")                           # initial subdomain\n"
"(?:                                  #\n"
"(?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*  \\.                        # if led by a period...\n"
"(?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*   (?:\n"
"[^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]+    # some number of atom characters...\n"
"(?![^(\\040)<>@,;:\".\\\\\\[\\]\\000-\\037\\x80-\\xff]) # ..not followed by something that could be part of an atom\n"
"|   \\[                         # [\n"
"(?: [^\\\\\\x80-\\xff\\n\\015\\[\\]] |  \\\\ [^\\x80-\\xff]  )*    #    stuff\n"
"\\]                        #           ]\n"
")                     #   ...further okay\n"
")*\n"
"#       address spec\n"
"(?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*  > #                  trailing >\n"
"# name and address\n"
")  (?: [\\040\\t] |  \\(\n"
"(?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  |  \\( (?:  [^\\\\\\x80-\\xff\\n\\015()]  |  \\\\ [^\\x80-\\xff]  )* \\)  )*\n"
"\\)  )*                       # optional trailing comment\n"
"\n";

   do{
      test_info<char>::set_info(__FILE__, __LINE__, 
         big_expression, 
         perl|mod_x, "Alan Other <user@dom.ain>", match_default, 
         make_array(0, 25, -2, -2));
      test(char(0), test_regex_search_tag());
   }while(0);
#if !defined(BOOST_NO_WREGEX) && !defined(BOOST_NO_TEMPLATED_ITERATOR_CONSTRUCTORS) && !BOOST_WORKAROUND(BOOST_MSVC, < 1300)
   do{
      std::string st(big_expression);
      test_info<wchar_t>::set_info(__FILE__, __LINE__, 
         std::wstring(st.begin(), st.end()), 
         perl|mod_x, L"Alan Other <user@dom.ain>", match_default, 
         make_array(0, 25, -2, -2));
      test(char(0), test_regex_search_tag());
   }while(0);
#endif
   do{
      test_info<char>::set_info(__FILE__, __LINE__, 
         big_expression, 
         perl|mod_x, "<user@dom.ain>", match_default, 
         make_array(1, 13, -2, -2));
      test(char(0), test_regex_search_tag());
   }while(0);
#if !defined(BOOST_NO_WREGEX) && !defined(BOOST_NO_TEMPLATED_ITERATOR_CONSTRUCTORS) && !BOOST_WORKAROUND(BOOST_MSVC, < 1300)
   do{
      std::string st(big_expression);
      test_info<wchar_t>::set_info(__FILE__, __LINE__, 
         std::wstring(st.begin(), st.end()), 
         perl|mod_x, L"<user@dom.ain>", match_default, 
         make_array(1, 13, -2, -2));
      test(char(0), test_regex_search_tag());
   }while(0);
#endif
   do{
      test_info<char>::set_info(__FILE__, __LINE__, 
         big_expression, 
         perl|mod_x, "\"A. Other\" <user.1234@dom.ain> (a comment)", match_default, 
         make_array(0, 42, -2, -2));
      test(char(0), test_regex_search_tag());
   }while(0);
#if !defined(BOOST_NO_WREGEX) && !defined(BOOST_NO_TEMPLATED_ITERATOR_CONSTRUCTORS) && !BOOST_WORKAROUND(BOOST_MSVC, < 1300)
   do{
      std::string st(big_expression);
      test_info<wchar_t>::set_info(__FILE__, __LINE__, 
         std::wstring(st.begin(), st.end()), 
         perl|mod_x, L"\"A. Other\" <user.1234@dom.ain> (a comment)", match_default, 
         make_array(0, 42, -2, -2));
      test(char(0), test_regex_search_tag());
   }while(0);
#endif
   do{
      test_info<char>::set_info(__FILE__, __LINE__, 
         big_expression, 
         perl|mod_x, "A. Other <user.1234@dom.ain> (a comment)", match_default, 
         make_array(2, 40, -2, -2));
      test(char(0), test_regex_search_tag());
   }while(0);
#if !defined(BOOST_NO_WREGEX) && !defined(BOOST_NO_TEMPLATED_ITERATOR_CONSTRUCTORS) && !BOOST_WORKAROUND(BOOST_MSVC, < 1300)
   do{
      std::string st(big_expression);
      test_info<wchar_t>::set_info(__FILE__, __LINE__, 
         std::wstring(st.begin(), st.end()), 
         perl|mod_x, L"A. Other <user.1234@dom.ain> (a comment)", match_default, 
         make_array(2, 40, -2, -2));
      test(char(0), test_regex_search_tag());
   }while(0);
#endif
   do{
      test_info<char>::set_info(__FILE__, __LINE__, 
         big_expression, 
         perl|mod_x, "\"/s=user/ou=host/o=place/prmd=uu.yy/admd= /c=gb/\"@x400-re.lay", match_default, 
         make_array(0, 61, -2, -2));
      test(char(0), test_regex_search_tag());
   }while(0);
#if !defined(BOOST_NO_WREGEX) && !defined(BOOST_NO_TEMPLATED_ITERATOR_CONSTRUCTORS) && !BOOST_WORKAROUND(BOOST_MSVC, < 1300)
   do{
      std::string st(big_expression);
      test_info<wchar_t>::set_info(__FILE__, __LINE__, 
         std::wstring(st.begin(), st.end()), 
         perl|mod_x, L"\"/s=user/ou=host/o=place/prmd=uu.yy/admd= /c=gb/\"@x400-re.lay", match_default, 
         make_array(0, 61, -2, -2));
      test(char(0), test_regex_search_tag());
   }while(0);
#endif
   do{
      test_info<char>::set_info(__FILE__, __LINE__, 
         big_expression, 
         perl|mod_x, "A missing angle <user@some.where", match_default, 
         make_array(17, 32, -2, -2));
      test(char(0), test_regex_search_tag());
   }while(0);
#if !defined(BOOST_NO_WREGEX) && !defined(BOOST_NO_TEMPLATED_ITERATOR_CONSTRUCTORS) && !BOOST_WORKAROUND(BOOST_MSVC, < 1300)
   do{
      std::string st(big_expression);
      test_info<wchar_t>::set_info(__FILE__, __LINE__, 
         std::wstring(st.begin(), st.end()), 
         perl|mod_x, L"A missing angle <user@some.where", match_default, 
         make_array(17, 32, -2, -2));
      test(char(0), test_regex_search_tag());
   }while(0);
#endif
}

