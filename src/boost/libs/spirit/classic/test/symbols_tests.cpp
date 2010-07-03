/*=============================================================================
    Copyright (c) 1998-2003 Joel de Guzman
    Copyright (c)      2003 Martin Wille
    http://spirit.sourceforge.net/

    Use, modification and distribution is subject to the Boost Software
    License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <iostream>
#include <string>
#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/classic_core.hpp>
#include <boost/spirit/include/classic_symbols.hpp>
#include <boost/detail/lightweight_test.hpp>

///////////////////////////////////////////////////////////////////////////////
using namespace std;
using namespace BOOST_SPIRIT_CLASSIC_NS;

///////////////////////////////////////////////////////////////////////////////

template <typename IteratorT>
bool
equal(IteratorT p, IteratorT q)
{
    while (*p && *p == *q)
    {
        ++p;
        ++q;
    }
    return *p == *q;
}

template <class SymbolsT, typename CharT>
void
check
(
    SymbolsT const &sym,
    CharT    const *candidate,
    bool           hit,
    CharT    const *result,
    int            length
)
{
    parse_info<CharT const*> info = parse(candidate, sym);

#define correctly_matched hit == info.hit
#define correct_match_length unsigned(length) == info.length
#define correct_tail equal(candidate + (hit?1:0)*length, result)

    BOOST_TEST(correctly_matched);

    if (hit)
    {
        BOOST_TEST(correct_match_length);
        BOOST_TEST(correct_tail);
    }
    else
    {
        BOOST_TEST(correct_tail);
    }
}

template <typename T>
struct store_action
{
    store_action(T const &v) : value(v) {}
    void operator()(T &v) const { v = value; }
private:
    T const value;
};

template <typename T>
store_action<T>
store(T const &v)
{
    return v;
}

template <typename T>
struct check_action
{
    check_action(T const &v) : value(v) {}

#define correct_value_stored (v==value)
    void operator()(T const &v) const { BOOST_TEST(correct_value_stored); }
private:
    T const value;
};

template <typename T>
check_action<T>
check(T const &v)
{
    return v;
}


static void
default_constructible()
{   // this actually a compile time test
    symbols<>                     ns1;
    symbols<int, wchar_t>         ws1;
    symbols<std::string, char>    ns2;
    symbols<std::string, wchar_t> ws2;

    (void)ns1; (void)ws1; (void)ns2; (void)ws2;
}

static void
narrow_match_tests()
{
    symbols<>   sym;
    sym = "pineapple", "orange", "banana", "applepie", "apple";

    check(sym, "pineapple", true, "", 9);
    check(sym, "orange", true, "", 6);
    check(sym, "banana", true, "", 6);
    check(sym, "apple", true, "", 5);
    check(sym, "pizza", false, "pizza", -1);
    check(sym, "steak", false, "steak", -1);
    check(sym, "applepie", true, "", 8);
    check(sym, "bananarama", true, "rama", 6);
    check(sym, "applet", true, "t", 5);
    check(sym, "applepi", true, "pi", 5);
    check(sym, "appl", false, "appl", -1);

    check(sym, "pineapplez", true, "z", 9);
    check(sym, "orangez", true, "z", 6);
    check(sym, "bananaz", true, "z", 6);
    check(sym, "applez", true, "z", 5);
    check(sym, "pizzaz", false, "pizzaz", -1);
    check(sym, "steakz", false, "steakz", -1);
    check(sym, "applepiez", true, "z", 8);
    check(sym, "bananaramaz", true, "ramaz", 6);
    check(sym, "appletz", true, "tz", 5);
    check(sym, "applepix", true, "pix", 5);
}

static void
narrow_copy_ctor_tests()
{
    symbols<>   sym;
    sym = "pineapple", "orange", "banana", "applepie", "apple";

    symbols<>   sym2(sym);
    check(sym2, "pineapple", true, "", 9);
    check(sym2, "pizza", false, "pizza", -1);
    check(sym2, "bananarama", true, "rama", 6);
}

static void
narrow_assigment_operator_tests()
{
    symbols<>   sym;
    sym = "pineapple", "orange", "banana", "applepie", "apple";

    symbols<>   sym2;
    sym2 = sym;

    check(sym2, "pineapple", true, "", 9);
    check(sym2, "pizza", false, "pizza", -1);
    check(sym2, "bananarama", true, "rama", 6);
}

static void
narrow_value_tests()
{   // also tests the add member functions
    symbols<>   sym;

    sym = "orange", "banana";
    sym.add("pineapple",1234);
    sym.add("lemon");

    parse("orange", sym[store(12345)]);
    parse("orange", sym[check(12345)]);
    parse("pineapple", sym[check(1234)]);
    parse("banana", sym[check(int())]);
    parse("lemon", sym[check(int())]);
}

static void
narrow_free_functions_tests()
{
    symbols<>   sym;

#define add_returned_non_null_value (res!=0)
#define add_returned_null (res==0)
#define find_returned_non_null_value (res!=0)
#define find_returned_null (res==0)

    int *res = add(sym,"pineapple");
    BOOST_TEST(add_returned_non_null_value);
    res = add(sym,"pineapple");
    BOOST_TEST(add_returned_null);

    res = find(sym, "pineapple");
    BOOST_TEST(find_returned_non_null_value);
    res = find(sym, "banana");
    BOOST_TEST(find_returned_null);
}

static void
wide_match_tests()
{
    symbols<int, wchar_t>   sym;
    sym = L"pineapple", L"orange", L"banana", L"applepie", L"apple";

    check(sym, L"pineapple", true, L"", 9);
    check(sym, L"orange", true, L"", 6);
    check(sym, L"banana", true, L"", 6);
    check(sym, L"apple", true, L"", 5);
    check(sym, L"pizza", false, L"pizza", -1);
    check(sym, L"steak", false, L"steak", -1);
    check(sym, L"applepie", true, L"", 8);
    check(sym, L"bananarama", true, L"rama", 6);
    check(sym, L"applet", true, L"t", 5);
    check(sym, L"applepi", true, L"pi", 5);
    check(sym, L"appl", false, L"appl", -1);

    check(sym, L"pineapplez", true, L"z", 9);
    check(sym, L"orangez", true, L"z", 6);
    check(sym, L"bananaz", true, L"z", 6);
    check(sym, L"applez", true, L"z", 5);
    check(sym, L"pizzaz", false, L"pizzaz", -1);
    check(sym, L"steakz", false, L"steakz", -1);
    check(sym, L"applepiez", true, L"z", 8);
    check(sym, L"bananaramaz", true, L"ramaz", 6);
    check(sym, L"appletz", true, L"tz", 5);
    check(sym, L"applepix", true, L"pix", 5);
}

static void
wide_copy_ctor_tests()
{
    symbols<int, wchar_t>   sym;
    sym = L"pineapple", L"orange", L"banana", L"applepie", L"apple";

    symbols<int, wchar_t>   sym2(sym);
    check(sym2, L"pineapple", true, L"", 9);
    check(sym2, L"pizza", false, L"pizza", -1);
    check(sym2, L"bananarama", true, L"rama", 6);
}

static void
wide_assigment_operator_tests()
{
    symbols<int, wchar_t>   sym;
    sym = L"pineapple", L"orange", L"banana", L"applepie", L"apple";

    symbols<int, wchar_t>   sym2;
    sym2 = sym;

    check(sym2, L"pineapple", true, L"", 9);
    check(sym2, L"pizza", false, L"pizza", -1);
    check(sym2, L"bananarama", true, L"rama", 6);
}

static void
wide_value_tests()
{   // also tests the add member functions
    symbols<int, wchar_t>   sym;

    sym = L"orange", L"banana";
    sym.add(L"pineapple",1234);
    sym.add(L"lemon");

    parse(L"orange", sym[store(12345)]);
    parse(L"orange", sym[check(12345)]);
    parse(L"pineapple", sym[check(1234)]);
    parse(L"banana", sym[check(int())]);
    parse(L"lemon", sym[check(int())]);
}

static void
wide_free_functions_tests()
{
    symbols<int, wchar_t>   sym;

    int *res = add(sym,L"pineapple");
    BOOST_TEST(add_returned_non_null_value);
    res = add(sym,L"pineapple");
    BOOST_TEST(add_returned_null);

    res = find(sym, L"pineapple");
    BOOST_TEST(find_returned_non_null_value);
    res = find(sym, L"banana");
    BOOST_TEST(find_returned_null);
}

static 
void free_add_find_functions_tests()
{
    symbols<> sym;
    BOOST_TEST(*add(sym, "a", 0) == 0);
    BOOST_TEST(*add(sym, "a2", 1) == 1);
    BOOST_TEST(find(sym, "a2"));
    BOOST_TEST(find(sym, "a"));
}

int
main()
{
    default_constructible();
    narrow_match_tests();
    narrow_copy_ctor_tests();
    narrow_assigment_operator_tests();
    narrow_value_tests();
    narrow_free_functions_tests();
    wide_match_tests();
    wide_copy_ctor_tests();
    wide_assigment_operator_tests();
    wide_value_tests();
    wide_free_functions_tests();
    free_add_find_functions_tests();
    return boost::report_errors();
}
