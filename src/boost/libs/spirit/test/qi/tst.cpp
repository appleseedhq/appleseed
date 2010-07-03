/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/home/qi/string/tst.hpp>
#include <boost/spirit/home/qi/string/tst_map.hpp>

#include <string>
#include <cctype>
#include <iostream>

namespace
{
    template <typename TST, typename Char>
    void add(TST& tst, Char const* s, int data)
    {
        Char const* last = s;
        while (*last)
            last++;
        tst.add(s, last, data);
    }

    template <typename TST, typename Char>
    void remove(TST& tst, Char const* s)
    {
        Char const* last = s;
        while (*last)
            last++;
        tst.remove(s, last);
    }

    template <typename TST, typename Char>
    void check(TST const& tst, Char const* s, bool expected, int N = 0, int val = -1)
    {
        Char const* first = s;
        Char const* last = s;
        while (*last)
            last++;
        int* r = tst.find(s, last);
        BOOST_TEST((r != 0) == expected);
        if (r != 0)
            BOOST_TEST((s-first) == N);
        if (r)
            BOOST_TEST(*r == val);
    }

    struct printer
    {
        template <typename String, typename Data>
        void operator()(String const& s, Data const& data)
        {
            std::cout << "    " << s << ": " << data << std::endl;
        }
    };

    template <typename TST>
    void print(TST const& tst)
    {
        std::cout << '[' << std::endl;
        tst.for_each(printer());
        std::cout << ']' << std::endl;
    }

    struct no_case_filter
    {
        template <typename Char>
        Char operator()(Char ch) const
        {
            return std::tolower(ch);
        }
    };

    template <typename TST, typename Char>
    void nc_check(TST const& tst, Char const* s, bool expected, int N = 0, int val = -1)
    {
        Char const* first = s;
        Char const* last = s;
        while (*last)
            last++;
        int* r = tst.find(s, last, no_case_filter());
        BOOST_TEST((r != 0) == expected);
        if (r != 0)
            BOOST_TEST((s-first) == N);
        if (r)
            BOOST_TEST(*r == val);
    }
}

template <typename Lookup, typename WideLookup>
void tests()
{
    { // basic tests
        Lookup lookup;

        check(lookup, "not-yet-there", false);
        check(lookup, "", false);

        add(lookup, "apple", 123);
        check(lookup, "apple", true, 5, 123); // full match
        check(lookup, "banana", false); // no-match
        check(lookup, "applexxx", true, 5, 123); // partial match

        add(lookup, "applepie", 456);
        check(lookup, "applepie", true, 8, 456); // full match
        check(lookup, "banana", false); // no-match
        check(lookup, "applepiexxx", true, 8, 456); // partial match
        check(lookup, "apple", true, 5, 123); // full match
        check(lookup, "applexxx", true, 5, 123); // partial match
    }

    { // variation of above
        Lookup lookup;

        add(lookup, "applepie", 456);
        add(lookup, "apple", 123);

        check(lookup, "applepie", true, 8, 456); // full match
        check(lookup, "banana", false); // no-match
        check(lookup, "applepiexxx", true, 8, 456); // partial match
        check(lookup, "apple", true, 5, 123); // full match
        check(lookup, "applexxx", true, 5, 123); // partial match
    }
    { // variation of above
        Lookup lookup;

        add(lookup, "applepie", 456);
        add(lookup, "apple", 123);

        check(lookup, "applepie", true, 8, 456); // full match
        check(lookup, "banana", false); // no-match
        check(lookup, "applepiexxx", true, 8, 456); // partial match
        check(lookup, "apple", true, 5, 123); // full match
        check(lookup, "applexxx", true, 5, 123); // partial match
    }

    { // narrow char tests
        Lookup lookup;
        add(lookup, "pineapple", 1);
        add(lookup, "orange", 2);
        add(lookup, "banana", 3);
        add(lookup, "applepie", 4);
        add(lookup, "apple", 5);

        check(lookup, "pineapple", true, 9, 1);
        check(lookup, "orange", true, 6, 2);
        check(lookup, "banana", true, 6, 3);
        check(lookup, "apple", true, 5, 5);
        check(lookup, "pizza", false);
        check(lookup, "steak", false);
        check(lookup, "applepie", true, 8, 4);
        check(lookup, "bananarama", true, 6, 3);
        check(lookup, "applet", true, 5, 5);
        check(lookup, "applepi", true, 5, 5);
        check(lookup, "appl", false);

        check(lookup, "pineapplez", true, 9, 1);
        check(lookup, "orangez", true, 6, 2);
        check(lookup, "bananaz", true, 6, 3);
        check(lookup, "applez", true, 5, 5);
        check(lookup, "pizzaz", false);
        check(lookup, "steakz", false);
        check(lookup, "applepiez", true, 8, 4);
        check(lookup, "bananaramaz", true, 6, 3);
        check(lookup, "appletz", true, 5, 5);
        check(lookup, "applepix", true, 5, 5);
    }

    { // wide char tests
        WideLookup lookup;
        add(lookup, L"pineapple", 1);
        add(lookup, L"orange", 2);
        add(lookup, L"banana", 3);
        add(lookup, L"applepie", 4);
        add(lookup, L"apple", 5);

        check(lookup, L"pineapple", true, 9, 1);
        check(lookup, L"orange", true, 6, 2);
        check(lookup, L"banana", true, 6, 3);
        check(lookup, L"apple", true, 5, 5);
        check(lookup, L"pizza", false);
        check(lookup, L"steak", false);
        check(lookup, L"applepie", true, 8, 4);
        check(lookup, L"bananarama", true, 6, 3);
        check(lookup, L"applet", true, 5, 5);
        check(lookup, L"applepi", true, 5, 5);
        check(lookup, L"appl", false);

        check(lookup, L"pineapplez", true, 9, 1);
        check(lookup, L"orangez", true, 6, 2);
        check(lookup, L"bananaz", true, 6, 3);
        check(lookup, L"applez", true, 5, 5);
        check(lookup, L"pizzaz", false);
        check(lookup, L"steakz", false);
        check(lookup, L"applepiez", true, 8, 4);
        check(lookup, L"bananaramaz", true, 6, 3);
        check(lookup, L"appletz", true, 5, 5);
        check(lookup, L"applepix", true, 5, 5);
    }

    { // test remove
        Lookup lookup;
        add(lookup, "pineapple", 1);
        add(lookup, "orange", 2);
        add(lookup, "banana", 3);
        add(lookup, "applepie", 4);
        add(lookup, "apple", 5);

        check(lookup, "pineapple", true, 9, 1);
        check(lookup, "orange", true, 6, 2);
        check(lookup, "banana", true, 6, 3);
        check(lookup, "apple", true, 5, 5);
        check(lookup, "applepie", true, 8, 4);
        check(lookup, "bananarama", true, 6, 3);
        check(lookup, "applet", true, 5, 5);
        check(lookup, "applepi", true, 5, 5);
        check(lookup, "appl", false);

        remove(lookup, "banana");
        check(lookup, "pineapple", true, 9, 1);
        check(lookup, "orange", true, 6, 2);
        check(lookup, "banana", false);
        check(lookup, "apple", true, 5, 5);
        check(lookup, "applepie", true, 8, 4);
        check(lookup, "bananarama", false);
        check(lookup, "applet", true, 5, 5);
        check(lookup, "applepi", true, 5, 5);
        check(lookup, "appl", false);

        remove(lookup, "apple");
        check(lookup, "pineapple", true, 9, 1);
        check(lookup, "orange", true, 6, 2);
        check(lookup, "apple", false);
        check(lookup, "applepie", true, 8, 4);
        check(lookup, "applet", false);
        check(lookup, "applepi", false);
        check(lookup, "appl", false);

        remove(lookup, "orange");
        check(lookup, "pineapple", true, 9, 1);
        check(lookup, "orange", false);
        check(lookup, "applepie", true, 8, 4);

        remove(lookup, "pineapple");
        check(lookup, "pineapple", false);
        check(lookup, "orange", false);
        check(lookup, "applepie", true, 8, 4);

        remove(lookup, "applepie");
        check(lookup, "applepie", false);
    }

    { // copy/assign/clear test
        Lookup lookupa;
        add(lookupa, "pineapple", 1);
        add(lookupa, "orange", 2);
        add(lookupa, "banana", 3);
        add(lookupa, "applepie", 4);
        add(lookupa, "apple", 5);

        Lookup lookupb(lookupa); // copy ctor
        check(lookupb, "pineapple", true, 9, 1);
        check(lookupb, "orange", true, 6, 2);
        check(lookupb, "banana", true, 6, 3);
        check(lookupb, "apple", true, 5, 5);
        check(lookupb, "pizza", false);
        check(lookupb, "steak", false);
        check(lookupb, "applepie", true, 8, 4);
        check(lookupb, "bananarama", true, 6, 3);
        check(lookupb, "applet", true, 5, 5);
        check(lookupb, "applepi", true, 5, 5);
        check(lookupb, "appl", false);

        lookupb.clear(); // clear
        check(lookupb, "pineapple", false);
        check(lookupb, "orange", false);
        check(lookupb, "banana", false);
        check(lookupb, "apple", false);
        check(lookupb, "applepie", false);
        check(lookupb, "bananarama", false);
        check(lookupb, "applet", false);
        check(lookupb, "applepi", false);
        check(lookupb, "appl", false);

        lookupb = lookupa; // assign
        check(lookupb, "pineapple", true, 9, 1);
        check(lookupb, "orange", true, 6, 2);
        check(lookupb, "banana", true, 6, 3);
        check(lookupb, "apple", true, 5, 5);
        check(lookupb, "pizza", false);
        check(lookupb, "steak", false);
        check(lookupb, "applepie", true, 8, 4);
        check(lookupb, "bananarama", true, 6, 3);
        check(lookupb, "applet", true, 5, 5);
        check(lookupb, "applepi", true, 5, 5);
        check(lookupb, "appl", false);
    }

    { // test for_each
        Lookup lookup;
        add(lookup, "pineapple", 1);
        add(lookup, "orange", 2);
        add(lookup, "banana", 3);
        add(lookup, "applepie", 4);
        add(lookup, "apple", 5);

        print(lookup);
    }

    { // case insensitive tests
        Lookup lookup;

        // NOTE: make sure all entries are in lower-case!!!
        add(lookup, "pineapple", 1);
        add(lookup, "orange", 2);
        add(lookup, "banana", 3);
        add(lookup, "applepie", 4);
        add(lookup, "apple", 5);

        nc_check(lookup, "pineapple", true, 9, 1);
        nc_check(lookup, "orange", true, 6, 2);
        nc_check(lookup, "banana", true, 6, 3);
        nc_check(lookup, "apple", true, 5, 5);
        nc_check(lookup, "applepie", true, 8, 4);

        nc_check(lookup, "PINEAPPLE", true, 9, 1);
        nc_check(lookup, "ORANGE", true, 6, 2);
        nc_check(lookup, "BANANA", true, 6, 3);
        nc_check(lookup, "APPLE", true, 5, 5);
        nc_check(lookup, "APPLEPIE", true, 8, 4);

        nc_check(lookup, "pineApple", true, 9, 1);
        nc_check(lookup, "orangE", true, 6, 2);
        nc_check(lookup, "Banana", true, 6, 3);
        nc_check(lookup, "aPPLe", true, 5, 5);
        nc_check(lookup, "ApplePie", true, 8, 4);

        print(lookup);
    }
}

int
main()
{
    using namespace boost::spirit::qi;

    tests<tst<char, int>, tst<wchar_t, int> >();
    tests<tst_map<char, int>, tst_map<wchar_t, int> >();

    return boost::report_errors();
}

