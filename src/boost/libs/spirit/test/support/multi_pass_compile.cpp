/*=============================================================================
    Copyright (c) 2004-2008 Hartmut Kaiser

    Distributed under the Boost Software License, Version 1.0. (See accompanying 
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

//  This is a compile only test for verifying, whether the multi_pass<>
//  iterator works ok with an input iterator, which returns a value_type and not
//  a reference from its dereferencing operator.

#include <cstdio>
#include <fstream>
#include <iterator>
#include <boost/detail/lightweight_test.hpp>

#include <boost/spirit/include/qi_char.hpp>
#include <boost/spirit/include/qi_numeric.hpp>
#include <boost/spirit/include/qi_operator.hpp>
#include <boost/spirit/include/qi_nonterminal.hpp>
#include <boost/spirit/include/qi_parse.hpp>
#include <boost/spirit/include/support_multi_pass.hpp>

#if defined(BOOST_HAS_UNISTD_H)
#include <unistd.h>    // unlink()
#endif

#if defined(__MINGW32__)
#include <io.h>    // unlink()
#endif

using namespace boost::spirit;
using namespace boost::spirit::qi;
using namespace std;

int main ()
{
    // create a sample file
    {
        ofstream out("./input_file.txt");
        out << 1.0 << "," << 2.0;
    }

    // read in the values from the sample file
    {
        ifstream in("./input_file.txt"); // we get our input from this file

        typedef multi_pass<istreambuf_iterator<char> > iterator_type;

        iterator_type first(make_multi_pass(istreambuf_iterator<char>(in)));
        iterator_type last(make_multi_pass(istreambuf_iterator<char>()));

        rule<iterator_type> n_list; 
        n_list = double_ >> *(char_(',') >> double_);
        BOOST_TEST(parse(first, last, n_list));
    }

#if !defined(__COMO_VERSION__)
    unlink("./input_file.txt");
#endif

    return boost::report_errors();
}
