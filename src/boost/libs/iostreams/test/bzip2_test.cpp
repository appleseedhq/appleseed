// (C) Copyright 2008 CodeRage, LLC (turkanis at coderage dot com)
// (C) Copyright 2004-2007 Jonathan Turkanis
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt.)

// See http://www.boost.org/libs/iostreams for documentation.

#include <string>
#include <boost/iostreams/filter/bzip2.hpp>
#include <boost/iostreams/filter/test.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test.hpp>
#include "detail/sequence.hpp"

using namespace std;
using namespace boost;
using namespace boost::iostreams;
using namespace boost::iostreams::test;
using boost::unit_test::test_suite;  

struct bzip2_alloc : std::allocator<char> { };

void bzip2_test()
{
    text_sequence data;
    BOOST_CHECK(
        test_filter_pair( bzip2_compressor(), 
                          bzip2_decompressor(), 
                          std::string(data.begin(), data.end()) )
    );
    BOOST_CHECK(
        test_filter_pair( basic_bzip2_compressor<bzip2_alloc>(), 
                          basic_bzip2_decompressor<bzip2_alloc>(), 
                          std::string(data.begin(), data.end()) )
    );
}    

test_suite* init_unit_test_suite(int, char* []) 
{
    test_suite* test = BOOST_TEST_SUITE("bzip2 test");
    test->add(BOOST_TEST_CASE(&bzip2_test));
    return test;
}
