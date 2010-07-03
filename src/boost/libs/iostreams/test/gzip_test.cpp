// (C) Copyright 2008 CodeRage, LLC (turkanis at coderage dot com)
// (C) Copyright 2004-2007 Jonathan Turkanis
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt.)

// See http://www.boost.org/libs/iostreams for documentation.

#include <string>
#include <boost/iostreams/filter/gzip.hpp>
#include <boost/iostreams/filter/test.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test.hpp>
#include "detail/sequence.hpp"
#include "detail/verification.hpp"

using namespace boost;
using namespace boost::iostreams;
using namespace boost::iostreams::test;
using boost::unit_test::test_suite;     

struct gzip_alloc : std::allocator<char> { };

void gzip_test()
{
    text_sequence data;
    BOOST_CHECK(
        test_filter_pair( gzip_compressor(), 
                          gzip_decompressor(), 
                          std::string(data.begin(), data.end()) )
    );
    BOOST_CHECK(
        test_filter_pair( basic_gzip_compressor<gzip_alloc>(), 
                          basic_gzip_decompressor<gzip_alloc>(), 
                          std::string(data.begin(), data.end()) )
    );
}

test_suite* init_unit_test_suite(int, char* []) 
{
    test_suite* test = BOOST_TEST_SUITE("gzip test");
    test->add(BOOST_TEST_CASE(&gzip_test));
    return test;
}
