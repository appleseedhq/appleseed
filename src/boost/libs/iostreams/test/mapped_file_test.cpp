// (C) Copyright 2008 CodeRage, LLC (turkanis at coderage dot com)
// (C) Copyright 2004-2007 Jonathan Turkanis
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt.)

// See http://www.boost.org/libs/iostreams for documentation.

#include <fstream>
#include <boost/config.hpp>
#include <boost/detail/workaround.hpp>
#include <boost/iostreams/device/mapped_file.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test.hpp>
#include "detail/temp_file.hpp"
#include "detail/verification.hpp"

using namespace std;
using namespace boost;
using namespace boost::iostreams;
using namespace boost::iostreams::test;
using boost::unit_test::test_suite;

// Code generation bugs cause tests to fail with global optimization.
#if BOOST_WORKAROUND(BOOST_MSVC, < 1300)
# pragma optimize("g", off)
#endif

void mapped_file_test()
{
    BOOST_MESSAGE("about to begin");

    //--------------Reading from a mapped_file_source-------------------------//

    {
        // Note: the ifstream second is placed in a nested scope because 
        // closing and reopening a single ifstream failed for CW 9.4 on Windows.

        // Test reading from a stream based on a mapped_file_source,
        // in chars.
        test_file test1, test2;
        stream<mapped_file_source> first(test1.name());
        {
            ifstream second( test2.name().c_str(), 
                             BOOST_IOS::in | BOOST_IOS::binary );
            BOOST_CHECK_MESSAGE(
                compare_streams_in_chars(first, second),
                "failed reading from stream<mapped_file_source> in chars"
            );

            BOOST_MESSAGE(
                "done reading from stream<mapped_file_source> in chars"
            );
        }
        first.close();

        // Test reading from a stream based on a mapped_file_source,
        // in chunks. (Also tests reopening the stream.)
        first.open(mapped_file_source(test1.name()));
        {
            ifstream second( test2.name().c_str(), 
                             BOOST_IOS::in | BOOST_IOS::binary );
            BOOST_CHECK_MESSAGE(
                compare_streams_in_chunks(first, second),
                "failed reading from stream<mapped_file_source> in chunks"
            );

            BOOST_MESSAGE(
                "done reading from stream<mapped_file_source> in chunks"
            );
        }
    }

    //--------------Writing to a mapped_file_sink-----------------------------//

    {
        // Test writing to a stream based on a mapped_file_sink, in 
        // chars.
        uppercase_file  first, second; // Will overwrite these.
        test_file       test;

        stream<mapped_file_sink> out;
        out.open(mapped_file_sink(first.name()));
        write_data_in_chars(out);
        out.close();
        BOOST_CHECK_MESSAGE(
            compare_files(first.name(), test.name()),
            "failed writing to stream<mapped_file_sink> in chars"
        );

        BOOST_MESSAGE(
            "done writing to stream<mapped_file_source> in chars"
        );

        // Test writing to a stream based on a mapped_file_sink, in 
        // chunks. (Also tests reopening the stream.)
        out.open(mapped_file_sink(second.name()));
        write_data_in_chunks(out);
        out.close();
        BOOST_CHECK_MESSAGE(
            compare_files(second.name(), test.name()),
            "failed writing to stream<mapped_file_sink> in chunks"
        );

        BOOST_MESSAGE(
            "done writing to stream<mapped_file_source> in chunks"
        );
    }

    //--------------Writing to a newly created file-----------------------------//

    {
        // Test writing to a newly created mapped file.
        temp_file  first, second;
        test_file  test;

        mapped_file_params p(first.name());
        p.new_file_size = data_reps * data_length();
        stream<mapped_file_sink> out;
        out.open(mapped_file_sink(p));
        write_data_in_chars(out);
        out.close();
        BOOST_CHECK_MESSAGE(
            compare_files(first.name(), test.name()),
            "failed writing to newly created mapped file in chars"
        );

        
        // Test writing to a newly created mapped file. 
        // (Also tests reopening the stream.)
        p.path = second.name();
        out.open(mapped_file_sink(p));
        write_data_in_chunks(out);
        out.close();
        BOOST_CHECK_MESSAGE(
            compare_files(second.name(), test.name()),
            "failed writing to newly created mapped file in chunks"
        );
    }

    //--------------Random access with a mapped_file--------------------------//

    {
        // Test reading, writing and seeking within a stream based on a 
        // mapped_file, in chars.
        test_file test;
        stream<mapped_file> io;
        io.open(mapped_file(test.name()));
        BOOST_CHECK_MESSAGE(
            test_seekable_in_chars(io),
            "failed seeking within stream<mapped_file> in chars"
        );

        BOOST_MESSAGE(
            "done seeking within stream<mapped_file> in chars"
        );

        io.close();

        // Test reading, writing and seeking within a stream based on a 
        // mapped_file, in chunks. (Also tests reopening the 
        // stream.)
        io.open(mapped_file(test.name()));
        BOOST_CHECK_MESSAGE(
            test_seekable_in_chunks(io),
            "failed seeking within stream<mapped_file> in chunks"
        );

        BOOST_MESSAGE(
            "done seeking within stream<mapped_file> in chunks"
        );
    }
}

#if BOOST_WORKAROUND(BOOST_MSVC, < 1300)
# pragma optimize("", on)
#endif

test_suite* init_unit_test_suite(int, char* []) 
{
    test_suite* test = BOOST_TEST_SUITE("mapped_file test");
    test->add(BOOST_TEST_CASE(&mapped_file_test));
    return test;
}
