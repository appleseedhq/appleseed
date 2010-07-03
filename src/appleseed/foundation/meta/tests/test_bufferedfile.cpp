
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

// appleseed.foundation headers.
#include "foundation/platform/types.h"
#include "foundation/utility/bufferedfile.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <string>

FOUNDATION_TEST_SUITE(Foundation_Utility_BufferedFile)
{
    using namespace foundation;
    using namespace std;

    const char* Filename = "output/test_bufferedfile.tmp";
    const size_t BufferSize = 4;
    const string DataString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    FOUNDATION_TEST_CASE(InitialStateIsCorrect)
    {
        BufferedFile file;

        FOUNDATION_EXPECT_FALSE(file.is_open());
    }

    FOUNDATION_TEST_CASE(TestFileOpeningAtConstruction)
    {
        BufferedFile file(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::WriteMode,
            BufferSize);

        FOUNDATION_EXPECT_TRUE(file.is_open());
        FOUNDATION_EXPECT_EQ(0, file.tell());
    }

    FOUNDATION_TEST_CASE(TestFileOpeningWithOpenMethod)
    {
        BufferedFile file;
        const bool result =
            file.open(
                Filename,
                BufferedFile::BinaryType,
                BufferedFile::WriteMode,
                BufferSize);

        FOUNDATION_EXPECT_TRUE(result);
        FOUNDATION_EXPECT_TRUE(file.is_open());
        FOUNDATION_EXPECT_EQ(0, file.tell());
    }

    FOUNDATION_TEST_CASE(TestDoubleFileOpening)
    {
        BufferedFile file(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::WriteMode,
            BufferSize);

        const bool result =
            file.open(
                Filename,
                BufferedFile::BinaryType,
                BufferedFile::WriteMode,
                BufferSize);

        FOUNDATION_EXPECT_FALSE(result);
        FOUNDATION_EXPECT_TRUE(file.is_open());
        FOUNDATION_EXPECT_EQ(0, file.tell());
    }

    FOUNDATION_TEST_CASE(TestFileClosing)
    {
        BufferedFile file(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::WriteMode,
            BufferSize);

        file.close();

        FOUNDATION_EXPECT_FALSE(file.is_open());
    }

    FOUNDATION_TEST_CASE(TestFileOpeningThenClosingThenReopening)
    {
        BufferedFile file(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::WriteMode,
            BufferSize);

        file.close();

        const bool result =
            file.open(
                Filename,
                BufferedFile::BinaryType,
                BufferedFile::WriteMode,
                BufferSize);

        FOUNDATION_EXPECT_TRUE(result);
        FOUNDATION_EXPECT_TRUE(file.is_open());
        FOUNDATION_EXPECT_EQ(0, file.tell());
    }

    FOUNDATION_TEST_CASE(TestBufferedWritingAndReadingOfStringAtFileBeginning)
    {
        BufferedFile file(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::WriteMode,
            BufferSize);

        FOUNDATION_EXPECT_EQ(DataString.size(), file.write(DataString));
        FOUNDATION_EXPECT_EQ(DataString.size(), file.tell());

        file.close();

        file.open(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::ReadMode,
            BufferSize);

        char buf[100];
        FOUNDATION_EXPECT_EQ(DataString.size(), file.read(buf, DataString.size()));
        FOUNDATION_EXPECT_EQ(DataString, string(buf, DataString.size()));
        FOUNDATION_EXPECT_EQ(DataString.size(), file.tell());
    }

    FOUNDATION_TEST_CASE(TestBufferedWritingAndReadingOf32BitIntegerAtFileBeginning)
    {
        BufferedFile file(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::WriteMode,
            BufferSize);

        const uint32 Value = 0xDEADBEEFUL;
        FOUNDATION_EXPECT_EQ(4, file.write(Value));
        FOUNDATION_EXPECT_EQ(4, file.tell());
        
        file.close();

        file.open(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::ReadMode,
            BufferSize);

        uint32 value;
        FOUNDATION_EXPECT_EQ(4, file.read(value));
        FOUNDATION_EXPECT_EQ(Value, value);
        FOUNDATION_EXPECT_EQ(4, file.tell());
    }

    FOUNDATION_TEST_CASE(TestUnbufferedWritingAndReadingOfBytesAtFileBeginning)
    {
        BufferedFile file(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::WriteMode,
            BufferSize);

        FOUNDATION_EXPECT_EQ(DataString.size(), file.write_unbuf(DataString));
        FOUNDATION_EXPECT_EQ(DataString.size(), file.tell());

        file.close();

        file.open(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::ReadMode,
            BufferSize);

        char buf[100];
        FOUNDATION_EXPECT_EQ(DataString.size(), file.read(buf, DataString.size()));
        FOUNDATION_EXPECT_EQ(DataString, string(buf, DataString.size()));
        FOUNDATION_EXPECT_EQ(DataString.size(), file.tell());
    }

    struct FileReadingFixture
    {
        BufferedFile m_file;

        FileReadingFixture()
        {
            m_file.open(
                Filename,
                BufferedFile::BinaryType,
                BufferedFile::WriteMode,
                BufferSize);
            m_file.write(DataString);
            m_file.close();

            m_file.open(
                Filename,
                BufferedFile::BinaryType,
                BufferedFile::ReadMode,
                BufferSize);
        }
    };

    FOUNDATION_TEST_CASE_WITH_FIXTURE(TestSeekingForwardWhileReading, FileReadingFixture)
    {
        char buf[100];
        m_file.read(buf, 8);
        FOUNDATION_EXPECT_TRUE(m_file.seek(4, BufferedFile::SeekFromCurrent));
        FOUNDATION_EXPECT_EQ(8, m_file.read(buf, 8));
        FOUNDATION_EXPECT_EQ("MNOPQRST", string(buf, 8));
    }

    FOUNDATION_TEST_CASE_WITH_FIXTURE(TestSeekingBackwardWhileReading, FileReadingFixture)
    {
        char buf[100];
        m_file.read(buf, 8);
        FOUNDATION_EXPECT_TRUE(m_file.seek(-4, BufferedFile::SeekFromCurrent));
        FOUNDATION_EXPECT_EQ(8, m_file.read(buf, 8));
        FOUNDATION_EXPECT_EQ("EFGHIJKL", string(buf, 8));
    }

    FOUNDATION_TEST_CASE_WITH_FIXTURE(TestSeekingFromBeginningWhileReading, FileReadingFixture)
    {
        char buf[100];
        m_file.read(buf, 8);
        FOUNDATION_EXPECT_TRUE(m_file.seek(2, BufferedFile::SeekFromBeginning));
        FOUNDATION_EXPECT_EQ(8, m_file.read(buf, 8));
        FOUNDATION_EXPECT_EQ("CDEFGHIJ", string(buf, 8));
    }

    FOUNDATION_TEST_CASE_WITH_FIXTURE(TestSeekingFromEndWhileReading, FileReadingFixture)
    {
        char buf[100];
        m_file.read(buf, 8);
        FOUNDATION_EXPECT_TRUE(m_file.seek(-8, BufferedFile::SeekFromEnd));
        FOUNDATION_EXPECT_EQ(8, m_file.read(buf, 8));
        FOUNDATION_EXPECT_EQ("STUVWXYZ", string(buf, 8));
    }

    FOUNDATION_TEST_CASE(TestSeekingBackwardWhileWriting)
    {
        BufferedFile file(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::WriteMode,
            BufferSize);

        const uint32 Value1 = 0xDEADBEEFUL;
        file.write(Value1);

        FOUNDATION_EXPECT_TRUE(file.seek(-4, BufferedFile::SeekFromCurrent));

        const uint32 Value2 = 0xFADEBABEUL;
        file.write(Value2);
        file.close();

        file.open(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::ReadMode,
            BufferSize);

        uint32 value;
        FOUNDATION_EXPECT_EQ(4, file.read(value));
        FOUNDATION_EXPECT_EQ(Value2, value);
    }
}
