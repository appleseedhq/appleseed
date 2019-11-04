
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/utility/bufferedfile.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <cstdint>
#include <string>

using namespace foundation;

TEST_SUITE(Foundation_Utility_BufferedFile)
{
    const char* Filename = "unit tests/outputs/test_bufferedfile.tmp";
    const size_t BufferSize = 4;
    const std::string DataString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    TEST_CASE(InitialStateIsCorrect)
    {
        BufferedFile file;

        EXPECT_FALSE(file.is_open());
    }

    TEST_CASE(TestFileOpeningAtConstruction)
    {
        BufferedFile file(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::WriteMode,
            BufferSize);

        EXPECT_TRUE(file.is_open());
        EXPECT_EQ(0, file.tell());
    }

    TEST_CASE(TestFileOpeningWithOpenMethod)
    {
        BufferedFile file;
        const bool result =
            file.open(
                Filename,
                BufferedFile::BinaryType,
                BufferedFile::WriteMode,
                BufferSize);

        EXPECT_TRUE(result);
        EXPECT_TRUE(file.is_open());
        EXPECT_EQ(0, file.tell());
    }

    TEST_CASE(TestFileClosing)
    {
        BufferedFile file(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::WriteMode,
            BufferSize);

        file.close();

        EXPECT_FALSE(file.is_open());
    }

    TEST_CASE(TestFileOpeningThenClosingThenReopening)
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

        EXPECT_TRUE(result);
        EXPECT_TRUE(file.is_open());
        EXPECT_EQ(0, file.tell());
    }

    TEST_CASE(TestBufferedWritingAndReadingOfStringAtFileBeginning)
    {
        BufferedFile file(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::WriteMode,
            BufferSize);

        EXPECT_EQ(DataString.size(), file.write(DataString));
        EXPECT_EQ(DataString.size(), file.tell());

        file.close();

        file.open(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::ReadMode,
            BufferSize);

        char buf[100];
        EXPECT_EQ(DataString.size(), file.read(buf, DataString.size()));
        EXPECT_EQ(DataString, std::string(buf, DataString.size()));
        EXPECT_EQ(DataString.size(), file.tell());
    }

    TEST_CASE(TestBufferedWritingAndReadingOf32BitIntegerAtFileBeginning)
    {
        BufferedFile file(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::WriteMode,
            BufferSize);

        const std::uint32_t Value = 0xDEADBEEFu;
        EXPECT_EQ(4, file.write(Value));
        EXPECT_EQ(4, file.tell());

        file.close();

        file.open(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::ReadMode,
            BufferSize);

        std::uint32_t value;
        EXPECT_EQ(4, file.read(value));
        EXPECT_EQ(Value, value);
        EXPECT_EQ(4, file.tell());
    }

    TEST_CASE(TestUnbufferedWritingAndReadingOfBytesAtFileBeginning)
    {
        BufferedFile file(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::WriteMode,
            BufferSize);

        EXPECT_EQ(DataString.size(), file.write_unbuf(DataString));
        EXPECT_EQ(DataString.size(), file.tell());

        file.close();

        file.open(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::ReadMode,
            BufferSize);

        char buf[100];
        EXPECT_EQ(DataString.size(), file.read(buf, DataString.size()));
        EXPECT_EQ(DataString, std::string(buf, DataString.size()));
        EXPECT_EQ(DataString.size(), file.tell());
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

    TEST_CASE_F(TestSeekingForwardWhileReading, FileReadingFixture)
    {
        char buf[100];
        m_file.read(buf, 6);
        EXPECT_TRUE(m_file.seek(4, BufferedFile::SeekFromCurrent));
        EXPECT_EQ(8, m_file.read(buf, 8));
        EXPECT_EQ("KLMNOPQR", std::string(buf, 8));
    }

    TEST_CASE_F(TestSeekingBackwardWhileReading, FileReadingFixture)
    {
        char buf[100];
        m_file.read(buf, 6);
        EXPECT_TRUE(m_file.seek(-4, BufferedFile::SeekFromCurrent));
        EXPECT_EQ(8, m_file.read(buf, 8));
        EXPECT_EQ("CDEFGHIJ", std::string(buf, 8));
    }

    TEST_CASE_F(TestSeekingFromBeginningWhileReading, FileReadingFixture)
    {
        char buf[100];
        m_file.read(buf, 8);
        EXPECT_TRUE(m_file.seek(2, BufferedFile::SeekFromBeginning));
        EXPECT_EQ(8, m_file.read(buf, 8));
        EXPECT_EQ("CDEFGHIJ", std::string(buf, 8));
    }

    TEST_CASE_F(TestSeekingFromEndWhileReading, FileReadingFixture)
    {
        char buf[100];
        m_file.read(buf, 8);
        EXPECT_TRUE(m_file.seek(-8, BufferedFile::SeekFromEnd));
        EXPECT_EQ(8, m_file.read(buf, 8));
        EXPECT_EQ("STUVWXYZ", std::string(buf, 8));
    }

    TEST_CASE(TestSeekingBackwardInsideBufferWhileWriting)
    {
        BufferedFile file(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::WriteMode,
            BufferSize);

        const std::uint32_t Value1 = 0xDEADBEEFu;
        file.write(Value1);

        EXPECT_TRUE(file.seek(-4, BufferedFile::SeekFromCurrent));

        const std::uint32_t Value2 = 0xFADEBABEu;
        file.write(Value2);

        file.close();

        file.open(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::ReadMode,
            BufferSize);

        std::uint32_t value;

        EXPECT_EQ(4, file.read(value));
        EXPECT_EQ(Value2, value);
    }

    TEST_CASE(TestSeekingBackwardOutsideBufferWhileWriting)
    {
        BufferedFile file(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::WriteMode,
            BufferSize);

        const std::uint32_t Value1 = 0xDEADBEEFu;
        file.write(Value1);

        const std::uint32_t Value2 = 0xFADEBABEu;
        file.write(Value2);

        EXPECT_TRUE(file.seek(0, BufferedFile::SeekFromBeginning));

        const std::uint32_t Value3 = 0x12345678u;
        file.write(Value3);

        file.close();

        file.open(
            Filename,
            BufferedFile::BinaryType,
            BufferedFile::ReadMode,
            BufferSize);

        std::uint32_t value;

        EXPECT_EQ(4, file.read(value));
        EXPECT_EQ(Value3, value);

        EXPECT_EQ(4, file.read(value));
        EXPECT_EQ(Value2, value);
    }
}
