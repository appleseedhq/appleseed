
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "foundation/math/rng.h"
#include "foundation/platform/types.h"
#include "foundation/utility/containers/anyarray.h"
#include "foundation/utility/numerictype.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <limits>
#include <string>

#if 0

using namespace std;

namespace foundation
{

//
// foundation::AnyArray unit test.
//

FOUNDATION_IMPLEMENT_TEST(UnitTestAnyArray, UnitTest, "foundation::AnyArray")
{
    // Check AnyArray::empty() and AnyArray::size() on an empty array.
    {
        AnyArray array;
        FOUNDATION_CHECK(array.empty());
        FOUNDATION_CHECK_EQ(array.size(), 0);
    }

    // Check AnyArray::insert() for all supported types.
    {
        AnyArray array;
        try
        {
            array.insert<int8>  ("int8",    numeric_limits<int8>::min());
            array.insert<int16> ("int16",   numeric_limits<int16>::min());
            array.insert<int32> ("int32",   numeric_limits<int32>::min());
            array.insert<int64> ("int64",   numeric_limits<int64>::min());
            array.insert<uint8> ("uint8",   numeric_limits<uint8>::max());
            array.insert<uint16>("uint16",  numeric_limits<uint16>::max());
            array.insert<uint32>("uint32",  numeric_limits<uint32>::max());
            array.insert<uint64>("uint64",  numeric_limits<uint64>::max());
            array.insert<float> ("float",   numeric_limits<float>::min());
            array.insert<double>("double",  numeric_limits<double>::min());
        }
        catch (...) { FOUNDATION_CHECK(false); }
        FOUNDATION_CHECK(!array.empty());
        FOUNDATION_CHECK_EQ(array.size(), 10);
    }

    // Attempt to duplicate an element.
    {
        AnyArray array;
        array.insert<int32>("element", 42);
        bool duplicate = false;
        try
        {
            array.insert<int32>("element", 100);
        }
        catch (const AnyArray::ExceptionDuplicateName&) { duplicate = true; }
        catch (...) { FOUNDATION_CHECK(false); }
        FOUNDATION_CHECK(duplicate);
    }

    // Attempt to access a non-existing element.
    {
        AnyArray array;
        array.insert<int32>("element", 42);
        bool missing = false;
        try
        {
            array.at<uint8>("non-existing element");
        }
        catch (const AnyArray::ExceptionUnknownName&)   { missing = true; }
        catch (...) { FOUNDATION_CHECK(false); }
        FOUNDATION_CHECK(missing);
    }

    // Attempt to access an element with the wrong type.
    {
        AnyArray array;
        array.insert<int32>("element", 42);
        bool mismatch = false;
        try
        {
            array.at<float>("element");
        }
        catch (const AnyArray::ExceptionTypeMismatch&)  { mismatch = true; }
        catch (...) { FOUNDATION_CHECK(false); }
        FOUNDATION_CHECK(mismatch);
    }

    // Alter and restore the value of an element.
    {
        AnyArray array;
        array.insert<int32>("element", 42);
        try
        {
            const int32 old_value = array.at<int32>("element");
            array.at<int32>("element") = 100;
            FOUNDATION_CHECK_FEQ(array.at<int32>("element"), 100);
            array.at<int32>("element") = old_value;
            FOUNDATION_CHECK_FEQ(array.at<int32>("element"), old_value);
        }
        catch (...) { FOUNDATION_CHECK(false); }
    }

    // Check AnyArray::clear().
    {
        AnyArray array;
        array.insert<int32>("element", 42);

        FOUNDATION_CHECK(!array.empty());
        FOUNDATION_CHECK_EQ(array.size(), 1);

        array.clear();

        FOUNDATION_CHECK(array.empty());
        FOUNDATION_CHECK_EQ(array.size(), 0);
    }

    // Check AnyArray::begin(), AnyArray::end() and AnyArray::ConstIterator::operator==
    // when the array is empty.
    {
        AnyArray array;

        const AnyArray::ConstIterator b = array.begin();
        const AnyArray::ConstIterator e = array.end();

        FOUNDATION_CHECK(b == b);
        FOUNDATION_CHECK(e == e);
        FOUNDATION_CHECK(b == e);
    }

    // Check AnyArray::begin(), AnyArray::end(), AnyArray::ConstIterator::operator==
    // and AnyArray::ConstIterator::operator!= when the array is non-empty.
    {
        AnyArray array;
        array.insert<int32>("element", 42);

        const AnyArray::ConstIterator b = array.begin();
        const AnyArray::ConstIterator e = array.end();

        FOUNDATION_CHECK(b == b);
        FOUNDATION_CHECK(e == e);
        FOUNDATION_CHECK(b != e);
    }

    // Check AnyArray::find().
    {
        AnyArray array;
        array.insert<int32>("element", 42);

        try
        {
            const AnyArray::ConstIterator i = array.find("element");
            FOUNDATION_CHECK(i == array.begin());
            FOUNDATION_CHECK(i != array.end());

            const AnyArray::ConstIterator j = array.find("non-existing element");
            FOUNDATION_CHECK(j != array.begin());
            FOUNDATION_CHECK(j == array.end());
        }
        catch (...) { FOUNDATION_CHECK(false); }
    }

    // Check AnyArray construction by copy, assignment and iteration.
    {
        AnyArray array;
        array.insert<int8>  ("int8",    numeric_limits<int8>::min());
        array.insert<int16> ("int16",   numeric_limits<int16>::min());
        array.insert<int32> ("int32",   numeric_limits<int32>::min());
        array.insert<int64> ("int64",   numeric_limits<int64>::min());
        array.insert<uint8> ("uint8",   numeric_limits<uint8>::max());
        array.insert<uint16>("uint16",  numeric_limits<uint16>::max());
        array.insert<uint32>("uint32",  numeric_limits<uint32>::max());
        array.insert<uint64>("uint64",  numeric_limits<uint64>::max());
        array.insert<float> ("float",   numeric_limits<float>::min());
        array.insert<double>("double",  numeric_limits<double>::min());

        // Make a copy of the array using copy constructor.
        const AnyArray copy1(array);

        // Make a copy of the array using assignment operator.
        AnyArray copy2;
        copy2 = array;

        // Compare the contents of array, copy1 and copy2.
        FOUNDATION_CHECK_EQ(copy1.size(), array.size());
        FOUNDATION_CHECK_EQ(copy2.size(), array.size());
        try
        {
            size_t iterations = 0;
            for (AnyArray::ConstIterator it = array.begin(), e = array.end(); it != e; ++it)
            {
                ++iterations;

                // Find this element in copy1.
                const AnyArray::ConstIterator copy1_it = copy1.find(it.name());
                FOUNDATION_CHECK_EQ(copy1_it.name(), it.name());
                FOUNDATION_CHECK_EQ(copy1_it.type(), it.type());

                // Find this element in copy2.
                const AnyArray::ConstIterator copy2_it = copy2.find(it.name());
                FOUNDATION_CHECK_EQ(copy2_it.name(), it.name());
                FOUNDATION_CHECK_EQ(copy2_it.type(), it.type());

                // Check that all three elements match.
                switch (it.type())
                {
                  case NumericTypeInt8:
                    FOUNDATION_CHECK_EQ(it.value<int8>(), copy1_it.value<int8>());
                    FOUNDATION_CHECK_EQ(it.value<int8>(), copy2_it.value<int8>());
                    break;
                  case NumericTypeInt16:
                    FOUNDATION_CHECK_EQ(it.value<int16>(), copy1_it.value<int16>());
                    FOUNDATION_CHECK_EQ(it.value<int16>(), copy2_it.value<int16>());
                    break;
                  case NumericTypeInt32:
                    FOUNDATION_CHECK_EQ(it.value<int32>(), copy1_it.value<int32>());
                    FOUNDATION_CHECK_EQ(it.value<int32>(), copy2_it.value<int32>());
                    break;
                  case NumericTypeInt64:
                    FOUNDATION_CHECK_EQ(it.value<int64>(), copy1_it.value<int64>());
                    FOUNDATION_CHECK_EQ(it.value<int64>(), copy2_it.value<int64>());
                    break;
                  case NumericTypeUInt8:
                    FOUNDATION_CHECK_EQ(it.value<uint8>(), copy1_it.value<uint8>());
                    FOUNDATION_CHECK_EQ(it.value<uint8>(), copy2_it.value<uint8>());
                    break;
                  case NumericTypeUInt16:
                    FOUNDATION_CHECK_EQ(it.value<uint16>(), copy1_it.value<uint16>());
                    FOUNDATION_CHECK_EQ(it.value<uint16>(), copy2_it.value<uint16>());
                    break;
                  case NumericTypeUInt32:
                    FOUNDATION_CHECK_EQ(it.value<uint32>(), copy1_it.value<uint32>());
                    FOUNDATION_CHECK_EQ(it.value<uint32>(), copy2_it.value<uint32>());
                    break;
                  case NumericTypeUInt64:
                    FOUNDATION_CHECK_EQ(it.value<uint64>(), copy1_it.value<uint64>());
                    FOUNDATION_CHECK_EQ(it.value<uint64>(), copy2_it.value<uint64>());
                    break;
                  case NumericTypeFloat:
                    FOUNDATION_CHECK_EQ(it.value<float>(), copy1_it.value<float>());        // exact comparison
                    FOUNDATION_CHECK_EQ(it.value<float>(), copy2_it.value<float>());        // exact comparison
                    break;
                  case NumericTypeDouble:
                    FOUNDATION_CHECK_EQ(it.value<double>(), copy1_it.value<double>());      // exact comparison
                    FOUNDATION_CHECK_EQ(it.value<double>(), copy2_it.value<double>());      // exact comparison
                    break;
                  default:
                    FOUNDATION_CHECK(false);
                }
            }
            FOUNDATION_CHECK_EQ(iterations, array.size());
        }
        catch (...) { FOUNDATION_CHECK(false); }
    }

    // Insert many elements of all types, in random order, and check consistency.
    {
        AnyArray array;
        const size_t N = 1000;
        try
        {
            MersenneTwister rng;
            for (size_t i = 0; i < N; ++i)
            {
                const string prefix = to_string(i) + "=";
                switch (rand_int1(rng, 0, 9))
                {
                  case 0:
                    {
                        const int8 val =
                            static_cast<int8>(rand_int1(rng, numeric_limits<int8>::min(), numeric_limits<int8>::max()));
                        array.insert<int8>(prefix + to_string(val), val);
                        break;
                    }
                  case 1:
                    {
                        const int16 val =
                            static_cast<int16>(rand_int1(rng, numeric_limits<int16>::min(), numeric_limits<int16>::max()));
                        array.insert<int16>(prefix + to_string(val), val);
                        break;
                    }
                  case 2:
                    {
                        const int32 val = rand_int1(rng, numeric_limits<int32>::min(), numeric_limits<int32>::max());
                        array.insert<int32>(prefix + to_string(val), val);
                        break;
                    }
                  case 3:
                    {
                        const int64 val =
                            static_cast<int64>(rand_int1(rng, numeric_limits<int32>::min(), numeric_limits<int32>::max()));
                        array.insert<int64>(prefix + to_string(val), val);
                        break;
                    }
                  case 4:
                    {
                        const uint8 val =
                            static_cast<uint8>(rand_int1(rng, 0, numeric_limits<uint8>::max()));
                        array.insert<uint8>(prefix + to_string(val), val);
                        break;
                    }
                  case 5:
                    {
                        const uint16 val =
                            static_cast<uint16>(rand_int1(rng, 0, numeric_limits<uint16>::max()));
                        array.insert<uint16>(prefix + to_string(val), val);
                        break;
                    }
                  case 6:
                    {
                        const uint32 val =
                            static_cast<uint32>(rand_int1(rng, 0, numeric_limits<int32>::max()));
                        array.insert<uint32>(prefix + to_string(val), val);
                        break;
                    }
                  case 7:
                    {
                        const uint64 val =
                            static_cast<uint64>(rand_int1(rng, 0, numeric_limits<int32>::max()));
                        array.insert<uint64>(prefix + to_string(val), val);
                        break;
                    }
                  case 8:
                    {
                        const float val = rand_float1(rng, -1000.0f, 1000.0f);
                        array.insert<float>(prefix + to_string(val), val);
                        break;
                    }
                  case 9:
                    {
                        const double val = rand_double1(rng, -1000.0, 1000.0);
                        array.insert<double>(prefix + to_string(val), val);
                        break;
                    }
                  default:
                    FOUNDATION_CHECK(false);
                }
            }
        }
        catch (...) { FOUNDATION_CHECK(false); }
        FOUNDATION_CHECK_EQ(array.size(), N);

        // Check all elements.
        size_t found = 0;
        for (AnyArray::ConstIterator it = array.begin(), e = array.end(); it != e; ++it)
        {
            ++found;
            const string name = it.name();
            const string ref_value = name.substr(name.find_first_of("=") + 1);
            switch (it.type())
            {
              case NumericTypeInt8:
                FOUNDATION_CHECK_EQ(it.value<int8>(), from_string<int8>(ref_value));
                break;
              case NumericTypeInt16:
                FOUNDATION_CHECK_EQ(it.value<int16>(), from_string<int16>(ref_value));
                break;
              case NumericTypeInt32:
                FOUNDATION_CHECK_EQ(it.value<int32>(), from_string<int32>(ref_value));
                break;
              case NumericTypeInt64:
                FOUNDATION_CHECK_EQ(it.value<int64>(), from_string<int64>(ref_value));
                break;
              case NumericTypeUInt8:
                FOUNDATION_CHECK_EQ(it.value<uint8>(), from_string<uint8>(ref_value));
                break;
              case NumericTypeUInt16:
                FOUNDATION_CHECK_EQ(it.value<uint16>(), from_string<uint16>(ref_value));
                break;
              case NumericTypeUInt32:
                FOUNDATION_CHECK_EQ(it.value<uint32>(), from_string<uint32>(ref_value));
                break;
              case NumericTypeUInt64:
                FOUNDATION_CHECK_EQ(it.value<uint64>(), from_string<uint64>(ref_value));
                break;
              case NumericTypeFloat:
                FOUNDATION_CHECK_FEQ_EPS(it.value<float>(), from_string<float>(ref_value), 1.0e-3f);
                break;
              case NumericTypeDouble:
                FOUNDATION_CHECK_FEQ_EPS(it.value<double>(), from_string<double>(ref_value), 1.0e-3);
                break;
              default:
                FOUNDATION_CHECK(false);
            }
        }
        FOUNDATION_CHECK_EQ(found, N);
    }
}

}   // namespace foundation

#endif
