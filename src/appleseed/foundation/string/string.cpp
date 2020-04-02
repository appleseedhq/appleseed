
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

// Interface header.
#include "string.h"

// Standard headers.
#include <cstring>

namespace foundation
{

//
// Constants.
//

const char* Blanks = " \t\n\v\f\r";


//
// C strings manipulation functions implementation.
//

char* duplicate_string(const char* s)
{
    assert(s);

    char* result = new char[strlen(s) + 1];
    std::strcpy(result, s);

    return result;
}

void free_string(const char* s)
{
    delete[] s;
}

int strcmp_nocase(
    const char* lhs,
    const char* rhs)
{
    assert(lhs != nullptr);
    assert(rhs != nullptr);

    for (; ; ++lhs, ++rhs)
    {
        const char lhs_c = *lhs;
        const char rhs_c = *rhs;

        if (lhs_c != 0)
        {
            if (rhs_c != 0)
            {
                const int lhs_upper_c = std::toupper(lhs_c);
                const int rhs_upper_c = std::toupper(rhs_c);

                if (lhs_upper_c < rhs_upper_c)
                    return -1;

                if (lhs_upper_c > rhs_upper_c)
                    return +1;
            }
            else
            {
                // Left-hand string is longer than right-hand one.
                return +1;
            }
        }
        else
        {
            if (rhs_c != 0)
            {
                // Left-hand string is shorter than right-hand one.
                return -1;
            }
            else
            {
                // Strings have equal length.
                return 0;
            }
        }
    }
}


//
// Fast string-to-number functions implementation.
//

namespace
{
    inline bool is_digit(const char c)
    {
        return c >= '0' && c <= '9';
    }
}

long fast_strtol_base10(const char* str, const char** end_ptr)
{
    const char* p = str;

    // Handle sign, if any.
    bool positive = true;
    if (*p == '-')
    {
        positive = false;
        ++p;
    }
    else if (*p == '+')
        ++p;

    // Get digits.
    long value = 0;
    while (is_digit(*p))
    {
        value = value * 10 + (*p - '0');
        ++p;
    }

    // Optionally return a pointer to the next character after the value.
    if (end_ptr)
        *end_ptr = p;

    return positive ? value : -value;
}

double fast_strtod(const char* str, const char** end_ptr)
{
    const char* p = str;

    // Handle sign, if any.
    bool positive = true;
    if (*p == '-')
    {
        positive = false;
        ++p;
    }
    else if (*p == '+')
        ++p;

    // Get digits before decimal point or exponent, if any.
    double value = 0.0;
    while (is_digit(*p))
    {
        value = value * 10.0 + (*p - '0');
        ++p;
    }

    // Get digits after decimal point, if any.
    if (*p == '.')
    {
        ++p;

        double power = 1.0;

        while (is_digit(*p))
        {
            value = value * 10.0 + (*p - '0');
            power *= 10.0;
            ++p;
        }

        value /= power;
    }

    // Handle exponent, if any.
    if (*p == 'e' || *p == 'E')
    {
        ++p;

        // Handle exponent sign, if any.
        bool exponent_positive = true;
        if (*p == '-')
        {
            exponent_positive = false;
            ++p;
        }
        else if (*p == '+')
            ++p;

        // Get exponent digits.
        long exponent = 0;
        while (is_digit(*p))
        {
            exponent = exponent * 10 + (*p - '0');
            ++p;
        }

        // Apply exponent.
        if (exponent_positive)
        {
            while (exponent >= 64) { value *= 1.0e64; exponent -= 64; }
            while (exponent >= 8) { value *= 1.0e8; exponent -= 8; }
            while (exponent > 0) { value *= 1.0e1; exponent -= 1; }
        }
        else
        {
            while (exponent >= 64) { value *= 1.0e-64; exponent -= 64; }
            while (exponent >= 8) { value *= 1.0e-8; exponent -= 8; }
            while (exponent > 0) { value *= 1.0e-1; exponent -= 1; }
        }
    }

    // Optionally return a pointer to the next character after the value.
    if (end_ptr)
        *end_ptr = p;

    return positive ? value : -value;
}

}   // namespace foundation
