
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_UTILITY_STRING_H
#define APPLESEED_FOUNDATION_UTILITY_STRING_H

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/types.h"
#include "foundation/utility/countof.h"
#include "foundation/utility/typetraits.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstddef>
#include <ctime>
#include <iomanip>
#include <ios>
#include <iterator>
#include <sstream>
#include <string>

namespace foundation
{

// todo: add a new function pretty_time_compact() that would return
// strings of the form xx:xx:xx.xx.


//
// Constants.
//

// All blank characters in one string.
extern APPLESEED_DLLSYMBOL const char* Blanks;


//
// String <-> value conversion functions.
//

// Exception thrown by the utility function foundation::from_string()
// when an error occurred when converting a value to a string.
class ExceptionStringConversionError : public Exception {};

// Convert a value to a string.
template <typename T>
std::string to_string(const T& value);

// Convert an array of values to a string.
template <typename T>
std::string to_string(
    const T                 array[],
    const size_t            n,
    const std::string&      separator = " ");

// Convert a string to a value.
template <typename T>
T from_string(const std::string& s);
template <typename T>
T from_string(const char* s);


//
// C strings manipulation functions.
//

// Return true if a C string is empty.
bool is_empty_string(const char* s);

// Duplicate a C string. The returned string must be freed using free_string().
APPLESEED_DLLSYMBOL char* duplicate_string(const char* s);

// Deallocate a C string allocated by duplicate_string().
APPLESEED_DLLSYMBOL void free_string(const char* s);

// Convert a C string allocated by duplicate_string() to an std::string, and dellocate the C string.
APPLESEED_FORCE_INLINE std::string convert_to_std_string(const char* s);


//
// C++ strings manipulation functions.
//

// Convert all characters of a string to lower case.
std::string lower_case(const std::string& s);

// Convert all characters of a string to upper case.
std::string upper_case(const std::string& s);

// Compare two strings lexicographically, regardless of their case.
// Returns -1 if lhs < rhs, +1 if lhs > rhs, and 0 if lhs == rhs.
int strcmp_nocase(
    const std::string&      lhs,
    const std::string&      rhs);

// Return a given string left- or right-padded to a given length.
// The string 's' is returned unchanged if it is already longer than
// the specified length.
std::string pad_left(
    const std::string&      s,
    const char              padding,
    const size_t            length);
std::string pad_right(
    const std::string&      s,
    const char              padding,
    const size_t            length);

// Remove leading, trailing or leading and trailing characters from
// a given string. Typically used to remove blanks around a string.
std::string trim_left(
    const std::string&      s,
    const std::string&      delimiters = Blanks);
std::string trim_right(
    const std::string&      s,
    const std::string&      delimiters = Blanks);
std::string trim_both(
    const std::string&      s,
    const std::string&      delimiters = Blanks);

// Return true if a given string starts with a given prefix.
bool starts_with(const std::string& s, const std::string& prefix);

// Return true if a given string ends with a given suffix.
bool ends_with(const std::string& s, const std::string& suffix);

// Split a given string into multiple individual tokens of a given
// type, according to a set of delimiting characters.
template <typename Vec>
void tokenize(
    const std::string&      s,
    const std::string&      delimiters,
    Vec&                    tokens);

// A variant of tokenize() that stores the tokens into a C array of
// a given maximum size. Returns the number of tokens stored in the
// array. 'max_tokens' must be greater than 0.
template <typename T>
size_t tokenize(
    const std::string&      s,
    const std::string&      delimiters,
    T                       tokens[],
    const size_t            max_tokens);

// Like tokenize(), but consider that there are empty tokens between delimiters.
// This function has the same semantics as Python's string.split() function:
//   http://docs.python.org/2/library/string.html#string.split
//   http://codepad.org/MCL9GcKH
template <typename Vec>
void split(
    const std::string&      s,
    const std::string&      delimiters,
    Vec&                    tokens);

// Return a copy of the input string 's' where all occurrences of 'old_string'
// were replaced by 'new_string'.
std::string replace(
    const std::string&      s,
    const std::string&      old_string,
    const std::string&      new_string);

// Prefix all lines of a multi-line string.
std::string prefix_all_lines(
    const std::string&      s,
    const std::string&      prefix);

// Formatting functions, similar to the .NET String.Format() method.
// Placeholders are of the form {n} with n starting at 0, e.g. {0}, {1}, etc.
// Example: format("Hello {0}", "World") will return "Hello World".
template <typename T1>
std::string format(const std::string& fmt, const T1& arg1);
template <typename T1, typename T2>
std::string format(const std::string& fmt, const T1& arg1, const T2& arg2);
template <typename T1, typename T2, typename T3>
std::string format(const std::string& fmt, const T1& arg1, const T2& arg2, const T3& arg3);
template <typename T1, typename T2, typename T3, typename T4>
std::string format(const std::string& fmt, const T1& arg1, const T2& arg2, const T3& arg3, const T4& arg4);

// Return a copy of the input pattern where consecutive '#' characters have
// been replaced by 'value', with leading zeroes added as necessary.
std::string get_numbered_string(
    const std::string&      pattern,
    const size_t            value);

// Return the maximum value that can be return by get_numbered_string() for
// a given pattern.
size_t get_numbered_string_max_value(const std::string& pattern);

// Return a time stamp string based on the current date and time.
// The returned string has the form YYYYMMDD.HHmmSS.XXX, where:
//   Y = year, M = month, D = day
//   H = hour, m = minute, s = second
//   X is implementation defined
std::string get_time_stamp_string();

// Replace all special characters of a string by the corresponding XML entities.
std::string replace_special_xml_characters(const std::string& s);


//
// Fast alternative to std::strtol().
//
// Compared to std::strtol(), this function:
//
//   * assumes base 10
//   * does not skip leading white space characters
//   * does not consider the current locale
//
// Reference:
//
//   http://pubs.opengroup.org/onlinepubs/000095399/functions/strtol.html
//

long fast_strtol_base10(const char* str, const char** end_ptr);
long fast_strtol_base10(char* str, char** end_ptr);


//
// Fast alternative to std::strtod().
//
// Compared to std::strtod(), this function:
//
//   * is potentially less accurate
//   * does not skip leading white space characters
//   * does no error checking
//   * does not consider the current locale
//
// Reference:
//
//   http://pubs.opengroup.org/onlinepubs/000095399/functions/strtod.html
//

double fast_strtod(const char* str, const char** end_ptr);
double fast_strtod(char* str, char** end_ptr);


//
// Filename manipulation functions.
//

// Replace all unsafe characters in a filename by a given substitute character.
// The set of safe characters is defined in the Open Group Base Specification:
// http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_278
std::string make_safe_filename(
    const std::string&      filename,
    const char              substitute = '_');


//
// Pretty-print functions.
//

// Capitalize the first letter of each word of a string and convert
// all other characters to lower case. Everything else is preserved.
std::string capitalize(const std::string& s);

// Return the plural of a unit, depending on a value.
template <typename T>
std::string plural(
    const T                 value,
    const std::string&      unit);
template <typename T>
std::string plural(
    const T                 value,
    const std::string&      unit_singular,
    const std::string&      unit_plural);

// Pretty-print an unsigned integer value.
std::string pretty_uint(const uint64 value);

// Pretty-print a signed integer value.
std::string pretty_int(const int64 value);

// Pretty-print a floating-point value.
std::string pretty_scalar(
    const double            value,
    const std::streamsize   precision = 1);

// Pretty-print the ratio numerator/denominator.
// Returns the string "infinite" if the numerator is greater than zero
// and the denominator is equal to zero.
// Returns the string "n/a" if both the numerator and the denominator are zero.
template <typename T>
std::string pretty_ratio(
    const T                 numerator,
    const T                 denominator,
    const std::streamsize   precision = 1);

// Pretty-print the ratio numerator/denominator as a percentage.
// Returns the string "infinite" if the numerator is greater than zero
// and the denominator is equal to zero.
// Returns the string "n/a" if both the numerator and the denominator are zero.
template <typename T>
std::string pretty_percent(
    const T                 numerator,
    const T                 denominator,
    const std::streamsize   precision = 1);

// Pretty-print a time value, given in seconds.
std::string pretty_time(
    const double            seconds,
    const std::streamsize   precision = 1);

// Pretty-print a size, given in bytes.
std::string pretty_size(
    const uint64            bytes,
    const std::streamsize   precision = 1);


//
// Value-to-string conversion functions implementation.
//

namespace impl
{
    template <typename T, bool IsPointer>
    struct ToString;

    // General non-pointer case.
    template <typename T>
    struct ToString<T, false>
    {
        static std::string to_string(const T& value)
        {
            std::stringstream sstr;
            sstr << value;
            return sstr.str();
        }
    };

    // Pointer case.
    template <typename T>
    struct ToString<T, true>
    {
        static std::string to_string(const T& value)
        {
            std::stringstream sstr;
            sstr << "0x"
                 << std::hex
                 << std::uppercase
                 << std::setw(2 * sizeof(void*))
                 << std::setfill('0')
                 << reinterpret_cast<uintptr_t>(value);
            return sstr.str();
        }
    };
}

// General entry-point.
template <typename T>
std::string to_string(const T& value)
{
    return impl::ToString<T, IsPointer<T>::R>::to_string(value);
}

// Handle booleans separately.
template <>
inline std::string to_string(const bool& value)
{
    return value ? "true" : "false";
}

// Handle 8-bit integers as integers, not as characters.
template <>
inline std::string to_string(const int8& value)
{
    return to_string(static_cast<int>(value));
}
template <>
inline std::string to_string(const uint8& value)
{
    return to_string(static_cast<unsigned int>(value));
}

// Handle C strings separately.
inline std::string to_string(const char* value)
{
    std::stringstream sstr;
    sstr << (value ? value : "<null>");
    return sstr.str();
}
inline std::string to_string(char* value)
{
    return to_string(static_cast<const char*>(value));
}

template <typename T>
std::string to_string(
    const T             array[],
    const size_t        n,
    const std::string&  separator)
{
    std::string s;

    for (size_t i = 0; i < n; ++i)
    {
        if (i > 0)
            s += separator;
        s += to_string(array[i]);
    }

    return s;
}


//
// String-to-value conversion functions implementation.
//

template <typename T>
T from_string(const char* s)
{
    return from_string<T>(std::string(s));
}

template <typename T>
T from_string(const std::string& s)
{
    std::istringstream istr(s);

    T val;
    istr >> val;

    if (!istr || !istr.eof())
        throw ExceptionStringConversionError();

    return val;
}

template <>
inline std::string from_string(const std::string& s)
{
    return s;
}

template <>
inline bool from_string(const std::string& s)
{
    const std::string t = lower_case(s);

    if (t == "1" || t == "true" || t == "on" || t == "yes")
        return true;
    else if (t == "0" || t == "false" || t == "off" || t == "no")
        return false;
    else throw ExceptionStringConversionError();
}

template <>
inline int8 from_string(const std::string& s)
{
    std::istringstream istr(s);

    int val;
    istr >> val;

    if (!istr || !istr.eof())
        throw ExceptionStringConversionError();

    return static_cast<int8>(val);
}

template <>
inline uint8 from_string(const std::string& s)
{
    std::istringstream istr(s);

    unsigned int val;
    istr >> val;

    if (!istr || !istr.eof())
        throw ExceptionStringConversionError();

    return static_cast<uint8>(val);
}


//
// C strings manipulation functions implementation.
//

inline bool is_empty_string(const char* s)
{
    assert(s);
    return s[0] == '\0';
}


//
// C++ strings manipulation functions implementation.
//

APPLESEED_FORCE_INLINE std::string convert_to_std_string(const char* s)
{
    const std::string result = s;
    free_string(s);
    return result;
}

inline std::string lower_case(const std::string& s)
{
    std::string result;
    std::transform(
        s.begin(),
        s.end(),
        std::back_inserter(result),
        static_cast<int(*)(int)>(std::tolower));
    return result;
}

inline std::string upper_case(const std::string& s)
{
    std::string result;
    std::transform(
        s.begin(),
        s.end(),
        std::back_inserter(result),
        static_cast<int(*)(int)>(std::toupper));
    return result;
}

inline int strcmp_nocase(
    const std::string&      lhs,
    const std::string&      rhs)
{
    std::string::const_iterator lhs_it = lhs.begin();
    std::string::const_iterator rhs_it = rhs.begin();

    while (lhs_it != lhs.end() && rhs_it != rhs.end())
    {
        if (std::toupper(*lhs_it) != std::toupper(*rhs_it))
            return (std::toupper(*lhs_it) < std::toupper(*rhs_it)) ? -1 : 1;
        ++lhs_it;
        ++rhs_it;
    }

    if (lhs.size() == rhs.size())
        return 0;
    else return lhs.size() < rhs.size() ? -1 : 1;
}

inline std::string pad_left(
    const std::string&      s,
    const char              padding,
    const size_t            length)
{
    return
        s.size() < length
            ? std::string(length - s.size(), padding) + s
            : s;
}

inline std::string pad_right(
    const std::string&      s,
    const char              padding,
    const size_t            length)
{
    return
        s.size() < length
            ? s + std::string(length - s.size(), padding)
            : s;
}

inline std::string trim_left(
    const std::string&      s,
    const std::string&      delimiters)
{
    const std::string::size_type begin = s.find_first_not_of(delimiters);
    return begin == std::string::npos ? "" : s.substr(begin);
}

inline std::string trim_right(
    const std::string&      s,
    const std::string&      delimiters)
{
    const std::string::size_type end = s.find_last_not_of(delimiters);
    return end == std::string::npos ? "" : s.substr(0, end + 1);
}

inline std::string trim_both(
    const std::string&      s,
    const std::string&      delimiters)
{
    const std::string::size_type begin = s.find_first_not_of(delimiters);
    const std::string::size_type end = s.find_last_not_of(delimiters);
    return begin == std::string::npos ? "" : s.substr(begin, end - begin + 1);
}

inline bool starts_with(const std::string& s, const std::string& prefix)
{
    assert(!prefix.empty());
    return
        s.size() < prefix.size()
            ? false
            : s.compare(0, prefix.size(), prefix) == 0;
}

inline bool ends_with(const std::string& s, const std::string& suffix)
{
    assert(!suffix.empty());
    return
        s.size() < suffix.size()
            ? false
            : s.compare(s.size() - suffix.size(), suffix.size(), suffix) == 0;
}

template <typename Vec>
void tokenize(
    const std::string&      s,
    const std::string&      delimiters,
    Vec&                    tokens)
{
    // Skip delimiters at the beginning.
    std::string::size_type last_pos = s.find_first_not_of(delimiters, 0);

    // Find the next delimiter.
    std::string::size_type pos = s.find_first_of(delimiters, last_pos);

    while (pos != std::string::npos || last_pos != std::string::npos)
    {
        // Found a token, append it to the vector.
        tokens.push_back(
            from_string<typename Vec::value_type>(
                s.substr(last_pos, pos - last_pos)));

        // Skip delimiters.
        last_pos = s.find_first_not_of(delimiters, pos);

        // Find the next delimiter.
        pos = s.find_first_of(delimiters, last_pos);
    }
}

template <typename T>
size_t tokenize(
    const std::string&      s,
    const std::string&      delimiters,
    T                       tokens[],
    const size_t            max_tokens)
{
    assert(tokens);
    assert(max_tokens > 0);

    // Skip delimiters at the beginning.
    std::string::size_type last_pos = s.find_first_not_of(delimiters, 0);

    // Find the next delimiter.
    std::string::size_type pos = s.find_first_of(delimiters, last_pos);

    size_t token_count = 0;
    while (pos != std::string::npos || last_pos != std::string::npos)
    {
        // Found a token, insert it into the array.
        assert(token_count < max_tokens);
        tokens[token_count++] =
            from_string<T>(s.substr(last_pos, pos - last_pos));
        if (token_count == max_tokens)
            break;

        // Skip delimiters.
        last_pos = s.find_first_not_of(delimiters, pos);

        // Find the next delimiter.
        pos = s.find_first_of(delimiters, last_pos);
    }

    // Return the number of tokens that were stored.
    return token_count;
}

template <typename Vec>
void split(
    const std::string&      s,
    const std::string&      delimiters,
    Vec&                    tokens)
{
    std::string::size_type pos = 0;

    while (true)
    {
        // Find the next delimiter.
        const std::string::size_type delimiter_pos =
            s.find_first_of(delimiters, pos);

        if (delimiter_pos == std::string::npos)
        {
            // No delimiter found: the rest of the text constitutes the last token.
            tokens.push_back(
                from_string<typename Vec::value_type>(
                    s.substr(pos)));

            // Done.
            break;
        }
        else
        {
            // Found a delimiter: the text up to this delimiter constitutes the next token.
            tokens.push_back(
                from_string<typename Vec::value_type>(
                    s.substr(pos, delimiter_pos - pos)));

            // Move past the delimiter.
            pos = delimiter_pos + 1;
        }
    }
}

inline std::string replace(
    const std::string&      s,
    const std::string&      old_string,
    const std::string&      new_string)
{
    assert(!old_string.empty());

    std::string::size_type pos = s.find(old_string);

    if (pos == std::string::npos)
        return s;

    std::string result = s;

    do
    {
        result.replace(pos, old_string.size(), new_string);
        pos += new_string.size();
    } while ((pos = result.find(old_string, pos)) != std::string::npos);

    return result;
}

inline std::string prefix_all_lines(
    const std::string&      s,
    const std::string&      prefix)
{
    return prefix + replace(s, "\n", "\n" + prefix);
}

template <typename T1>
std::string format(const std::string& fmt, const T1& arg1)
{
    return replace(fmt, "{0}", to_string(arg1));
}

template <typename T1, typename T2>
std::string format(const std::string& fmt, const T1& arg1, const T2& arg2)
{
    return replace(format(fmt, arg1), "{1}", to_string(arg2));
}

template <typename T1, typename T2, typename T3>
std::string format(const std::string& fmt, const T1& arg1, const T2& arg2, const T3& arg3)
{
    return replace(format(fmt, arg1, arg2), "{2}", to_string(arg3));
}

template <typename T1, typename T2, typename T3, typename T4>
std::string format(const std::string& fmt, const T1& arg1, const T2& arg2, const T3& arg3, const T4& arg4)
{
    return replace(format(fmt, arg1, arg2, arg3), "{3}", to_string(arg4));
}

inline std::string get_numbered_string(
    const std::string&      pattern,
    const size_t            value)
{
    const size_t b = pattern.find_first_of('#');

    if (b == std::string::npos)
        return pattern;

    size_t e = pattern.find_first_not_of('#', b);

    if (e == std::string::npos)
        e = pattern.size();

    const size_t n = e - b;

    std::stringstream sstr;
    sstr << std::setw(n) << std::setfill('0');
    sstr << value;

    std::string value_string;
    sstr >> value_string;

    return replace(pattern, std::string(n, '#'), value_string);
}

inline size_t get_numbered_string_max_value(const std::string& pattern)
{
    const size_t b = pattern.find_first_of('#');

    if (b == std::string::npos)
        return 0;

    size_t e = pattern.find_first_not_of('#', b);

    if (e == std::string::npos)
        e = pattern.size();

    const size_t n = e - b;

    return pow_int<size_t>(10, n) - 1;
}

inline std::string get_time_stamp_string()
{
    // Retrieve the current date and time.
    std::time_t t;
    std::time(&t);
    const std::tm* local_time = std::localtime(&t);

    // Build the time stamp string.
    std::stringstream sstr;
    sstr << std::setfill('0');
    sstr << std::setw(4) << local_time->tm_year + 1900;
    sstr << std::setw(2) << local_time->tm_mon + 1;
    sstr << std::setw(2) << local_time->tm_mday;
    sstr << ".";
    sstr << std::setw(2) << local_time->tm_hour;
    sstr << std::setw(2) << local_time->tm_min;
    sstr << std::setw(2) << local_time->tm_sec;
    sstr << ".";
    sstr << std::setw(3) << 0;  // milliseconds not available, set field to 000

    return sstr.str();
}

namespace impl
{
    struct XMLEntity
    {
        const char* m_character;
        const char* m_entity;
    };
}

inline std::string replace_special_xml_characters(const std::string& s)
{
    // Reference: http://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references

    static const impl::XMLEntity XMLEntities[] =
    {
        { "&",  "&amp;" },      // must stay first!
        { "\"", "&quot;" },
        { "'",  "&apos;" },
        { "<",  "&lt;" },
        { ">",  "&gt;" }
    };

    std::string result = s;

    for (size_t i = 0; i < countof(XMLEntities); ++i)
    {
        const impl::XMLEntity& e = XMLEntities[i];
        result = replace(result, e.m_character, e.m_entity);
    }

    return result;
}


//
// Fast string-to-number functions implementation.
//

inline long fast_strtol_base10(char* str, char** end_ptr)
{
    return
        fast_strtol_base10(
            const_cast<const char*>(str),
            const_cast<const char**>(end_ptr));
}

inline double fast_strtod(char* str, char** end_ptr)
{
    return
        fast_strtod(
            const_cast<const char*>(str),
            const_cast<const char**>(end_ptr));
}


//
// Filename manipulation functions implementation.
//

inline std::string make_safe_filename(
    const std::string&      filename,
    const char              substitute)
{
    std::string result = filename;

    for (std::string::iterator i = result.begin(); i != result.end(); ++i)
    {
        const char c = *i;
        const bool is_safe =
            (c >= 'A' && c <= 'Z') ||
            (c >= 'a' && c <= 'z') ||
            (c >= '0' && c <= '9') ||
            c == '.' ||
            c == '_' ||
            c == '-';
        if (!is_safe)
            *i = substitute;
    }

    return result;
}


//
// Pretty-print functions implementation.
//

inline std::string capitalize(const std::string& s)
{
    std::string result = s;
    bool cap = true;

    for (std::string::iterator i = result.begin(); i != result.end(); ++i)
    {
        if (std::isspace(static_cast<unsigned char>(*i)))
            cap = true;
        else
        {
            *i = cap ? std::toupper(*i) : std::tolower(*i);
            cap = false;
        }
    }

    return result;
}

template <typename T>
std::string plural(
    const T                 value,
    const std::string&      unit)
{
    return unit + (value > T(1) ? "s" : "");
}

template <typename T>
std::string plural(
    const T                 value,
    const std::string&      unit_singular,
    const std::string&      unit_plural)
{
    return value > T(1) ? unit_plural : unit_singular;
}

inline std::string pretty_uint(const uint64 value)
{
    const std::string s = to_string(value);
    std::string result;
    size_t digits = 0;

    for (std::string::const_reverse_iterator i = s.rbegin(), e = s.rend(); i != e; ++i)
    {
        if (digits == 3)
        {
            result += ',';
            digits = 0;
        }

        result += *i;
        ++digits;
    }

    std::reverse(result.begin(), result.end());

    return result;
}

inline std::string pretty_int(const int64 value)
{
    const std::string result = pretty_uint(abs(value));
    return value < 0 ? '-' + result : result;
}

inline std::string pretty_scalar(
    const double            value,
    const std::streamsize   precision)
{
    assert(precision >= 0);

    std::stringstream sstr;
    sstr << std::fixed;
    sstr << std::setprecision(precision);
    sstr << value;

    return sstr.str();
}

template <typename T>
inline std::string pretty_ratio(
    const T                 numerator,
    const T                 denominator,
    const std::streamsize   precision)
{
    assert(numerator >= 0);
    assert(denominator >= 0);
    assert(precision >= 0);

    if (denominator == 0)
        return numerator == 0 ? "n/a" : "infinite";

    return pretty_scalar(static_cast<double>(numerator) / denominator, precision);
}

template <typename T>
inline std::string pretty_percent(
    const T                 numerator,
    const T                 denominator,
    const std::streamsize   precision)
{
    assert(numerator >= 0);
    assert(denominator >= 0);
    assert(precision >= 0);

    if (denominator == 0)
        return numerator == 0 ? "n/a" : "infinite";

    return
        pretty_ratio(
            static_cast<double>(numerator * 100.0),
            static_cast<double>(denominator),
            precision) + "%";
}

inline std::string pretty_time(
    const double            seconds,
    const std::streamsize   precision)
{
    assert(seconds >= 0.0);
    assert(precision >= 0);

    std::string result;

    // Handle the case where the input time is less than 1 second.
    if (seconds < 1.0)
    {
        const double ms = 1000.0 * seconds;
        result += pretty_scalar(ms, precision > 3 ? precision - 3 : 0);
        result += " ms";
        return result;
    }

    // Number of seconds in 1 minute, 1 hour, 1 day and 1 week.
    const size_t Minute = 60;
    const size_t Hour   = 60 * Minute;
    const size_t Day    = 24 * Hour;
    const size_t Week   = 7  * Day;

    const size_t integral_seconds = static_cast<size_t>(seconds);
    size_t s = integral_seconds;

    // Compute and print the number of weeks.
    if (integral_seconds >= Week)
    {
        const size_t w = s / Week;
        s -= w * Week;
        result += pretty_uint(w);
        result += plural(w, " week");
        result += ' ';
    }

    // Compute and print the number of days.
    if (integral_seconds >= Day)
    {
        const size_t d = s / Day;
        s -= d * Day;
        result += to_string(d);
        result += plural(d, " day");
        result += ' ';
    }

    // Compute and print the number of hours.
    if (integral_seconds >= Hour)
    {
        const size_t h = s / Hour;
        s -= h * Hour;
        result += to_string(h);
        result += plural(h, " hour");
        result += ' ';
    }

    // Compute and print the number of minutes.
    if (integral_seconds >= Minute)
    {
        const size_t m = s / Minute;
        s -= m * Minute;
        result += to_string(m);
        result += plural(m, " minute");
        result += ' ';
    }

    // Print the number of seconds.
    result += to_string(s);
    result += pretty_scalar(seconds - integral_seconds, precision).substr(1);
    result += plural(s, " second");

    return result;
}

inline std::string pretty_size(
    const uint64            bytes,
    const std::streamsize   precision)
{
    assert(precision >= 0);

    // Number of bytes in 1 kilobyte, 1 megabyte, 1 gigabyte and 1 terabyte.
    const uint64 KB = 1024;
    const uint64 MB = 1024 * KB;
    const uint64 GB = 1024 * MB;
    const uint64 TB = 1024 * GB;

    // Pretty-print.
    if (bytes == 0)
    {
        return "0 byte";
    }
    else if (bytes == 1)
    {
        return "1 byte";
    }
    else if (bytes < KB)
    {
        return pretty_uint(bytes) + " bytes";
    }
    else if (bytes < MB)
    {
        return pretty_ratio(bytes, KB, precision) + " KB";
    }
    else if (bytes < GB)
    {
        return pretty_ratio(bytes, MB, precision) + " MB";
    }
    else if (bytes < TB)
    {
        return pretty_ratio(bytes, GB, precision) + " GB";
    }
    else
    {
        return pretty_ratio(bytes, TB, precision) + " TB";
    }
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_STRING_H
