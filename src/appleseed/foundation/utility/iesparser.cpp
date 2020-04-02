
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Artem Bishev, The appleseedhq Organization
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
#include "iesparser.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/utility/countof.h"

// Boost headers.
#include "boost/algorithm/cxx11/any_of.hpp"
#include "boost/regex.hpp"

namespace foundation
{

const char* const IESParser::KeywordLineRegex = "\\[(\\w*)\\]\\s*(\\S.*)?";
const char* const IESParser::TiltLineRegex = "TILT\\s*=\\s*(\\S.*)";


//
// Auxiliary functions.
//

namespace
{
    // Predicate that checks if the value is negative.
    template <typename ValueType>
    bool is_negative(const ValueType& type)
    {
        return type < ValueType(0);
    }

    // Lexical cast without exception.
    template <typename ValueType>
    bool string_to_number(const std::string& str, ValueType* value)
    {
        try
        {
            *value = from_string<ValueType>(str);
        }
        catch (const ExceptionStringConversionError&)
        {
            return false;
        }
        return true;
    }

    // Convert string to positive number.
    template <typename ValueType>
    bool string_to_positive_number(const std::string& str, ValueType* value)
    {
        try
        {
            *value = from_string<ValueType>(str);
        }
        catch (const ExceptionStringConversionError&)
        {
            return false;
        }
        if (*value <= ValueType(0))
        {
            return false;
        }
        return true;
    }

    // Convert string to photometric type.
    bool string_to_photometric_type(
        const std::string& str, IESParser::PhotometricType* type)
    {
        if (str == "1")
        {
            *type = IESParser::PhotometricTypeC;
        }
        else if (str == "2")
        {
            *type = IESParser::PhotometricTypeB;
        }
        else if (str == "3")
        {
            *type = IESParser::PhotometricTypeA;
        }
        else
        {
            return false;
        }
        return true;
    }

    // Convert string to units type.
    bool string_to_units_type(
        const std::string& str, IESParser::LuminousOpeningShape::UnitsType* type)
    {
        if (str == "1")
        {
            *type = IESParser::LuminousOpeningShape::Feet;
        }
        else if (str == "2")
        {
            *type = IESParser::LuminousOpeningShape::Meters;
        }
        else
        {
            return false;
        }
        return true;
    }

    // Split string and convert it to the vector of values.
    // Returns false, if one of the tokens cannot be converted
    // to the value of specified type.
    template <typename ValueType>
    bool string_to_vector(
        const std::string& str, std::vector<ValueType>* output)
    {
        std::vector<std::string> tokens;
        boost::split(tokens, str, isspace, boost::token_compress_on);

        output->clear();
        output->reserve(tokens.size());

        for (const std::string& token : tokens)
        {
            try
            {
                output->push_back(from_string<ValueType>(token));
            }
            catch (const ExceptionStringConversionError&)
            {
                return false;
            }
        }

        return true;
    }

    template <typename ValueType>
    bool check_increasing_order(const std::vector<ValueType>& values)
    {
        for (size_t i = 1; i < values.size(); ++i)
        {
            if (values[i] <= values[i - 1])
                return false;
        }
        return true;
    }
}

//
// Class implementation.
//

IESParser::Exception::Exception(const char* what, const int line)
  : m_line_number(line)
{
    set_what(format("{0} (line {1})", what, line).c_str());
}

IESParser::IESParser()
  : m_ignore_allowed_keywords(false)
  , m_ignore_required_keywords(false)
  , m_ignore_empty_lines(true)
  , m_ignore_tilt(false)
{
}

void IESParser::parse(std::istream& input_stream)
{
    reset(input_stream);

    // Parse version.
    parse_format_version(input_stream);

    // Parse block before TILT and TILT line.
    parse_keywords_and_tilt(input_stream);

    // Parse TILT data if it is included.
    if (m_tilt_specification == IncludeTilt)
    {
        parse_tilt_data(input_stream);
    }

    // Parse the rest of the data.
    parse_photometric_data(input_stream);
    parse_angles(input_stream);
    parse_candela_values(input_stream);

    if (!input_stream.eof())
    {
        throw ParsingException("Expected end of file", m_line_counter);
    }
}

bool IESParser::is_keyword_allowed_by_iesna02(const std::string& keyword)
{
    return
        keyword == "TEST" ||
        keyword == "TESTLAB" ||
        keyword == "TESTDATE" ||
        keyword == "TESTMETHOD" ||
        keyword == "NEARFIELD" ||
        keyword == "MANUFAC" ||
        keyword == "LUMCAT" ||
        keyword == "LUMINAIRE" ||
        keyword == "LAMPCAT" ||
        keyword == "LAMP" ||
        keyword == "BALLAST" ||
        keyword == "BALLASTCAT" ||
        keyword == "MAINTCAT" ||
        keyword == "DISTRIBUTION" ||
        keyword == "FLASHAREA" ||
        keyword == "COLORCONSTANT" ||
        keyword == "LAMPPOSITION" ||
        keyword == "ISSUEDATE" ||
        keyword == "OTHER" ||
        keyword == "SEARCH" ||
        keyword == "MORE";
}

bool IESParser::is_keyword_allowed_by_iesna95(const std::string& keyword)
{
    return
        keyword == "TEST" ||
        keyword == "DATE" ||
        keyword == "NEARFIELD" ||
        keyword == "MANUFAC" ||
        keyword == "LUMCAT" ||
        keyword == "LUMINAIRE" ||
        keyword == "LAMPCAT" ||
        keyword == "LAMP" ||
        keyword == "BALLAST" ||
        keyword == "BALLASTCAT" ||
        keyword == "MAINTCAT" ||
        keyword == "DISTRIBUTION" ||
        keyword == "FLASHAREA" ||
        keyword == "COLORCONSTANT" ||
        keyword == "OTHER" ||
        keyword == "SEARCH" ||
        keyword == "MORE" ||
        keyword == "BLOCK" ||
        keyword == "ENDBLOCK";
}

bool IESParser::is_keyword_allowed_by_iesna91(const std::string& keyword)
{
    return
        keyword == "TEST" ||
        keyword == "DATE" ||
        keyword == "MANUFAC" ||
        keyword == "LUMCAT" ||
        keyword == "LUMINAIRE" ||
        keyword == "LAMPCAT" ||
        keyword == "LAMP" ||
        keyword == "BALLAST" ||
        keyword == "BALLASTCAT" ||
        keyword == "MAINTCAT" ||
        keyword == "DISTRIBUTION" ||
        keyword == "FLASHAREA" ||
        keyword == "COLORCONSTANT" ||
        keyword == "MORE";
}

void IESParser::reset(std::istream& input_stream)
{
    m_line_counter = 0;
    m_line = "";
    read_trimmed_line(input_stream);
    m_keywords_dictionary.clear();
    m_last_added_keyword = m_keywords_dictionary.end();
}

void IESParser::read_line(std::istream& input_stream)
{
    m_line_counter++;
    std::getline(input_stream, m_line);
}

void IESParser::read_trimmed_line(std::istream& input_stream)
{
    do
    {
        read_line(input_stream);
        boost::trim(m_line);
    } while (m_ignore_empty_lines && input_stream && m_line.empty());
}

void IESParser::parse_format_version(std::istream& input_stream)
{
    check_empty(input_stream);

    if (m_line == "IESNA91")
    {
        m_format = Format1991;
    }
    else if (m_line == "IESNA:LM-63-1995")
    {
        m_format = Format1995;
    }
    else if (m_line == "IESNA:LM-63-2002")
    {
        m_format = Format2002;
    }
    else
    {
        // The first line is not a format line,
        // which is possible only when the format is LM-63-1986.
        m_format = Format1986;
        // Do not read the next line, since the current one is not processed:
        return;
    }

    read_trimmed_line(input_stream);
}

void IESParser::parse_keywords_and_tilt(std::istream& input_stream)
{
    m_last_added_keyword = m_keywords_dictionary.end();

    bool tilt_reached = false;
    while (!tilt_reached)
    {
        check_empty(input_stream);

        if (is_keyword_line(m_line))
        {
            parse_keyword_line(m_line);
        }
        else if (is_tilt_line(m_line))
        {
            if (!m_ignore_required_keywords) check_required_keywords();
            tilt_reached = true;
            parse_tilt_line(m_line);
        }
        else
        {
            if (m_format != Format1986)
            {
                // LM-63-1986 format allows arbitrary data until TILT=<...> is reached.
                // In any other case, raise the parsing exception:
                throw ParsingException("Expected keyword line or TILT line", m_line_counter);
            }
        }

        read_trimmed_line(input_stream);
    }
}

void IESParser::parse_tilt_data(std::istream& input_stream)
{
    check_empty(input_stream);

    // parse lamp-to-luminaire geometry
    if (m_line == "1")
        m_lamp_to_luminaire_geometry = VerticalGeometry;
    else if (m_line == "2")
        m_lamp_to_luminaire_geometry = HorizontalInvariantGeometry;
    else if (m_line == "3")
        m_lamp_to_luminaire_geometry = HorizontalNonInvariantGeometry;
    else
    {
        throw ParsingException(
            "Wrong lamp-to-luminaire geometry value, expected 1, 2 or 3", m_line_counter);
    }

    read_trimmed_line(input_stream);

    check_empty(input_stream);

    // parse number of tilt entries
    int number_of_tilt_entries;
    try
    {
        number_of_tilt_entries = from_string<int>(m_line);
    }
    catch (const ExceptionStringConversionError&)
    {
        throw ParsingException(
            "Wrong number of tilt entries: expected an integer value", m_line_counter);
    }
    if (number_of_tilt_entries < 0)
    {
        throw ParsingException(
            "Wrong number of tilt entries: "
            "number of tilt entries must be non-negative", m_line_counter);
    }

    read_trimmed_line(input_stream);

    check_empty(input_stream);

    // parse tilt angles
    if (!string_to_vector<double>(m_line, &m_tilt_angles))
    {
        throw ParsingException(
            "Error while parsing tilt angles: "
            "value is not a floating point number", m_line_counter);
    }
    if (m_tilt_angles.size() != number_of_tilt_entries)
    {
        throw ParsingException(
            "The specified number of tilt entries does not match "
            "the actual number of entries in the line", m_line_counter);
    }

    read_trimmed_line(input_stream);

    check_empty(input_stream);

    // parse tilt multipliers
    if (!string_to_vector<double>(m_line, &m_tilt_multiplying_factors))
    {
        throw ParsingException(
            "Error while parsing tilt multipliers: "
            "value is not a floating point number", m_line_counter);
    }
    if (m_tilt_multiplying_factors.size() != number_of_tilt_entries)
    {
        throw ParsingException(
            "The specified number of tilt entries does not match "
            "the actual number of tilt multipliers in the line", m_line_counter);
    }
    if (boost::algorithm::any_of(m_tilt_multiplying_factors, is_negative<double>))
    {
        throw ParsingException(
            "Error while parsing tilt multipliers: "
            "value must be non-negative", m_line_counter);
    }

    read_trimmed_line(input_stream);
}

void IESParser::parse_photometric_data(std::istream& input_stream)
{
    std::vector<std::string> values =
        parse_to_vector<std::string>(input_stream, 10);

    if (!string_to_positive_number(values[0], &m_number_of_lamps))
    {
        throw ParsingException(
            "Number of lamps is expected to be a positive integer number",
            m_line_counter);
    }

    m_absolute_photometry = false;
    if (values[1] == "-1")
    {
        m_lumens_per_lamp = -1.0;
        m_absolute_photometry = true;
    }
    else if (!string_to_positive_number(values[1], &m_lumens_per_lamp))
    {
        throw ParsingException(
            "Lumens per lamp are expected to be a positive floating point number or -1",
            m_line_counter);
    }

    if (!string_to_positive_number(values[2], &m_candela_multiplier))
    {
        throw ParsingException(
            "Candela multiplier is expected to be a positive floating point number",
            m_line_counter);
    }

    if (!string_to_positive_number(values[3], &m_number_of_vertical_angles))
    {
        throw ParsingException(
            "Number of vertical angles is expected to be a positive integer number",
            m_line_counter);
    }

    if (!string_to_positive_number(values[4], &m_number_of_horizontal_angles))
    {
        throw ParsingException(
            "Number of horizontal angles is expected to be a positive integer number",
            m_line_counter);
    }

    if (!string_to_photometric_type(values[5], &m_photometric_type))
    {
        throw ParsingException(
            "Wrong photometric type: expected 1, 2 or 3",
            m_line_counter);
    }

    if (!string_to_units_type(values[6], &m_luminous_opening.m_units_type))
    {
        throw ParsingException(
            "Wrong units type for luminous opening: expected 1 or 2",
            m_line_counter);
    }

    if (!string_to_number(values[7], &m_luminous_opening.m_width))
    {
        throw ParsingException(
            "Wrong width for luminous opening: expected floating point number",
            m_line_counter);
    }

    if (!string_to_number(values[8], &m_luminous_opening.m_length))
    {
        throw ParsingException(
            "Wrong length for luminous opening: expected floating point number",
            m_line_counter);
    }

    if (!string_to_number(values[9], &m_luminous_opening.m_height))
    {
        throw ParsingException(
            "Wrong height for luminous opening: expected floating point number",
            m_line_counter);
    }

    check_empty(input_stream);

    values = parse_to_vector<std::string>(input_stream, 3);

    if (!string_to_positive_number(values[0], &m_ballast_factor))
    {
        throw ParsingException(
            "Ballast factor is expected to be a positive floating point number",
            m_line_counter);
    }

    if (!string_to_positive_number(values[1], &m_ballast_lamp_photometric_factor))
    {
        throw ParsingException(
            "Ballast-lamp photometric factor is expected to be a positive floating point number",
            m_line_counter);
    }

    if (!string_to_positive_number(values[2], &m_input_watts))
    {
        throw ParsingException(
            "Input watts are expected to be a positive floating point number",
            m_line_counter);
    }
}

void IESParser::parse_candela_values(std::istream& input_stream)
{
    m_candela_values.resize(m_number_of_horizontal_angles);
    for (std::vector<double>& i : m_candela_values)
    {
        try
        {
            i = parse_to_vector<double>(input_stream, m_number_of_vertical_angles);
        }
        catch (const ExceptionStringConversionError&)
        {
            throw ParsingException(
                "Error while parsing candela values: "
                "value is not a floating point number", m_line_counter);
        }
    }
}

void IESParser::parse_angles(std::istream& input_stream)
{
    // Parse vertical angles:

    try
    {
        m_vertical_angles = parse_to_vector<double>(input_stream, m_number_of_vertical_angles);
    }
    catch (const ExceptionStringConversionError&)
    {
        throw ParsingException(
            "Error while parsing vertical angles: "
            "value is not a floating point number", m_line_counter);
    }

    assert(m_photometric_type != PhotometricTypeNotSpecified);

    if (m_photometric_type == PhotometricTypeC)
    {
        if (!feq(m_vertical_angles.front(), 0.0) &&
            !feq(m_vertical_angles.front(), 90.0))
        {
            throw ParsingException(
                "With photometric type C, first vertical angle "
                "must be either 0 or 90 degrees", m_line_counter);
        }
        if (!feq(m_vertical_angles.back(), 90.0) &&
            !feq(m_vertical_angles.back(), 180.0))
        {
            throw ParsingException(
                "With photometric type C, last vertical angle "
                "must be either 90 or 180 degrees", m_line_counter);
        }
    }
    else if (m_photometric_type == PhotometricTypeA ||
             m_photometric_type == PhotometricTypeB)
    {
        if (!feq(m_vertical_angles.front(), -90.0) &&
            !feq(m_vertical_angles.front(), 0.0))
        {
            throw ParsingException(
                "With photometric types A and B, first vertical angle "
                "must be either -90 or 0 degrees", m_line_counter);
        }
        if (!feq(m_vertical_angles.back(), 90.0))
        {
            throw ParsingException(
                "With photometric types A and B, last vertical angle "
                "must be 90 degrees", m_line_counter);
        }
    }
    if (feq(m_vertical_angles.front(), m_vertical_angles.back()))
    {
        throw ParsingException(
            "First and last vertical angles must be different", m_line_counter);
    }
    if (!check_increasing_order(m_vertical_angles))
    {
        throw ParsingException(
            "Vertical angles must be mentioned in increasing order", m_line_counter);
    }

    // Parse horizontal angles:

    try
    {
        m_horizontal_angles = parse_to_vector<double>(input_stream, m_number_of_horizontal_angles);
    }
    catch (const ExceptionStringConversionError&)
    {
        throw ParsingException(
            "Error while parsing horizontal angles: "
            "value is not a floating point number", m_line_counter);
    }
    if (m_photometric_type == PhotometricTypeC)
    {
        if (feq(m_horizontal_angles.front(), 90.0))
        {
            if (feq(m_horizontal_angles.back(), 270.0))
            {
                m_symmetry = SymmetricHalvesY;
            }
            else
            {
                throw ParsingException(
                    "With photometric type C, if the first horizontal angle "
                    "is 90 degrees, then the last angle can be only 270 degrees",
                    m_line_counter);
            }
        }
        else if (feq(m_horizontal_angles.front(), 0.0))
        {
            if (feq(m_horizontal_angles.back(), 0.0))
            {
                m_symmetry = FullySymmetric;
            }
            else if (feq(m_horizontal_angles.back(), 90.0))
            {
                m_symmetry = SymmetricQuadrants;
            }
            else if (feq(m_horizontal_angles.back(), 180.0))
            {
                m_symmetry = SymmetricHalvesX;
            }
            else if (feq(m_horizontal_angles.back(), 360.0))
            {
                m_symmetry = NoSymmetry;
            }
            else
            {
                throw ParsingException(
                    "With photometric type C, the last horizontal angle "
                    "can be only 0, 90, 180 or 360 degrees", m_line_counter);
            }
        }
        else
        {
            throw ParsingException(
                "With photometric type C, the first horizontal angle "
                "can be only 0 or 90 degrees", m_line_counter);
        }
    }
    else if (m_photometric_type == PhotometricTypeA ||
             m_photometric_type == PhotometricTypeB)
    {
        if (feq(m_horizontal_angles.front(), 0.0) &&
            feq(m_horizontal_angles.back(), 90.0))
        {
            m_symmetry = SymmetricHalvesX;
        }
        else if (feq(m_horizontal_angles.front(), -90.0) &&
                 feq(m_horizontal_angles.back(), 90.0))
        {
            m_symmetry = NoSymmetry;
        }
        else
        {
            throw ParsingException(
                "With photometric types A and B, the first horizontal angle "
                "can be only 0 or -90 degrees, and the last horizontal "
                "angle must be 90 degrees", m_line_counter);
        }
    }
    if (!check_increasing_order(m_horizontal_angles))
    {
        throw ParsingException(
            "Horizontal angles must be mentioned in increasing order", m_line_counter);
    }
}

bool IESParser::is_keyword_line(const std::string& line)
{
    static const boost::regex Regex(KeywordLineRegex);
    return boost::regex_match(line, Regex);
}

bool IESParser::is_tilt_line(const std::string& line)
{
    static const boost::regex Regex(TiltLineRegex);
    return boost::regex_match(line, Regex);
}

void IESParser::parse_keyword_line(const std::string& line)
{
    static const boost::regex Regex(KeywordLineRegex);
    boost::smatch what;
    if (!boost::regex_match(line, what, Regex))
    {
        throw ParsingException("Keyword is expected", m_line_counter);
    }

    const std::string key = what[1];
    const std::string value = what[2];

    // Check if the specified standard allows this keyword.
    if (!m_ignore_allowed_keywords) accept_keyword(key);

    // Process MORE, BLOCK and ENDBLOCK keywords separately.
    // For all other keywords - just add them to dictionary.
    process_block_keyword(key);
    if (key == "MORE")
    {
        if (m_last_added_keyword == m_keywords_dictionary.end())
        {
            throw ParsingException(
                "Keyword MORE occured before any other keyword", m_line_counter);
        }
        m_last_added_keyword->second += ("\n" + value);
    }
    else
    {
        KeywordsDictionary::iterator it = m_keywords_dictionary.find(key);
        if (it != m_keywords_dictionary.end())
        {
            static const char* ErrorMsg = "Keyword {0} is duplicated";
            throw ParsingException(format(ErrorMsg, key).c_str(), m_line_counter);
        }
        m_keywords_dictionary[key] = value;
        m_last_added_keyword = m_keywords_dictionary.find(key);
    }
}

void IESParser::parse_tilt_line(const std::string& line)
{
    static const boost::regex Regex(TiltLineRegex);
    boost::smatch what;
    if (!boost::regex_match(line, what, Regex))
    {
        throw ParsingException("TILT line is expected", m_line_counter);
    }

    const std::string value = what[1];

    if (value == "INCLUDE")
        m_tilt_specification = IncludeTilt;
    else if (value == "NONE")
        m_tilt_specification = NoTilt;
    else if (!m_ignore_tilt)
    {
        throw NotSupportedException(
            "TILT specification from file is not supported", m_line_counter);
    }
}

void IESParser::process_block_keyword(const std::string& keyword)
{
    if (keyword == "ENDBLOCK" || keyword == "BLOCK")
    {
        throw NotSupportedException("Block keywords are not supported", m_line_counter);
    }
}

void IESParser::accept_keyword(const std::string& keyword)
{
    if (keyword.empty())
    {
        throw ParsingException("Keyword is empty", m_line_counter);
    }

    assert(m_format != UnknownFormat);

    switch (m_format)
    {
    case Format2002:
        if (!(keyword[0] == '_' || is_keyword_allowed_by_iesna02(keyword)))
        {
            static const char* ErrorMsg =
                "Keyword {0} is not allowed by IESNA LM-63-2002 standard";
            throw ParsingException(format(ErrorMsg, keyword).c_str(), m_line_counter);
        }
        break;
    case Format1995:
        if (!(keyword[0] == '_' || is_keyword_allowed_by_iesna95(keyword)))
        {
            static const char* ErrorMsg =
                "Keyword {0} is not allowed by IESNA LM-63-95 standard";
            throw ParsingException(format(ErrorMsg, keyword).c_str(), m_line_counter);
        }
        break;
    case Format1991:
        if (keyword[0] == '_')
        {
            throw ParsingException(
                "User keywords are not allowed by IESNA LM-63-91 standard",
                m_line_counter);
        }
        if (!is_keyword_allowed_by_iesna91(keyword))
        {
            static const char* ErrorMsg = "Keyword {0} is not allowed by IESNA LM-63-91 standard";
            throw ParsingException(format(ErrorMsg, keyword).c_str(), m_line_counter);
        }
        break;
    }
}

void IESParser::check_required_keywords() const
{
    assert(m_format != UnknownFormat);

    switch (m_format)
    {
    case Format2002:
        check_iesna02_required_keywords();
        break;
    case Format1995:
        // There are no required keywords in LM-63-95 standard.
        break;
    case Format1991:
        check_iesna91_required_keywords();
        break;
    default:
        break;
    }
}

void IESParser::check_iesna02_required_keywords() const
{
    static const char* RequiredKeywords[] =
    {
        "TEST",
        "TESTLAB",
        "ISSUEDATE",
        "MANUFAC"
    };
    for (size_t i = 0; i < countof(RequiredKeywords); ++i)
    {
        const char* required_keyword = RequiredKeywords[i];
        if (m_keywords_dictionary.count(required_keyword) == 0)
        {
            static const char* ErrorMsg =
                "Keyword {0}, required by IESNA LM-63-2002 standard, was not found";
            throw ParsingException(format(ErrorMsg, required_keyword).c_str(), m_line_counter);
        }
    }
}

void IESParser::check_iesna91_required_keywords() const
{
    static const char* RequiredKeywords[] =
    {
        "TEST",
        "MANUFAC"
    };
    for (size_t i = 0; i < countof(RequiredKeywords); ++i)
    {
        const char* required_keyword = RequiredKeywords[i];
        if (m_keywords_dictionary.count(required_keyword) == 0)
        {
            static const char* ErrorMsg =
                "Keyword {0}, required by IESNA LM-63-91 standard, was not found";
            throw ParsingException(format(ErrorMsg, required_keyword).c_str(), m_line_counter);
        }
    }
}

void IESParser::check_empty(std::istream& input_stream) const
{
    if (!input_stream)
        throw ParsingException("End of file is not expectied", m_line_counter);
    if (m_line.empty())
        throw ParsingException("Empty line is not expected", m_line_counter);
}

} // namespace foundation
