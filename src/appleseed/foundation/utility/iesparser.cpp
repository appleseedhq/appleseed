
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Artem Bishev, The appleseedhq Organization
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

// Boost headers.
#include "boost/lexical_cast.hpp"
#include "boost/regex.hpp"
#include "boost/algorithm/string.hpp"
#include "boost/algorithm/cxx11/any_of.hpp"

// Standard headers.
#include <string>
#include <vector>

// Foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/utility/countof.h"
#include "foundation/utility/foreach.h"

namespace foundation
{

const char* const IESParser::KEYWORD_LINE_REGEX = "\\[(\\w*)\\]\\s*(\\S.*)?";
const char* const IESParser::TILT_LINE_REGEX = "TILT\\s*=\\s*(\\S.*)";

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
            *value = boost::lexical_cast<ValueType>(str);
        }
        catch (boost::bad_lexical_cast&)
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
            *value = boost::lexical_cast<ValueType>(str);
        }
        catch (boost::bad_lexical_cast&)
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

        for (each< std::vector<std::string> > token = tokens; ++token; token)
        {
            try
            {
                output->push_back(boost::lexical_cast<ValueType>(*token));
            }
            catch (boost::bad_lexical_cast&)
            {
                return false;
            }
        }

        return true;
    }

    template <typename ValueType>
    bool check_increasing_order(const std::vector<ValueType>& values)
    {
        for (int i = 1; i < values.size(); ++i)
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

IESParser::IESParser() :
    ignore_allowed_keywords(false),
    ignore_required_keywords(false),
    ignore_empty_lines(true),
    ignore_tilt(false)
{}


void IESParser::parse(std::istream& input_stream)
{
    reset(input_stream);

    // Parse version.
    parse_format_version(input_stream);

    // Parse block before TILT and TILT line.
    parse_keywords_and_tilt(input_stream);

    // Parse TILT data if it is included.
    if (tilt_specification == IncludeTilt)
    {
        parse_tilt_data(input_stream);
    }

    // Parse the rest of the data.
    parse_photometric_data(input_stream);
    parse_angles(input_stream);
    parse_candela_values(input_stream);

    if (!input_stream.eof())
    {
        throw ParsingException("Expected end of file", line_counter);
    }
}


bool IESParser::is_keyword_allowed_by_iesna02(const std::string& keyword)
{
    if (keyword == "TEST" ||
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
        keyword == "MORE")
    {
        return true;
    }
    return false;
}


bool IESParser::is_keyword_allowed_by_iesna95(const std::string& keyword)
{
    if (keyword == "TEST" ||
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
        keyword == "ENDBLOCK")
    {
        return true;
    }
    return false;
}


bool IESParser::is_keyword_allowed_by_iesna91(const std::string& keyword)
{
    if (keyword == "TEST" ||
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
        keyword == "MORE")
    {
        return true;
    }
    return false;
}


void IESParser::reset(std::istream& input_stream)
{
    line_counter = 0;
    line = "";
    read_trimmed_line(input_stream);
    keywords_dictionary.clear();
    last_added_keyword = keywords_dictionary.end();
}


void IESParser::read_line(std::istream& input_stream)
{
    line_counter++;
    std::getline(input_stream, line);
}


void IESParser::read_trimmed_line(std::istream& input_stream)
{
    do
    {
        read_line(input_stream);
        boost::trim(line);
    } while (ignore_empty_lines && input_stream && line.empty());
}


void IESParser::parse_format_version(std::istream& input_stream)
{
    check_empty(input_stream);

    if (line == "IESNA91")
    {
        format = Format1991;
    }
    else if (line == "IESNA:LM-63-1995")
    {
        format = Format1995;
    }
    else if (line == "IESNA:LM-63-2002")
    {
        format = Format2002;
    }
    else
    {
        // The first line is not a format line,
        // which is possible only when the format is LM-63-1986.
        format = Format1986;
        // Do not read the next line, since the current one is not processed:
        return;
    }

    read_trimmed_line(input_stream);
}


void IESParser::parse_keywords_and_tilt(std::istream& input_stream)
{
    last_added_keyword = keywords_dictionary.end();

    bool tilt_reached = false;
    while (!tilt_reached)
    {
        check_empty(input_stream);

        if (is_keyword_line(line))
        {
            parse_keyword_line(line);
        }
        else if (is_tilt_line(line))
        {
            if (!ignore_required_keywords) check_required_keywords();
            tilt_reached = true;
            parse_tilt_line(line);
        }
        else
        {
            if (format != Format1986)
            {
                // LM-63-1986 format allows arbitrary data until TILT=<...> is reached.
                // In any other case, raise the parsing exception:
                throw ParsingException("Expected keyword line or TILT line", line_counter);
            }
        }

        read_trimmed_line(input_stream);
    }
}


void IESParser::parse_tilt_data(std::istream& input_stream)
{
    check_empty(input_stream);

    // parse lamp-to-luminaire geometry
    if (line == "1")
        lamp_to_luminaire_geometry = VerticalGeometry;
    else if (line == "2")
        lamp_to_luminaire_geometry = HorizontalInvariantGeometry;
    else if (line == "3")
        lamp_to_luminaire_geometry = HorizontalNonInvariantGeometry;
    else
    {
        throw ParsingException(
            "Wrong lamp-to-luminaire geometry value. 1, 2 or 3 was expected.", line_counter);
    }

    read_trimmed_line(input_stream);

    check_empty(input_stream);

    // parse number of tilt entries
    int number_of_tilt_entries;
    try
    {
        number_of_tilt_entries = boost::lexical_cast<int>(line);
    }
    catch (boost::bad_lexical_cast&)
    {
        throw ParsingException(
            "Wrong number of tilt entries. An integer was expected.", line_counter);
    }
    if (number_of_tilt_entries < 0)
    {
        throw ParsingException(
            "Wrong number of tilt entries. "
            "Number of tilt entries must be non-negative.", line_counter);
    }

    read_trimmed_line(input_stream);

    check_empty(input_stream);

    // parse tilt angles
    if (!string_to_vector<double>(line, &tilt_angles))
    {
        throw ParsingException(
            "Error while parsing tilt angles: "
            "value is not a floating point number", line_counter);
    }
    if (tilt_angles.size() != number_of_tilt_entries)
    {
        throw ParsingException(
            "The specified number of tilt entries does not match "
            "the actual number of entries in the line", line_counter);
    }

    read_trimmed_line(input_stream);

    check_empty(input_stream);

    // parse tilt multipliers
    if (!string_to_vector<double>(line, &tilt_multiplying_factors))
    {
        throw ParsingException(
            "Error while parsing tilt multipliers: "
            "value is not a floating point number", line_counter);
    }
    if (tilt_multiplying_factors.size() != number_of_tilt_entries)
    {
        throw ParsingException(
            "The specified number of tilt entries does not match "
            "the actual number of tilt multipliers in the line", line_counter);
    }
    if (boost::algorithm::any_of(tilt_multiplying_factors, is_negative<double>))
    {
        throw ParsingException(
            "Error while parsing tilt multipliers: "
            "value must be non-negative", line_counter);
    }

    read_trimmed_line(input_stream);
}


void IESParser::parse_photometric_data(std::istream& input_stream)
{
    std::vector<std::string> values =
        parse_to_vector<std::string>(input_stream, 10);

    if (!string_to_positive_number(values[0], &number_of_lamps))
    {
        throw ParsingException(
            "Number of lamps expected to be a positive integer number",
            line_counter);
    }

    absolute_photometry = false;
    if (values[1] == "-1")
    {
        lumens_per_lamp = -1.0;
        absolute_photometry = true;
    }
    else if (!string_to_positive_number(values[1], &lumens_per_lamp))
    {
        throw ParsingException(
            "Lumens per lamp expected to be a positive floating point number or -1",
            line_counter);
    }

    if (!string_to_positive_number(values[2], &candela_multiplier))
    {
        throw ParsingException(
            "Candela multiplier expected to be a positive floating point number",
            line_counter);
    }

    if (!string_to_positive_number(values[3], &number_of_vertical_angles))
    {
        throw ParsingException(
            "Number of vertical angles expected to be a positive integer number",
            line_counter);
    }

    if (!string_to_positive_number(values[4], &number_of_horizontal_angles))
    {
        throw ParsingException(
            "Number of horizontal angles expected to be a positive integer number",
            line_counter);
    }

    if (!string_to_photometric_type(values[5], &photometric_type))
    {
        throw ParsingException(
            "Wrong photometric type specified. Suppored values are 1, 2 or 3.",
            line_counter);
    }

    if (!string_to_units_type(values[6], &luminous_opening.units_type))
    {
        throw ParsingException(
            "Wrong units type specified for luminous opening. Suppored values are 1 or 2.",
            line_counter);
    }

    if (!string_to_number(values[7], &luminous_opening.width))
    {
        throw ParsingException(
            "Wrong width specified for luminous opening. Expected floating point number.",
            line_counter);
    }

    if (!string_to_number(values[8], &luminous_opening.length))
    {
        throw ParsingException(
            "Wrong length specified for luminous opening. Expected floating point number.",
            line_counter);
    }

    if (!string_to_number(values[9], &luminous_opening.height))
    {
        throw ParsingException(
            "Wrong height specified for luminous opening. Expected floating point number.",
            line_counter);
    }

    check_empty(input_stream);

    values = parse_to_vector<std::string>(input_stream, 3);

    if (!string_to_positive_number(values[0], &ballast_factor))
    {
        throw ParsingException(
            "Ballast factor expected to be a positive floating point number",
            line_counter);
    }

    if (!string_to_positive_number(values[1], &ballast_lamp_photometric_factor))
    {
        throw ParsingException(
            "Ballast-lamp photometric factor expected to be a positive floating point number",
            line_counter);
    }

    if (!string_to_positive_number(values[2], &input_watts))
    {
        throw ParsingException(
            "Input Watts expected to be a positive floating point number",
            line_counter);
    }
}


void IESParser::parse_candela_values(std::istream& input_stream)
{
    candela_values.resize(number_of_horizontal_angles);
    for (each<PhotometricGrid> i = candela_values; i; ++i)
    {
        try
        {
            *i = parse_to_vector<double>(input_stream, number_of_vertical_angles);
        }
        catch (boost::bad_lexical_cast&)
        {
            throw ParsingException("Candela values"
                " must be floating point numbers", line_counter);
        }
    }
}


void IESParser::parse_angles(std::istream& input_stream)
{
    // Parse vertical angles:

    try
    {
        vertical_angles = parse_to_vector<double>(input_stream, number_of_vertical_angles);
    }
    catch (boost::bad_lexical_cast&)
    {
        throw ParsingException("Vertical angles must be floating point numbers", line_counter);
    }

    assert(photometric_type != PhotometricTypeNotSpecified);
    if (photometric_type == PhotometricTypeC)
    {
        if (!feq(vertical_angles.front(), 0.0) &&
            !feq(vertical_angles.front(), 90.0))
        {
            throw ParsingException("With photometric type C,"
                "first vertical angle must be either 0 or 90 degrees", line_counter);
        }
        if (!feq(vertical_angles.back(), 90.0) &&
            !feq(vertical_angles.back(), 180.0))
        {
            throw ParsingException("With photometric type C,"
                "last vertical angle must be either 90 or 180 degrees", line_counter);
        }
    }
    else if (photometric_type == PhotometricTypeA ||
        photometric_type == PhotometricTypeB)
    {
        if (!feq(vertical_angles.front(), -90.0) &&
            !feq(vertical_angles.front(), 0.0))
        {
            throw ParsingException("With photometric type A or B,"
                "first vertical angle must be either -90 or 0 degrees", line_counter);
        }
        if (!feq(vertical_angles.back(), 90.0))
        {
            throw ParsingException("With photometric type A or B,"
                "last vertical angle must be 90 degrees", line_counter);
        }
    }
    if (feq(vertical_angles.front(), vertical_angles.back()))
    {
        throw ParsingException("First and last vertical angles must be different", line_counter);
    }
    if (!check_increasing_order(vertical_angles))
    {
        throw ParsingException("Vertical angles"
            " must be mentioned in increasing order", line_counter);
    }

    // Parse horizontal angles:

    try
    {
        horizontal_angles = parse_to_vector<double>(input_stream, number_of_horizontal_angles);
    }
    catch (boost::bad_lexical_cast&)
    {
        throw ParsingException("Horizontal angles must be floating point numbers", line_counter);
    }
    if (photometric_type == PhotometricTypeC)
    {
        if (feq(horizontal_angles.front(), 90.0))
        {
            if (feq(horizontal_angles.back(), 270.0))
            {
                symmetry = SymmetricHalvesY;
            }
            else
            {
                throw ParsingException("With photometric type C, if the first"
                    " horizontal angle is 90 degrees, then the last angle can be only 270 degrees",
                    line_counter);
            }
        }
        else if (feq(horizontal_angles.front(), 0.0))
        {
            if (feq(horizontal_angles.back(), 0.0))
            {
                symmetry = FullySymmetric;
            }
            else if (feq(horizontal_angles.back(), 90.0))
            {
                symmetry = SymmetricQuadrants;
            }
            else if (feq(horizontal_angles.back(), 180.0))
            {
                symmetry = SymmetricHalvesX;
            }
            else if (feq(horizontal_angles.back(), 360.0))
            {
                symmetry = NoSymmetry;
            }
            else
            {
                throw ParsingException("With photometric type C, the last horizontal angle"
                    " can be only 0, 90, 180 or 360 degrees", line_counter);
            }
        }
        else
        {
            throw ParsingException("With photometric type C, the first horizontal angle"
                " can be only 0 or 90 degrees", line_counter);
        }
    }
    else if (photometric_type == PhotometricTypeA ||
        photometric_type == PhotometricTypeB)
    {
        if (feq(horizontal_angles.front(), 0.0) &&
            feq(horizontal_angles.back(), 90.0))
        {
            symmetry = SymmetricHalvesX;
        }
        else if (feq(horizontal_angles.front(), -90.0) &&
            feq(horizontal_angles.back(), 90.0))
        {
            symmetry = NoSymmetry;
        }
        else
        {
            throw ParsingException("With photometric type C, the first horizontal angle"
                " can be only 0 or -90 degrees, the last horizontal angle should be 90 degrees",
                line_counter);
        }
    }
    if (!check_increasing_order(horizontal_angles))
    {
        throw ParsingException("Horizontal angles"
            " must be mentioned in increasing order", line_counter);
    }
}


bool IESParser::is_keyword_line(const std::string& line)
{
    boost::regex regex(KEYWORD_LINE_REGEX);
    return boost::regex_match(line, regex);
}


bool IESParser::is_tilt_line(const std::string& line)
{
    boost::regex regex(TILT_LINE_REGEX);
    return boost::regex_match(line, regex);
}


void IESParser::parse_keyword_line(const std::string& line)
{
    boost::regex regex(KEYWORD_LINE_REGEX);
    boost::smatch what;
    if (!boost::regex_match(line, what, regex))
    {
        throw ParsingException("Keyword is expected", line_counter);
    }

    std::string key = what[1];
    std::string value = what[2];

    // Check if the specified standard allows this keyword.
    if (!ignore_allowed_keywords) accept_keyword(key);

    // Process MORE, BLOCK and ENDBLOCK keywords separately.
    // For all other keywords - just add them to dictionary.
    process_block_keyword(key);
    if (key == "MORE")
    {
        if (last_added_keyword == keywords_dictionary.end())
        {
            throw ParsingException(
                "Keyword MORE occured before any other keyword", line_counter);
        }
        last_added_keyword->second += ("\n" + value);
    }
    else
    {
        KeywordsDictionary::iterator it = keywords_dictionary.find(key);
        if (it != keywords_dictionary.end())
        {
            throw ParsingException(
                ("Keyword " + key + " is duplicated").c_str(), line_counter);
        }
        keywords_dictionary[key] = value;
        last_added_keyword = keywords_dictionary.find(key);
    }
}


void IESParser::parse_tilt_line(const std::string& line)
{
    boost::regex regex(TILT_LINE_REGEX);
    boost::smatch what;
    if (!boost::regex_match(line, what, regex))
    {
        throw ParsingException("TILT line is expected", line_counter);
    }

    std::string value = what[1];

    if (value == "INCLUDE")
        tilt_specification = IncludeTilt;
    else if (value == "NONE")
        tilt_specification = NoTilt;
    else if (!ignore_tilt)
    {
        throw NotSupportedException(
            "TILT specification from file is not supported", line_counter);
    }
}


void IESParser::process_block_keyword(const std::string& keyword)
{
    if (keyword == "ENDBLOCK" || keyword == "BLOCK")
    {
        throw NotSupportedException("Block keywords are not supported", line_counter);
    }
}


void IESParser::accept_keyword(const std::string& keyword)
{
    if (keyword.empty())
    {
        throw ParsingException("Keyword is empty", line_counter);
    }

    assert(format != UnknownFormat);

    switch (format)
    {
    case Format2002:
        if (!(keyword[0] == '_' || is_keyword_allowed_by_iesna02(keyword)))
        {
            throw ParsingException(
                ("Keyword " + keyword + " is not allowed by IESNA LM-63-2002 standard").c_str(),
                line_counter);
        }
        break;
    case Format1995:
        if (!(keyword[0] == '_' || is_keyword_allowed_by_iesna95(keyword)))
        {
            throw ParsingException(
                ("Keyword " + keyword + " is not allowed by IESNA LM-63-95 standard").c_str(),
                line_counter);
        }
        break;
    case Format1991:
        if (keyword[0] == '_')
        {
            throw ParsingException(
                "User keywords are not allowed by IESNA LM-63-91 standard",
                line_counter);
        }
        if (!is_keyword_allowed_by_iesna91(keyword))
        {
            throw ParsingException(
                ("Keyword " + keyword + " is not allowed by IESNA LM-63-91 standard").c_str(),
                line_counter);
        }
        break;
    }
}


void IESParser::check_required_keywords() const
{
    assert(format != UnknownFormat);

    switch (format)
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
    const char* REQUIRED_KEYWORDS[] =
    {
        "TEST",
        "TESTLAB",
        "ISSUEDATE",
        "MANUFAC"
    };
    for (size_t i = 0; i < countof(REQUIRED_KEYWORDS); ++i)
    {
        const char*& required_keyword = REQUIRED_KEYWORDS[i];
        if (keywords_dictionary.count(required_keyword) == 0)
        {
            throw ParsingException(
                (std::string("Keyword ") + required_keyword +
                ", required by IESNA LM-63-91 standard, was not found").c_str(),
                line_counter);
        }
    }
}


void IESParser::check_iesna91_required_keywords() const
{
    const char* REQUIRED_KEYWORDS[] =
    {
        "TEST",
        "MANUFAC"
    };
    for (size_t i = 0; i < countof(REQUIRED_KEYWORDS); ++i)
    {
        const char*& required_keyword = REQUIRED_KEYWORDS[i];
        if (keywords_dictionary.count(required_keyword) == 0)
        {
            throw ParsingException(
                (std::string("Keyword ") + required_keyword +
                 ", required by IESNA LM-63-91 standard, was not found").c_str(),
                line_counter);
        }
    }
}


void IESParser::check_empty(std::istream& input_stream) const
{
    if (!input_stream)
        throw ParsingException("End of file is not expectied", line_counter);
    if (line.empty())
        throw ParsingException("Empty line is not expected", line_counter);
}

} // namespace foundation
