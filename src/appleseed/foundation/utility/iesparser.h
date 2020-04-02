
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

#pragma once

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/string/string.h"
#include "foundation/utility/test.h"

// Boost headers.
#include "boost/algorithm/string.hpp"

// Standard headers.
#include <string>
#include <unordered_map>
#include <vector>

DECLARE_TEST_CASE(Foundation_Utility_Iesparser, ParseFormat);
DECLARE_TEST_CASE(Foundation_Utility_Iesparser, ReadLine_IgnoreEmptyLines);
DECLARE_TEST_CASE(Foundation_Utility_Iesparser, ReadLine_DoNotIgnoreEmptyLines);
DECLARE_TEST_CASE(Foundation_Utility_Iesparser, CheckEmpty);
DECLARE_TEST_CASE(Foundation_Utility_Iesparser, ParseKeywords);
DECLARE_TEST_CASE(Foundation_Utility_Iesparser, ParseToVector_Empty);
DECLARE_TEST_CASE(Foundation_Utility_Iesparser, ParseToVector_Good);
DECLARE_TEST_CASE(Foundation_Utility_Iesparser, ParseToVector_Bad);

namespace foundation
{

//
// Class for parsing IESNA LM-63 Photometric Data File
// and storing its contents.
//

class IESParser
{
  public:
    // Constructor.
    IESParser();

    // Exception that can store erroneous line number.
    class Exception
      : public foundation::Exception
    {
      public:
        Exception(const char* message, const int line);

        virtual int line() const
        {
            return m_line_number;
        }

      protected:
        int m_line_number;
    };

    // Exception thrown when file format violates the IES specifications.
    class ParsingException : public Exception
    {
      public:
        ParsingException(const char* message, int line)
          : Exception(message, line)
        {
        }
    };

    // Exception thrown when the feature of IES file is not supported by this parser.
    class NotSupportedException : public Exception
    {
      public:
        NotSupportedException(const char* message, int line)
          : Exception(message, line)
        {
        }
    };

    // Dictionary containing IES file keywords and corresponding values.
    typedef std::unordered_map<std::string, std::string> KeywordsDictionary;

    // Type of IES file format.
    enum Format
    {
        UnknownFormat,
        Format1986,
        Format1991,
        Format1995,
        Format2002,
    };

    // How TILT information is specified in the file.
    enum TiltSpecification
    {
        IncludeTilt,                    // TILT is included in this IES file
        TiltFromFile,                   // TILT is specified in the separate file
        NoTilt                          // the lamp output does not vary as a function
                                        // of the luminaire tilt angle (and TILT is not specified)
    };

    // Lamp-to-luminaire geometry types.
    // For more information, see the IESNA LM-63 specifications
    enum LampToLuminaireGeometry
    {
        GeometryNotSpecified,
        VerticalGeometry,
        HorizontalInvariantGeometry,
        HorizontalNonInvariantGeometry
    };

    // Photometric types.
    enum PhotometricType
    {
        PhotometricTypeNotSpecified,
        PhotometricTypeC,
        PhotometricTypeB,
        PhotometricTypeA
    };

    // Types of symmetry that candela values have with respect to horziontal angle
    enum SymmetryType
    {
        NoSymmetry,
        SymmetricHalvesX,               // bilaterally symmetric about the 0-180 degree axis
        SymmetricHalvesY,               // bilaterally symmetric about the 90-270 degree axis
        SymmetricQuadrants,             // symmetric in each quadrant
        FullySymmetric                  // full rotational symmetry
    };

    // Shape of luminous opening.
    struct LuminousOpeningShape
    {
        enum UnitsType
        {
            Feet,
            Meters
        };

        UnitsType   m_units_type;
        double      m_width;
        double      m_length;
        double      m_height;
    };

    typedef std::vector<std::vector<double>> PhotometricGrid;

    //
    // Options.
    //

    bool m_ignore_allowed_keywords;     // if true, all keywors are allowed not depending on format version
    bool m_ignore_required_keywords;    // if true, some required keywors can be missing
    bool m_ignore_empty_lines;          // if true, than the file can contain whitespace lines
    bool m_ignore_tilt;                 // if true, TILT section is not parsed

    //
    // Main methods.
    //

    // Parse input stream containing IESNA LM-63 Photometric Data.
    void parse(std::istream& input_stream);

    // Check if the keyword is allowed by IESNA LM-63-2002 standard.
    static bool is_keyword_allowed_by_iesna02(const std::string& keyword);

    // Check if the keyword is allowed by IESNA LM-63-95 standard.
    static bool is_keyword_allowed_by_iesna95(const std::string& keyword);

    // Check if the keyword is allowed by IESNA LM-63-91 standard.
    static bool is_keyword_allowed_by_iesna91(const std::string& keyword);

    //
    // Getters.
    //

    Format get_format() const
    {
        return m_format;
    }

    LampToLuminaireGeometry get_lamp_to_luminaire_geometry() const
    {
        return m_lamp_to_luminaire_geometry;
    }

    const std::vector<double>& get_tilt_angles() const
    {
        return m_tilt_angles;
    }

    const std::vector<double>& get_tilt_multiplying_factors() const
    {
        return m_tilt_multiplying_factors;
    }

    int get_number_of_lamps() const
    {
        return m_number_of_lamps;
    }

    double get_lumens_per_lamp() const
    {
        return m_lumens_per_lamp;
    }

    bool is_absolute_photometry() const
    {
        return m_absolute_photometry;
    }

    double get_candela_multiplier() const
    {
        return m_candela_multiplier;
    }

    int get_number_of_vertical_angles() const
    {
        return m_number_of_vertical_angles;
    }

    int get_number_of_horizontal_angles() const
    {
        return m_number_of_horizontal_angles;
    }

    PhotometricType get_photometric_type() const
    {
        return m_photometric_type;
    }

    const LuminousOpeningShape& get_luminous_opening() const
    {
        return m_luminous_opening;
    }

    double get_ballast_factor() const
    {
        return m_ballast_factor;
    }

    double get_ballast_lamp_photometric_factor() const
    {
        return m_ballast_lamp_photometric_factor;
    }

    double get_input_watts() const
    {
        return m_input_watts;
    }

    SymmetryType get_symmetry() const
    {
        return m_symmetry;
    }

    const std::vector<double>& get_vertical_angles() const
    {
        return m_vertical_angles;
    }

    const std::vector<double>& get_horizontal_angles() const
    {
        return m_horizontal_angles;
    }

    const PhotometricGrid& get_candela_values() const
    {
        return m_candela_values;
    }

    const KeywordsDictionary& get_keywords_dictionary() const
    {
        return m_keywords_dictionary;
    }

    const std::string& get_keyword_value(const std::string& keyword) const
    {
        return m_keywords_dictionary.at(keyword);
    }

  private:
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_Iesparser, ParseFormat);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_Iesparser, ReadLine_IgnoreEmptyLines);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_Iesparser, ReadLine_DoNotIgnoreEmptyLines);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_Iesparser, CheckEmpty);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_Iesparser, ParseKeywords);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_Iesparser, ParseToVector_Empty);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_Iesparser, ParseToVector_Good);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_Iesparser, ParseToVector_Bad);

    static const char* const KeywordLineRegex;
    static const char* const TiltLineRegex;

    // Reset parser.
    void reset(std::istream& input_stream);

    // Read line and increase the counter.
    void read_line(std::istream& input_stream);

    // Read line, trim it and increase the counter.
    // When ignore_empty_lines set to true this method
    // ignores all lines consisting of whitespace characters.
    void read_trimmed_line(std::istream& input_stream);

    // Retrieve format version.
    // LM_63_1986 is set if this line is not one of supported version strings.
    void parse_format_version(std::istream& input_stream);

    // Parse all data before TILT=<...> line
    // and store keywords and corresponding values.
    void parse_keywords_and_tilt(std::istream& input_stream);

    // Parse TILT section.
    void parse_tilt_data(std::istream& input_stream);

    // Parse all photometric data after the TILT section
    void parse_photometric_data(std::istream& input_stream);

    void parse_angles(std::istream& input_stream);

    void parse_candela_values(std::istream& input_stream);

    // Check if the line defines a valid keyword-value pair.
    static bool is_keyword_line(const std::string& line);

    // Check if the line is a valid TILT=<...> line.
    static bool is_tilt_line(const std::string& line);

    // Parse the line containing keyword-value pair
    // and store this pair in the dictionary.
    void parse_keyword_line(const std::string& line);

    // Parse TILT=<...> line.
    void parse_tilt_line(const std::string& line);

    // Process BLOCK and ENDBLOCK keywords.
    void process_block_keyword(const std::string& keyword);

    // Check if the specified standard allows this keyword.
    void accept_keyword(const std::string& keyword);

    // Check if all required keywords were found.
    void check_required_keywords() const;

    void check_iesna02_required_keywords() const;

    void check_iesna91_required_keywords() const;

    // Check if line is not empty and EOF is not reached.
    void check_empty(std::istream& input_stream) const;

    // Read lines, split each line and write the values to the vector,
    // until the total number of values does not exceed the specified size.
    // If number of values in the last parsed line it too large, raise a ParsingException.
    // Can also raise a boost::bad_cast exception, when a token cannot be converted
    // to the value of specified type.
    template <typename ValueType>
    std::vector<ValueType> parse_to_vector(std::istream& input_stream, size_t count)
    {
        std::vector<ValueType> output;
        output.reserve(count);

        while (output.size() < count)
        {
            check_empty(input_stream);

            std::vector<std::string> tokens;
            boost::split(tokens, m_line, isspace, boost::token_compress_on);
            if (output.size() + tokens.size() > count)
            {
                const std::string expected_number_of_values = to_string(count - output.size());
                static const char* ErrorMsg = "Too many values in the line, expected {0}";
                throw ParsingException(
                    format(ErrorMsg, expected_number_of_values).c_str(), m_line_counter);
            }

            for (const std::string& token : tokens)
                output.push_back(from_string<ValueType>(token));

            read_trimmed_line(input_stream);
        }

        return output;
    }

    //
    // All information retrieved from file:
    //

    // File format.
    Format                          m_format;

    // TILT specification.
    TiltSpecification               m_tilt_specification;
    std::string                     m_tilt_specification_filename;

    // TILT section.
    LampToLuminaireGeometry         m_lamp_to_luminaire_geometry;
    std::vector<double>             m_tilt_angles;
    std::vector<double>             m_tilt_multiplying_factors;

    // Photometric information.
    int                             m_number_of_lamps;
    double                          m_lumens_per_lamp;
    bool                            m_absolute_photometry;
    double                          m_candela_multiplier;
    int                             m_number_of_vertical_angles;
    int                             m_number_of_horizontal_angles;
    PhotometricType                 m_photometric_type;
    LuminousOpeningShape            m_luminous_opening;
    double                          m_ballast_factor;
    double                          m_ballast_lamp_photometric_factor;
    double                          m_input_watts;
    SymmetryType                    m_symmetry;
    std::vector<double>             m_vertical_angles;
    std::vector<double>             m_horizontal_angles;
    PhotometricGrid                 m_candela_values;

    // Keywords.
    KeywordsDictionary              m_keywords_dictionary;

    //
    // Variables that describe the current parsing state:
    //

    KeywordsDictionary::iterator    m_last_added_keyword;
    int                             m_line_counter;
    std::string                     m_line;
};

}   // namespace foundation
