
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Esteban Tovagliari, The appleseedhq Organization
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
#include "appleseedprojectfilereader.h"

// appleseed.renderer headers.
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/camera/camerafactoryregistrar.h"
#include "renderer/modeling/camera/icamerafactory.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/project/configurationcontainer.h"
#include "renderer/modeling/project/eventcounters.h"
#include "renderer/modeling/project/configuration.h"
#include "renderer/modeling/project/configurationcontainer.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/project/projectfilereader.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/utility/searchpaths.h"

// Standard headers.
#include <cstdio>
#include <string>

using namespace foundation;

namespace renderer
{
namespace
{

const char* gEmptyStr = "";

class StringView final
{
  public:
    StringView()
      : m_chars(gEmptyStr)
      , m_size(0)
    {
    }

    StringView(const char* str, const std::size_t len)
      : m_chars(str)
      , m_size(len)
    {
        assert(str);
        m_chars = str;
        m_size = len;
    }

    StringView(const char* str)
    {
        assert(str);
        m_chars = str;
        m_size = std::strlen(str);
    }

    StringView(const std::string& str)
      : StringView(str.c_str(), str.length())
    {
    }

    const char* c_str() const
    {
        return m_chars;
    }

    bool empty() const
    {
        return m_size == 0;
    }

    std::size_t lenght() const
    {
        return m_size;
    }

    bool operator==(const StringView& other) const
    {
        if (m_size == other.m_size)
        {
            if (m_size)
                return std::memcmp(m_chars, other.m_chars, m_size) == 0;

            return true;
        }

        return false;
    }

    bool operator==(const char* other) const
    {
        if (c_str() == other)
            return true;

        return std::strcmp(c_str(), other) == 0;
    }

    bool operator!=(const StringView& other) const
    {
        return !(*this == other);
    }

    bool operator!=(const char* other) const
    {
        return !(*this == other);
    }

    operator std::string() const
    {
        return std::string(c_str(), lenght());
    }

  private:
    const char* m_chars;
    std::size_t m_size;
};

class ParseException
{
  public:
    ParseException(
        const std::string&  message,
        const std::size_t   line,
        const std::size_t   col)
    {
        m_msg =
            std::string("Parse error, line ") + to_string(line) +
            ", pos " + to_string(col) +
            ", error: " + message;
    }

    const char* what() const noexcept
    {
        return m_msg.c_str();
    }

  private:
    std::string m_msg;
};

class Lexer
{
  public:
    explicit Lexer(FILE* file)
      : m_file(file)
      , m_has_unget_token(false)
      , m_line(1)
      , m_col(0)
    {
    }

    // non copyable.
    Lexer(const Lexer&) = delete;
    Lexer& operator=(const Lexer&) = delete;

    std::size_t line() const
    {
        return m_line;
    }

    std::size_t col() const
    {
        return m_col;
    }

    StringView next_token()
    {
        if (m_has_unget_token)
        {
            m_has_unget_token = false;
            return StringView(m_unget_token);
        }

        m_token.clear();
        skip_whitespace();

        while (true)
        {
            char c;
            if (!get_char(c))
                break;

            if (std::isspace(c))
                break;

            m_token.push_back(c);
        }

        return StringView(m_token);
    }

    void unget_token(StringView token)
    {
        assert(!m_has_unget_token);
        m_unget_token = std::string(token.c_str(), token.lenght());
        m_has_unget_token = true;
    }

    bool next_token_is(const char* str)
    {
        const auto t = next_token();
        if (!t.empty())
        {
            if (t == str)
                return true;

            unget_token(t);
        }

        return false;
    }

    StringView get_string_literal()
    {
        if (m_has_unget_token)
        {
            m_token = m_unget_token;
            m_has_unget_token = false;
        }
        else
            m_token.clear();

        skip_whitespace();

        while (true)
        {
            char c;
            if (!get_char(c))
                break;

            if (is_newline(c))
                break;

            m_token.push_back(c);
        }

        return StringView(m_token);
    }

    template <typename T>
    T get_value()
    {
        try
        {
            return from_string<T>(next_token().c_str());
        }
        catch (const ExceptionStringConversionError&)
        {
            throw_error("Conversion error!!!");
            return T();
        }
    }

    void skip_line()
    {
        while (true)
        {
            char c;

            if (!get_char(c))
                break;

            if (is_newline(c))
                break;
        }
    }

  private:
    FILE*       m_file;
    std::string m_token;
    std::string m_unget_token;
    bool        m_has_unget_token;
    std::size_t m_line;
    std::size_t m_col;

    bool get_char(char& c)
    {
        const int next_c = std::fgetc(m_file);
        if (next_c == EOF)
            return false;

        c = next_c;
        if (is_newline(c))
        {
            ++m_line;
            m_col = 0;
        }
        else
            ++m_col;

        return true;
    }

    bool is_newline(char c)
    {
        // TODO: handle windows end of lines here.
        return c == '\n';
    }

    void skip_whitespace()
    {
        while (true)
        {
            const int next_c = std::fgetc(m_file);
            if (next_c == EOF)
                break;

            const char c = next_c;
            if (!std::isspace(c))
            {
                std::ungetc(next_c, m_file);
                break;
            }

            if (is_newline(c))
            {
                ++m_line;
                m_col = 0;
            }
            else
                ++m_col;
        }
    }

    void throw_error(const std::string& message)
    {
        throw ParseException(
            message,
            line(),
            col());
    }
};

class Parser
{
  public:
    Parser(
        const char* project_filepath,
        Lexer&      lexer,
        const int   options)
      : m_lexer(lexer)
      , m_project_filepath(project_filepath)
      , m_options(options)
    {
    }

    auto_release_ptr<Project> parse()
    {
        while (true)
        {
            const auto token = m_lexer.next_token();

            if (token.empty())
                break;

            // Handle comments.
            if (token == "#")
            {
                m_lexer.skip_line();
                continue;
            }

            if (token == "project")
            {
                if (m_project)
                    throw_error("Multiple project definitions");

                parse_project();
                continue;
            }

            throw_error(std::string("Unexpected token ") + token.c_str());
        }

        if (!m_project)
            throw_error("No project in file");

        return m_project;
    }

  private:
    Lexer&                      m_lexer;
    const char*                 m_project_filepath;
    const int                   m_options;

    auto_release_ptr<Project>   m_project;

    void parse_project()
    {
        const auto name = m_lexer.get_string_literal();
        if (name.empty())
            throw_error("Missing project name");

        validate_identifier(name);
        m_project = ProjectFactory::create(name.c_str());
        m_project->set_path(m_project_filepath);

        if (!m_lexer.next_token_is("{"))
            throw_error(std::string("Unexpected token ") + m_lexer.next_token().c_str());

        if (!m_lexer.next_token_is("format_revision"))
            throw_error("Missing format_revision entry");

        const auto project_revision = m_lexer.get_value<std::size_t>();
        if (project_revision == 0)
            throw_error("Bad error!!!");

        while (true)
        {
            const auto& token = m_lexer.next_token();

            if (token == "}")
                break;

            if (token == "search_paths")
            {
                parse_search_paths();
                continue;
            }

            if (token == "scene")
            {
                if (m_project->get_scene())
                    throw_error("Multiple scene definitions");

                parse_scene();
                continue;
            }

            if (token == "frame")
            {
                if (m_project->get_frame())
                    throw_error("Multiple frame definitions");

                parse_frame();
                continue;
            }

            if (token == "configurations")
            {
                //if (!m_project->configurations().empty())
                //    throw_error("Multiple configurations definitions");

                parse_configurations();
                continue;
            }

            throw_error(std::string("Unexpected token ") + token.c_str());
        }
    }

    void parse_search_paths()
    {
        if (!m_lexer.next_token_is("{"))
            throw_error(std::string("Unexpected token ") + m_lexer.next_token().c_str());

        while (true)
        {
            const auto path = m_lexer.get_string_literal();

            if (path == "}")
                break;

            if (path.empty())
                continue;

            if (!(m_options & ProjectFileReader::OmitSearchPaths))
                m_project->search_paths().push_back_explicit_path(path.c_str());
        }
    }

    void parse_scene()
    {
        if (!m_lexer.next_token_is("{"))
            throw_error(std::string("Unexpected token ") + m_lexer.next_token().c_str());

        m_project->set_scene(SceneFactory::create());

        while (true)
        {
            const auto& token = m_lexer.next_token();

            if (token == "}")
                break;

            if (token == "camera")
            {
                parse_camera();
                continue;
            }

            throw_error(std::string("Unexpected token ") + token.c_str());
        }
    }

    void parse_camera()
    {
        const std::string model = m_lexer.next_token();

        const CameraFactoryRegistrar cameraFactoryRegistrar;
        const auto *cameraFactory = cameraFactoryRegistrar.lookup(model.c_str());
        if (!cameraFactory)
            throw_error(std::string("Unknown camera model ") + model.c_str());

        const std::string name = m_lexer.get_string_literal();
        if (name.empty())
            throw_error("Missing camera name");

        validate_identifier(name);

        if (!m_lexer.next_token_is("{"))
            throw_error(std::string("Unexpected token ") + m_lexer.next_token().c_str());

        ParamArray params;
        TransformSequence transformSequence;

        while (true)
        {
            const auto& token = m_lexer.next_token();

            if (token == "}")
                break;

            if (token == "params")
            {
                parse_params(params);
                continue;
            }

            if (token == "transform_sequence")
            {
                parse_transform_sequence(transformSequence);
                continue;
            }

            throw_error(std::string("Unexpected token ") + token.c_str());
        }

        auto camera = cameraFactory->create(name.c_str(), params);
        camera->transform_sequence() = transformSequence;
        m_project->get_scene()->cameras().insert(camera);
    }

    void parse_frame()
    {
        const auto name = m_lexer.get_string_literal();
        if (name.empty())
            throw_error("Missing frame name");

        validate_identifier(name);
        m_project->set_frame(FrameFactory::create(name.c_str(), ParamArray()));

        if (!m_lexer.next_token_is("{"))
            throw_error(std::string("Unexpected token ") + m_lexer.next_token().c_str());

        while (true)
        {
            const auto& token = m_lexer.next_token();

            if (token == "}")
                break;

            throw_error(std::string("Unexpected token ") + token.c_str());
        }
    }

    void parse_configurations()
    {
        if (!m_lexer.next_token_is("{"))
            throw_error(std::string("Unexpected token ") + m_lexer.next_token().c_str());

        while (true)
        {
            const auto& token = m_lexer.next_token();

            if (token == "}")
                break;

            if (token == "configuration")
            {
                parse_configuration();
                continue;
            }

            throw_error(std::string("Unexpected token ") + token.c_str());
        }
    }

    void parse_configuration()
    {
        const std::string name = m_lexer.get_string_literal();
        if (name.empty())
            throw_error("Missing configuration name");

        validate_identifier(StringView(name.c_str(), name.length()));

        if (!m_lexer.next_token_is("{"))
            throw_error(std::string("Unexpected token ") + m_lexer.next_token().c_str());

        std::string base_name;
        ParamArray params;

        while (true)
        {
            const auto& token = m_lexer.next_token();

            if (token == "}")
                break;

            if (token == "base")
            {
                base_name = m_lexer.get_string_literal();
                if (base_name.empty())
                    throw_error("Missing configuration base name");

                validate_identifier(StringView(base_name.c_str(), base_name.length()));
                continue;
            }

            if (token == "params")
            {
                parse_params(params);
                continue;
            }

            throw_error(std::string("Unexpected token ") + token.c_str());
        }

       auto configuration =
            ConfigurationFactory::create(
                name.c_str(),
                params);

        // Handle configuration inheritance.
        if (!base_name.empty())
        {
            const Configuration* base =
                m_project->configurations().get_by_name(base_name.c_str());

            if (base)
                configuration->set_base(base);
            else
                throw_error(std::string("configuration ") + base_name + " does not exist");
        }

        m_project->configurations().insert(configuration);
    }

    void parse_params(Dictionary& params)
    {
        if (!m_lexer.next_token_is("{"))
            throw_error(std::string("Unexpected token ") + m_lexer.next_token().c_str());

        while (true)
        {
            if (m_lexer.next_token_is("}"))
                break;

            const std::string key = m_lexer.next_token();
            validate_identifier(key);

            const std::string value = m_lexer.get_string_literal();

            if (value == "{")
            {
                m_lexer.unget_token(value);
                Dictionary dict;
                parse_params(dict);

                if (!dict.empty())
                    params.dictionaries().insert(key.c_str(), dict);
            }
            else
            {
                if (value.empty())
                    throw_error("Premature end of file");

                params.insert(key.c_str(), value.c_str());
            }
        }
    }

    void parse_transform_sequence(TransformSequence& transformSequence)
    {
        if (!m_lexer.next_token_is("{"))
            throw_error(std::string("Unexpected token ") + m_lexer.next_token().c_str());

        while (true)
        {
            const auto& token = m_lexer.next_token();

            if (token == "}")
                break;

            if (token == "matrix")
            {
                parse_matrix(transformSequence);
                continue;
            }

            throw_error(std::string("Unexpected token ") + token.c_str());
        }
    }

    void parse_matrix(TransformSequence& transformSequence)
    {
        const auto time = m_lexer.get_value<float>();

        Matrix4d matrix;
        parse_matrix(matrix);

        transformSequence.set_transform(time, Transformd(matrix));
    }

    void parse_matrix(Matrix4d& matrix)
    {
        if (!m_lexer.next_token_is("{"))
            throw_error(std::string("Unexpected token ") + m_lexer.next_token().c_str());

        for (std::size_t i = 0; i < 16; ++i)
            matrix[i] = m_lexer.get_value<double>();

        if (!m_lexer.next_token_is("}"))
            throw_error(std::string("Unexpected token ") + m_lexer.next_token().c_str());
    }

    template <typename EntityType>
    auto_release_ptr<EntityType> parse_entity()
    {
        // TODO: implement me...
    }

    void validate_identifier(const StringView& id)
    {
        if (id.empty())
            throw_error("Premature end of file");

        // TODO: implement me...
    }

    void throw_error(const std::string& message)
    {
        throw ParseException(
            message,
            m_lexer.line(),
            m_lexer.col());
    }
};

auto_release_ptr<Project> load_project(
    const char*         project_filepath,
    const int           options,
    EventCounters&      event_counters,
    const SearchPaths*  /*search_paths*/)
{
    FILE* file = fopen(project_filepath, "r");

    if (!file)
    {
        RENDERER_LOG_ERROR("Could not open project file %s", project_filepath);
        return auto_release_ptr<Project>();
    }

    try
    {
        Lexer lexer(file);
        Parser parser(project_filepath, lexer, options);
        return parser.parse();
    }
    catch (const ParseException& e)
    {
        RENDERER_LOG_ERROR("%s", e.what());
        event_counters.signal_error();
    }
    catch(const std::exception& e)
    {
        RENDERER_LOG_ERROR("%s", e.what());
        event_counters.signal_error();
    }
    catch(...)
    {
        RENDERER_LOG_ERROR("Unknown error while reading project.");
        event_counters.signal_error();
    }

    return auto_release_ptr<Project>();
}


}

//
// AppleseedProjectFileReader class implementation.
//

auto_release_ptr<Project> AppleseedProjectFileReader::read(
    const char*         project_filepath,
    const int           options,
    EventCounters&      event_counters)
{
    return load_project(
        project_filepath,
        options,
        event_counters,
        nullptr);
}

auto_release_ptr<Project> AppleseedProjectFileReader::read_archive(
    const char*         archive_filepath,
    const SearchPaths&  search_paths,
    const int           options,
    EventCounters&      event_counters)
{
    return load_project(
        archive_filepath,
        options,
        event_counters,
        &search_paths);
}

}   // namespace renderer
