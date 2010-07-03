/*=============================================================================
    Copyright (c) 2002 2004 2006 Joel de Guzman
    Copyright (c) 2004 Eric Niebler
    http://spirit.sourceforge.net/

    Use, modification and distribution is subject to the Boost Software
    License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "./utils.hpp"
#include <boost/spirit/include/classic_core.hpp>

#include <cctype>
#include <cstring>
#include <stdexcept>
#include <fstream>
#include <iostream>
#include <map>

namespace quickbook {
    extern bool ms_errors;
}

namespace quickbook { namespace detail
{
    void print_char(char ch, std::ostream& out)
    {
        switch (ch)
        {
            case '<': out << "&lt;";    break;
            case '>': out << "&gt;";    break;
            case '&': out << "&amp;";   break;
            case '"': out << "&quot;";  break;
            default:  out << ch;        break;
            // note &apos; is not included. see the curse of apos:
            // http://fishbowl.pastiche.org/2003/07/01/the_curse_of_apos
        }
    }

    void print_string(std::basic_string<char> const& str, std::ostream& out)
    {
        for (std::string::const_iterator cur = str.begin();
            cur != str.end(); ++cur)
        {
            print_char(*cur, out);
        }
    }

    void print_space(char ch, std::ostream& out)
    {
        out << ch;
    }

    char filter_identifier_char(char ch)
    {
        if (!std::isalnum(static_cast<unsigned char>(ch)))
            ch = '_';
        return static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
    }

    // un-indent a code segment
    void unindent(std::string& program)
    {
        // Erase leading blank lines and newlines:
        std::string::size_type start = program.find_first_not_of(" \t");
        if (start != std::string::npos &&
            (program[start] == '\r' || program[start] == '\n'))
        {
            program.erase(0, start);
        }
        start = program.find_first_not_of("\r\n");
        program.erase(0, start);

        if (program.size() == 0)
            return; // nothing left to do

        // Get the first line indent
        std::string::size_type indent = program.find_first_not_of(" \t");
        std::string::size_type pos = 0;
        if (std::string::npos == indent)
        {
            // Nothing left to do here. The code is empty (just spaces).
            // We clear the program to signal the caller that it is empty
            // and return early.
            program.clear();
            return;
        }

        // Calculate the minimum indent from the rest of the lines
        do
        {
            pos = program.find_first_not_of("\r\n", pos);
            if (std::string::npos == pos)
                break;

            std::string::size_type n = program.find_first_not_of(" \t", pos);
            if (n != std::string::npos)
            {
                char ch = program[n];
                if (ch != '\r' && ch != '\n') // ignore empty lines
                    indent = (std::min)(indent, n-pos);
            }
        }
        while (std::string::npos != (pos = program.find_first_of("\r\n", pos)));

        // Trim white spaces from column 0..indent
        pos = 0;
        program.erase(0, indent);
        while (std::string::npos != (pos = program.find_first_of("\r\n", pos)))
        {
            if (std::string::npos == (pos = program.find_first_not_of("\r\n", pos)))
            {
                break;
            }

            std::string::size_type next = program.find_first_of("\r\n", pos);
            program.erase(pos, (std::min)(indent, next-pos));
        }
    }

    // remove the extension from a filename
    std::string
    remove_extension(std::string const& filename)
    {
        std::string::size_type const n = filename.find_last_of('.');
        if(std::string::npos == n)
        {
            return filename;
        }
        else
        {
            return std::string(filename.begin(), filename.begin()+n);
        }
    }

    std::string escape_uri(std::string uri)
    {
        for (std::string::size_type n = 0; n < uri.size(); ++n)
        {
            static char const mark[] = "-_.!~*'()?\\/";
            if((!std::isalnum(static_cast<unsigned char>(uri[n])) || 127 < static_cast<unsigned char>(uri[n]))
              && 0 == std::strchr(mark, uri[n]))
            {
                static char const hex[] = "0123456789abcdef";
                char escape[] = { hex[uri[n] / 16], hex[uri[n] % 16] };
                uri.insert(n + 1, escape, 2);
                uri[n] = '%';
                n += 2;
            }
        }
        return uri;
    }

    std::ostream& outerr(std::string const& file, int line)
    {
        if (line >= 0)
        {
            if (ms_errors)
                return std::clog << file << "(" << line << "): error: ";
            else
                return std::clog << file << ":" << line << ": error: ";
        }
        else
        {
            return std::clog << file << ": error: ";
        }
    }

    std::ostream& outwarn(std::string const& file, int line)
    {
        if (line >= 0)
        {
            if (ms_errors)
                return std::clog << file << "(" << line << "): warning: ";
            else
                return std::clog << file << ":" << line << ": warning: ";
        }
        else
        {
            return std::clog << file << ": warning: ";
        }
    }

    // Copy a string, converting mac and windows style newlines to unix
    // newlines.

    template <class InputIterator, class OutputIterator>
    void normalize_newlines(InputIterator begin, InputIterator end,
            OutputIterator out)
    {
        while(begin != end) {
            if(*begin == '\r') {
                *out++ = '\n';
                ++begin;
                if(begin != end && *begin == '\n') ++begin;
            }
            else {
                *out++ = *begin++;
            }
        }
    }

    int load(std::string const& filename, std::string& storage)
    {
        using std::cerr;
        using std::endl;
        using std::ios;
        using std::ifstream;
        using std::istream_iterator;

        ifstream in(filename.c_str(), std::ios_base::in);

        if (!in)
        {
            outerr(filename) << "Could not open input file." << endl;
            return 1;
        }

        // Turn off white space skipping on the stream
        in.unsetf(ios::skipws);

        normalize_newlines(
            istream_iterator<char>(in),
            istream_iterator<char>(),
            std::back_inserter(storage));

        //  ensure that we have enough trailing newlines to eliminate
        //  the need to check for end of file in the grammar.
        storage.push_back('\n');
        storage.push_back('\n');
        return 0;
    }

    file_type get_file_type(std::string const& extension)
    {
        static std::map<std::string, file_type> ftypes;
        if (ftypes.empty())
        {
            // init the map of types
            ftypes["cpp"] = cpp_file;
            ftypes["hpp"] = cpp_file;
            ftypes["h"] = cpp_file;
            ftypes["c"] = cpp_file;
            ftypes["cxx"] = cpp_file;
            ftypes["hxx"] = cpp_file;
            ftypes["ipp"] = cpp_file;
            ftypes["py"] = python_file;
        }
        return ftypes[extension];
    }

}}


