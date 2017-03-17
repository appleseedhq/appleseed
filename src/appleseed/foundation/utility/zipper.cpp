
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Gleb Mishchenko, The appleseedhq Organization
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
#include "zipper.h"

// appleseed.foundation headers.
#include "foundation/utility/minizip/zip.h"
#include "foundation/utility/string.h"

// Boost headers.
#include "boost/filesystem.hpp"

// Standard headers.
#include <ctime>
#include <vector>
#include <fstream>

using namespace std;
namespace bf = boost::filesystem;

namespace foundation
{

    ZipException::ZipException(const char *what)
            : Exception(what)
    {
    }

    ZipException::ZipException(const char *what, const int err)
    {
        string string_what = what + to_string(err);
        set_what(string_what.c_str());
    }

    void zip_close_current_file(zipFile &zip_file)
    {
        const int err = zipCloseFileInZip(zip_file);

        if (err != ZIP_OK)
            ZipException("Error while closing file in zip", err);
    }

    void write_chunk(zipFile &zip_file, char *buffer, const int chunk_size)
    {
        const int err = zipWriteInFileInZip(zip_file, buffer, chunk_size);

        if (err < 0)
        {
            throw ZipException("Error while writing to zip", err);
        }
    }

    void open_new_file_in_zip(zipFile zip_file, string filename_in_zip, zip_fileinfo zip_file_info)
    {
        int err = zipOpenNewFileInZip(zip_file, filename_in_zip.c_str(), &zip_file_info,
                                      0, 0, 0, 0, 0,
                                      Z_DEFLATED, Z_DEFAULT_COMPRESSION);

        if (err != ZIP_OK)
            throw ZipException(("error while opening " + filename_in_zip + " in zipfile").c_str());
    }

    zip_fileinfo set_file_timestamp(string filename)
    {
        time_t timestamp = bf::last_write_time(filename);
        tm *timestamp_components = localtime(&timestamp);

        zip_fileinfo zip_file_info;
        zip_file_info.tmz_date.tm_sec = timestamp_components->tm_sec;
        zip_file_info.tmz_date.tm_min = timestamp_components->tm_min;
        zip_file_info.tmz_date.tm_hour = timestamp_components->tm_hour;
        zip_file_info.tmz_date.tm_mday = timestamp_components->tm_mday;
        zip_file_info.tmz_date.tm_mon = timestamp_components->tm_mon;
        zip_file_info.tmz_date.tm_year = timestamp_components->tm_year;

        zip_file_info.dosDate = 0;
        zip_file_info.internal_fa = 0;
        zip_file_info.external_fa = 0;

        return zip_file_info;
    }

    void zip_current_file(zipFile zip_file, const string &filename_to_zip)
    {
        const zip_fileinfo zip_file_info = set_file_timestamp(filename_to_zip);

        // name of file inside zip archive
        string filename_inside_zip = bf::path(filename_to_zip).filename().string();

        // The path name saved should not include a leading slash.
        // If it does, windows/xp and dynazip couldn't read the zip file.
        while (filename_inside_zip[0] == '\\' || filename_inside_zip[0] == '/')
        {
            filename_inside_zip.erase(filename_inside_zip.begin());
        }

        open_new_file_in_zip(zip_file, filename_inside_zip, zip_file_info);

        if (bf::is_directory(filename_to_zip))
            throw ZipException("Can't handle directories");

        fstream in(filename_to_zip.c_str(), ios_base::in | ios_base::binary);

        if (in.fail())
            throw ZipException(("Can't open file " + filename_to_zip).c_str());

        const size_t BUFFER_SIZE = 4096;
        char buffer[BUFFER_SIZE];

        do
        {
            in.read((char *) &buffer, BUFFER_SIZE);
            const streamsize read = in.gcount();
            write_chunk(zip_file, (char *) &buffer, read);
        } while (!in.eof());

        in.close();
        zip_close_current_file(zip_file);
    }

    void zip(const string &zip_filename, const vector<string> &filenames_to_zip)
    {
        try
        {
            zipFile zip_file = zipOpen(zip_filename.c_str(), 0);
            if (zip_file == 0)
                throw ZipException(("Can't open file " + zip_filename).c_str());

            for (size_t i = 0; i != filenames_to_zip.size(); ++i)
            {
                const string filename_to_zip = filenames_to_zip[i];
                zip_current_file(zip_file, filename_to_zip);
            }

            zipClose(zip_file, NULL);
        }
        catch (exception e)
        {
            bf::remove(zip_filename);
            throw e;
        }
    }

}   // namespace foundation


