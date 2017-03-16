
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
#include "unzipper.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/utility/minizip/unzip.h"
#include "foundation/utility/string.h"

// Boost headers.
#include "boost/filesystem.hpp"

// Standard headers.
#include <fstream>
#include <sstream>

using namespace std;
namespace bf = boost::filesystem;

namespace foundation
{

UnzipException::UnzipException(const char* what)
  : Exception(what)
{
}

UnzipException::UnzipException(const char* what, const int err)
{
    string string_what = what + to_string(err);
    set_what(string_what.c_str());
}

bool is_zip_entry_directory(const string& dirname)
{
    // used own implementation of is_zip_entry_directory instead of boost implementation
    // because this directory is not in filesystem, but in zipfile
    return dirname[dirname.size() - 1] == '/';
}

void open_current_file(unzFile& zip_file)
{
    const int err = unzOpenCurrentFile(zip_file);
    if (err != UNZ_OK)
        throw UnzipException("Can't open file inside zip: ", err);
}

int read_chunk(unzFile& zip_file, char* buffer, const int chunk_size)
{
    const int err = unzReadCurrentFile(zip_file, buffer, chunk_size);

    if (err == UNZ_ERRNO)
        throw UnzipException("IO error while reading from zip");
    if (err < 0)
        throw UnzipException("zLib error while decompressing file: ", err);

    return err;
}

void close_current_file(unzFile& zip_file)
{
    const int err = unzCloseCurrentFile(zip_file);

    if (err == UNZ_CRCERROR)
        throw UnzipException("CRC32 is not good");
}

string read_filename(unzFile& zip_file)
{
    unz_file_info zip_file_info;
    unzGetCurrentFileInfo(zip_file, &zip_file_info, 0, 0, 0, 0, 0, 0);

    vector<char> filename(zip_file_info.size_filename + 1);
    unzGetCurrentFileInfo(
        zip_file,
        &zip_file_info,
        &filename[0],
        static_cast<uLong>(filename.size()),
        0, 0,
        0, 0);
    filename[filename.size() - 1] = '\0';

    const string inzip_filename(&filename[0]);
    return inzip_filename;
}

string get_filepath(unzFile& zip_file, const string& unzipped_dir)
{
    string filename = read_filename(zip_file);
    return (bf::path(unzipped_dir) / bf::path(filename)).string();
}

void extract_current_file(unzFile& zip_file, const string& unzipped_dir)
{
    const string filepath = get_filepath(zip_file, unzipped_dir);

    if (is_zip_entry_directory(filepath))
    {
        bf::create_directories(bf::path(filepath));
        return;
    }
    else open_current_file(zip_file);

    fstream out(filepath.c_str(), ios_base::out | ios_base::binary);

    do
    {
        const size_t BUFFER_SIZE = 4096;
        char buffer[BUFFER_SIZE];

        const int read = read_chunk(zip_file, (char*) &buffer, BUFFER_SIZE);
        out.write((char*) &buffer, read);
    }
    while (!unzeof(zip_file));

    out.close();
    close_current_file(zip_file);
}

void unzip(const string& zip_filename, const string& unzipped_dir)
{
    try
    {
        bf::create_directories(bf::path(unzipped_dir));

        unzFile zip_file = unzOpen(zip_filename.c_str());
        if (zip_file == 0)
            throw UnzipException(("Can't open file " + zip_filename).c_str());

        unzGoToFirstFile(zip_file);

        int has_next = UNZ_OK;
        while (has_next == UNZ_OK)
        {
            extract_current_file(zip_file, unzipped_dir);
            has_next = unzGoToNextFile(zip_file);
        }

        unzClose(zip_file);
    }
    catch (exception e)
    {
        bf::remove_all(bf::path(unzipped_dir));
        throw e;
    }
}

bool is_zip_file(const char* filename)
{
    unzFile zip_file = unzOpen(filename);

    if (zip_file == 0)
        return false;
    else
    {
        unzClose(zip_file);
        return true;
    }
}

vector<string> get_filenames_with_extension_from_zip(const string& zip_filename, const string& extension) 
{
    vector<string> filenames;

    unzFile zip_file = unzOpen(zip_filename.c_str());
    if (zip_file == 0)
        throw UnzipException(("Can't open file " + zip_filename).c_str());

    unzGoToFirstFile(zip_file);

    int has_next = UNZ_OK;
    while (has_next == UNZ_OK)
    {
        const string filename = read_filename(zip_file);

        if (ends_with(filename, extension))
            filenames.push_back(filename);

        has_next = unzGoToNextFile(zip_file);
    }

    unzClose(zip_file);

    return filenames;
}

}   // namespace foundation
