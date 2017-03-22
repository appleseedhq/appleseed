
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
#include "foundation/utility/minizip/unzip.h"
#include "foundation/utility/string.h"

// Boost headers.
#include "boost/filesystem.hpp"

// Standard headers.
#include <ctime>
#include <vector>
#include <fstream>
#include <set>

using namespace std;
namespace bf = boost::filesystem;

namespace foundation
{

    ZipException::ZipException(const char* what)
        : Exception(what)
    {
    }

    ZipException::ZipException(const char* what, const int err)
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
            throw ZipException("Can't open file inside zip: ", err);
    }

    int read_chunk(unzFile& zip_file, char* buffer, const int chunk_size)
    {
        const int err = unzReadCurrentFile(zip_file, buffer, chunk_size);

        if (err == UNZ_ERRNO)
            throw ZipException("IO error while reading from zip");
        if (err < 0)
            throw ZipException("zLib error while decompressing file: ", err);

        return err;
    }

    void unzip_close_current_file(unzFile& zip_file)
    {
        const int err = unzCloseCurrentFile(zip_file);

        if (err == UNZ_CRCERROR)
            throw ZipException("CRC32 is not good");
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

        return string(&filename[0]);
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
            bf::create_directories(filepath);
            return;
        }
        else open_current_file(zip_file);

        fstream out(filepath.c_str(), ios_base::out | ios_base::binary);
        if (out.fail())
            throw ZipException(("Can't open file " + filepath).c_str());

        const size_t BUFFER_SIZE = 4096;
        char buffer[BUFFER_SIZE];

        do
        {
            const int read = read_chunk(zip_file, (char*) &buffer, BUFFER_SIZE);
            out.write((char*) &buffer, read);
        }
        while (!unzeof(zip_file));

        out.close();
        unzip_close_current_file(zip_file);
    }

    void unzip(const string& zip_filename, const string& unzipped_dir)
    {
        try
        {
            bf::create_directories(unzipped_dir);

            unzFile zip_file = unzOpen(zip_filename.c_str());
            if (zip_file == 0)
                throw ZipException(("Can't open file " + zip_filename).c_str());

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
            bf::remove_all(unzipped_dir);
            throw e;
        }
    }

    void zip_close_current_file(zipFile& zip_file)
    {
        const int err = zipCloseFileInZip(zip_file);

        if (err != ZIP_OK)
            ZipException("Error while closing file in zip", err);
    }

    void write_chunk(zipFile& zip_file, char* buffer, const int chunk_size)
    {
        const int err = zipWriteInFileInZip(zip_file, buffer, chunk_size);

        if (err < 0)
        {
            throw ZipException("Error while writing to zip", err);
        }
    }

    void open_new_file_in_zip(zipFile& zip_file, string filename_in_zip, zip_fileinfo zip_file_info)
    {
        int err = zipOpenNewFileInZip(zip_file, filename_in_zip.c_str(), &zip_file_info,
                                      0, 0, 0, 0, 0,
                                      Z_DEFLATED, Z_DEFAULT_COMPRESSION);

        if (err != ZIP_OK)
            throw ZipException(("error while opening " + filename_in_zip + " in zipfile").c_str());
    }

    zip_fileinfo set_file_timestamp(const string& filename)
    {
        time_t timestamp = bf::last_write_time(filename);
        tm* timestamp_components = localtime(&timestamp);

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

    void zip_current_file(zipFile& zip_file, const string& filename, const string& base_directory)
    {
        const string filename_in_fs = (bf::path(base_directory) / filename).string();

        const zip_fileinfo zip_file_info = set_file_timestamp(filename_in_fs);

        open_new_file_in_zip(zip_file, filename, zip_file_info);

        fstream in(filename_in_fs.c_str(), ios_base::in | ios_base::binary);
        if (in.fail())
            throw ZipException(("Can't open file " + filename_in_fs).c_str());

        const size_t BUFFER_SIZE = 4096;
        char buffer[BUFFER_SIZE];

        do
        {
            in.read((char*) &buffer, BUFFER_SIZE);
            const streamsize read = in.gcount();
            write_chunk(zip_file, (char*) &buffer, read);
        }
        while (!in.eof());

        in.close();
        zip_close_current_file(zip_file);
    }

    void zip(const string& zip_filename, const string& directory_to_zip)
    {
        try
        {
            set<string> files_to_zip = recursive_ls(directory_to_zip);

            zipFile zip_file = zipOpen(zip_filename.c_str(), 0);
            if (zip_file == 0)
                throw ZipException(("Can't open file " + zip_filename).c_str());

            for (set<string>::iterator it = files_to_zip.begin();
                 it != files_to_zip.end(); ++it)
            {
                const string filename_to_zip = *it;
                zip_current_file(zip_file, filename_to_zip, directory_to_zip);
            }

            zipClose(zip_file, NULL);
        }
        catch (exception e)
        {
            bf::remove(zip_filename);
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
            throw ZipException(("Can't open file " + zip_filename).c_str());

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

    set<string> recursive_ls(const bf::path& dir)
    {
        set<string> files;

        // A default constructed directory_iterator acts as the end iterator
        bf::directory_iterator end_iter;
        for (bf::directory_iterator dir_itr(dir); dir_itr != end_iter; ++dir_itr)
        {
            const bf::path current_path = dir_itr->path();

            if (bf::is_directory(current_path))
            {
                const string dirname = current_path.filename().string();
                const set<string> files_in_subdir = recursive_ls(current_path);

                for (set<string>::iterator it = files_in_subdir.begin(); it != files_in_subdir.end(); ++it)
                    files.insert(dirname + "/" + *it);
            }
            else
                files.insert(current_path.filename().string());
        }

        return files;
    }

}   // namespace foundation


