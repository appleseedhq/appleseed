
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Gleb Mishchenko, The appleseedhq Organization
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
#include "zip.h"

// appleseed.foundation headers.
#include "foundation/string/string.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/minizip/unzip.h"
#include "foundation/utility/minizip/zip.h"

// Boost headers.
#include "boost/filesystem.hpp"
#include "boost/range/iterator_range.hpp"

// Standard headers.
#include <ctime>
#include <fstream>
#include <set>
#include <vector>

namespace bf = boost::filesystem;

namespace foundation
{

//
// ZipException class implementation.
//

ZipException::ZipException(const char* what)
  : Exception(what)
{
}

ZipException::ZipException(const char* what, const int err)
{
    const std::string string_what = std::string(what) + " (error code: " + to_string(err) + ")";
    set_what(string_what.c_str());
}


//
// Free functions implementation.
//

namespace
{
    bool is_zip_entry_directory(const std::string& dirname)
    {
        // Use our own implementation of is_zip_entry_directory instead of Boost's one
        // because this directory is not in the filesystem but in the zip file.
        return dirname[dirname.size() - 1] == '/';
    }

    void open_current_file(unzFile& zip_file)
    {
        const int err = unzOpenCurrentFile(zip_file);
        if (err != UNZ_OK)
            throw ZipException("can't open file inside zip: ", err);
    }

    size_t read_chunk(unzFile& zip_file, char* buffer, const size_t chunk_size)
    {
        const int err =
            unzReadCurrentFile(
                zip_file,
                buffer,
                static_cast<unsigned int>(chunk_size));

        if (err == UNZ_ERRNO)
            throw ZipException("i/o error while reading from zip");

        if (err < 0)
            throw ZipException("zlib error while decompressing file: ", err);

        return static_cast<size_t>(err);
    }

    void unzip_close_current_file(unzFile& zip_file)
    {
        const int err = unzCloseCurrentFile(zip_file);

        if (err == UNZ_CRCERROR)
            throw ZipException("crc error");
    }

    std::string read_filename(unzFile& zip_file)
    {
        unz_file_info zip_file_info;
        unzGetCurrentFileInfo(zip_file, &zip_file_info, nullptr, 0, nullptr, 0, nullptr, 0);

        std::vector<char> filename(zip_file_info.size_filename + 1);
        unzGetCurrentFileInfo(
            zip_file,
            &zip_file_info,
            &filename[0],
            static_cast<uLong>(filename.size()),
            nullptr, 0,
            nullptr, 0);
        filename[filename.size() - 1] = '\0';

        return std::string(&filename[0]);
    }

    std::string get_filepath(unzFile& zip_file, const std::string& unzipped_dir)
    {
        const std::string filename = read_filename(zip_file);
        return (bf::path(unzipped_dir) / bf::path(filename)).string();
    }

    void create_subdirectories(const std::string& filepath)
    {
        const bf::path parent_path = bf::path(filepath).parent_path();
        if (!bf::exists(parent_path))
            bf::create_directories(parent_path);
    }

    void extract_current_file(unzFile& zip_file, const std::string& unzipped_dir)
    {
        const std::string filepath = get_filepath(zip_file, unzipped_dir);

        if (is_zip_entry_directory(filepath))
            return;

        create_subdirectories(filepath);
        open_current_file(zip_file);

        std::fstream out(filepath.c_str(), std::ios_base::out | std::ios_base::binary);
        if (out.fail())
            throw ZipException(("can't open file " + filepath).c_str());

        do
        {
            const size_t BUFFER_SIZE = 4096;
            char buffer[BUFFER_SIZE];

            const size_t read = read_chunk(zip_file, buffer, BUFFER_SIZE);
            out.write(buffer, read);
        }
        while (!unzeof(zip_file));

        out.close();
        unzip_close_current_file(zip_file);
    }

    void zip_close_current_file(zipFile& zip_file)
    {
        const int err = zipCloseFileInZip(zip_file);

        if (err != ZIP_OK)
            throw ZipException("error while closing file in zip", err);
    }

    void write_chunk(zipFile& zip_file, char* buffer, const size_t chunk_size)
    {
        const int err =
            zipWriteInFileInZip(
                zip_file,
                buffer,
                static_cast<unsigned int>(chunk_size));

        if (err < 0)
            throw ZipException("error while writing to zip", err);
    }

    void open_new_file_in_zip(zipFile& zip_file, std::string filename_in_zip, zip_fileinfo zip_file_info)
    {
        const int err =
            zipOpenNewFileInZip(
                zip_file,
                filename_in_zip.c_str(),
                &zip_file_info,
                nullptr, 0, nullptr, 0, nullptr,
                Z_DEFLATED,
                Z_DEFAULT_COMPRESSION);

        if (err != ZIP_OK)
            throw ZipException(("error while opening " + filename_in_zip + " in zipfile").c_str());
    }

    zip_fileinfo make_zip_fileinfo(const std::string& filename)
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

    void zip_current_file(zipFile& zip_file, const std::string& filename, const std::string& base_directory)
    {
        const std::string filename_in_fs = (bf::path(base_directory) / filename).string();

        const zip_fileinfo zip_file_info = make_zip_fileinfo(filename_in_fs);

        open_new_file_in_zip(zip_file, filename, zip_file_info);

        std::fstream in(filename_in_fs.c_str(), std::ios_base::in | std::ios_base::binary);
        if (in.fail())
            throw ZipException(("can't open file " + filename_in_fs).c_str());

        do
        {
            const size_t BUFFER_SIZE = 4096;
            char buffer[BUFFER_SIZE];

            in.read(&buffer[0], BUFFER_SIZE);
            write_chunk(zip_file, &buffer[0], in.gcount());
        }
        while (!in.eof());

        in.close();
        zip_close_current_file(zip_file);
    }
}

void unzip(const std::string& zip_filename, const std::string& unzipped_dir)
{
    try
    {
        bf::create_directories(unzipped_dir);

        unzFile zip_file = unzOpen(zip_filename.c_str());
        if (zip_file == nullptr)
            throw ZipException(("can't open file " + zip_filename).c_str());

        unzGoToFirstFile(zip_file);

        int has_next = UNZ_OK;
        while (has_next == UNZ_OK)
        {
            extract_current_file(zip_file, unzipped_dir);
            has_next = unzGoToNextFile(zip_file);
        }

        unzClose(zip_file);
    }
    catch (const std::exception& e)
    {
        bf::remove_all(unzipped_dir);
        throw e;
    }
}

void zip(const std::string& zip_filename, const std::string& directory_to_zip)
{
    try
    {
        std::set<std::string> files_to_zip = recursive_ls(directory_to_zip);

        zipFile zip_file = zipOpen(zip_filename.c_str(), 0);
        if (zip_file == nullptr)
            throw ZipException(("can't open file " + zip_filename).c_str());

        for (const std::string& filename_to_zip : files_to_zip)
            zip_current_file(zip_file, filename_to_zip, directory_to_zip);

        zipClose(zip_file, nullptr);
    }
    catch (const std::exception& e)
    {
        bf::remove(zip_filename);
        throw e;
    }
}

bool is_zip_file(const char* filename)
{
    unzFile zip_file = unzOpen(filename);

    if (zip_file == nullptr)
        return false;
    else
    {
        unzClose(zip_file);
        return true;
    }
}

std::vector<std::string> get_filenames_with_extension_from_zip(const std::string& zip_filename, const std::string& extension)
{
    std::vector<std::string> filenames;

    unzFile zip_file = unzOpen(zip_filename.c_str());
    if (zip_file == nullptr)
        throw ZipException(("can't open file " + zip_filename).c_str());

    unzGoToFirstFile(zip_file);

    int has_next = UNZ_OK;
    while (has_next == UNZ_OK)
    {
        const std::string filename = read_filename(zip_file);

        if (ends_with(filename, extension))
            filenames.push_back(filename);

        has_next = unzGoToNextFile(zip_file);
    }

    unzClose(zip_file);

    return filenames;
}

std::set<std::string> recursive_ls(const bf::path& dir)
{
    std::set<std::string> files;

    for (const bf::path& entry_path : boost::make_iterator_range(bf::directory_iterator(dir)))
    {
        if (bf::is_directory(entry_path))
        {
            const std::string dirname = entry_path.filename().string();

            for (const std::string& filepath : recursive_ls(entry_path))
                files.insert(dirname + "/" + filepath);
        }
        else files.insert(entry_path.filename().string());
    }

    return files;
}

}   // namespace foundation
