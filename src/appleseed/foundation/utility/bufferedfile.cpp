
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "bufferedfile.h"

// appleseed.foundation headers.
#include "foundation/memory/memory.h"
#include "foundation/utility/otherwise.h"

// LZ4 headers.
#include <lz4.h>

// Standard headers.
#include <algorithm>
#include <cstring>

namespace foundation
{

//
// BufferedFile class implementation.
//

BufferedFile::BufferedFile()
{
    reset();
}

BufferedFile::BufferedFile(
    const char*         path,
    const FileType      type,
    const FileMode      mode,
    const size_t        buffer_size)
{
    reset();
    open(path, type, mode, buffer_size);
}

BufferedFile::~BufferedFile()
{
    close();
}

namespace
{
    std::string build_mode_string(
        const BufferedFile::FileType    type,
        const BufferedFile::FileMode    mode)
    {
        std::string s;

        switch (mode)
        {
          case BufferedFile::ReadMode:
            s += 'r';
            break;

          case BufferedFile::WriteMode:
            s += 'w';
            break;

          assert_otherwise;
        }

        switch (type)
        {
          case BufferedFile::TextType:
            s += 't';
            break;

          case BufferedFile::BinaryType:
            s += 'b';
            break;

          assert_otherwise;
        }

        return s;
    }
}

bool BufferedFile::open(
    const char*         path,
    const FileType      type,
    const FileMode      mode,
    const size_t        buffer_size)
{
    assert(m_file == 0);
    assert(path);
    assert(buffer_size > 0);

    const std::string mode_string = build_mode_string(type, mode);

    m_file = fopen(path, mode_string.c_str());

    if (m_file == nullptr)
        return false;

    m_file_mode = mode;
    m_file_index = 0;
    m_buffer = new std::uint8_t[buffer_size];
    m_buffer_size = buffer_size;
    m_buffer_end = mode == ReadMode ? 0 : m_buffer_size;
    m_buffer_index = 0;

    return true;
}

bool BufferedFile::close()
{
    bool success = true;

    if (m_file_mode == WriteMode)
        success = flush_buffer();

    if (m_file)
    {
        if (fclose(m_file))
            success = false;
    }

    delete[] m_buffer;

    reset();

    return success;
}

bool BufferedFile::is_open() const
{
    return m_file != nullptr;
}

void BufferedFile::reset()
{
    m_file = nullptr;
    m_file_mode = ReadMode;
    m_file_index = 0;
    m_buffer = nullptr;
    m_buffer_size = 0;
    m_buffer_end = 0;
    m_buffer_index = 0;
}

void BufferedFile::fill_buffer()
{
    assert(m_file);
    assert(m_file_mode == ReadMode);
    assert(m_buffer);

    m_file_index += static_cast<std::int64_t>(m_buffer_index);

    m_buffer_end = fread(m_buffer, 1, m_buffer_size, m_file);
    m_buffer_index = 0;
}

bool BufferedFile::flush_buffer()
{
    assert(m_file);
    assert(m_file_mode == WriteMode);
    assert(m_buffer);

    if (m_buffer_index == 0)
        return true;

    const size_t written = fwrite(m_buffer, 1, m_buffer_index, m_file);

    m_file_index += static_cast<std::int64_t>(written);

    const bool success = written == m_buffer_index;

    m_buffer_index = 0;

    return success;
}

size_t BufferedFile::read(
    void*               outbuf,
    const size_t        size)
{
    assert(m_file);
    assert(m_file_mode == ReadMode);
    assert(m_buffer);
    assert(outbuf);

    size_t bytes = 0;

    while (bytes < size)
    {
        // If the I/O buffer is exhausted, refill it.
        if (m_buffer_index == m_buffer_end)
        {
            fill_buffer();

            // Stop if the end of the file has been reached.
            if (m_buffer_end == 0)
                break;
        }

        // Copy the contents of the I/O buffer to the output buffer.
        const size_t left = size - bytes;
        const size_t available = m_buffer_end - m_buffer_index;
        const size_t count = std::min(left, available);
        memcpy(
            &reinterpret_cast<std::uint8_t*>(outbuf)[bytes],
            &m_buffer[m_buffer_index],
            count);
        m_buffer_index += count;
        bytes += count;
    }

    // Return the number of bytes successfully read.
    return bytes;
}

size_t BufferedFile::read_unbuf(
    void*               outbuf,
    const size_t        size)
{
    assert(m_file);
    assert(m_file_mode == ReadMode);
    assert(m_buffer);
    assert(outbuf);

    size_t bytes = 0;

    while (bytes < size)
    {
        // As soon as the I/O buffer is exhausted, switch to unbuffered reading.
        if (m_buffer_index == m_buffer_end)
        {
            m_file_index += static_cast<std::int64_t>(m_buffer_index);

            invalidate_buffer();

            // Read all remaining data from disk directly into the output buffer.
            const size_t read =
                fread(
                    &reinterpret_cast<std::uint8_t*>(outbuf)[bytes],
                    1,
                    size - bytes,
                    m_file);
            bytes += read;

            m_file_index += static_cast<std::int64_t>(read);

            break;
        }

        // Copy the contents of the I/O buffer into the output buffer.
        const size_t left = size - bytes;
        const size_t available = m_buffer_end - m_buffer_index;
        const size_t count = std::min(left, available);
        memcpy(
            &reinterpret_cast<std::uint8_t*>(outbuf)[bytes],
            &m_buffer[m_buffer_index],
            count);
        m_buffer_index += count;
        bytes += count;
    }

    return bytes;
}

size_t BufferedFile::write(
    const void*         inbuf,
    const size_t        size)
{
    assert(m_file);
    assert(m_file_mode == WriteMode);
    assert(m_buffer);
    assert(inbuf);

    size_t bytes = 0;

    while (bytes < size)
    {
        // If the I/O buffer is full, flush it to disk.
        if (m_buffer_index == m_buffer_end)
        {
            if (!flush_buffer())
                break;
        }

        // Copy the contents of the input buffer to the I/O buffer.
        const size_t left = size - bytes;
        const size_t available = m_buffer_end - m_buffer_index;
        const size_t count = std::min(left, available);
        memcpy(
            &m_buffer[m_buffer_index],
            &reinterpret_cast<const std::uint8_t*>(inbuf)[bytes],
            count);
        m_buffer_index += count;
        bytes += count;
    }

    return bytes;
}

size_t BufferedFile::write_unbuf(
    const void*         inbuf,
    const size_t        size)
{
    assert(m_file);
    assert(m_file_mode == WriteMode);
    assert(m_buffer);
    assert(inbuf);

    size_t bytes = 0;

    while (bytes < size)
    {
        // As soon as the I/O buffer is full, switch to unbuffered writing.
        if (m_buffer_index == m_buffer_end)
        {
            if (!flush_buffer())
                break;

            // Write all remaining data directly to disk.
            const size_t written =
                fwrite(
                    &reinterpret_cast<const std::uint8_t*>(inbuf)[bytes],
                    1,
                    size - bytes,
                    m_file);
            bytes += written;

            m_file_index += static_cast<std::int64_t>(written);

            break;
        }

        // Copy the contents of the input buffer to the I/O buffer.
        const size_t left = size - bytes;
        const size_t available = m_buffer_end - m_buffer_index;
        const size_t count = std::min(left, available);
        memcpy(
            &m_buffer[m_buffer_index],
            &reinterpret_cast<const std::uint8_t*>(inbuf)[bytes],
            count);
        m_buffer_index += count;
        bytes += count;
    }

    return bytes;
}

namespace
{
    std::int64_t portable_fseek(FILE* file, const std::int64_t offset, const int mode)
    {
#ifdef _WIN32
        return _fseeki64(file, offset, mode);
#else
        return fseek(file, offset, mode);
#endif
    }

    std::int64_t portable_ftell(FILE* file)
    {
#ifdef _WIN32
        return _ftelli64(file);
#else
        return ftell(file);
#endif
    }
}

bool BufferedFile::seek(
    const std::int64_t  offset,
    const SeekOrigin    origin)
{
    assert(m_file);
    assert(m_buffer);

    // Seeking from the end is handled separately, since we don't know the size of the file.
    if (origin == SeekFromEnd)
    {
        if (m_file_mode == ReadMode)
            invalidate_buffer();
        else flush_buffer();

        if (portable_fseek(m_file, offset, SEEK_END))
            return false;

        m_file_index = portable_ftell(m_file);
    }
    else
    {
        std::int64_t target_index;

        if (origin == SeekFromBeginning)
            target_index = offset;
        else
        {
            assert(origin == SeekFromCurrent);
            const std::int64_t current_index = m_file_index + static_cast<std::int64_t>(m_buffer_index);
            target_index = std::max<std::int64_t>(current_index + offset, 0);
        }

        if (target_index >= m_file_index &&
            target_index <  m_file_index + static_cast<std::int64_t>(m_buffer_end))
        {
            // Seek within the I/O buffer.
            m_buffer_index = static_cast<size_t>(target_index - m_file_index);
        }
        else
        {
            std::int64_t current_file_index;

            if (m_file_mode == ReadMode)
            {
                current_file_index = m_file_index + static_cast<std::int64_t>(m_buffer_end);
                invalidate_buffer();
            }
            else
            {
                flush_buffer();
                current_file_index = m_file_index;
            }

            if (portable_fseek(m_file, target_index - current_file_index, SEEK_CUR))
                return false;

            m_file_index = portable_ftell(m_file);
        }
    }

    return true;
}


//
// CompressedWriterAdapter class implementation.
//

CompressedWriterAdapter::CompressedWriterAdapter(
    BufferedFile&       file,
    const size_t        buffer_size)
  : m_file(file)
  , m_buffer_size(buffer_size)
  , m_buffer_index(0)
{
}

CompressedWriterAdapter::~CompressedWriterAdapter()
{
    assert(m_buffer_index == 0);
}

size_t CompressedWriterAdapter::write(
    const void*         inbuf,
    const size_t        size)
{
    if (size > 0)
    {
        ensure_minimum_size(m_buffer, m_buffer_index + size);
        memcpy(&m_buffer[m_buffer_index], inbuf, size);
        m_buffer_index += size;

        if (m_buffer_index >= m_buffer_size)
            flush_buffer();
    }

    return size;
}


//
// CompressedReaderAdapter class implementation.
//

CompressedReaderAdapter::CompressedReaderAdapter(BufferedFile& file)
  : m_file(file)
  , m_buffer_index(0)
  , m_buffer_end(0)
{
}

size_t CompressedReaderAdapter::read(
    void*               outbuf,
    const size_t        size)
{
    size_t remaining = size;

    while (remaining > 0)
    {
        if (m_buffer_index == m_buffer_end)
        {
            if (!fill_buffer())
                break;
        }

        const size_t copy = std::min(size, m_buffer_end - m_buffer_index);
        memcpy(outbuf, &m_buffer[m_buffer_index], copy);

        outbuf = reinterpret_cast<std::uint8_t*>(outbuf) + copy;
        m_buffer_index += copy;
        remaining -= copy;
    }

    return size - remaining;
}


//
// Convenient function to read a 64-bit unsigned integer from a foundation::BufferedFile.
//

namespace
{
    template <typename T>
    inline size_t read_uint64(BufferedFile& file, T& x)
    {
        std::uint64_t y;
        const size_t result = file.read(y);
        x = static_cast<T>(y);
        return result;
    }
}


//
// LZ4CompressedWriterAdapter class implementation.
//

LZ4CompressedWriterAdapter::LZ4CompressedWriterAdapter(BufferedFile& file)
  : CompressedWriterAdapter(file)
{
}

LZ4CompressedWriterAdapter::LZ4CompressedWriterAdapter(
    BufferedFile&       file,
    const size_t        buffer_size)
  : CompressedWriterAdapter(file, buffer_size)
{
}

LZ4CompressedWriterAdapter::~LZ4CompressedWriterAdapter()
{
    if (m_buffer_index > 0)
        flush_buffer();
}

void LZ4CompressedWriterAdapter::flush_buffer()
{
    // Make sure we have some data to compress and write.
    assert(m_buffer_index > 0);

    // Allocate memory for the compressed buffer.
    const size_t max_compressed_buffer_size =
        static_cast<size_t>(LZ4_compressBound(static_cast<int>(m_buffer_index)));
    ensure_minimum_size(m_compressed_buffer, max_compressed_buffer_size);

    // Compress data.
    const int compressed_buffer_size =
        LZ4_compress_default(
            reinterpret_cast<const char*>(&m_buffer[0]),
            reinterpret_cast<char*>(&m_compressed_buffer[0]),
            static_cast<int>(m_buffer_index),
            static_cast<int>(max_compressed_buffer_size));
    assert(compressed_buffer_size > 0);

    // Write uncompressed size.
    m_file.write(static_cast<std::uint64_t>(m_buffer_index));

    // Write compressed size.
    m_file.write(static_cast<std::uint64_t>(compressed_buffer_size));

    // Write compressed data.
    m_file.write(&m_compressed_buffer[0], compressed_buffer_size);

    m_buffer_index = 0;
}


//
// LZ4CompressedReaderAdapter class implementation.
//

LZ4CompressedReaderAdapter::LZ4CompressedReaderAdapter(BufferedFile& file)
  : CompressedReaderAdapter(file)
{
}

bool LZ4CompressedReaderAdapter::fill_buffer()
{
    // Read uncompressed size.
    size_t buffer_size;
    if (read_uint64(m_file, buffer_size) == 0)
        return false;

    // Allocate memory for the uncompressed buffer.
    ensure_minimum_size(m_buffer, buffer_size);

    // Read compressed size.
    size_t compressed_buffer_size;
    read_uint64(m_file, compressed_buffer_size);

    // Allocate memory for the compressed buffer.
    ensure_minimum_size(m_compressed_buffer, compressed_buffer_size);

    // Read compressed data.
    m_file.read(&m_compressed_buffer[0], compressed_buffer_size);

    // Decompress data.
#ifndef NDEBUG
    const int decompressed_bytes =
#endif
        LZ4_decompress_safe(
            reinterpret_cast<const char*>(&m_compressed_buffer[0]),
            reinterpret_cast<char*>(&m_buffer[0]),
            static_cast<int>(compressed_buffer_size),
            static_cast<int>(buffer_size));
    assert(decompressed_bytes == static_cast<int>(buffer_size));

    m_buffer_index = 0;
    m_buffer_end = buffer_size;

    return true;
}

}   // namespace foundation
