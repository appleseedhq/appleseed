
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

// Standard headers.
#include <algorithm>
#include <cstring>

using namespace std;

namespace foundation
{

//
// BufferedFile class implementation.
//

// Constructors.
BufferedFile::BufferedFile()
  : m_file(0)
  , m_file_mode(ReadMode)
  , m_file_index(0)
  , m_buffer(0)
  , m_buffer_size(0)
  , m_buffer_end(0)
  , m_buffer_index(0)
{
}
BufferedFile::BufferedFile(
    const char*         path,
    const FileType      type,
    const FileMode      mode,
    const size_t        buffer_size)
  : m_file(0)
  , m_file_mode(ReadMode)
  , m_file_index(0)
  , m_buffer(0)
  , m_buffer_size(0)
  , m_buffer_end(0)
  , m_buffer_index(0)
{
    open(path, type, mode, buffer_size);
}

// Destructor, closes the file if it is still open.
BufferedFile::~BufferedFile()
{
    close();
}

// Open a file.
bool BufferedFile::open(
    const char*         path,
    const FileType      type,
    const FileMode      mode,
    const size_t        buffer_size)
{
    assert(path);
    assert(buffer_size > 0);

    // Error: a file is already open.
    if (m_file)
        return false;

    // Build the file mode string.
    char mode_str[3] = { 0, 0, 0 };
    switch (mode)
    {
      case ReadMode:   mode_str[0] = 'r'; break;
      case WriteMode:  mode_str[0] = 'w'; break;
      default: assert(!"Invalid file mode.");
    };
    switch (type)
    {
      case TextType:   mode_str[1] = 't'; break;
      case BinaryType: mode_str[1] = 'b'; break;
      default: assert(!"Invalid file type.");
    };

    // Open the file.
    m_file = fopen(path, mode_str);

    // Error: the file could not be open or created.
    if (m_file == 0)
        return false;

    // Allocate the I/O buffer.
    m_buffer = new uint8[buffer_size];

    // Error: the I/O buffer could not be allocated.
    if (m_buffer == 0)
    {
        fclose(m_file);
        m_file = 0;
        return false;
    }

    // Store the file mode.
    m_file_mode = mode;

    // Reset the file index.
    m_file_index = 0;

    // Store the I/O buffer size.
    m_buffer_size = buffer_size;

    if (mode == ReadMode)
    {
        // Invalidate the I/O buffer.
        m_buffer_end = 0;
        m_buffer_index = 0;
    }
    else
    {
        // Clear the I/O buffer.
        m_buffer_end = buffer_size;
        m_buffer_index = 0;
    }

    // Success.
    return true;
}

// Close the file.
bool BufferedFile::close()
{
    bool success = true;

    // In write mode, if the I/O buffer is not empty, flush it to disk.
    if (m_file_mode == WriteMode && m_buffer_index > 0)
    {
        // Write the contents of the I/O buffer to disk.
        if (fwrite(m_buffer, 1, m_buffer_index, m_file) < m_buffer_index)
            success = false;
    }

    // Invalidate the I/O buffer.
    m_buffer_end = 0;
    m_buffer_index = 0;

    // Reset the I/O buffer size.
    m_buffer_size = 0;

    // Deallocate the I/O buffer.
    delete [] m_buffer;
    m_buffer = 0;

    // Reset the file index.
    m_file_index = 0;

    // Reset the file mode.
    m_file_mode = ReadMode;

    // Close the file.
    if (m_file)
    {
        if (fclose(m_file))
            success = false;
        m_file = 0;
    }

    return success;
}

// Return true if the file is open, false otherwise.
bool BufferedFile::is_open() const
{
    return m_file != 0;
}

// Fill the I/O buffer (read mode only).
void BufferedFile::fill_buffer()
{
    // Update the file index.
    m_file_index += static_cast<int64>(m_buffer_index);

    // Read data from disk into the I/O buffer.
    m_buffer_index = 0;
    m_buffer_end = fread(m_buffer, 1, m_buffer_size, m_file);
}

// Flush the I/O buffer to disk (write mode only).
bool BufferedFile::flush_buffer()
{
    // Write the contents of the I/O buffer to disk.
    const size_t written = fwrite(m_buffer, 1, m_buffer_index, m_file);
    const bool success = (written == m_buffer_index);

    // Update the file index.
    m_file_index += static_cast<int64>(written);

    // Clear the I/O buffer.
    m_buffer_index = 0;

    return success;
}

// Read a contiguous sequence of bytes from the file.
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
            // Fill the I/O buffer.
            fill_buffer();

            // Stop if the end of the file has been reached.
            if (m_buffer_end == 0)
                break;
        }

        // Copy the contents of the I/O buffer to the output buffer.
        const size_t left = size - bytes;
        const size_t available = m_buffer_end - m_buffer_index;
        const size_t count = min(left, available);
        memcpy(
            &reinterpret_cast<uint8*>(outbuf)[bytes],
            &m_buffer[m_buffer_index],
            count);
        m_buffer_index += count;
        bytes += count;
    }

    // Return the number of bytes successfully read.
    return bytes;
}

// Same as read(), but without buffering.
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
            // Update the file index.
            m_file_index += static_cast<int64>(m_buffer_index);

            // Invalidate the I/O buffer.
            m_buffer_end = 0;
            m_buffer_index = 0;

            // Read all remaining data from disk directly into the output buffer.
            const size_t read =
                fread(
                    &reinterpret_cast<uint8*>(outbuf)[bytes],
                    1,
                    size - bytes,
                    m_file);
            bytes += read;

            // Update the file index.
            m_file_index += static_cast<int64>(read);

            // And we're done.
            break;
        }

        // Copy the contents of the I/O buffer into the output buffer.
        const size_t left = size - bytes;
        const size_t available = m_buffer_end - m_buffer_index;
        const size_t count = min(left, available);
        memcpy(
            &reinterpret_cast<uint8*>(outbuf)[bytes],
            &m_buffer[m_buffer_index],
            count);
        m_buffer_index += count;
        bytes += count;
    }

    // Return the number of bytes successfully read.
    return bytes;
}

// Write a contiguous sequence of bytes to the file.
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
            // Flush the I/O buffer to disk.
            if (!flush_buffer())
                break;
        }

        // Copy the contents of the input buffer to the I/O buffer.
        const size_t left = size - bytes;
        const size_t available = m_buffer_end - m_buffer_index;
        const size_t count = min(left, available);
        memcpy(
            &m_buffer[m_buffer_index],
            &reinterpret_cast<const uint8*>(inbuf)[bytes],
            count);
        m_buffer_index += count;
        bytes += count;
    }

    // Return the number of bytes successfully written.
    return bytes;
}

// Same as write(), but without buffering.
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
            // Flush the I/O buffer to disk.
            if (!flush_buffer())
                break;

            // Write all remaining data directly to disk.
            const size_t written =
                fwrite(
                    &reinterpret_cast<const uint8*>(inbuf)[bytes],
                    1,
                    size - bytes,
                    m_file);
            bytes += written;

            // Update the file index.
            m_file_index += static_cast<int64>(written);

            // And we're done.
            break;
        }

        // Copy the contents of the input buffer to the I/O buffer.
        const size_t left = size - bytes;
        const size_t available = m_buffer_end - m_buffer_index;
        const size_t count = min(left, available);
        memcpy(
            &m_buffer[m_buffer_index],
            &reinterpret_cast<const uint8*>(inbuf)[bytes],
            count);
        m_buffer_index += count;
        bytes += count;
    }

    // Return the number of bytes successfully written.
    return bytes;
}

// Move the file pointer to a specified location.
bool BufferedFile::seek(
    const int64         offset,
    const SeekOrigin    origin)
{
    assert(m_file);
    assert(m_buffer);

    // Attempt to seek into the buffer.
    if (m_file_mode == ReadMode && origin != SeekFromEnd)
    {
        // Compute the target index in the file.
        int64 target_index;
        if (origin == SeekFromBeginning)
        {
            assert(offset >= 0);
            target_index = offset;
        }
        else
        {
            const int64 current = m_file_index + static_cast<int64>(m_buffer_index);
            assert(offset >= -current);
            target_index = current + offset;
        }

        // Handle the case where the target index is within the bounds of the I/O buffer.
        if (target_index >= m_file_index &&
            target_index < m_file_index + static_cast<int64>(m_buffer_end))
        {
            // Compute the new index into the I/O buffer.
            m_buffer_index = static_cast<size_t>(target_index - m_file_index);

            // And we're done with seeking.
            return true;
        }
    }

    // Deal with the I/O buffer.
    if (m_file_mode == ReadMode)
    {
        // In read mode, invalidate the I/O buffer.
        m_buffer_end = 0;
        m_buffer_index = 0;
    }
    else
    {
        // In write mode, flush the I/O buffer to disk.
        if (m_buffer_index > 0)
        {
            // Write the contents of the I/O buffer to disk.
            const size_t written = fwrite(m_buffer, 1, m_buffer_index, m_file);
            if (written < m_buffer_index)
                return false;

            // Clear the I/O buffer.
            m_buffer_index = 0;
        }
    }

    // Figure out the seek mode for fseek().
    int seek_mode;
    switch (origin)
    {
      case SeekFromBeginning: seek_mode = SEEK_SET; break;
      case SeekFromCurrent:   seek_mode = SEEK_CUR; break;
      case SeekFromEnd:       seek_mode = SEEK_END; break;
      default:
        assert("Invalid seek origin.");
        seek_mode = SEEK_SET;
        break;
    }

    // Seek into the file.
#ifdef _WIN32
    if (_fseeki64(m_file, offset, seek_mode))
        return false;
#else
    if (fseek(m_file, offset, seek_mode))
        return false;
#endif

    // Recover the position of the file pointer.
#ifdef _WIN32
    m_file_index = _ftelli64(m_file);
#else
    m_file_index = ftell(m_file);
#endif

    // Success.
    return true;
}

}   // namespace foundation
