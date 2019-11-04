
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

#pragma once

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/platform/compiler.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <string>
#include <vector>

namespace foundation
{

//
// A thread-local buffered file providing high performance disk I/O.
//
// Notes:
//
//   - Buffered and unbuffered read/write may be mixed freely.
//
//   - For performance reasons, error checking is reduced to the minimum.
//
//   - After an error has been reported by any of the method of the class
//     except open(), the BufferedFile object is left in an unknown state
//     and no other method except close() may be called safely.
//

class APPLESEED_DLLSYMBOL BufferedFile
{
  public:
    // File type.
    enum FileType
    {
        TextType,
        BinaryType
    };

    // File access mode.
    enum FileMode
    {
        ReadMode,
        WriteMode
    };

    // Default read/write buffer size, in bytes.
    enum
    {
        DefaultBufferSize = 32 * 1024
    };

    // Initial position for seek().
    enum SeekOrigin
    {
        SeekFromBeginning,  // offset must be positive or null
        SeekFromCurrent,    // offset may be positive, negative or null
        SeekFromEnd         // offset must be negative or null
    };

    // Constructors.
    BufferedFile();
    BufferedFile(
        const char*         path,
        const FileType      type,
        const FileMode      mode,
        const size_t        buffer_size = DefaultBufferSize);

    // Destructor, closes the file if it is still open.
    ~BufferedFile();

    // Open a file.
    // Return true on success, false on error.
    bool open(
        const char*         path,
        const FileType      type,
        const FileMode      mode,
        const size_t        buffer_size = DefaultBufferSize);

    // Close the file.
    // Return true on success, false on error.
    bool close();

    // Return true if the file is open, false otherwise.
    bool is_open() const;

    // Read a contiguous sequence of bytes from the file.
    // The file must be open in read mode.
    // Return the number of bytes that were successfully read.
    size_t read(
        void*               outbuf,
        const size_t        size);

    // Read one byte from the file.
    // The file must be open in read mode.
    // Return the number of bytes that were successfully read (0 or 1).
    size_t read(void* outbuf);

    // Read an object from the file.
    // The file must be open in read mode.
    // Return the number of bytes that were successfully read.
    template <typename T>
    size_t read(T& object);

    // Same as read(), but without buffering.
    size_t read_unbuf(
        void*               outbuf,
        const size_t        size);
    template <typename T>
    size_t read_unbuf(T& object);

    // Write a contiguous sequence of bytes to the file.
    // The file must be open in write mode.
    // Return the number of bytes that were successfully written.
    size_t write(
        const void*         inbuf,
        const size_t        size);

    // Write one byte to the file.
    // The file must be open in write mode.
    // Return the number of bytes that were successfully written (0 or 1).
    size_t write(const void* inbuf);

    // Write an object to the file.
    // The file must be open in write mode.
    // Return the number of bytes that were successfully written.
    template <typename T>
    size_t write(const T& object);

    // Same as write(), but without buffering.
    size_t write_unbuf(
        const void*         inbuf,
        const size_t        size);
    template <typename T>
    size_t write_unbuf(const T& object);

    // Move the file pointer to a specified location.
    // The file must be binary, and open in read or write mode.
    // Return true on success, false on error.
    bool seek(
        const std::int64_t  offset,
        const SeekOrigin    origin);

    // Return the current position of the file pointer.
    // The file must be binary, and open in read or write mode.
    std::int64_t tell() const;

  private:
    std::FILE*              m_file;
    FileMode                m_file_mode;
    std::int64_t            m_file_index;       // index in the file of the first byte of the I/O buffer
    std::uint8_t*           m_buffer;           // I/O buffer
    size_t                  m_buffer_size;      // size of the I/O buffer
    size_t                  m_buffer_end;       // one past the index of the last byte in the I/O buffer
    size_t                  m_buffer_index;     // index of the next byte in the I/O buffer

    // Reset the internal state of the object.
    void reset();

    // Invalidate the I/O buffer (read mode only).
    void invalidate_buffer();

    // Fill the I/O buffer (read mode only).
    void fill_buffer();

    // Flush the I/O buffer to disk (write mode only).
    // Return true on success, false on error.
    bool flush_buffer();
};


//
// Utility free functions.
//

struct ExceptionEOF : public Exception {};

// Throws foundation::ExceptionEOF or foundation::ExceptionIOError.
template <typename File>
void checked_read(File& file, void* outbuf, const size_t size);

// Throws foundation::ExceptionEOF or foundation::ExceptionIOError.
template <typename File, typename T>
void checked_read(File& file, T& object);

// Throws foundation::ExceptionIOError.
template <typename File>
void checked_write(File& file, const void* inbuf, const size_t size);

// Throws foundation::ExceptionIOError.
template <typename File, typename T>
void checked_write(File& file, const T& object);


//
// Base classes for foundation::BufferedFile adapters.
//

class WriterAdapter
{
  public:
    virtual ~WriterAdapter() {}

    virtual size_t write(
        const void*         inbuf,
        const size_t        size) = 0;

    template <typename T>
    size_t write(const T& object);
};

class ReaderAdapter
{
  public:
    virtual ~ReaderAdapter() {}

    virtual size_t read(
        void*               outbuf,
        const size_t        size) = 0;

    template <typename T>
    size_t read(T& object);
};


//
// Passthrough adapters.
//

class PassthroughWriterAdapter
  : public WriterAdapter
{
  public:
    explicit PassthroughWriterAdapter(BufferedFile& file);

    size_t write(
        const void*         inbuf,
        const size_t        size) override;

  private:
    BufferedFile&           m_file;
};

class PassthroughReaderAdapter
  : public ReaderAdapter
{
  public:
    explicit PassthroughReaderAdapter(BufferedFile& file);

    size_t read(
        void*               outbuf,
        const size_t        size) override;

  private:
    BufferedFile&           m_file;
};


//
// Base classes for adapters providing data compression on top of foundation::BufferedFile.
//

class CompressedWriterAdapter
  : public WriterAdapter
{
  public:
    CompressedWriterAdapter(
        BufferedFile&           file,
        const size_t            buffer_size = 64 * 1024);   // compression buffer size, in bytes

    ~CompressedWriterAdapter() override;

    size_t write(
        const void*             inbuf,
        const size_t            size) override;

  protected:
    BufferedFile&               m_file;
    const size_t                m_buffer_size;
    size_t                      m_buffer_index;
    std::vector<std::uint8_t>   m_buffer;

    virtual void flush_buffer() = 0;
};

class CompressedReaderAdapter
  : public ReaderAdapter
{
  public:
    explicit CompressedReaderAdapter(BufferedFile& file);

    size_t read(
        void*                   outbuf,
        const size_t            size) override;

  protected:
    BufferedFile&               m_file;
    size_t                      m_buffer_index;
    size_t                      m_buffer_end;
    std::vector<std::uint8_t>   m_buffer;

    virtual bool fill_buffer() = 0;
};


//
// LZ4 compression adapters.
//

class LZ4CompressedWriterAdapter
  : public CompressedWriterAdapter
{
  public:
    explicit LZ4CompressedWriterAdapter(BufferedFile& file);

    LZ4CompressedWriterAdapter(
        BufferedFile&           file,
        const size_t            buffer_size);               // compression buffer size, in bytes

    ~LZ4CompressedWriterAdapter() override;

  private:
    std::vector<std::uint8_t>   m_compressed_buffer;

    void flush_buffer() override;
};

class LZ4CompressedReaderAdapter
  : public CompressedReaderAdapter
{
  public:
    explicit LZ4CompressedReaderAdapter(BufferedFile& file);

  private:
    std::vector<std::uint8_t>   m_compressed_buffer;

    bool fill_buffer() override;
};


//
// BufferedFile class implementation.
//

inline size_t BufferedFile::read(void* outbuf)
{
    assert(m_file);
    assert(m_file_mode == ReadMode);
    assert(m_buffer);
    assert(outbuf);

    // If the I/O buffer is exhausted, refill it.
    if (m_buffer_index == m_buffer_end)
    {
        // Fill the I/O buffer.
        fill_buffer();

        // Stop if the end of the file has been reached.
        if (m_buffer_end == 0)
            return 0;
    }

    // Copy one byte from the I/O buffer to the output buffer.
    reinterpret_cast<std::uint8_t*>(outbuf)[0] = m_buffer[m_buffer_index++];

    // Return the number of bytes successfully read.
    return 1;
}

template <typename T>
inline size_t BufferedFile::read(T& object)
{
    return read(&object, sizeof(T));
}

template <typename T>
inline size_t BufferedFile::read_unbuf(T& object)
{
    return read_unbuf(&object, sizeof(T));
}

inline size_t BufferedFile::write(const void* inbuf)
{
    assert(m_file);
    assert(m_file_mode == WriteMode);
    assert(m_buffer);
    assert(inbuf);

    // If the I/O buffer is full, flush it to disk.
    if (m_buffer_index == m_buffer_end)
    {
        // Flush the I/O buffer to disk.
        if (!flush_buffer())
            return 0;
    }

    // Copy one byte from the input buffer to the I/O buffer.
    m_buffer[m_buffer_index++] = reinterpret_cast<const std::uint8_t*>(inbuf)[0];

    // Return the number of bytes successfully written.
    return 1;
}

template <typename T>
inline size_t BufferedFile::write(const T& object)
{
    return write(&object, sizeof(T));
}

template <>
inline size_t BufferedFile::write(const std::string& s)
{
    return write(s.c_str(), s.size());
}

template <typename T>
inline size_t BufferedFile::write_unbuf(const T& object)
{
    return write_unbuf(&object, sizeof(T));
}

template <>
inline size_t BufferedFile::write_unbuf(const std::string& s)
{
    return write_unbuf(s.c_str(), s.size());
}

inline std::int64_t BufferedFile::tell() const
{
    assert(m_file);
    assert(m_buffer);

    return m_file_index + static_cast<std::int64_t>(m_buffer_index);
}

inline void BufferedFile::invalidate_buffer()
{
    assert(m_file_mode == ReadMode);

    m_buffer_end = 0;
    m_buffer_index = 0;
}


//
// Utility free functions implementation.
//

template <typename File>
inline void checked_read(File& file, void* outbuf, const size_t size)
{
    if (size == 0)
        return;

    const size_t bytes_read = file.read(outbuf, size);

    if (bytes_read == 0)
        throw ExceptionEOF();

    if (bytes_read < size)
        throw ExceptionIOError();
}

template <typename File, typename T>
inline void checked_read(File& file, T& object)
{
    checked_read(file, &object, sizeof(T));
}

template <typename File>
inline void checked_write(File& file, const void* inbuf, const size_t size)
{
    const size_t bytes_written = file.write(inbuf, size);

    if (bytes_written < size)
        throw ExceptionIOError();
}

template <typename File, typename T>
inline void checked_write(File& file, const T& object)
{
    checked_write(file, &object, sizeof(T));
}


//
// WriterAdapter class implementation.
//

template <typename T>
inline size_t WriterAdapter::write(const T& object)
{
    return write(&object, sizeof(T));
}


//
// ReaderAdapter class implementation.
//

template <typename T>
inline size_t ReaderAdapter::read(T& object)
{
    return read(&object, sizeof(T));
}


//
// PassthroughWriterAdapter class implementation.
//

inline PassthroughWriterAdapter::PassthroughWriterAdapter(BufferedFile& file)
  : m_file(file)
{
}

inline size_t PassthroughWriterAdapter::write(
    const void*         inbuf,
    const size_t        size)
{
    return m_file.write(inbuf, size);
}


//
// PassthroughReaderAdapter class implementation.
//

inline PassthroughReaderAdapter::PassthroughReaderAdapter(BufferedFile& file)
  : m_file(file)
{
}

inline size_t PassthroughReaderAdapter::read(
    void*               outbuf,
    const size_t        size)
{
    return m_file.read(outbuf, size);
}

}   // namespace foundation
