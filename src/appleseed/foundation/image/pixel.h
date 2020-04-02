
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
#include "foundation/math/half.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/otherwise.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace foundation
{

// todo: apply jittering/dithering when converting to lower precision formats.
// todo: throw exceptions for unsupported / invalid conversions.


//
// Supported pixel formats.
//

enum PixelFormat
{
    PixelFormatUInt8,
    PixelFormatUInt16,
    PixelFormatUInt32,
    PixelFormatHalf,
    PixelFormatFloat,
    PixelFormatDouble
};

// Return a string identifying a pixel format.
APPLESEED_DLLSYMBOL const char* pixel_format_name(const PixelFormat pixel_format);


//
// Pixel class, providing types and functions related to pixels.
//

class Pixel
{
  public:
    // Return the size in bytes of a given pixel format.
    static size_t size(PixelFormat format);

    //
    // The convert_*() methods below allow conversion of a given range of value
    // in a given pixel format to another pixel format, with arbitrary striding
    // in both source and destination. They take advantage of the fact that in
    // most cases, either the source format or the destination format is known
    // at compile time.

    //
    // The non-specialized versions of these methods are intentionally left
    // unimplemented so that attempting to convert to or from a non-supported
    // pixel format will result in a compilation error.
    //

    // Convert from templatized format to variable format.
    template <typename T>
    static void convert_to_format(
        const T*            src_begin,      // points to the first value to convert
        const T*            src_end,        // one beyond the last value to convert
        const size_t        src_stride,     // source stride (in words)
        const PixelFormat   dest_format,    // destination format
        void*               dest,           // destination
        const size_t        dest_stride);   // destination stride (in words)

    // Convert from variable format to templatized format.
    template <typename T>
    static void convert_from_format(
        const PixelFormat   src_format,     // source format
        const void*         src_begin,      // points to the first value to convert
        const void*         src_end,        // one beyond the last value to convert
        const size_t        src_stride,     // source stride (in words)
        T*                  dest,           // destination
        const size_t        dest_stride);   // destination stride (in words)

    // Convert from variable format to variable format.
    static void convert(
        const PixelFormat   src_format,     // source format
        const void*         src_begin,      // points to the first value to convert
        const void*         src_end,        // one beyond the last value to convert
        const size_t        src_stride,     // source stride (in words)
        const PixelFormat   dest_format,    // destination format
        void*               dest,           // destination
        const size_t        dest_stride);   // destination stride (in words)

    //
    // The convert_and_shuffle() method allow conversion of a given range of pixels,
    // with a given number of channels in a given pixel format, to a new set of
    // pixels with potentially a different number of channels, in different order,
    // in a different pixel format.
    //
    // Example: the following converts a set of pixels in RGBA order with one 32-bit
    // float per channel to a new set of pixels in BGR order (omitting the alpha
    // channel) with one 8-bit integer per channel:
    //
    //      const size_t shuffle_table[4] = { 2, 1, 0, Pixel::SkipChannel };
    //
    //      Pixel::convert_and_shuffle(
    //          PixelFormatFloat,           // source format
    //          4,                          // source channels
    //          src_begin,                  // source begin
    //          src_end,                    // source end
    //          PixelFormatUInt8,           // destination format
    //          3,                          // destination channels
    //          dest,                       // destination
    //          shuffle_table);             // channel shuffling table
    //

    static void convert_and_shuffle(
        const PixelFormat   src_format,     // source format
        const size_t        src_channels,   // number of source channels
        const void*         src_begin,      // points to the first value to convert
        const void*         src_end,        // one beyond the last value to convert
        const PixelFormat   dest_format,    // destination format
        const size_t        dest_channels,  // number of destination channels
        void*               dest,           // destination
        const size_t*       shuffle_table); // channel shuffling table

    // Use this value in a channel shuffling table to indicate that a channel
    // must be skipped.
    static const size_t SkipChannel = ~size_t(0);

    // Return the number of destination channels specified by a channel shuffling table.
    static size_t get_dest_channel_count(
        const size_t        src_channels,   // number of source channels
        const size_t*       shuffle_table); // channel shuffling table

};


//
// Pixel class implementation.
//

inline size_t Pixel::size(PixelFormat format)
{
    switch (format)
    {
      case PixelFormatUInt8:    return 1;
      case PixelFormatUInt16:   return 2;
      case PixelFormatUInt32:   return 4;
      case PixelFormatHalf:     return 2;
      case PixelFormatFloat:    return 4;
      case PixelFormatDouble:   return 8;
      default:
        assert(false);
        return 0;
    }
}

template <>
inline void Pixel::convert_to_format<std::uint8_t>(
    const std::uint8_t*     src_begin,
    const std::uint8_t*     src_end,
    const size_t            src_stride,
    const PixelFormat       dest_format,
    void*                   dest,
    const size_t            dest_stride)
{
    assert(src_begin);
    assert(src_end);
    assert(dest);

    switch (dest_format)
    {
      case PixelFormatUInt8:                // lossless std::uint8_t -> std::uint8_t
        {
            std::uint8_t* typed_dest = reinterpret_cast<std::uint8_t*>(dest);
            for (const std::uint8_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = *it;
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossless std::uint8_t -> std::uint16_t
        {
            std::uint16_t* typed_dest = reinterpret_cast<std::uint16_t*>(dest);
            for (const std::uint8_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<std::uint16_t>(*it) * (65535 / 255);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossless std::uint8_t -> std::uint32_t
        {
            std::uint32_t* typed_dest = reinterpret_cast<std::uint32_t*>(dest);
            for (const std::uint8_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<std::uint32_t>(*it) * (4294967295u / 255);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossless std::uint8_t -> half
        {
            Half* typed_dest = reinterpret_cast<Half*>(dest);
            for (const std::uint8_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<Half>(static_cast<float>(*it) * (1.0f / 255));
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossless std::uint8_t -> float
        {
            float* typed_dest = reinterpret_cast<float*>(dest);
            for (const std::uint8_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<float>(*it) * (1.0f / 255);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossless std::uint8_t -> double
        {
            double* typed_dest = reinterpret_cast<double*>(dest);
            for (const std::uint8_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<double>(*it) * (1.0 / 255);
                typed_dest += dest_stride;
            }
        }
        break;

      assert_otherwise;
    }
}

template <>
inline void Pixel::convert_to_format<std::uint16_t>(
    const std::uint16_t*    src_begin,
    const std::uint16_t*    src_end,
    const size_t            src_stride,
    const PixelFormat       dest_format,
    void*                   dest,
    const size_t            dest_stride)
{
    assert(src_begin);
    assert(src_end);
    assert(dest);

    switch (dest_format)
    {
      case PixelFormatUInt8:                // lossy std::uint16_t -> std::uint8_t
        {
            std::uint8_t* typed_dest = reinterpret_cast<std::uint8_t*>(dest);
            for (const std::uint16_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<std::uint8_t>(*it >> 8);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossless std::uint16_t -> std::uint16_t
        {
            std::uint16_t* typed_dest = reinterpret_cast<std::uint16_t*>(dest);
            for (const std::uint16_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = *it;
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossless std::uint16_t -> std::uint32_t
        {
            std::uint32_t* typed_dest = reinterpret_cast<std::uint32_t*>(dest);
            for (const std::uint16_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<std::uint32_t>(*it) * (4294967295u / 65535);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossy std::uint16_t -> half
        {
            Half* typed_dest = reinterpret_cast<Half*>(dest);
            for (const std::uint16_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<Half>(static_cast<float>(*it) * (1.0f / 65535));
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossless std::uint16_t -> float
        {
            float* typed_dest = reinterpret_cast<float*>(dest);
            for (const std::uint16_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<float>(*it) * (1.0f / 65535);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossless std::uint16_t -> double
        {
            double* typed_dest = reinterpret_cast<double*>(dest);
            for (const std::uint16_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<double>(*it) * (1.0 / 65535);
                typed_dest += dest_stride;
            }
        }
        break;

      assert_otherwise;
    }
}

template <>
inline void Pixel::convert_to_format<std::uint32_t>(
    const std::uint32_t*    src_begin,
    const std::uint32_t*    src_end,
    const size_t            src_stride,
    const PixelFormat       dest_format,
    void*                   dest,
    const size_t            dest_stride)
{
    assert(src_begin);
    assert(src_end);
    assert(dest);

    switch (dest_format)
    {
      case PixelFormatUInt8:                // lossy std::uint32_t -> std::uint8_t
        {
            std::uint8_t* typed_dest = reinterpret_cast<std::uint8_t*>(dest);
            for (const std::uint32_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<std::uint8_t>(*it >> 24);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossy std::uint32_t -> std::uint16_t
        {
            std::uint16_t* typed_dest = reinterpret_cast<std::uint16_t*>(dest);
            for (const std::uint32_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<std::uint16_t>(*it >> 16);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossless std::uint32_t -> std::uint32_t
        {
            std::uint32_t* typed_dest = reinterpret_cast<std::uint32_t*>(dest);
            for (const std::uint32_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = *it;
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossy std::uint32_t -> half
        {
            Half* typed_dest = reinterpret_cast<Half*>(dest);
            for (const std::uint32_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<Half>(static_cast<float>(*it) * (1.0f / 4294967295u));
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossy std::uint32_t -> float
        {
            float* typed_dest = reinterpret_cast<float*>(dest);
            for (const std::uint32_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<float>(*it) * (1.0f / 4294967295u);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossless std::uint32_t -> double
        {
            double* typed_dest = reinterpret_cast<double*>(dest);
            for (const std::uint32_t* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<double>(*it) * (1.0 / 4294967295u);
                typed_dest += dest_stride;
            }
        }
        break;

      assert_otherwise;
    }
}

template <>
inline void Pixel::convert_to_format<Half>(
    const Half*             src_begin,
    const Half*             src_end,
    const size_t            src_stride,
    const PixelFormat       dest_format,
    void*                   dest,
    const size_t            dest_stride)
{
    assert(src_begin);
    assert(src_end);
    assert(dest);

    switch (dest_format)
    {
      case PixelFormatUInt8:                // lossy half -> std::uint8_t
        {
            std::uint8_t* typed_dest = reinterpret_cast<std::uint8_t*>(dest);
            for (const Half* it = src_begin; it < src_end; it += src_stride)
            {
                const float val = clamp(*it * 256.0f, 0.0f, 255.0f);
                *typed_dest = truncate<std::uint8_t>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossy half -> std::uint16_t
        {
            std::uint16_t* typed_dest = reinterpret_cast<std::uint16_t*>(dest);
            for (const Half* it = src_begin; it < src_end; it += src_stride)
            {
                const float val = clamp(*it * 65536.0f, 0.0f, 65535.0f);
                *typed_dest = truncate<std::uint16_t>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossy half -> std::uint32_t
        {
            std::uint32_t* typed_dest = reinterpret_cast<std::uint32_t*>(dest);
            for (const Half* it = src_begin; it < src_end; it += src_stride)
            {
                const double val = clamp(static_cast<double>(*it) * 4294967296.0, 0.0, 4294967295.0);
                *typed_dest = truncate<std::uint32_t>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossless half -> half
        {
            Half* typed_dest = reinterpret_cast<Half*>(dest);
            for (const Half* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = *it;
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossless half -> float
        {
            float* typed_dest = reinterpret_cast<float*>(dest);
            for (const Half* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<float>(*it);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossless half -> double
        {
            double* typed_dest = reinterpret_cast<double*>(dest);
            for (const Half* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<double>(*it);
                typed_dest += dest_stride;
            }
        }
        break;

      assert_otherwise;
    }
}

template <>
inline void Pixel::convert_to_format<float>(
    const float*            src_begin,
    const float*            src_end,
    const size_t            src_stride,
    const PixelFormat       dest_format,
    void*                   dest,
    const size_t            dest_stride)
{
    assert(src_begin);
    assert(src_end);
    assert(dest);

    switch (dest_format)
    {
      case PixelFormatUInt8:                // lossy float -> std::uint8_t
        {
            // todo: optimize this case using SSE?
            std::uint8_t* typed_dest = reinterpret_cast<std::uint8_t*>(dest);
            for (const float* it = src_begin; it < src_end; it += src_stride)
            {
                const float val = clamp(*it * 256.0f, 0.0f, 255.0f);
                *typed_dest = truncate<std::uint8_t>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossy float -> std::uint16_t
        {
            std::uint16_t* typed_dest = reinterpret_cast<std::uint16_t*>(dest);
            for (const float* it = src_begin; it < src_end; it += src_stride)
            {
                const float val = clamp(*it * 65536.0f, 0.0f, 65535.0f);
                *typed_dest = truncate<std::uint16_t>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossy float -> std::uint32_t
        {
            std::uint32_t* typed_dest = reinterpret_cast<std::uint32_t*>(dest);
            for (const float* it = src_begin; it < src_end; it += src_stride)
            {
                const double val = clamp(static_cast<double>(*it) * 4294967296.0, 0.0, 4294967295.0);
                *typed_dest = truncate<std::uint32_t>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossy float -> half
        {
            Half* typed_dest = reinterpret_cast<Half*>(dest);
            for (const float* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<Half>(*it);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossless float -> float
        {
            float* typed_dest = reinterpret_cast<float*>(dest);
            for (const float* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = *it;
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossless float -> double
        {
            double* typed_dest = reinterpret_cast<double*>(dest);
            for (const float* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<double>(*it);
                typed_dest += dest_stride;
            }
        }
        break;

      assert_otherwise;
    }
}

template <>
inline void Pixel::convert_to_format<double>(
    const double*           src_begin,
    const double*           src_end,
    const size_t            src_stride,
    const PixelFormat       dest_format,
    void*                   dest,
    const size_t            dest_stride)
{
    assert(src_begin);
    assert(src_end);
    assert(dest);

    switch (dest_format)
    {
      case PixelFormatUInt8:                // lossy double -> std::uint8_t
        {
            std::uint8_t* typed_dest = reinterpret_cast<std::uint8_t*>(dest);
            for (const double* it = src_begin; it < src_end; it += src_stride)
            {
                const double val = clamp(*it * 256.0, 0.0, 255.0);
                *typed_dest = truncate<std::uint8_t>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossy double -> std::uint16_t
        {
            std::uint16_t* typed_dest = reinterpret_cast<std::uint16_t*>(dest);
            for (const double* it = src_begin; it < src_end; it += src_stride)
            {
                const double val = clamp(*it * 65536.0, 0.0, 65535.0);
                *typed_dest = truncate<std::uint16_t>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossy double -> std::uint32_t
        {
            std::uint32_t* typed_dest = reinterpret_cast<std::uint32_t*>(dest);
            for (const double* it = src_begin; it < src_end; it += src_stride)
            {
                const double val = clamp(*it * 4294967296.0, 0.0, 4294967295.0);
                *typed_dest = truncate<std::uint32_t>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossy double -> half
        {
            Half* typed_dest = reinterpret_cast<Half*>(dest);
            for (const double* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<Half>(static_cast<float>(*it));
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossy double -> float
        {
            float* typed_dest = reinterpret_cast<float*>(dest);
            for (const double* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<float>(*it);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossless double -> double
        {
            double* typed_dest = reinterpret_cast<double*>(dest);
            for (const double* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = *it;
                typed_dest += dest_stride;
            }
        }
        break;

      assert_otherwise;
    }
}

template <>
inline void Pixel::convert_from_format<std::uint8_t>(
    const PixelFormat       src_format,
    const void*             src_begin,
    const void*             src_end,
    const size_t            src_stride,
    std::uint8_t*           dest,
    const size_t            dest_stride)
{
    assert(src_begin);
    assert(src_end);
    assert(dest);

    switch (src_format)
    {
      case PixelFormatUInt8:                // lossless std::uint8_t -> std::uint8_t
        {
            const std::uint8_t* it = reinterpret_cast<const std::uint8_t*>(src_begin);
            for (; it < src_end; it += src_stride)
            {
                *dest = *it;
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossy std::uint16_t -> std::uint8_t
        {
            const std::uint16_t* it = reinterpret_cast<const std::uint16_t*>(src_begin);
            for (; it < reinterpret_cast<const std::uint16_t*>(src_end); it += src_stride)
            {
                *dest = static_cast<std::uint8_t>(*it >> 8);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossy std::uint32_t -> std::uint8_t
        {
            const std::uint32_t* it = reinterpret_cast<const std::uint32_t*>(src_begin);
            for (; it < reinterpret_cast<const std::uint32_t*>(src_end); it += src_stride)
            {
                *dest = static_cast<std::uint8_t>(*it >> 24);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossy half -> std::uint8_t
        {
            const Half* it = reinterpret_cast<const Half*>(src_begin);
            for (; it < reinterpret_cast<const Half*>(src_end); it += src_stride)
            {
                const Half val = static_cast<Half>(clamp(*it * 256.0f, 0.0f, 255.0f));
                *dest = truncate<std::uint8_t>(val);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossy float -> std::uint8_t
        {
            const float* it = reinterpret_cast<const float*>(src_begin);
            for (; it < reinterpret_cast<const float*>(src_end); it += src_stride)
            {
                const float val = clamp(*it * 256.0f, 0.0f, 255.0f);
                *dest = truncate<std::uint8_t>(val);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossy double -> std::uint8_t
        {
            const double* it = reinterpret_cast<const double*>(src_begin);
            for (; it < reinterpret_cast<const double*>(src_end); it += src_stride)
            {
                const double val = clamp(*it * 256.0, 0.0, 255.0);
                *dest = truncate<std::uint8_t>(val);
                dest += dest_stride;
            }
        }
        break;

      assert_otherwise;
    }
}

template <>
inline void Pixel::convert_from_format<std::uint16_t>(
    const PixelFormat       src_format,
    const void*             src_begin,
    const void*             src_end,
    const size_t            src_stride,
    std::uint16_t*          dest,
    const size_t            dest_stride)
{
    assert(src_begin);
    assert(src_end);
    assert(dest);

    switch (src_format)
    {
      case PixelFormatUInt8:                // lossless std::uint8_t -> std::uint16_t
        {
            const std::uint8_t* it = reinterpret_cast<const std::uint8_t*>(src_begin);
            for (; it < reinterpret_cast<const std::uint8_t*>(src_end); it += src_stride)
            {
                *dest = static_cast<std::uint16_t>(*it) * (65535 / 255);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossless std::uint16_t -> std::uint16_t
        {
            const std::uint16_t* it = reinterpret_cast<const std::uint16_t*>(src_begin);
            for (; it < reinterpret_cast<const std::uint16_t*>(src_end); it += src_stride)
            {
                *dest = *it;
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossy std::uint32_t -> std::uint16_t
        {
            const std::uint32_t* it = reinterpret_cast<const std::uint32_t*>(src_begin);
            for (; it < reinterpret_cast<const std::uint32_t*>(src_end); it += src_stride)
            {
                *dest = static_cast<std::uint16_t>(*it >> 16);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossy half -> std::uint16_t
        {
            const Half* it = reinterpret_cast<const Half*>(src_begin);
            for (; it < reinterpret_cast<const Half*>(src_end); it += src_stride)
            {
                const Half val = static_cast<Half>(clamp(*it * 65536.0f, 0.0f, 65535.0f));
                *dest = truncate<std::uint16_t>(val);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossy float -> std::uint16_t
        {
            const float* it = reinterpret_cast<const float*>(src_begin);
            for (; it < reinterpret_cast<const float*>(src_end); it += src_stride)
            {
                const float val = clamp(*it * 65536.0f, 0.0f, 65535.0f);
                *dest = truncate<std::uint16_t>(val);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossy double -> std::uint16_t
        {
            const double* it = reinterpret_cast<const double*>(src_begin);
            for (; it < reinterpret_cast<const double*>(src_end); it += src_stride)
            {
                const double val = clamp(*it * 65536.0, 0.0, 65535.0);
                *dest = truncate<std::uint16_t>(val);
                dest += dest_stride;
            }
        }
        break;

      assert_otherwise;
    }
}

template <>
inline void Pixel::convert_from_format<std::uint32_t>(
    const PixelFormat       src_format,
    const void*             src_begin,
    const void*             src_end,
    const size_t            src_stride,
    std::uint32_t*          dest,
    const size_t            dest_stride)
{
    assert(src_begin);
    assert(src_end);
    assert(dest);

    switch (src_format)
    {
      case PixelFormatUInt8:                // lossless std::uint8_t -> std::uint32_t
        {
            const std::uint8_t* it = reinterpret_cast<const std::uint8_t*>(src_begin);
            for (; it < reinterpret_cast<const std::uint8_t*>(src_end); it += src_stride)
            {
                *dest = static_cast<std::uint32_t>(*it) * (4294967295u / 255);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossless std::uint16_t -> std::uint32_t
        {
            const std::uint16_t* it = reinterpret_cast<const std::uint16_t*>(src_begin);
            for (; it < reinterpret_cast<const std::uint16_t*>(src_end); it += src_stride)
            {
                *dest = static_cast<std::uint32_t>(*it) * (4294967295u / 65535);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossless std::uint32_t -> std::uint32_t
        {
            const std::uint32_t* it = reinterpret_cast<const std::uint32_t*>(src_begin);
            for (; it < reinterpret_cast<const std::uint32_t*>(src_end); it += src_stride)
            {
                *dest = *it;
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossy half -> std::uint32_t
        {
            const Half* it = reinterpret_cast<const Half*>(src_begin);
            for (; it < reinterpret_cast<const Half*>(src_end); it += src_stride)
            {
                const double val = clamp(static_cast<double>(*it) * 4294967296.0, 0.0, 4294967295.0);
                *dest = truncate<std::uint32_t>(val);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossy float -> std::uint32_t
        {
            const float* it = reinterpret_cast<const float*>(src_begin);
            for (; it < reinterpret_cast<const float*>(src_end); it += src_stride)
            {
                const double val = clamp(static_cast<double>(*it) * 4294967296.0, 0.0, 4294967295.0);
                *dest = truncate<std::uint32_t>(val);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossy double -> std::uint32_t
        {
            const double* it = reinterpret_cast<const double*>(src_begin);
            for (; it < reinterpret_cast<const double*>(src_end); it += src_stride)
            {
                const double val = clamp(*it * 4294967296.0, 0.0, 4294967295.0);
                *dest = truncate<std::uint32_t>(val);
                dest += dest_stride;
            }
        }
        break;

      assert_otherwise;
    }
}

template <>
inline void Pixel::convert_from_format<float>(
    const PixelFormat       src_format,
    const void*             src_begin,
    const void*             src_end,
    const size_t            src_stride,
    float*                  dest,
    const size_t            dest_stride)
{
    assert(src_begin);
    assert(src_end);
    assert(dest);

    switch (src_format)
    {
      case PixelFormatUInt8:                // lossless std::uint8_t -> float
        {
            const std::uint8_t* it = reinterpret_cast<const std::uint8_t*>(src_begin);
            for (; it < reinterpret_cast<const std::uint8_t*>(src_end); it += src_stride)
            {
                *dest = static_cast<float>(*it) * (1.0f / 255);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossless std::uint16_t -> float
        {
            const std::uint16_t* it = reinterpret_cast<const std::uint16_t*>(src_begin);
            for (; it < reinterpret_cast<const std::uint16_t*>(src_end); it += src_stride)
            {
                *dest = static_cast<float>(*it) * (1.0f / 65535);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossy std::uint32_t -> float
        {
            const std::uint32_t* it = reinterpret_cast<const std::uint32_t*>(src_begin);
            for (; it < reinterpret_cast<const std::uint32_t*>(src_end); it += src_stride)
            {
                *dest = static_cast<float>(*it) * (1.0f / 4294967295u);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossless half -> float
        {
            const Half* it = reinterpret_cast<const Half*>(src_begin);
            for (; it < reinterpret_cast<const Half*>(src_end); it += src_stride)
            {
                *dest = static_cast<float>(*it);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossless float -> float
        {
            const float* it = reinterpret_cast<const float*>(src_begin);
            for (; it < reinterpret_cast<const float*>(src_end); it += src_stride)
            {
                *dest = *it;
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossy double -> float
        {
            const double* it = reinterpret_cast<const double*>(src_begin);
            for (; it < reinterpret_cast<const double*>(src_end); it += src_stride)
            {
                *dest = static_cast<float>(*it);
                dest += dest_stride;
            }
        }
        break;

      assert_otherwise;
    }
}

template <>
inline void Pixel::convert_from_format<double>(
    const PixelFormat       src_format,
    const void*             src_begin,
    const void*             src_end,
    const size_t            src_stride,
    double*                 dest,
    const size_t            dest_stride)
{
    assert(src_begin);
    assert(src_end);
    assert(dest);

    switch (src_format)
    {
      case PixelFormatUInt8:                // lossless std::uint8_t -> double
        {
            const std::uint8_t* it = reinterpret_cast<const std::uint8_t*>(src_begin);
            for (; it < reinterpret_cast<const std::uint8_t*>(src_end); it += src_stride)
            {
                *dest = static_cast<double>(*it) * (1.0 / 255);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossless std::uint16_t -> double
        {
            const std::uint16_t* it = reinterpret_cast<const std::uint16_t*>(src_begin);
            for (; it < reinterpret_cast<const std::uint16_t*>(src_end); it += src_stride)
            {
                *dest = static_cast<double>(*it) * (1.0 / 65535);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossless std::uint32_t -> double
        {
            const std::uint32_t* it = reinterpret_cast<const std::uint32_t*>(src_begin);
            for (; it < reinterpret_cast<const std::uint32_t*>(src_end); it += src_stride)
            {
                *dest = static_cast<double>(*it) * (1.0 / 4294967295u);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossless half -> double
        {
            const Half* it = reinterpret_cast<const Half*>(src_begin);
            for (; it < reinterpret_cast<const Half*>(src_end); it += src_stride)
            {
                *dest = static_cast<double>(*it);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossless float -> double
        {
            const float* it = reinterpret_cast<const float*>(src_begin);
            for (; it < reinterpret_cast<const float*>(src_end); it += src_stride)
            {
                *dest = static_cast<double>(*it);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossless double -> double
        {
            const double* it = reinterpret_cast<const double*>(src_begin);
            for (; it < reinterpret_cast<const double*>(src_end); it += src_stride)
            {
                *dest = *it;
                dest += dest_stride;
            }
        }
        break;

      assert_otherwise;
    }
}

inline void Pixel::convert(
    const PixelFormat       src_format,
    const void*             src_begin,
    const void*             src_end,
    const size_t            src_stride,
    const PixelFormat       dest_format,
    void*                   dest,
    const size_t            dest_stride)
{
    assert(src_begin);
    assert(src_end);
    assert(dest);

    switch (src_format)
    {
      case PixelFormatUInt8:                // std::uint8_t -> destination format
        convert_to_format<std::uint8_t>(
            reinterpret_cast<const std::uint8_t*>(src_begin),
            reinterpret_cast<const std::uint8_t*>(src_end),
            src_stride,
            dest_format,
            dest,
            dest_stride);
        break;

      case PixelFormatUInt16:               // std::uint16_t -> destination format
        convert_to_format<std::uint16_t>(
            reinterpret_cast<const std::uint16_t*>(src_begin),
            reinterpret_cast<const std::uint16_t*>(src_end),
            src_stride,
            dest_format,
            dest,
            dest_stride);
        break;

      case PixelFormatUInt32:               // std::uint32_t -> destination format
        convert_to_format<std::uint32_t>(
            reinterpret_cast<const std::uint32_t*>(src_begin),
            reinterpret_cast<const std::uint32_t*>(src_end),
            src_stride,
            dest_format,
            dest,
            dest_stride);
        break;

      case PixelFormatHalf:                 // half -> destination format
        convert_to_format<Half>(
            reinterpret_cast<const Half*>(src_begin),
            reinterpret_cast<const Half*>(src_end),
            src_stride,
            dest_format,
            dest,
            dest_stride);
        break;

      case PixelFormatFloat:                // float -> destination format
        convert_to_format<float>(
            reinterpret_cast<const float*>(src_begin),
            reinterpret_cast<const float*>(src_end),
            src_stride,
            dest_format,
            dest,
            dest_stride);
        break;

      case PixelFormatDouble:               // double -> destination format
        convert_to_format<double>(
            reinterpret_cast<const double*>(src_begin),
            reinterpret_cast<const double*>(src_end),
            src_stride,
            dest_format,
            dest,
            dest_stride);
        break;

      assert_otherwise;
    }
}

}   // namespace foundation
