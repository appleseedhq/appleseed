
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_IMAGE_PIXEL_H
#define APPLESEED_FOUNDATION_IMAGE_PIXEL_H

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/platform/types.h"
#include "foundation/utility/otherwise.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// OpenEXR headers.
#include "foundation/platform/exrheaderguards.h"
BEGIN_EXR_INCLUDES
#include "OpenEXR/half.h"
END_EXR_INCLUDES

// Standard headers.
#include <cassert>
#include <cstddef>

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
    static const size_t SkipChannel = ~0;

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
inline void Pixel::convert_to_format<uint8>(
    const uint8*            src_begin,
    const uint8*            src_end,
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
      case PixelFormatUInt8:                // lossless uint8 -> uint8
        {
            uint8* typed_dest = reinterpret_cast<uint8*>(dest);
            for (const uint8* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = *it;
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossless uint8 -> uint16
        {
            uint16* typed_dest = reinterpret_cast<uint16*>(dest);
            for (const uint8* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<uint16>(*it) * (65535 / 255);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossless uint8 -> uint32
        {
            uint32* typed_dest = reinterpret_cast<uint32*>(dest);
            for (const uint8* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<uint32>(*it) * (4294967295UL / 255);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossless uint8 -> half
        {
            half* typed_dest = reinterpret_cast<half*>(dest);
            for (const uint8* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<half>(static_cast<float>(*it) * (1.0f / 255));
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossless uint8 -> float
        {
            float* typed_dest = reinterpret_cast<float*>(dest);
            for (const uint8* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<float>(*it) * (1.0f / 255);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossless uint8 -> double
        {
            double* typed_dest = reinterpret_cast<double*>(dest);
            for (const uint8* it = src_begin; it < src_end; it += src_stride)
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
inline void Pixel::convert_to_format<uint16>(
    const uint16*           src_begin,
    const uint16*           src_end,
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
      case PixelFormatUInt8:                // lossy uint16 -> uint8
        {
            uint8* typed_dest = reinterpret_cast<uint8*>(dest);
            for (const uint16* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<uint8>(*it >> 8);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossless uint16 -> uint16
        {
            uint16* typed_dest = reinterpret_cast<uint16*>(dest);
            for (const uint16* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = *it;
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossless uint16 -> uint32
        {
            uint32* typed_dest = reinterpret_cast<uint32*>(dest);
            for (const uint16* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<uint32>(*it) * (4294967295UL / 65535);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossy uint16 -> half
        {
            half* typed_dest = reinterpret_cast<half*>(dest);
            for (const uint16* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<half>(static_cast<float>(*it) * (1.0f / 65535));
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossless uint16 -> float
        {
            float* typed_dest = reinterpret_cast<float*>(dest);
            for (const uint16* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<float>(*it) * (1.0f / 65535);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossless uint16 -> double
        {
            double* typed_dest = reinterpret_cast<double*>(dest);
            for (const uint16* it = src_begin; it < src_end; it += src_stride)
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
inline void Pixel::convert_to_format<uint32>(
    const uint32*           src_begin,
    const uint32*           src_end,
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
      case PixelFormatUInt8:                // lossy uint32 -> uint8
        {
            uint8* typed_dest = reinterpret_cast<uint8*>(dest);
            for (const uint32* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<uint8>(*it >> 24);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossy uint32 -> uint16
        {
            uint16* typed_dest = reinterpret_cast<uint16*>(dest);
            for (const uint32* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<uint16>(*it >> 16);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossless uint32 -> uint32
        {
            uint32* typed_dest = reinterpret_cast<uint32*>(dest);
            for (const uint32* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = *it;
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossy uint32 -> half
        {
            half* typed_dest = reinterpret_cast<half*>(dest);
            for (const uint32* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<half>(static_cast<float>(*it) * (1.0f / 4294967295UL));
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossy uint32 -> float
        {
            float* typed_dest = reinterpret_cast<float*>(dest);
            for (const uint32* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<float>(*it) * (1.0f / 4294967295UL);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossless uint32 -> double
        {
            double* typed_dest = reinterpret_cast<double*>(dest);
            for (const uint32* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<double>(*it) * (1.0 / 4294967295UL);
                typed_dest += dest_stride;
            }
        }
        break;

      assert_otherwise;
    }
}

template <>
inline void Pixel::convert_to_format<half>(
    const half*             src_begin,
    const half*             src_end,
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
      case PixelFormatUInt8:                // lossy half -> uint8
        {
            uint8* typed_dest = reinterpret_cast<uint8*>(dest);
            for (const half* it = src_begin; it < src_end; it += src_stride)
            {
                const float val = clamp(*it * 256.0f, 0.0f, 255.0f);
                *typed_dest = truncate<uint8>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossy half -> uint16
        {
            uint16* typed_dest = reinterpret_cast<uint16*>(dest);
            for (const half* it = src_begin; it < src_end; it += src_stride)
            {
                const float val = clamp(*it * 65536.0f, 0.0f, 65535.0f);
                *typed_dest = truncate<uint16>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossy half -> uint32
        {
            uint32* typed_dest = reinterpret_cast<uint32*>(dest);
            for (const half* it = src_begin; it < src_end; it += src_stride)
            {
                const double val = clamp(static_cast<double>(*it) * 4294967296.0, 0.0, 4294967295.0);
                *typed_dest = truncate<uint32>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossless half -> half
        {
            half* typed_dest = reinterpret_cast<half*>(dest);
            for (const half* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = *it;
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossless half -> float
        {
            float* typed_dest = reinterpret_cast<float*>(dest);
            for (const half* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<float>(*it);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossless half -> double
        {
            double* typed_dest = reinterpret_cast<double*>(dest);
            for (const half* it = src_begin; it < src_end; it += src_stride)
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
      case PixelFormatUInt8:                // lossy float -> uint8
        {
            // todo: optimize this case using SSE?
            uint8* typed_dest = reinterpret_cast<uint8*>(dest);
            for (const float* it = src_begin; it < src_end; it += src_stride)
            {
                const float val = clamp(*it * 256.0f, 0.0f, 255.0f);
                *typed_dest = truncate<uint8>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossy float -> uint16
        {
            uint16* typed_dest = reinterpret_cast<uint16*>(dest);
            for (const float* it = src_begin; it < src_end; it += src_stride)
            {
                const float val = clamp(*it * 65536.0f, 0.0f, 65535.0f);
                *typed_dest = truncate<uint16>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossy float -> uint32
        {
            uint32* typed_dest = reinterpret_cast<uint32*>(dest);
            for (const float* it = src_begin; it < src_end; it += src_stride)
            {
                const double val = clamp(static_cast<double>(*it) * 4294967296.0, 0.0, 4294967295.0);
                *typed_dest = truncate<uint32>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossy float -> half
        {
            half* typed_dest = reinterpret_cast<half*>(dest);
            for (const float* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<half>(*it);
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
      case PixelFormatUInt8:                // lossy double -> uint8
        {
            uint8* typed_dest = reinterpret_cast<uint8*>(dest);
            for (const double* it = src_begin; it < src_end; it += src_stride)
            {
                const double val = clamp(*it * 256.0, 0.0, 255.0);
                *typed_dest = truncate<uint8>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossy double -> uint16
        {
            uint16* typed_dest = reinterpret_cast<uint16*>(dest);
            for (const double* it = src_begin; it < src_end; it += src_stride)
            {
                const double val = clamp(*it * 65536.0, 0.0, 65535.0);
                *typed_dest = truncate<uint16>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossy double -> uint32
        {
            uint32* typed_dest = reinterpret_cast<uint32*>(dest);
            for (const double* it = src_begin; it < src_end; it += src_stride)
            {
                const double val = clamp(*it * 4294967296.0, 0.0, 4294967295.0);
                *typed_dest = truncate<uint32>(val);
                typed_dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossy double -> half
        {
            half* typed_dest = reinterpret_cast<half*>(dest);
            for (const double* it = src_begin; it < src_end; it += src_stride)
            {
                *typed_dest = static_cast<half>(static_cast<float>(*it));
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
inline void Pixel::convert_from_format<uint8>(
    const PixelFormat       src_format,
    const void*             src_begin,
    const void*             src_end,
    const size_t            src_stride,
    uint8*                  dest,
    const size_t            dest_stride)
{
    assert(src_begin);
    assert(src_end);
    assert(dest);

    switch (src_format)
    {
      case PixelFormatUInt8:                // lossless uint8 -> uint8
        {
            const uint8* it = reinterpret_cast<const uint8*>(src_begin);
            for (; it < src_end; it += src_stride)
            {
                *dest = *it;
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossy uint16 -> uint8
        {
            const uint16* it = reinterpret_cast<const uint16*>(src_begin);
            for (; it < reinterpret_cast<const uint16*>(src_end); it += src_stride)
            {
                *dest = static_cast<uint8>(*it >> 8);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossy uint32 -> uint8
        {
            const uint32* it = reinterpret_cast<const uint32*>(src_begin);
            for (; it < reinterpret_cast<const uint32*>(src_end); it += src_stride)
            {
                *dest = static_cast<uint8>(*it >> 24);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossy half -> uint8
        {
            const half* it = reinterpret_cast<const half*>(src_begin);
            for (; it < reinterpret_cast<const half*>(src_end); it += src_stride)
            {
                const half val = static_cast<half>(clamp(*it * 256.0f, 0.0f, 255.0f));
                *dest = truncate<uint8>(val);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossy float -> uint8
        {
            const float* it = reinterpret_cast<const float*>(src_begin);
            for (; it < reinterpret_cast<const float*>(src_end); it += src_stride)
            {
                const float val = clamp(*it * 256.0f, 0.0f, 255.0f);
                *dest = truncate<uint8>(val);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossy double -> uint8
        {
            const double* it = reinterpret_cast<const double*>(src_begin);
            for (; it < reinterpret_cast<const double*>(src_end); it += src_stride)
            {
                const double val = clamp(*it * 256.0, 0.0, 255.0);
                *dest = truncate<uint8>(val);
                dest += dest_stride;
            }
        }
        break;

      assert_otherwise;
    }
}

template <>
inline void Pixel::convert_from_format<uint16>(
    const PixelFormat       src_format,
    const void*             src_begin,
    const void*             src_end,
    const size_t            src_stride,
    uint16*                 dest,
    const size_t            dest_stride)
{
    assert(src_begin);
    assert(src_end);
    assert(dest);

    switch (src_format)
    {
      case PixelFormatUInt8:                // lossless uint8 -> uint16
        {
            const uint8* it = reinterpret_cast<const uint8*>(src_begin);
            for (; it < reinterpret_cast<const uint8*>(src_end); it += src_stride)
            {
                *dest = static_cast<uint16>(*it) * (65535 / 255);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossless uint16 -> uint16
        {
            const uint16* it = reinterpret_cast<const uint16*>(src_begin);
            for (; it < reinterpret_cast<const uint16*>(src_end); it += src_stride)
            {
                *dest = *it;
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossy uint32 -> uint16
        {
            const uint32* it = reinterpret_cast<const uint32*>(src_begin);
            for (; it < reinterpret_cast<const uint32*>(src_end); it += src_stride)
            {
                *dest = static_cast<uint16>(*it >> 16);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossy half -> uint16
        {
            const half* it = reinterpret_cast<const half*>(src_begin);
            for (; it < reinterpret_cast<const half*>(src_end); it += src_stride)
            {
                const half val = static_cast<half>(clamp(*it * 65536.0f, 0.0f, 65535.0f));
                *dest = truncate<uint16>(val);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossy float -> uint16
        {
            const float* it = reinterpret_cast<const float*>(src_begin);
            for (; it < reinterpret_cast<const float*>(src_end); it += src_stride)
            {
                const float val = clamp(*it * 65536.0f, 0.0f, 65535.0f);
                *dest = truncate<uint16>(val);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossy double -> uint16
        {
            const double* it = reinterpret_cast<const double*>(src_begin);
            for (; it < reinterpret_cast<const double*>(src_end); it += src_stride)
            {
                const double val = clamp(*it * 65536.0, 0.0, 65535.0);
                *dest = truncate<uint16>(val);
                dest += dest_stride;
            }
        }
        break;

      assert_otherwise;
    }
}

template <>
inline void Pixel::convert_from_format<uint32>(
    const PixelFormat       src_format,
    const void*             src_begin,
    const void*             src_end,
    const size_t            src_stride,
    uint32*                 dest,
    const size_t            dest_stride)
{
    assert(src_begin);
    assert(src_end);
    assert(dest);

    switch (src_format)
    {
      case PixelFormatUInt8:                // lossless uint8 -> uint32
        {
            const uint8* it = reinterpret_cast<const uint8*>(src_begin);
            for (; it < reinterpret_cast<const uint8*>(src_end); it += src_stride)
            {
                *dest = static_cast<uint32>(*it) * (4294967295UL / 255);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossless uint16 -> uint32
        {
            const uint16* it = reinterpret_cast<const uint16*>(src_begin);
            for (; it < reinterpret_cast<const uint16*>(src_end); it += src_stride)
            {
                *dest = static_cast<uint32>(*it) * (4294967295UL / 65535);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossless uint32 -> uint32
        {
            const uint32* it = reinterpret_cast<const uint32*>(src_begin);
            for (; it < reinterpret_cast<const uint32*>(src_end); it += src_stride)
            {
                *dest = *it;
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossy half -> uint32
        {
            const half* it = reinterpret_cast<const half*>(src_begin);
            for (; it < reinterpret_cast<const half*>(src_end); it += src_stride)
            {
                const double val = clamp(static_cast<double>(*it) * 4294967296.0, 0.0, 4294967295.0);
                *dest = truncate<uint32>(val);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatFloat:                // lossy float -> uint32
        {
            const float* it = reinterpret_cast<const float*>(src_begin);
            for (; it < reinterpret_cast<const float*>(src_end); it += src_stride)
            {
                const double val = clamp(static_cast<double>(*it) * 4294967296.0, 0.0, 4294967295.0);
                *dest = truncate<uint32>(val);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatDouble:               // lossy double -> uint32
        {
            const double* it = reinterpret_cast<const double*>(src_begin);
            for (; it < reinterpret_cast<const double*>(src_end); it += src_stride)
            {
                const double val = clamp(*it * 4294967296.0, 0.0, 4294967295.0);
                *dest = truncate<uint32>(val);
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
      case PixelFormatUInt8:                // lossless uint8 -> float
        {
            const uint8* it = reinterpret_cast<const uint8*>(src_begin);
            for (; it < reinterpret_cast<const uint8*>(src_end); it += src_stride)
            {
                *dest = static_cast<float>(*it) * (1.0f / 255);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossless uint16 -> float
        {
            const uint16* it = reinterpret_cast<const uint16*>(src_begin);
            for (; it < reinterpret_cast<const uint16*>(src_end); it += src_stride)
            {
                *dest = static_cast<float>(*it) * (1.0f / 65535);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossy uint32 -> float
        {
            const uint32* it = reinterpret_cast<const uint32*>(src_begin);
            for (; it < reinterpret_cast<const uint32*>(src_end); it += src_stride)
            {
                *dest = static_cast<float>(*it) * (1.0f / 4294967295UL);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossless half -> float
        {
            const half* it = reinterpret_cast<const half*>(src_begin);
            for (; it < reinterpret_cast<const half*>(src_end); it += src_stride)
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
      case PixelFormatUInt8:                // lossless uint8 -> double
        {
            const uint8* it = reinterpret_cast<const uint8*>(src_begin);
            for (; it < reinterpret_cast<const uint8*>(src_end); it += src_stride)
            {
                *dest = static_cast<double>(*it) * (1.0 / 255);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt16:               // lossless uint16 -> double
        {
            const uint16* it = reinterpret_cast<const uint16*>(src_begin);
            for (; it < reinterpret_cast<const uint16*>(src_end); it += src_stride)
            {
                *dest = static_cast<double>(*it) * (1.0 / 65535);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatUInt32:               // lossless uint32 -> double
        {
            const uint32* it = reinterpret_cast<const uint32*>(src_begin);
            for (; it < reinterpret_cast<const uint32*>(src_end); it += src_stride)
            {
                *dest = static_cast<double>(*it) * (1.0 / 4294967295UL);
                dest += dest_stride;
            }
        }
        break;

      case PixelFormatHalf:                 // lossless half -> double
        {
            const half* it = reinterpret_cast<const half*>(src_begin);
            for (; it < reinterpret_cast<const half*>(src_end); it += src_stride)
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
      case PixelFormatUInt8:                // uint8 -> destination format
        convert_to_format<uint8>(
            reinterpret_cast<const uint8*>(src_begin),
            reinterpret_cast<const uint8*>(src_end),
            src_stride,
            dest_format,
            dest,
            dest_stride);
        break;

      case PixelFormatUInt16:               // uint16 -> destination format
        convert_to_format<uint16>(
            reinterpret_cast<const uint16*>(src_begin),
            reinterpret_cast<const uint16*>(src_end),
            src_stride,
            dest_format,
            dest,
            dest_stride);
        break;

      case PixelFormatUInt32:               // uint32 -> destination format
        convert_to_format<uint32>(
            reinterpret_cast<const uint32*>(src_begin),
            reinterpret_cast<const uint32*>(src_end),
            src_stride,
            dest_format,
            dest,
            dest_stride);
        break;

      case PixelFormatHalf:                 // half -> destination format
        convert_to_format<half>(
            reinterpret_cast<const half*>(src_begin),
            reinterpret_cast<const half*>(src_end),
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

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_IMAGE_PIXEL_H
