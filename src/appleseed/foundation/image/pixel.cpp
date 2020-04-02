
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
#include "pixel.h"

namespace foundation
{

const char* pixel_format_name(const PixelFormat pixel_format)
{
    switch (pixel_format)
    {
      case PixelFormatUInt8:    return "uint8";
      case PixelFormatUInt16:   return "uint16";
      case PixelFormatUInt32:   return "uint32";
      case PixelFormatHalf:     return "half";
      case PixelFormatFloat:    return "float";
      case PixelFormatDouble:   return "double";
      assert_otherwise;
    }

    // Keep the compiler happy.
    return "";
}

void Pixel::convert_and_shuffle(
    const PixelFormat   src_format,
    const size_t        src_channels,
    const void*         src_begin,
    const void*         src_end,
    const PixelFormat   dest_format,
    const size_t        dest_channels,
    void*               dest,
    const size_t*       shuffle_table)
{
    // Compute size in bytes of source and destination pixel formats.
    const size_t src_channel_size = size(src_format);
    const size_t dest_channel_size = size(dest_format);

    // Loop over all entries in the channel shuffling table.
    size_t dest_channel_offset = 0;
    for (size_t i = 0; i < src_channels; ++i)
    {
        // Fetch source channel index.
        const size_t src_channel_index = shuffle_table[i];

        // Check validity of source channel index.
        assert(
            src_channel_index == SkipChannel ||
            src_channel_index < src_channels);

        // Skip channels marked as such.
        if (src_channel_index == SkipChannel)
            continue;

        // Compute offset in bytes of source channel.
        const size_t src_channel_offset =
            src_channel_index * src_channel_size;

        // Convert source channel to destination channel.
        convert(
            src_format,
            reinterpret_cast<const std::uint8_t*>(src_begin) + src_channel_offset,
            reinterpret_cast<const std::uint8_t*>(src_end) + src_channel_offset,
            src_channels,
            dest_format,
            reinterpret_cast<std::uint8_t*>(dest) + dest_channel_offset,
            dest_channels);

        // Compute offset in bytes of next destination channel.
        dest_channel_offset += dest_channel_size;
    }
}

size_t Pixel::get_dest_channel_count(
    const size_t        src_channels,
    const size_t*       shuffle_table)
{
    size_t dest_channel_count = 0;

    for (size_t i = 0; i < src_channels; ++i)
    {
        // Fetch source channel index.
        const size_t src_channel_index = shuffle_table[i];

        // Check validity of source channel index.
        assert(
            src_channel_index == SkipChannel ||
            src_channel_index < src_channels);

        // Count destination channels.
        if (src_channel_index != SkipChannel)
            ++dest_channel_count;
    }

    return dest_channel_count;
}

}   // namespace foundation
