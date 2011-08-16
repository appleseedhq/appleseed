
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_IMAGE_TILE_H
#define APPLESEED_FOUNDATION_IMAGE_TILE_H

// appleseed.foundation headers.
#include "foundation/image/pixel.h"
#include "foundation/platform/types.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/serialization.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstring>

//
// On Windows, define FOUNDATIONDLL to __declspec(dllexport) when building the DLL
// and to __declspec(dllimport) when building an application using the DLL.
// Other platforms don't use this export mechanism and the symbol FOUNDATIONDLL is
// defined to evaluate to nothing.
//

#ifndef FOUNDATIONDLL
#ifdef _WIN32
#ifdef APPLESEED_FOUNDATION_EXPORTS
#define FOUNDATIONDLL __declspec(dllexport)
#else
#define FOUNDATIONDLL __declspec(dllimport)
#endif
#else
#define FOUNDATIONDLL
#endif
#endif

namespace foundation
{

//
// A tile, as a 2D array of pixels.
//

class FOUNDATIONDLL Tile
  : public ISerializable
{
  public:
    // Construct a new tile. The content of the tile is left undefined.
    Tile(
        const size_t        width,                  // tile width, in pixels
        const size_t        height,                 // tile height, in pixels
        const size_t        channel_count,          // number of channels
        const PixelFormat   pixel_format,           // pixel format
        uint8*              storage = 0);           // if provided, use this memory for pixel storage

    // Construct a tile by converting an existing tile to a given pixel format.
    Tile(
        const Tile&         tile,                   // source tile
        const PixelFormat   pixel_format,           // new pixel format
        uint8*              storage = 0);           // if provided, use this memory for pixel storage

    // Construct a tile by converting an existing tile to a given pixel format,
    // and allowing reordering, replication and deletion of channels.
    Tile(
        const Tile&         tile,                   // source tile
        const PixelFormat   pixel_format,           // new pixel format
        const size_t*       shuffle_table,          // channel shuffling table
        uint8*              storage = 0);           // if provided, use this memory for pixel storage

    // Copy constructor.
    Tile(const Tile&        rhs);

    // Destructor.
    virtual ~Tile();

    // Implements the foundation::ISerializable interface.
    virtual Serializer* serialize(Serializer* serializer);
    virtual Deserializer* deserialize(Deserializer* deserializer);

    // Tile properties.
    PixelFormat get_pixel_format() const;
    size_t get_width() const;
    size_t get_height() const;
    size_t get_channel_count() const;               // number of channels in one pixel
    size_t get_pixel_count() const;                 // number of pixels
    size_t get_size() const;                        // size in bytes of the pixel array

    // Return a pointer to the tile' storage.
    uint8* get_storage() const;

    // Direct access to a given pixel.
    uint8* pixel(
        const size_t        x,
        const size_t        y) const;
    uint8* pixel(
        const size_t        i) const;

    // Direct access to a given component of a given pixel.
    uint8* component(
        const size_t        x,
        const size_t        y,
        const size_t        c) const;
    uint8* component(
        const size_t        i,
        const size_t        c) const;

    // Structured write access to a given pixel, with automatic pixel format conversion.
    template <typename T>
    void set_pixel(
        const size_t        x,
        const size_t        y,
        const T&            val);                   // pixel value
    template <typename T>
    void set_pixel(
        const size_t        i,
        const T&            val);                   // pixel value

    // Structured write access to a given component of a given pixel.
    template <typename T>
    void set_component(
        const size_t        x,
        const size_t        y,
        const size_t        c,
        const T             val);                   // component value
    template <typename T>
    void set_component(
        const size_t        i,
        const size_t        c,
        const T             val);                   // component value

    // Structured read access to a given pixel, with automatic pixel format conversion.
    template <typename T>
    void get_pixel(
        const size_t        x,
        const size_t        y,
        T&                  val) const;             // [out] pixel value
    template <typename T>
    void get_pixel(
        const size_t        i,
        T&                  val) const;             // [out] pixel value

    // Structured read access to a given component of a given pixel.
    template <typename T>
    T get_component(
        const size_t        x,
        const size_t        y,
        const size_t        c) const;
    template <typename T>
    T get_component(
        const size_t        i,
        const size_t        c) const;

    // Set all pixels to a given color.
    template <typename T>
    void clear(const T&     val);                   // pixel value

  private:
    const size_t    m_width;                        // tile width, in pixels
    const size_t    m_height;                       // tile height, in pixels
    size_t          m_channel_count;                // number of channels per pixel
    PixelFormat     m_pixel_format;                 // pixel format
    size_t          m_pixel_count;                  // total number of pixels
    size_t          m_channel_size;                 // size in bytes of one channel
    size_t          m_pixel_size;                   // size in bytes of one pixel
    size_t          m_array_size;                   // size in bytes of the pixel array
    uint8*          m_pixel_array;                  // pixel array
    bool            m_own_storage;                  // does the tile own the memory used for pixel storage?

    // Forbid usage of assignment operator.
    const Tile& operator=(const Tile&);             // intentionally left unimplemented
};

// Return the size of a tile, including the dynamically allocated memory.
size_t dynamic_sizeof(const Tile& tile);


//
// Tile class implementation.
//

inline PixelFormat Tile::get_pixel_format() const
{
    return m_pixel_format;
}

inline size_t Tile::get_width() const
{
    return m_width;
}

inline size_t Tile::get_height() const
{
    return m_height;
}

inline size_t Tile::get_channel_count() const
{
    return m_channel_count;
}

inline size_t Tile::get_pixel_count() const
{
    return m_pixel_count;
}

inline size_t Tile::get_size() const
{
    return m_array_size;
}

inline uint8* Tile::get_storage() const
{
    return m_pixel_array;
}

inline uint8* Tile::pixel(
    const size_t    x,
    const size_t    y) const
{
    assert(x < m_width);
    assert(y < m_height);
    const size_t index = (y * m_width + x) * m_pixel_size;
    assert(index < m_array_size);
    return m_pixel_array + index;
}

inline uint8* Tile::pixel(
    const size_t    i) const
{
    assert(i < m_pixel_count);
    const size_t index = i * m_pixel_size;
    assert(index < m_array_size);
    return m_pixel_array + index;
}

inline uint8* Tile::component(
    const size_t    x,
    const size_t    y,
    const size_t    c) const
{
    return pixel(x, y) + c * m_channel_size;
}

inline uint8* Tile::component(
    const size_t    i,
    const size_t    c) const
{
    return pixel(i) + c * m_channel_size;
}

// Check that the number of channels in a pixel value matches
// the number of channels in the tile.
#define FOUNDATION_CHECK_PIXEL_SIZE(val) \
    assert(sizeof(T) == m_channel_count * sizeof(val[0]))

template <typename T>
inline void Tile::set_pixel(
    const size_t    x,
    const size_t    y,
    const T&        val)
{
    FOUNDATION_CHECK_PIXEL_SIZE(val);

    Pixel::convert_to_format(
        &val[0],                                // source begin
        &val[0] + m_channel_count,              // source end
        1,                                      // source stride
        m_pixel_format,                         // destination format
        pixel(x, y),                            // destination
        1);                                     // destination stride
}

template <typename T>
inline void Tile::set_pixel(
    const size_t    i,
    const T&        val)
{
    FOUNDATION_CHECK_PIXEL_SIZE(val);

    Pixel::convert_to_format(
        &val[0],                                // source begin
        &val[0] + m_channel_count,              // source end
        1,                                      // source stride
        m_pixel_format,                         // destination format
        pixel(i),                               // destination
        1);                                     // destination stride
}

template <typename T>
inline void Tile::set_component(
    const size_t    x,
    const size_t    y,
    const size_t    c,
    const T         val)
{
    Pixel::convert_to_format(
        &val,                                   // source begin
        &val + 1,                               // source end
        1,                                      // source stride
        m_pixel_format,                         // destination format
        component(x, y, c),                     // destination
        1);                                     // destination stride
}

template <typename T>
inline void Tile::set_component(
    const size_t    i,
    const size_t    c,
    const T         val)
{
    Pixel::convert_to_format(
        &val,                                   // source begin
        &val + 1,                               // source end
        1,                                      // source stride
        m_pixel_format,                         // destination format
        component(i, c),                        // destination
        1);                                     // destination stride
}

template <typename T>
inline void Tile::get_pixel(
    const size_t    x,
    const size_t    y,
    T&              val) const
{
    FOUNDATION_CHECK_PIXEL_SIZE(val);

    const uint8* src = pixel(x, y);

    Pixel::convert_from_format(
        m_pixel_format,                         // source format
        src,                                    // source begin
        src + m_pixel_size,                     // source end
        1,                                      // source stride
        &val[0],                                // destination
        1);                                     // destination stride
}

template <typename T>
inline void Tile::get_pixel(
    const size_t    i,
    T&              val) const
{
    FOUNDATION_CHECK_PIXEL_SIZE(val);

    const uint8* src = pixel(i);

    Pixel::convert_from_format(
        m_pixel_format,                         // source format
        src,                                    // source begin
        src + m_pixel_size,                     // source end
        1,                                      // source stride
        &val[0],                                // destination
        1);                                     // destination stride
}

template <typename T>
inline T Tile::get_component(
    const size_t    x,
    const size_t    y,
    const size_t    c) const
{
    const uint8* src = component(x, y, c);

    T val;
    Pixel::convert_from_format(
        m_pixel_format,                         // source format
        src,                                    // source begin
        src + m_channel_size,                   // source end
        1,                                      // source stride
        &val,                                   // destination
        1);                                     // destination stride

    return val;
}

template <typename T>
inline T Tile::get_component(
    const size_t    i,
    const size_t    c) const
{
    const uint8* src = component(i, c);

    T val;
    Pixel::convert_from_format(
        m_pixel_format,                         // source format
        src,                                    // source begin
        src + m_channel_size,                   // source end
        1,                                      // source stride
        &val,                                   // destination
        1);                                     // destination stride

    return val;
}

template <typename T>
inline void Tile::clear(
    const T&        val)
{
    FOUNDATION_CHECK_PIXEL_SIZE(val);

    // Set first pixel of first row.
    uint8* base = pixel(0, 0);
    Pixel::convert_to_format(
        &val[0],                                // source begin
        &val[0] + m_channel_count,              // source end
        1,                                      // source stride
        m_pixel_format,                         // destination format
        base,                                   // destination
        1);                                     // destination stride

    // Set remaining pixels of first row.
    uint8* dest = base + m_pixel_size;
    for (size_t i = 1; i < m_width; ++i)
    {
        std::memcpy(dest, base, m_pixel_size);
        dest += m_pixel_size;
    }

    // Set remaining rows.
    const size_t row_size = m_width * m_pixel_size;
    for (size_t i = 1; i < m_height; ++i)
    {
        std::memcpy(dest, base, row_size);
        dest += row_size;
    }
}

#undef FOUNDATION_CHECK_PIXEL_SIZE

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_IMAGE_TILE_H
