
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
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/image.h"
#include "foundation/math/cdf.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/job/iabortswitch.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <utility>

namespace foundation
{

//
// The ImageSampler type must conform to the following prototype:
//
//   class ImageSampler
//   {
//     public:
//       void sample(
//           const size_t   x,
//           const size_t   y,
//           Payload&       payload,
//           Importance&    importance);
//   };
//

template <typename Payload, typename Importance>
class ImageImportanceSampler
  : public NonCopyable
{
  public:
    typedef Vector<Importance, 2> Vector2Type;

    // Constructor.
    ImageImportanceSampler(
        const size_t        width,
        const size_t        height);

    // Destructor.
    ~ImageImportanceSampler();

    // Resample the image and rebuild the CDFs.
    template <typename ImageSampler>
    void rebuild(
        ImageSampler&       sampler,
        IAbortSwitch*       abort_switch = nullptr);

    // Sample the image and return the coordinates of the chosen pixel
    // and its probability density.
    void sample(
        const Vector2Type&  s,
        size_t&             x,
        size_t&             y,
        Importance&         probability) const;

    // Sample the image and return the coordinates of the chosen pixel,
    // its probability density and its associated payload.
    void sample(
        const Vector2Type&  s,
        size_t&             x,
        size_t&             y,
        Payload&            payload,
        Importance&         probability) const;

    // Return the probability density of a given pixel.
    Importance get_pdf(
        const size_t        x,
        const size_t        y) const;

  private:
    typedef CDF<size_t, Importance> RowCDF;
    typedef CDF<Payload, Importance> ColCDF;

    const size_t            m_width;
    const size_t            m_height;
    const Importance        m_rcp_pixel_count;

    ColCDF*                 m_cols_cdf;
    RowCDF                  m_rows_cdf;
};


//
// A simple foundation::Image sampler designed for RGB and RGBA images.
//

class ImageSampler
{
  public:
    struct Payload {};

    explicit ImageSampler(const Image& image);

    void sample(const size_t x, const size_t y, Payload& payload, float& importance) const;

  private:
    const Image& m_image;
};


//
// ImageImportanceSampler class implementation.
//

template <typename Payload, typename Importance>
ImageImportanceSampler<Payload, Importance>::ImageImportanceSampler(
    const size_t            width,
    const size_t            height)
  : m_width(width)
  , m_height(height)
  , m_rcp_pixel_count(Importance(1.0) / (width * height))
{
    m_cols_cdf = new ColCDF[m_height];
}

template <typename Payload, typename Importance>
ImageImportanceSampler<Payload, Importance>::~ImageImportanceSampler()
{
    delete[] m_cols_cdf;
}

template <typename Payload, typename Importance>
template <typename ImageSampler>
void ImageImportanceSampler<Payload, Importance>::rebuild(
    ImageSampler&           sampler,
    IAbortSwitch*           abort_switch)
{
    m_rows_cdf.clear();
    m_rows_cdf.reserve(m_height);

    for (size_t y = 0, ye = m_height; y < ye; ++y)
    {
        if (is_aborted(abort_switch))
        {
            m_rows_cdf.clear();
            break;
        }

        m_cols_cdf[y].clear();
        m_cols_cdf[y].reserve(m_width);

        for (size_t x = 0, xe = m_width; x < xe; ++x)
        {
            Payload payload;
            Importance importance;

            sampler.sample(x, y, payload, importance);

            m_cols_cdf[y].insert(payload, importance);
        }

        if (m_cols_cdf[y].valid())
            m_cols_cdf[y].prepare();

        m_rows_cdf.insert(y, m_cols_cdf[y].weight());
    }

    if (m_rows_cdf.valid())
        m_rows_cdf.prepare();
}

template <typename Payload, typename Importance>
inline void ImageImportanceSampler<Payload, Importance>::sample(
    const Vector2Type&      s,
    size_t&                 x,
    size_t&                 y,
    Importance&             probability) const
{
    if (m_rows_cdf.valid())
    {
        // Select a row.
        const typename RowCDF::ItemWeightPair& row = m_rows_cdf.sample(s[1]);
        assert(row.second != Importance(0.0));
        y = row.first;

        // Select a column within this row.
        const typename ColCDF::ItemWeightPair& col = m_cols_cdf[y].sample(s[0]);
        assert(col.second != Importance(0.0));
        x = &col - &m_cols_cdf[y][0];

        probability = row.second * col.second;
    }
    else
    {
        // Uniform random sampling.
        x = truncate<size_t>(s[0] * m_width);
        y = truncate<size_t>(s[1] * m_height);

        probability = m_rcp_pixel_count;
    }

    assert(probability > Importance(0.0));
}

template <typename Payload, typename Importance>
inline void ImageImportanceSampler<Payload, Importance>::sample(
    const Vector2Type&      s,
    size_t&                 x,
    size_t&                 y,
    Payload&                payload,
    Importance&             probability) const
{
    if (m_rows_cdf.valid())
    {
        // Select a row.
        const typename RowCDF::ItemWeightPair& row = m_rows_cdf.sample(s[1]);
        assert(row.second != Importance(0.0));
        y = row.first;

        // Select a column within this row.
        const typename ColCDF::ItemWeightPair& col = m_cols_cdf[y].sample(s[0]);
        assert(col.second != Importance(0.0));
        x = &col - &m_cols_cdf[y][0];

        payload = col.first;
        probability = row.second * col.second;
    }
    else
    {
        // Uniform random sampling.
        x = truncate<size_t>(s[0] * m_width);
        y = truncate<size_t>(s[1] * m_height);

        payload = m_cols_cdf[y][x].first;
        probability = m_rcp_pixel_count;
    }

    assert(probability > Importance(0.0));
}

template <typename Payload, typename Importance>
inline Importance ImageImportanceSampler<Payload, Importance>::get_pdf(
    const size_t            x,
    const size_t            y) const
{
    if (m_rows_cdf.valid())
    {
        if (m_cols_cdf[y].valid())
        {
            const typename RowCDF::ItemWeightPair& row = m_rows_cdf[y];
            const typename ColCDF::ItemWeightPair& col = m_cols_cdf[y][x];
            return row.second * col.second;
        }
        else
        {
            return Importance(0.0);
        }
    }
    else
    {
        return m_rcp_pixel_count;
    }
}


//
// ImageSampler class implementation.
//

inline ImageSampler::ImageSampler(const Image& image)
  : m_image(image)
{
    assert(
        m_image.properties().m_channel_count == 3 ||
        m_image.properties().m_channel_count == 4);
}

inline void ImageSampler::sample(const size_t x, const size_t y, Payload& payload, float& importance) const
{
    Color3f color;
    m_image.get_pixel(x, y, color);
    importance = luminance(color);
}

}   // namespace foundation
