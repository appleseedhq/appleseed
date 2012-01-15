
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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
#include "aovimagecollection.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/aov/aovcollection.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/image.h"

// Standard headers.
#include <cassert>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

struct AOVImageCollection::Impl
{
    size_t              m_canvas_width;
    size_t              m_canvas_height;
    size_t              m_tile_width;
    size_t              m_tile_height;

    struct NamedImage
    {
        string          m_name;
        Image*          m_image;
    };

    vector<NamedImage>  m_images;
};

AOVImageCollection::AOVImageCollection(
    const size_t    canvas_width,
    const size_t    canvas_height,
    const size_t    tile_width,
    const size_t    tile_height)
  : impl(new Impl())
{
    impl->m_canvas_width = canvas_width;
    impl->m_canvas_height = canvas_height;
    impl->m_tile_width = tile_width;
    impl->m_tile_height = tile_height;
}

AOVImageCollection::~AOVImageCollection()
{
    clear();

    delete impl;
}

bool AOVImageCollection::empty() const
{
    return impl->m_images.empty();
}

size_t AOVImageCollection::size() const
{
    return impl->m_images.size();
}

const char* AOVImageCollection::get_name(const size_t index) const
{
    assert(index < impl->m_images.size());
    return impl->m_images[index].m_name.c_str();
}

const Image& AOVImageCollection::get_image(const size_t index) const
{
    assert(index < impl->m_images.size());
    return *impl->m_images[index].m_image;
}

void AOVImageCollection::clear()
{
    for (size_t i = 0; i < impl->m_images.size(); ++i)
        delete impl->m_images[i].m_image;

    impl->m_images.clear();
}

void AOVImageCollection::insert(
    const char*             name,
    const PixelFormat       format)
{
    Impl::NamedImage named_image;

    named_image.m_name = name;
    named_image.m_image =
        new Image(
            impl->m_canvas_width,
            impl->m_canvas_height,
            impl->m_tile_width,
            impl->m_tile_height,
            4,
            format);

    impl->m_images.push_back(named_image);
}

void AOVImageCollection::set_pixel(
    const size_t            x,
    const size_t            y,
    const AOVCollection&    aovs) const
{
    const size_t size = aovs.size();

    assert(size == impl->m_images.size());

    for (size_t i = 0; i < size; ++i)
    {
        const Spectrum& spectrum = aovs[i];
        const Color4f linear_rgb(spectrum[0], spectrum[1], spectrum[2], 1.0f);

        impl->m_images[i].m_image->set_pixel(x, y, linear_rgb);
    }
}

}   // namespace renderer
