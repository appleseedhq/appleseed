
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

// Interface header.
#include "aovframecollection.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/aov/aovcollection.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"

// boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <string>
#include <vector>

using namespace boost;
using namespace foundation;
using namespace std;

namespace renderer
{

struct AOVFrameCollection::Impl
{
    struct AOVFrame
    {
        string              m_name;
        PixelFormat         m_format;
        Image*              m_frame;
    };

    vector<AOVFrame>        m_aov_frames;
};

AOVFrameCollection::AOVFrameCollection()
  : impl(new Impl())
{
}

AOVFrameCollection::~AOVFrameCollection()
{
    clear();

    delete impl;
}

bool AOVFrameCollection::empty() const
{
    return impl->m_aov_frames.empty();
}

size_t AOVFrameCollection::size() const
{
    return impl->m_aov_frames.size();
}

void AOVFrameCollection::clear()
{
    for (size_t i = 0; i < impl->m_aov_frames.size(); ++i)
        delete impl->m_aov_frames[i].m_frame;

    impl->m_aov_frames.clear();
}

size_t AOVFrameCollection::declare(
    const char*             name,
    const PixelFormat       format)
{
    const size_t index = impl->m_aov_frames.size();

    Impl::AOVFrame aov_frame;
    aov_frame.m_name = name;
    aov_frame.m_format = format;
    aov_frame.m_frame = 0;
    impl->m_aov_frames.push_back(aov_frame);

    return index;
}

void AOVFrameCollection::allocate_frames(const CanvasProperties& props)
{
    for (size_t i = 0; i < impl->m_aov_frames.size(); ++i)
    {
        CanvasProperties frame_props(props);
        frame_props.m_pixel_format = impl->m_aov_frames[i].m_format;
        impl->m_aov_frames[i].m_frame = new Image(frame_props);
    }
}

void AOVFrameCollection::set_pixel(
    const size_t            x,
    const size_t            y,
    const AOVCollection&    aovs) const
{
    const size_t size = aovs.size();

    assert(size == impl->m_aov_frames.size());

    for (size_t i = 0; i < size; ++i)
    {
        const Impl::AOVFrame& aov_frame = impl->m_aov_frames[i];

        const Spectrum& spectrum = aovs[i];
        const Color4f color(spectrum[0], spectrum[1], spectrum[2], 1.0f);

        aov_frame.m_frame->set_pixel(x, y, color);
    }
}

bool AOVFrameCollection::write(const char* path) const
{
    bool result = true;

    for (size_t i = 0; i < impl->m_aov_frames.size(); ++i)
    {
        const Impl::AOVFrame& aov_frame = impl->m_aov_frames[i];
        const string file_name = aov_frame.m_name + ".exr";
        const string file_path = (filesystem::path(path) / file_name).string();

        try
        {
            GenericImageFileWriter writer;
            writer.write(file_path.c_str(), *aov_frame.m_frame);
        }
        catch (const ExceptionIOError&)
        {
            RENDERER_LOG_ERROR(
                "failed to write image file %s: i/o error",
                file_path.c_str());

            result = false;
        }
        catch (const Exception& e)
        {
            RENDERER_LOG_ERROR(
                "failed to write image file %s: %s",
                file_path.c_str(),
                e.what());

            result = false;
        }
    }

    return result;
}

}   // namespace renderer
