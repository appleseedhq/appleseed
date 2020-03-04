
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Stephen Agyemang, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/kernel/rendering/ipixelrenderer.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/image.h"
#include "foundation/platform/compiler.h"

// OpenImageIO headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/imagebuf.h"
#include "foundation/platform/_endoiioheaders.h"

// Standard headers.
#include <cstddef>
#include <memory>

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace renderer      { class Frame; }
namespace renderer      { class ISampleRendererFactory; }

namespace renderer
{

//
// Texture-controlled pixel renderer.
//

class TextureControlledPixelRendererFactory
  : public IPixelRendererFactory
{
  public:
    // Return parameters metadata.
    static foundation::Dictionary get_params_metadata();

    // Constructor.
    TextureControlledPixelRendererFactory(
        const Frame&                    frame,
        ISampleRendererFactory*         factory,
        const ParamArray&               params);

    // Load a texture from a file.
    bool load_texture(const std::string& texture_path);

    // Delete this instance.
    void release() override;

    // Return a new texture-controlled pixel renderer instance.
    IPixelRenderer* create(
        const size_t                    thread_index) override;

  private:
    const Frame&                        m_frame;
    std::shared_ptr<OIIO::ImageBuf>     m_texture;
    ISampleRendererFactory*             m_factory;
    ParamArray                          m_params;
};

}   // namespace renderer
