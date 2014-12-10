
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_OSLCOMPONENTS_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_OSLCOMPONENTS_H

// appleseed.renderer headers.
#include "renderer/kernel/rendering/oiioerrorhandler.h"
#include "renderer/kernel/rendering/rendererservices.h"

// OpenImageIO headers.
#ifdef APPLESEED_WITH_OIIO
#include "OpenImageIO/texture.h"
#endif

// OSL headers.
#ifdef APPLESEED_WITH_OSL
#include "foundation/platform/oslheaderguards.h"
BEGIN_OSL_INCLUDES
#include "OSL/oslexec.h"
END_OSL_INCLUDES
#endif

// Standard headers.
#include <memory>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Project; }
namespace renderer      { class TextureStore; }

namespace renderer
{

//
// RAII-style OSL initialization.
//

class OSLComponents
{
  public:
    OSLComponents(
        const Project&              project,
        TextureStore&               texture_store,
        OIIO::TextureSystem&        texture_system);

    ~OSLComponents();

    RendererServices& get_renderer_services();
    OSL::ShadingSystem& get_shading_system();

    bool compile_osl_shaders(foundation::IAbortSwitch* abort_switch);

  private:
    const Project&                  m_project;
    TextureStore&                   m_texture_store;
    OIIO::TextureSystem&            m_texture_system;
    OIIOErrorHandler                m_error_handler;
    std::auto_ptr<RendererServices> m_renderer_services;
    OSL::ShadingSystem*             m_shading_system;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_OSLCOMPONENTS_H
