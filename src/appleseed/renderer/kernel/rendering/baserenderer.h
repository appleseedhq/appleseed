
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_BASERENDERER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_BASERENDERER_H

// appleseed.renderer headers.
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

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

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
#ifdef APPLESEED_WITH_OIIO
namespace renderer      { class OIIOErrorHandler; }
#endif
namespace renderer      { class Project; }
#ifdef APPLESEED_WITH_OSL
namespace renderer      { class RendererServices; }
#endif
namespace renderer      { class TextureStore; }

namespace renderer
{

//
// Base renderer, handles rendering of a given project.
//

class APPLESEED_DLLSYMBOL BaseRenderer
  : public foundation::NonCopyable
{
  public:
    // Destructor.
    ~BaseRenderer();

    // Return the parameters of the base renderer.
    ParamArray& get_parameters();
    const ParamArray& get_parameters() const;

    bool initialize_shading_system(
        TextureStore&               texture_store,
        foundation::IAbortSwitch&   abort_switch);

  protected:
    Project&                        m_project;
    ParamArray                      m_params;

#ifdef APPLESEED_WITH_OIIO
    OIIOErrorHandler*               m_error_handler;
    OIIO::TextureSystem*            m_texture_system;
#endif

#ifdef APPLESEED_WITH_OSL
    RendererServices*               m_renderer_services;
    OSL::ShadingSystem*             m_shading_system;
#endif

    // Constructor.
    BaseRenderer(
        Project&                    project,
        const ParamArray&           params);

  private:
#ifdef APPLESEED_WITH_OIIO
    void initialize_oiio();
#endif

#ifdef APPLESEED_WITH_OSL
    bool initialize_osl(
        TextureStore&               texture_store,
        foundation::IAbortSwitch&   abort_switch);
#endif
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_BASERENDERER_H
