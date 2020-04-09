
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/device/cpu/cpurendercontext.h"
#include "renderer/device/renderdevicebase.h"
#include "renderer/modeling/shadergroup/shadercompiler.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"

// Standard headers.
#include <memory>

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace renderer      { class OIIOErrorHandler; }
namespace renderer      { class OIIOTextureSystem; }
namespace renderer      { class OSLShadingSystem; }
namespace renderer      { class RendererComponents; }
namespace renderer      { class RendererServices; }
namespace renderer      { class TextureStore; }

namespace renderer
{

class CPURenderDevice
  : public RenderDeviceBase
{
  public:
    CPURenderDevice(
        Project&                        project,
        const ParamArray&               params);

    ~CPURenderDevice() override;

    bool initialize(
        const foundation::SearchPaths&  resource_search_paths,
        ITileCallbackFactory*           tile_callback_factory,
        foundation::IAbortSwitch&       abort_switch) override;

    bool build_or_update_scene() override;

    bool load_checkpoint(Frame& frame, const size_t pass_count) override;

    IRendererController* get_frame_renderer_controller() override;

    bool on_render_begin(
        OnRenderBeginRecorder&          recorder,
        foundation::IAbortSwitch*       abort_switch = nullptr) override;

    bool on_frame_begin(
        OnFrameBeginRecorder&           recorder,
        foundation::IAbortSwitch*       abort_switch = nullptr) override;

    const IRenderContext& get_render_context() const override;

    IRendererController::Status render_frame(
        ITileCallbackFactory*           tile_callback_factory,
        IRendererController&            renderer_controller,
        foundation::IAbortSwitch&       abort_switch) override;

    void print_settings() const override;

    // Return device metadata.
    static foundation::Dictionary get_metadata();

  private:
    CPURenderContext                                m_context;
    OIIOErrorHandler*                               m_error_handler;
    OIIOTextureSystem*                              m_texture_system;
    RendererServices*                               m_renderer_services;
    OSLShadingSystem*                               m_shading_system;
    foundation::auto_release_ptr<ShaderCompiler>    m_osl_compiler;
    TextureStore&                                   m_texture_store;
    std::unique_ptr<RendererComponents>             m_components;
};

}       // namespace renderer
