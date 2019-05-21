
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright(c) 2019 Joao Marcos Costa, The appleseedhq Organization
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

// Interface header
#include "renderer/kernel/rendering/renderercontrollercollection.h"

// Standard headers.
#include <vector>

namespace renderer
{

//
// RendererControllerCollection class implementation.
//

struct RendererControllerCollection::Impl
{
    std::vector<IRendererController*> m_renderer_controller;
};

RendererControllerCollection::RendererControllerCollection()
  : impl(new Impl())
{
}

RendererControllerCollection::~RendererControllerCollection()
{
    delete impl;
}

void RendererControllerCollection::on_rendering_begin()
{
    for (auto i : impl->m_renderer_controller)
        i->on_rendering_begin();
}

void RendererControllerCollection::on_rendering_success()
{
    for (auto i : impl->m_renderer_controller)
        i->on_rendering_success();
}

void RendererControllerCollection::on_rendering_abort()
{
    for (auto i : impl->m_renderer_controller)
        i->on_rendering_abort();
}

void RendererControllerCollection::on_rendering_pause()
{
    for (auto i : impl->m_renderer_controller)
        i->on_rendering_pause();
}

void RendererControllerCollection::on_rendering_resume()
{
    for (auto i : impl->m_renderer_controller)
        i->on_rendering_resume();
}

void RendererControllerCollection::on_frame_begin()
{
    for (auto i : impl->m_renderer_controller)
        i->on_frame_begin();
}

void RendererControllerCollection::on_frame_end()
{
    for (auto i : impl->m_renderer_controller)
        i->on_frame_end();
}

void RendererControllerCollection::on_progress()
{
    for (auto i : impl->m_renderer_controller)
        i->on_progress();
}

IRendererController::Status RendererControllerCollection::get_status() const
{
    for (auto i : impl->m_renderer_controller)
        if (i->get_status() != IRendererController::ContinueRendering)
            return i->get_status();
    return IRendererController::ContinueRendering;
}

void RendererControllerCollection::insert(IRendererController* renderer_controller)
{
    impl->m_renderer_controller.push_back(renderer_controller);
}

}   // namespace renderer
