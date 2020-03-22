
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

#pragma once

// appleseed.renderer headers.
#include "renderer/kernel/rendering/irenderercontroller.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

namespace renderer
{

//
// A collection of renderer controllers.
//

class APPLESEED_DLLSYMBOL RendererControllerCollection
  : public IRendererController
{
  public:
    // Constructor.
    RendererControllerCollection();

    // Destructor.
    ~RendererControllerCollection() override;

    void on_rendering_begin() override;
    void on_rendering_success() override;
    void on_rendering_abort() override;
    void on_rendering_pause() override;
    void on_rendering_resume() override;
    void on_frame_begin() override;
    void on_frame_end() override;
    void on_progress() override;
    Status get_status() const override;

    // Insert a renderer controller into the collection.
    void insert(IRendererController* renderer_controller);

  private:
    struct Impl;
    Impl* impl;
};

}   // namespace renderer
