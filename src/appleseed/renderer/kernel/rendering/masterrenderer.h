
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

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation { class SearchPaths; }
namespace renderer   { class IRendererController; }
namespace renderer   { class ITileCallback; }
namespace renderer   { class ITileCallbackFactory; }
namespace renderer   { class ParamArray; }
namespace renderer   { class Project; }

namespace renderer
{

//
// Master renderer, handles rendering of a given project.
//

class APPLESEED_DLLSYMBOL MasterRenderer
  : public foundation::NonCopyable
{
  public:
    // Constructor taking a tile callback factory.
    MasterRenderer(
        Project&                        project,
        const ParamArray&               params,
        const foundation::SearchPaths&  resource_search_paths,
        ITileCallbackFactory*           tile_callback_factory = nullptr);

    // Constructor taking a single tile callback (calls to it will be serialized).
    MasterRenderer(
        Project&                        project,
        const ParamArray&               params,
        const foundation::SearchPaths&  resource_search_paths,
        ITileCallback*                  tile_callback);

    // Destructor.
    ~MasterRenderer();

    // Return the parameters of the master renderer.
    ParamArray& get_parameters();
    const ParamArray& get_parameters() const;

    struct APPLESEED_DLLSYMBOL RenderingResult
    {
        enum Status { Succeeded, Aborted, Failed };

        Status m_status;

        RenderingResult();
    };

    // Render the project.
    RenderingResult render(IRendererController& renderer_controller);

  private:
    struct Impl;
    Impl* impl;
};

}   // namespace renderer
