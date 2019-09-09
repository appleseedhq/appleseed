
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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
#include "postprocessingstage.h"

// appleseed.renderer headers.
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

using namespace foundation;

namespace renderer
{

//
// PostProcessingStage class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID PostProcessingStage::get_class_uid()
{
    return g_class_uid;
}

PostProcessingStage::PostProcessingStage(
    const char*         name,
    const ParamArray&   params)
  : ConnectableEntity(g_class_uid, params)
{
    set_name(name);

    const EntityDefMessageContext context("post-processing stage", this);

    m_order = m_params.get_required<int>("order", 0, context);
}

}   // namespace renderer
