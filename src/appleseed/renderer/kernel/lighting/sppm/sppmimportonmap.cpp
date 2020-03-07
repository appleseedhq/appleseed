
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Francois Beaune, The appleseedhq Organization
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
#include "sppmimportonmap.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"

// appleseed.foundation headers.
#include "foundation/platform/defaulttimers.h"
#include "foundation/string/string.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cstddef>
#include <string>

using namespace foundation;

namespace renderer
{

SPPMImportonMap::SPPMImportonMap(SPPMImportonVector& importons)
{
    const size_t importon_count = importons.size();

    if (importon_count > 0)
    {
        RENDERER_LOG_INFO(
            "building sppm importon map from %s %s...",
            pretty_uint(importon_count).c_str(),
            importon_count > 1 ? "importons" : "importon");

        knn::Builder3f builder(*this);
        builder.build_move_points<DefaultWallclockTimer>(importons);

        Statistics statistics;
        statistics.insert_time("build time", builder.get_build_time());
        statistics.merge(knn::TreeStatistics<knn::Tree3f>(*this));

        RENDERER_LOG_DEBUG("%s",
            StatisticsVector::make(
                "sppm importon map statistics",
                statistics).to_string().c_str());
    }
    else
    {
        RENDERER_LOG_WARNING(
            "cannot build sppm importon map because no importon were stored.");
    }
}

}   // namespace renderer
