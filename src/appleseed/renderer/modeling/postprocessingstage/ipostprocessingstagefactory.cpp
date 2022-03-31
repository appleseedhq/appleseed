
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
#include "ipostprocessingstagefactory.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/utility/api/specializedapiarrays.h"

using namespace foundation;

namespace renderer
{

void IPostProcessingStageFactory::add_common_input_metadata(DictionaryArray& metadata)
{
    metadata.push_back(
        Dictionary()
            .insert("name", "order")
            .insert("label", "Order")
            .insert("type", "integer")
            .insert("min",
                Dictionary()
                    .insert("value", "0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "10")
                    .insert("type", "soft"))
            .insert("use", "required")
            .insert("help", "Determines the order in which this stage is applied")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "real_time_preview")
            .insert("label", "Real-time Preview")
            .insert("type", "boolean")
            .insert("use", "required")
            .insert("default", "false"));
}

}   // namespace renderer
