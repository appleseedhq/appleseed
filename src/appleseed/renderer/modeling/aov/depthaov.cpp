
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "depthaov.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/aov/aov.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Depth aov.
    //

    const char* Model = "depth_aov";

    class DepthAOV
      : public AOV
    {
      public:
        DepthAOV(const char* name, const ParamArray& params)
          : AOV(name, params)
        {
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual size_t get_channel_count() const APPLESEED_OVERRIDE
        {
            return 1;
        }

        virtual const char* get_channel_name(const size_t i) const APPLESEED_OVERRIDE
        {
            assert(i == 0);
            return "Z";
        }

        virtual bool has_color_data() const APPLESEED_OVERRIDE
        {
            return false;
        }
    };
}


//
// AOVFactory class implementation.
//

const char* DepthAOVFactory::get_model() const
{
    return Model;
}

Dictionary DepthAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Depth")
            .insert("default_model", "true");
}

DictionaryArray DepthAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> DepthAOVFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<AOV>(
            new DepthAOV(name, params));
}

auto_release_ptr<AOV> DepthAOVFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<AOV>(
            new DepthAOV(name, params));
}

}   // namespace renderer
