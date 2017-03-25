
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
#include "uvaov.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/aov/aov.h"

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
    // UV AOV accumulator.
    //

    class UVAOVAccumulator
      : public AOVAccumulator
    {
      public:
        explicit UVAOVAccumulator(const size_t index)
          : AOVAccumulator(index)
        {
        }

        virtual void reset() APPLESEED_OVERRIDE
        {
            m_uvs = Vector2f(0.0f, 0.0f);
        }

        virtual void write(
            const ShadingPoint&     shading_point,
            const Camera&           camera) APPLESEED_OVERRIDE
        {
            if (shading_point.hit())
                m_uvs = shading_point.get_uv(0);
        }

        virtual void flush(ShadingResult& result) APPLESEED_OVERRIDE
        {
            result.m_aovs[m_index].m_color =
                Color3f(m_uvs[0], m_uvs[1], 0.0f);
            result.m_aovs[m_index].m_alpha.set(1.0f);
        }

      private:
        Vector2f m_uvs;
    };


    //
    // UV AOV.
    //

    const char* Model = "uv_aov";

    class UVAOV
      : public AOV
    {
      public:
        UVAOV(const char* name, const ParamArray& params)
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
            return 2;
        }

        virtual const char* get_channel_name(const size_t i) const APPLESEED_OVERRIDE
        {
            static const char* channels[] = {"U", "V"};
            return channels[i];
        }

        virtual bool has_color_data() const APPLESEED_OVERRIDE
        {
            return false;
        }

        virtual auto_release_ptr<AOVAccumulator> create_accumulator(
            const size_t index) const APPLESEED_OVERRIDE
        {
            return auto_release_ptr<AOVAccumulator>(new UVAOVAccumulator(index));
        }
    };
}


//
// UVAOVFactory class implementation.
//

const char* UVAOVFactory::get_model() const
{
    return Model;
}

Dictionary UVAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "UV")
            .insert("default_model", "false");
}

DictionaryArray UVAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> UVAOVFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<AOV>(
            new UVAOV(name, params));
}

auto_release_ptr<AOV> UVAOVFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<AOV>(
            new UVAOV(name, params));
}

}   // namespace renderer
