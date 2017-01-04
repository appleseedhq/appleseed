
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "regexrenderlayerrule.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/project/renderlayerrule.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/filter/regexfilter.h"

// Standard headers.
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Render layer rule based on regular expressions.
    //

    const char* Model = "regex";

    class RegExRenderLayerRule
      : public RenderLayerRule
    {
      public:
        RegExRenderLayerRule(
            const char*         name,
            const ParamArray&   params)
          : RenderLayerRule(name, params)
        {
            const EntityDefMessageContext context("render layer rule", this);

            const string pattern = params.get_required<string>("pattern", "", context);

            m_filter.set_pattern(pattern.c_str());

            if (!m_filter.is_valid())
                RENDERER_LOG_ERROR("%s: invalid regular expression pattern: %s", context.get(), pattern.c_str());
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual bool applies(const Entity& entity) const APPLESEED_OVERRIDE
        {
            return
                m_filter.is_valid()
                    ? m_filter.accepts(entity.get_path().c_str())
                    : false;
        }

      private:
        RegExFilter m_filter;
    };
}


//
// RegExRenderLayerRuleFactory class implementation.
//

const char* RegExRenderLayerRuleFactory::get_model() const
{
    return Model;
}

Dictionary RegExRenderLayerRuleFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Regular Expression")
            .insert("default_model", "true");
}

DictionaryArray RegExRenderLayerRuleFactory::get_input_metadata() const
{
    DictionaryArray metadata = RenderLayerRuleFactory::get_input_metadata();

    metadata.push_back(
        Dictionary()
            .insert("name", "pattern")
            .insert("label", "Pattern")
            .insert("type", "text")
            .insert("use", "required"));

    return metadata;
}

auto_release_ptr<RenderLayerRule> RegExRenderLayerRuleFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<RenderLayerRule>(
            new RegExRenderLayerRule(name, params));
}

auto_release_ptr<RenderLayerRule> RegExRenderLayerRuleFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<RenderLayerRule>(
            new RegExRenderLayerRule(name, params));
}

}   // namespace renderer
