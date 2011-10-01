
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "pointlight.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/light/light.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionnotimplemented.h"
#include "foundation/math/transform.h"
#include "foundation/utility/containers/specializedarrays.h"

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Point light.
    //

    const char* Model = "point_light";

    class PointLight
      : public Light
    {
      public:
        PointLight(
            const char*         name,
            const ParamArray&   params)
          : Light(name, params)
        {
        }

        virtual void release() override
        {
            delete this;
        }

        virtual const char* get_model() const override
        {
            return Model;
        }

        virtual void sample(
            const void*         data,
            const Vector2d&     s,
            Vector3d&           outgoing,
            Spectrum&           value,
            double&             probability) const override
        {
            throw ExceptionNotImplemented();
        }

        virtual void evaluate(
            const void*         data,
            const Vector3d&     outgoing,
            Spectrum&           value) const override
        {
            throw ExceptionNotImplemented();
        }

        virtual void evaluate(
            const void*         data,
            const Vector3d&     outgoing,
            Spectrum&           value,
            double&             probability) const override
        {
            throw ExceptionNotImplemented();
        }

        virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     outgoing) const override
        {
            throw ExceptionNotImplemented();
            return 0.0;
        }
    };
}


//
// PointLightFactory class implementation.
//

const char* PointLightFactory::get_model() const
{
    return Model;
}

const char* PointLightFactory::get_human_readable_model() const
{
    return "Point Light";
}

DictionaryArray PointLightFactory::get_widget_definitions() const
{
    DictionaryArray definitions;

    definitions.push_back(
        Dictionary()
            .insert("name", "exitance")
            .insert("label", "Exitance")
            .insert("widget", "text_box")
            .insert("use", "required")
            .insert("default", ""));    // todo: set default

    return definitions;
}

auto_release_ptr<Light> PointLightFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Light>(new PointLight(name, params));
}

}   // namespace renderer
