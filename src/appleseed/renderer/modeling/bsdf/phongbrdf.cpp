
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "phongbrdf.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionnotimplemented.h"
#include "foundation/math/basis.h"

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Phong BRDF.
    //

    const char* Model = "phong_brdf";

    class PhongBRDF
      : public BSDF
    {
      public:
        PhongBRDF(
            const char*         name,
            const ParamArray&   params)
          : BSDF(params)
        {
            set_name(name);
        }

        virtual void release()
        {
            delete this;
        }

        virtual const char* get_model() const
        {
            return Model;
        }

        virtual void sample(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     s,
            const Vector3d&     outgoing,
            Vector3d&           incoming,
            Spectrum&           value,
            double&             probability,
            Mode&               mode) const
        {
            throw ExceptionNotImplemented();
        }

        virtual void evaluate(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            Spectrum&           value) const
        {
            throw ExceptionNotImplemented();
        }

        virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming) const
        {
            throw ExceptionNotImplemented();
            return 0.0;
        }

      private:
        struct InputValues
        {
        };
    };
}


//
// PhongBRDFFactory class implementation.
//

const char* PhongBRDFFactory::get_model() const
{
    return Model;
}

const char* PhongBRDFFactory::get_human_readable_model() const
{
    return "Phong Reflection";
}

DictionaryArray PhongBRDFFactory::get_widget_definitions() const
{
    DictionaryArray definitions;
    return definitions;
}

auto_release_ptr<BSDF> PhongBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<BSDF>(
            new PhongBRDF(name, params));
}

}   // namespace renderer
