
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
#include "edfenvironmentshader.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/scene/scene.h"

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // EDF-based environment shader.
    //

    class EDFEnvironmentShader
      : public EnvironmentShader
    {
      public:
        EDFEnvironmentShader(
            const char*             name,
            const ParamArray&       params)
          : EnvironmentShader(params)
          , m_name(name)
          , m_env_edf_name(m_params.get_required<string>("environment_edf", ""))
          , m_env_edf(0)
        {
        }

        virtual void release()
        {
            delete this;
        }

        virtual const char* get_model() const
        {
            return EDFEnvironmentShaderFactory::get_model();
        }

        virtual const char* get_name() const
        {
            return m_name.c_str();
        }

        virtual void on_frame_begin(const Scene& scene)
        {
            m_env_edf = 0;

            if (!m_env_edf_name.empty())
            {
                const size_t env_edf_index =
                    scene.environment_edfs().get_index(m_env_edf_name.c_str());

                if (env_edf_index == ~size_t(0))
                {
                    RENDERER_LOG_ERROR(
                        "while preparing environment shader \"%s\": "
                        "cannot find environment EDF \"%s\", "
                        "the environment will be transparent black",
                        m_name.c_str(),
                        m_env_edf_name.c_str());
                }
                else
                {
                    m_env_edf = scene.environment_edfs().get(env_edf_index);
                }
            }
        }

        virtual void evaluate(
            InputEvaluator&         input_evaluator,
            const Vector3d&         direction,
            ShadingResult&          shading_result) const
        {
            if (m_env_edf)
            {
                shading_result.m_color_space = ColorSpaceSpectral;
                shading_result.m_alpha.set(1.0f);

                m_env_edf->evaluate(
                    input_evaluator,
                    direction,
                    shading_result.m_color);
            }
            else
            {
                // Environment shader not properly initialized: return transparent black.
                shading_result.clear();
            }
        }

      private:
        const string        m_name;
        const string        m_env_edf_name;
        EnvironmentEDF*     m_env_edf;
    };
}


//
// EDFEnvironmentShaderFactory class implementation.
//

const char* EDFEnvironmentShaderFactory::get_model()
{
    return "edf_environment_shader";
}

auto_release_ptr<EnvironmentShader> EDFEnvironmentShaderFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<EnvironmentShader>(
            new EDFEnvironmentShader(name, params));
}

}   // namespace renderer
