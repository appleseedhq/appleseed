
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Tiago Chaves, The appleseedhq Organization
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
#include "chromaticaberrationpostprocessingstage.h"

// appleseed.renderer headers.
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/postprocessingstage/postprocessingstage.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cstddef>
#include <memory>

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Chromatic aberration post-processing stage.
    //

    const char* Model = "chromatic_aberration_post_processing_stage";

    //@TODO add default parameters

    class ChromaticAberrationPostProcessingStage
      : public PostProcessingStage
    {
      public:
        ChromaticAberrationPostProcessingStage(
            const char*             name,
            const ParamArray&       params)
          : PostProcessingStage(name, params)
        {
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) override
        {
            const OnFrameBeginMessageContext context("post-processing stage", this);

            //@TODO init parameters with m_params.get_optional

            return true;
        }

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
            //@TODO apply the effect :)

            // ref.: https://www.shadertoy.com/view/XssGz8
            //       https://www.shadertoy.com/view/MtXXDr
            //       https://github.com/keijiro/KinoFringe
        }

      private:
        //@TODO add parameters
    };
}


//
// ChromaticAberrationPostProcessingStageFactory class implementation.
//

void ChromaticAberrationPostProcessingStageFactory::release()
{
    delete this;
}

const char* ChromaticAberrationPostProcessingStageFactory::get_model() const
{
    return Model;
}

Dictionary ChromaticAberrationPostProcessingStageFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Chromatic Aberration");
}

DictionaryArray ChromaticAberrationPostProcessingStageFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    add_common_input_metadata(metadata);

    //@TODO expose parameters

    return metadata;
}

auto_release_ptr<PostProcessingStage> ChromaticAberrationPostProcessingStageFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<PostProcessingStage>(
        new ChromaticAberrationPostProcessingStage(name, params));
}

}   // namespace renderer
