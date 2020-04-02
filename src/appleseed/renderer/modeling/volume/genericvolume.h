
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Artem Bishev, The appleseedhq Organization
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

#pragma once

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/volume/ivolumefactory.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Volume; }

namespace renderer
{

//
// Generic volume input values.
//

APPLESEED_DECLARE_INPUT_VALUES(GenericVolumeInputValues)
{
    Spectrum    m_absorption;               // absorption coefficient of the media
    float       m_absorption_multiplier;    // absorption coefficient multiplier
    Spectrum    m_scattering;               // scattering coefficient of the media
    float       m_scattering_multiplier;    // scattering coefficient multiplier

    float       m_average_cosine;           // asymmetry parameter, often referred as g

    struct Precomputed
    {
        Spectrum    m_extinction;           // extinction coefficient of the media
    };

    Precomputed m_precomputed;
};


//
// Generic volume factory.
//

class APPLESEED_DLLSYMBOL GenericVolumeFactory
  : public IVolumeFactory
{
  public:
    // Delete this instance.
    void release() override;

    // Return a string identifying this volume model.
    const char* get_model() const override;

    // Return metadata for this volume model.
    foundation::Dictionary get_model_metadata() const override;

    // Return metadata for the inputs of this volume model.
    foundation::DictionaryArray get_input_metadata() const override;

    // Create a new volume instance.
    foundation::auto_release_ptr<Volume> create(
        const char*         name,
        const ParamArray&   params) const override;
};

}   // namespace renderer
