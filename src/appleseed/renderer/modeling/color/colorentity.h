
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/entity/entity.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/vector.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class DictionaryArray; }
namespace renderer      { class ParamArray; }

namespace renderer
{

//
// An array of color values.
//

typedef foundation::FloatArray ColorValueArray;


//
// Color entity.
//
// todo: allow to specify the lighting conditions of a color.
//

class APPLESEED_DLLSYMBOL ColorEntity
  : public Entity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Delete this instance.
    void release() override;

    // Return the color values.
    const ColorValueArray& get_values() const;

    // Return the alpha values.
    const ColorValueArray& get_alpha() const;

    // Return the color space of this color.
    foundation::ColorSpace get_color_space() const;

    // Return the range of wavelength of this color (spectral color space only).
    const foundation::Vector2f& get_wavelength_range() const;

    // Retrieve the multiplier value.
    float get_multiplier() const;

  private:
    friend class ColorEntityFactory;

    struct Impl;
    Impl* impl;

    // Constructors.
    ColorEntity(
        const char*             name,
        const ParamArray&       params);
    ColorEntity(
        const char*             name,
        const ParamArray&       params,
        const ColorValueArray&  values);
    ColorEntity(
        const char*             name,
        const ParamArray&       params,
        const ColorValueArray&  values,
        const ColorValueArray&  alpha);

    // Destructor.
    ~ColorEntity() override;

    void extract_parameters();
    void extract_values();
    void remove_color_alpha_parameters();
    void check_validity();
};


//
// Color entity factory.
//

class APPLESEED_DLLSYMBOL ColorEntityFactory
{
  public:
    // Return a set of input metadata for this color entity model.
    static foundation::DictionaryArray get_input_metadata();

    // Create a new color entity.
    static foundation::auto_release_ptr<ColorEntity> create(
        const char*             name,
        const ParamArray&       params);
    static foundation::auto_release_ptr<ColorEntity> create(
        const char*             name,
        const ParamArray&       params,
        const ColorValueArray&  values);
    static foundation::auto_release_ptr<ColorEntity> create(
        const char*             name,
        const ParamArray&       params,
        const ColorValueArray&  values,
        const ColorValueArray&  alpha);
};

}   // namespace renderer
