
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright(c) 2020 Joao Marcos Costa, The appleseedhq Organization
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

// This sun positioning algorithm is based on the National Oceanic and Atmospheric
// Administration's (NOAA) Solar Position Calculator which uses the calculations
// presented by Jean Meeus in his book Astronomical Algorithms.

// The Use of NOAA data and products are in the public domain and may be used
// freely by the public as specify in www.nws.noaa.gov/disclaimer.php

#pragma once

// appleseed.renderer headers.
#include "renderer/modeling/entity/entity.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingContext; }

namespace renderer
{

class APPLESEED_DLLSYMBOL SunPositioner
  :public Entity
{
  public:
    // Constructor
    SunPositioner(
        const char* name,
        const ParamArray& params);

    void compute_sun_position();
    void fetch_data();

    float get_zenith() const;
    float get_azimuth() const;
    float get_solar_noon() const;
    float get_sunrise() const;
    float get_sunset() const;

    void release() override;

  private:
    struct Impl;
    Impl* impl;

    double mean_obliquity_ecliptic(const double jc) const;
    double obliquity_correction(const double jc) const;

    double mean_sun_anomaly(const double jc) const;
    double mean_longitude_sun(const double jc) const;
    double sun_eqc_of_center(const double jc, const double msa) const;
    double sun_true_longitude(const double jc, const double mean_long_sun, const double mean_sun_ano) const;
    double apparent_longitude_of_sun(const double jc, const double mean_long_sun, const double mean_sun_ano) const;

    double sun_declination(const double obj_cor, const double app_lon_sun) const;
    double equation_of_time(const double jc, const double obl_cor, const double mean_long_sun, const double mean_sun_ano) const;
};

class APPLESEED_DLLSYMBOL SunPositionerFactory
{
  public:
    // Return string identifying the sun positioner.
    static const char* get_model();

    // Return a set of input metadata the sun positioner.
    static foundation::DictionaryArray get_input_metadata();

    // Create new SunPositioner.
    static foundation::auto_release_ptr<SunPositioner> create(
        const char*         name,
        const ParamArray&   param);
};

}   // namespace renderer
