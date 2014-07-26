
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Srinath Ravichandran, The appleseedhq Organization
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
#include "curveobject.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/aabb.h"
#include "foundation/math/rng.h"
#include "foundation/math/vector.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <fstream>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// CurveObject class implementation.
//

struct CurveObject::Impl
{
    RegionKit               m_region_kit;
    Lazy<RegionKit>         m_lazy_region_kit;
    vector<BezierCurve3d>   m_curves;
    vector<string>          m_material_slots;

    Impl()
      : m_lazy_region_kit(&m_region_kit)
    {
    }

    GAABB3 compute_bounds() const
    {
        AABB3d bbox;
        bbox.invalidate();

        const size_t curve_count = m_curves.size();

        for (size_t i = 0; i < curve_count; ++i)
            bbox.insert(m_curves[i].get_bbox());

        return bbox;
    }

    void create_hair_ball(const ParamArray& params)
    {
        const size_t ControlPointCount = 4;

        const size_t curve_count = params.get_optional<size_t>("curves", 100);
        const double curve_width = params.get_optional<double>("width", 0.001);

        Vector3d control_points[ControlPointCount];
        MersenneTwister rng;

        m_curves.reserve(curve_count);

        for (size_t c = 0; c < curve_count; ++c)
        {
            for (size_t p = 0; p < ControlPointCount; ++p)
            {
                // http://math.stackexchange.com/questions/87230/picking-random-points-in-the-volume-of-sphere-with-uniform-probability
                const double r = pow(rand_double1(rng), 1.0 / 3);

                Vector2d s;
                s[0] = rand_double1(rng);
                s[1] = rand_double1(rng);

                control_points[p] = r * sample_sphere_uniform(s);
            }

            const BezierCurve3d curve(&control_points[0], curve_width);
            m_curves.push_back(curve);
        }
    }

    // Read a custom curve file and load it in m_curves.
    void load_curve_file(const char* filename)
    {
        const double CurveWidth = 0.009;

        ifstream input;
        input.open(filename);

        if (input.good())
        {
            Stopwatch<DefaultWallclockTimer> stopwatch;
            stopwatch.start();

            // Read the number of curves.
            size_t curve_count;
            input >> curve_count;

            // Read the number of control points per curve.
            size_t control_point_count;
            input >> control_point_count;

            vector<Vector3d> control_points(control_point_count);

            m_curves.reserve(curve_count);

            for (size_t c = 0; c < curve_count; ++c)
            {
                for (size_t p = 0; p < control_point_count; ++p)
                {
                    Vector3d point;
                    input >> point.x >> point.y >> point.z;
                    control_points[p] = point;
                }

                const BezierCurve3d curve(&control_points[0], CurveWidth);
                m_curves.push_back(curve);
            }

            stopwatch.measure();

            RENDERER_LOG_INFO(
                "loaded curve file %s (%s curves) in %s.",
                filename,
                pretty_uint(curve_count).c_str(),
                pretty_time(stopwatch.get_seconds()).c_str());

            input.close();
        }
        else
        {
            RENDERER_LOG_ERROR("failed to load curve file %s.", filename);
        }
    }
};

CurveObject::CurveObject(
    const SearchPaths&  search_paths,
    const char*         name,
    const ParamArray&   params)
  : Object(name, params)
  , impl(new Impl())
{
    const string filename = params.get<string>("filename");

    if (filename == "builtin:hairball")
        impl->create_hair_ball(params);
    else
    {
        const string filepath = search_paths.qualify(filename);
        impl->load_curve_file(filepath.c_str());
    }
}

CurveObject::~CurveObject()
{
    delete impl;
}

void CurveObject::release()
{
    delete this;
}

const char* CurveObject::get_model() const
{
    return CurveObjectFactory::get_model();
}

GAABB3 CurveObject::compute_local_bbox() const
{
    return impl->compute_bounds();
}

Lazy<RegionKit>& CurveObject::get_region_kit()
{
    return impl->m_lazy_region_kit;
}

size_t CurveObject::get_material_slot_count() const
{
    return impl->m_material_slots.size();
}

const char* CurveObject::get_material_slot(const size_t index) const
{
    return impl->m_material_slots[index].c_str();
}

size_t CurveObject::get_curve_count() const
{
    return impl->m_curves.size();
}

const BezierCurve3d& CurveObject::get_curve(const size_t index) const
{
    assert(index < impl->m_curves.size());
    return impl->m_curves[index];
}


//
// CurveObjectFactory class implementation.
//

const char* CurveObjectFactory::get_model()
{
    return "curve_object";
}

auto_release_ptr<CurveObject> CurveObjectFactory::create(
    const SearchPaths&  search_paths,
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<CurveObject>(
            new CurveObject(search_paths, name, params));
}

}   // namespace renderer
