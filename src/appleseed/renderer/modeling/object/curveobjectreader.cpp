
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
#include "curveobjectreader.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/aabb.h"
#include "foundation/math/qmc.h"
#include "foundation/math/rng.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <fstream>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// CurveObjectReader class implementation.
//

auto_release_ptr<CurveObject> CurveObjectReader::read(
    const SearchPaths&      search_paths,
    const char*             name,
    const ParamArray&       params)
{
    const string filepath = params.get<string>("filepath");

    if (filepath == "builtin:hairball")
        return create_hair_ball(name, params);
    else if (filepath == "builtin:furryball")
        return create_furry_ball(name, params);
    else return load_curve_file(search_paths, name, params);
}

namespace
{
    template <typename RNG>
    static Vector2d rand_vector2d(RNG& rng)
    {
        Vector2d v;
        v[0] = rand_double2(rng);
        v[1] = rand_double2(rng);
        return v;
    }

    void split_and_store(CurveObject& object, const BezierCurve3d& curve, const size_t split_count)
    {
        if (split_count > 0)
        {
            BezierCurve3d child1, child2;
            curve.split(child1, child2);
            split_and_store(object, child1, split_count - 1);
            split_and_store(object, child2, split_count - 1);
        }
        else object.push_curve(curve);
    }
}

auto_release_ptr<CurveObject> CurveObjectReader::create_hair_ball(
    const char*             name,
    const ParamArray&       params)
{
    auto_release_ptr<CurveObject> object = CurveObjectFactory::create(name, params);

    const size_t ControlPointCount = 4;
    const size_t curve_count = params.get_optional<size_t>("curves", 100);
    const double curve_width = params.get_optional<double>("width", 0.002);
    const size_t split_count = params.get_optional<size_t>("presplits", 0);

    Vector3d points[ControlPointCount];
    MersenneTwister rng;

    object->reserve_curves(curve_count);

    for (size_t c = 0; c < curve_count; ++c)
    {
        for (size_t p = 0; p < ControlPointCount; ++p)
        {
            // http://math.stackexchange.com/questions/87230/picking-random-points-in-the-volume-of-sphere-with-uniform-probability
            const double r = pow(1.0 - rand_double2(rng), 1.0 / 3);
            const Vector3d d = sample_sphere_uniform(rand_vector2d(rng));
            points[p] = r * d;
        }

        const BezierCurve3d curve(&points[0], curve_width);
        split_and_store(object.ref(), curve, split_count);
    }

    return object;
}

auto_release_ptr<CurveObject> CurveObjectReader::create_furry_ball(
    const char*             name,
    const ParamArray&       params)
{
    auto_release_ptr<CurveObject> object = CurveObjectFactory::create(name, params);

    const size_t ControlPointCount = 4;
    const size_t curve_count = params.get_optional<size_t>("curves", 100);
    const double curve_length = params.get_optional<double>("length", 0.1);
    const double base_width = params.get_optional<double>("base_width", 0.001);
    const double tip_width = params.get_optional<double>("tip_width", 0.0001);
    const double length_fuzziness = params.get_optional<double>("length_fuzziness", 0.3);
    const double curliness = params.get_optional<double>("curliness", 0.5);
    const size_t split_count = params.get_optional<size_t>("presplits", 0);

    Vector3d points[ControlPointCount];
    double widths[ControlPointCount];

    MersenneTwister rng;

    object->reserve_curves(curve_count);

    for (size_t c = 0; c < curve_count; ++c)
    {
        static const size_t Bases[] = { 2 };
        const Vector2d s = hammersley_sequence<double, 2>(Bases, c, curve_count);
        const Vector3d d = sample_sphere_uniform(s);

        points[0] = d;
        widths[0] = base_width;

        const double f = rand_double1(rng, -length_fuzziness, +length_fuzziness);
        const double length = curve_length * (1.0 + f);

        for (size_t p = 1; p < ControlPointCount; ++p)
        {
            const double r = static_cast<double>(p) / (ControlPointCount - 1);
            const Vector3d f = curliness * sample_sphere_uniform(rand_vector2d(rng));
            points[p] = points[0] + length * (r * d + f);
            widths[p] = lerp(base_width, tip_width, r);
        }

        const BezierCurve3d curve(&points[0], &widths[0]);
        split_and_store(object.ref(), curve, split_count);
    }

    return object;
}

auto_release_ptr<CurveObject> CurveObjectReader::load_curve_file(
    const SearchPaths&      search_paths,
    const char*             name,
    const ParamArray&       params)
{
    auto_release_ptr<CurveObject> object = CurveObjectFactory::create(name, params);

    const string filepath = search_paths.qualify(params.get<string>("filepath"));
    const size_t split_count = params.get_optional<size_t>("presplits", 0);

    ifstream input;
    input.open(filepath);

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

        if (control_point_count != 4)
        {
            RENDERER_LOG_ERROR(
                "while loading curve file %s: only curves with 4 control points are currently supported.",
                filepath.c_str());
            return object;
        }

        vector<Vector3d> points(control_point_count);
        vector<double> widths(control_point_count);

        object->reserve_curves(curve_count);

        for (size_t c = 0; c < curve_count; ++c)
        {
            for (size_t p = 0; p < control_point_count; ++p)
            {
                input >> points[p].x >> points[p].y >> points[p].z;
                input >> widths[p];
            }

            const BezierCurve3d curve(&points[0], &widths[0]);
            split_and_store(object.ref(), curve, split_count);
        }

        input.close();

        stopwatch.measure();

        RENDERER_LOG_INFO(
            "loaded curve file %s (%s curves) in %s.",
            filepath.c_str(),
            pretty_uint(curve_count).c_str(),
            pretty_time(stopwatch.get_seconds()).c_str());
    }
    else
    {
        RENDERER_LOG_ERROR("failed to load curve file %s.", filepath.c_str());
    }

    return object;
}

}   // namespace renderer
