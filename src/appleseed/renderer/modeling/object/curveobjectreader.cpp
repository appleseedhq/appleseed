
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2015 Srinath Ravichandran, The appleseedhq Organization
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
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/aabb.h"
#include "foundation/math/qmc.h"
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
    void split_and_store(CurveObject& object, const CurveType3& curve, const size_t split_count)
    {
        if (split_count > 0)
        {
            CurveType3 child1, child2;
            curve.split(child1, child2);
            split_and_store(object, child1, split_count - 1);
            split_and_store(object, child2, split_count - 1);
        }
        else object.push_curve3(curve);
    }
}

auto_release_ptr<CurveObject> CurveObjectReader::create_hair_ball(
    const char*             name,
    const ParamArray&       params)
{
    auto_release_ptr<CurveObject> object = CurveObjectFactory::create(name, params);

    const size_t ControlPointCount = 4;
    const size_t curve_count = params.get_optional<size_t>("curves", 100);
    const GScalar curve_width = params.get_optional<GScalar>("width", GScalar(0.002));
    const size_t split_count = params.get_optional<size_t>("presplits", 0);

    GVector3 points[ControlPointCount];
    MersenneTwister rng;

    object->reserve_curves3(curve_count);

    for (size_t c = 0; c < curve_count; ++c)
    {
        for (size_t p = 0; p < ControlPointCount; ++p)
        {
            // http://math.stackexchange.com/questions/87230/picking-random-points-in-the-volume-of-sphere-with-uniform-probability
            const GScalar r = pow(GScalar(1.0) - rand2<GScalar>(rng), GScalar(1.0) / 3);
            const GVector3 d = sample_sphere_uniform(rand_vector2<GVector2>(rng));
            points[p] = r * d;
        }

        const CurveType3 curve(&points[0], curve_width);
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
    const GScalar curve_length = params.get_optional<GScalar>("length", GScalar(0.1));
    const GScalar length_fuzziness = params.get_optional<GScalar>("length_fuzziness", GScalar(0.3));
    const GScalar root_width = params.get_optional<GScalar>("root_width", GScalar(0.001));
    const GScalar tip_width = params.get_optional<GScalar>("tip_width", GScalar(0.0001));
    const GScalar curliness = params.get_optional<GScalar>("curliness", GScalar(0.5));
    const size_t split_count = params.get_optional<size_t>("presplits", 0);

    GVector3 points[ControlPointCount];
    GScalar widths[ControlPointCount];

    MersenneTwister rng;

    object->reserve_curves3(curve_count);

    for (size_t c = 0; c < curve_count; ++c)
    {
        static const size_t Bases[] = { 2 };
        const GVector2 s = hammersley_sequence<GScalar, 2>(Bases, c, curve_count);
        const GVector3 d = sample_sphere_uniform(s);

        points[0] = d;
        widths[0] = root_width;

        const GScalar f = rand1(rng, -length_fuzziness, +length_fuzziness);
        const GScalar length = curve_length * (GScalar(1.0) + f);

        for (size_t p = 1; p < ControlPointCount; ++p)
        {
            const GScalar r = static_cast<GScalar>(p) / (ControlPointCount - 1);
            const GVector3 f = curliness * sample_sphere_uniform(rand_vector2<GVector2>(rng));
            points[p] = points[0] + length * (r * d + f);
            widths[p] = lerp(root_width, tip_width, r);
        }

        const CurveType3 curve(&points[0], &widths[0]);
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
    input.open(filepath.c_str());

    if (!input.is_open())
    {
        RENDERER_LOG_ERROR("failed to open curve file %s.", filepath.c_str());
        return object;
    }

    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    size_t curve1_count;
    size_t curve3_count;
    input >> curve1_count;
    input >> curve3_count;

    vector<GVector3> points1(2);
    vector<GScalar> widths1(2);
    vector<GVector3> points3(4);
    vector<GScalar> widths3(4);

    object->reserve_curves1(curve1_count);
    object->reserve_curves3(curve3_count);

    for (size_t c = 0; c < curve1_count + curve3_count; ++c)
    {
        size_t control_point_count;
        input >> control_point_count;

        if (control_point_count != 2 && control_point_count != 4)
        {
            RENDERER_LOG_ERROR(
                "while loading curve file %s: only linear curves (2 control points) or cubic curves (4 control points) are currently supported.",
                filepath.c_str());
            return object;
        }

        if (control_point_count == 2)
        {
            for (size_t p = 0; p < control_point_count; ++p)
            {
                input >> points1[p].x >> points1[p].y >> points1[p].z;
                input >> widths1[p];
            }

            // We never presplit degree-1 curves.
            const CurveType1 curve(&points1[0], &widths1[0]);
            object->push_curve1(curve);
        }
        else
        {
            assert(control_point_count == 4);

            for (size_t p = 0; p < control_point_count; ++p)
            {
                input >> points3[p].x >> points3[p].y >> points3[p].z;
                input >> widths3[p];
            }

            const CurveType3 curve(&points3[0], &widths3[0]);
            split_and_store(object.ref(), curve, split_count);
        }
    }

    input.close();

    if (input.bad())
    {
        RENDERER_LOG_ERROR("failed to load curve file %s: i/o error.", filepath.c_str());
        return object;
    }

    stopwatch.measure();

    RENDERER_LOG_INFO(
        "loaded curve file %s (%s curves) in %s.",
        filepath.c_str(),
        pretty_uint(curve1_count + curve3_count).c_str(),
        pretty_time(stopwatch.get_seconds()).c_str());

    return object;
}

}   // namespace renderer
