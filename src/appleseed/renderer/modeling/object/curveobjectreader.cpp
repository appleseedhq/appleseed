
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Srinath Ravichandran, The appleseedhq Organization
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
#include <foundation/core/exceptions/exceptionioerror.h>
#include "foundation/core/exceptions/exceptionunsupportedfileformat.h"
#include "foundation/curve/genericcurvefilereader.h"
#include "foundation/curve/icurvebuilder.h"
#include "foundation/curve/icurvefilereader.h"
#include "foundation/math/aabb.h"
#include "foundation/math/fp.h"
#include "foundation/math/qmc.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/types.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/otherwise.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <fstream>
#include <string>

using namespace foundation;
using namespace std;
namespace bf = boost::filesystem;

namespace renderer
{

//
// CurveObjectReader class implementation.
//

namespace {

    //
    // Curve object builder.
    //

    class CurveObjectBuilder
            : public ICurveBuilder {
    public:

        CurveObjectBuilder(
                const ParamArray &params,
                const string &name)
                : m_params(params), m_name(name), m_split_count(0) {
        }

        CurveObject* get_object()
        {
            return m_object;
        }

        size_t get_curve_count()
        {
            return m_object->get_curve_count();
        }

        size_t get_total_vertex_count()
        {
            return m_total_vertex_count;
        }

        string get_basis()
        {
            string basis;

            switch(m_object->get_basis())
            {
                case CurveBasis::LINEAR:
                    basis = "linear";
                    break;

                case CurveBasis::BEZIER:
                    basis = "bezier";
                    break;

                case CurveBasis::BSPLINE:
                    basis = "b-spline";
                    break;

                case CurveBasis::CATMULLROM:
                    basis = "catmull-rom";
                    break;
            }

            return basis;
        }

        void begin_curve_object(unsigned char basis, uint32 count) override {
            // Create an empty curve object.
            m_object = static_cast<CurveObject *>(
                    CurveObjectFactory().create(m_name.c_str(), m_params).release());

            m_split_count = m_params.get_optional<size_t>("presplits", 0);

            m_object->push_basis(basis);
            m_object->push_curve_count(count);

            switch (static_cast<CurveBasis>(basis)) {
                case CurveBasis::LINEAR:
                    m_object->reserve_curves1(count);
                    m_object->reserve_curves3(0);
                    break;

                case CurveBasis::BEZIER:
                case CurveBasis::BSPLINE:
                case CurveBasis::CATMULLROM:
                    m_object->reserve_curves3(count);
                    m_object->reserve_curves1(0);
                    break;
            }

        }

        void begin_curve() override {
            // Reset curve variables.
            reset_curve_variables();

        }

        void end_curve() override
        {
            switch (m_object->get_basis()) {
                case CurveBasis::LINEAR:
                    push_curve1();
                    break;

                case CurveBasis::BEZIER:
                case CurveBasis::BSPLINE:
                case CurveBasis::CATMULLROM:
                    push_curve3();
                    break;
            }

            m_total_vertex_count += m_vertices.size();
        }

        void end_curve_object() override {

        }


        void push_vertex(const Vector3f &v) override {
            return m_vertices.push_back(GVector3(v));
        }

        void push_vertex_width(const float w) override {
            return m_widths.push_back(w);
        }

        void push_vertex_opacity(const float o) override {
            return m_opacities.push_back(o);
        }

        void push_vertex_color(const Color3f &c) override {
            return m_colors.push_back(GColor3(c));
        }


    private:
        const ParamArray    m_params;
        const string        m_name;
        CurveObject*        m_object;
        size_t              m_split_count;

        // Curve attributes and statistics
        vector<GVector3>    m_vertices;
        vector<GScalar>     m_widths;
        vector<GScalar>     m_opacities;
        vector<GColor3>     m_colors;

        // Global statistics
        size_t              m_total_vertex_count;


        void reset_curve_variables() {
            m_vertices.clear();
            m_widths.clear();
            m_opacities.clear();
            m_colors.clear();
        }


        void split_and_store(const Curve3Type &curve, const size_t split_count) {
            if (split_count > 0) {
                Curve3Type child1, child2;
                curve.split(child1, child2);
                split_and_store(child1, split_count - 1);
                split_and_store(child2, split_count - 1);
            } else m_object->push_curve3(curve);
        }

        void push_curve1() {
            for (int32 i = 0; i < m_vertices.size() - 1; ++i) {
                GVector3 points[2] = {m_vertices[i], m_vertices[i + 1]};
                GScalar widths[2] = {m_widths[i], m_widths[i + 1]};
                GScalar opacities[2] = {m_opacities[i], m_opacities[i + 1]};
                GColor3 colors[2] = {m_colors[i], m_colors[i + 1]};

                m_object->push_curve1(Curve1Type(points, widths, opacities, colors));
            }
        }

        void push_curve3() {
            for (int32 i = 0; i < m_vertices.size() - 3; i = i + 3) {
                GVector3 points[4] = {m_vertices[i], m_vertices[i + 1], m_vertices[i + 2], m_vertices[i + 3]};
                GScalar widths[4] = {m_widths[i], m_widths[i + 1], m_widths[i + 2], m_widths[i + 3]};
                GScalar opacities[4] = {m_opacities[i], m_opacities[i + 1], m_opacities[i + 2], m_opacities[i + 3]};
                GColor3 colors[4] = {m_colors[i], m_colors[i + 1], m_colors[i + 2], m_colors[i + 3]};

                split_and_store(Curve3Type(points, widths, opacities, colors), m_split_count);
            }
        }
    };
}


auto_release_ptr<CurveObject> CurveObjectReader::read(
    const SearchPaths&      search_paths,
    const char*             name,
    const ParamArray&       params)
{
    const string filepath = params.get<string>("filepath");

    GenericCurveFileReader reader(filepath.c_str());

    CurveObjectBuilder builder(params, name);

    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    try
    {
        reader.read(builder);
    }

    catch (const ExceptionIOError&)
    {
        RENDERER_LOG_ERROR(
                "failed to load curve file %s: i/o error.",
                filepath.c_str());

        return auto_release_ptr<CurveObject>(nullptr);
    }
    catch (const exception& e)
    {
        RENDERER_LOG_ERROR(
                "failed to load curve file %s: %s.",
                filepath.c_str(),
                e.what());

        return auto_release_ptr<CurveObject>(nullptr);
    }

    stopwatch.measure();

    RENDERER_LOG_INFO(
            "loaded curve file %s (%s %s, %s %s, %s %s) in %s.",
            filepath.c_str(),
            pretty_int(builder.get_curve_count()).c_str(),"curves",
            builder.get_basis().c_str(), "type",
            pretty_int(builder.get_total_vertex_count()).c_str(),
            builder.get_total_vertex_count() > 1 ? "vertices" : "vertex",
            pretty_time(stopwatch.get_seconds()).c_str());


    return auto_release_ptr<CurveObject>(builder.get_object());
}
/*

auto_release_ptr<CurveObject> CurveObjectReader::create_hair_ball(
    const char*             name,
    const ParamArray&       params)
{
    auto_release_ptr<CurveObject> object(CurveObjectFactory().create(name, params));

    const size_t ControlPointCount = 4;
    const size_t curve_count = params.get_optional<size_t>("curves", 100);
    const GScalar curve_width = params.get_optional<GScalar>("width", GScalar(0.002));
    const size_t split_count = params.get_optional<size_t>("presplits", 0);

    object->reserve_curves3(curve_count);

    MersenneTwister rng;

    for (size_t c = 0; c < curve_count; ++c)
    {
        GVector3 points[ControlPointCount];

        for (size_t p = 0; p < ControlPointCount; ++p)
        {
            // http://math.stackexchange.com/questions/87230/picking-random-points-in-the-volume-of-sphere-with-uniform-probability
            const GScalar r = pow(GScalar(1.0) - rand2<GScalar>(rng), GScalar(1.0) / 3);
            const GVector3 d = sample_sphere_uniform(rand_vector2<GVector2>(rng));
            points[p] = r * d;
        }

        const Curve3Type curve(points, curve_width, 1.0f, Color3f(0.2, 0.0, 0.7));
        split_and_store(object.ref(), curve, split_count);
    }

    return object;
}

auto_release_ptr<CurveObject> CurveObjectReader::create_furry_ball(
    const char*             name,
    const ParamArray&       params)
{
    auto_release_ptr<CurveObject> object(CurveObjectFactory().create(name, params));

    const size_t ControlPointCount = 4;
    const size_t curve_count = params.get_optional<size_t>("curves", 100);
    const GScalar curve_length = params.get_optional<GScalar>("length", GScalar(0.1));
    const GScalar length_fuzziness = params.get_optional<GScalar>("length_fuzziness", GScalar(0.3));
    const GScalar root_width = params.get_optional<GScalar>("root_width", GScalar(0.001));
    const GScalar tip_width = params.get_optional<GScalar>("tip_width", GScalar(0.0001));
    const GScalar curliness = params.get_optional<GScalar>("curliness", GScalar(0.5));
    const size_t split_count = params.get_optional<size_t>("presplits", 0);

    object->reserve_curves3(curve_count);

    MersenneTwister rng;

    for (size_t c = 0; c < curve_count; ++c)
    {
        GVector3 points[ControlPointCount];
        GScalar widths[ControlPointCount];
        GScalar opacities[ControlPointCount];
        GColor3 colors[ControlPointCount];

        static const size_t Bases[] = { 2 };
        const GVector2 s = hammersley_sequence<GScalar, 2>(Bases, curve_count, c);
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
            opacities[p] = 1.0f;
            colors[p] = GColor3(0.2, 0.0, 0.7);
        }

        const Curve3Type curve(points, widths, opacities, colors);
        split_and_store(object.ref(), curve, split_count);
    }

    return object;
}

auto_release_ptr<CurveObject> CurveObjectReader::load_text_curve_file(
    const SearchPaths&      search_paths,
    const char*             name,
    const ParamArray&       params)
{
    auto_release_ptr<CurveObject> object(CurveObjectFactory().create(name, params));

    const string filepath = to_string(search_paths.qualify(params.get("filepath")));


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

    object->reserve_curves1(curve1_count);
    object->reserve_curves3(curve3_count);

    const size_t curve_count = curve1_count + curve3_count;

    for (size_t c = 0; c < curve_count; ++c)
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
            GVector3 points[2];
            GScalar widths[2];
            GScalar opacities[2];
            GColor3 colors[2];

            for (size_t p = 0; p < control_point_count; ++p)
            {
                input >> points[p].x >> points[p].y >> points[p].z;
                input >> widths[p];
                opacities[p] = 1.0f;
                colors[p] = GColor3(0.2, 0.0, 0.7);
            }

            // We never presplit degree-1 curves.
            const Curve1Type curve(points, widths, opacities, colors);
            object->push_curve1(curve);
        }
        else
        {
            assert(control_point_count == 4);

            GVector3 points[4];
            GScalar widths[4];
            GScalar opacities[4];
            GColor3 colors[4];

            for (size_t p = 0; p < control_point_count; ++p)
            {
                input >> points[p].x >> points[p].y >> points[p].z;
                input >> widths[p];
                opacities[p] = 1.0f;
                colors[p] = GColor3(0.2, 0.0, 0.7);
            }

            const Curve3Type curve(points, widths, opacities, colors);
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
        "loaded curve file %s (%s curve%s) in %s.",
        filepath.c_str(),
        pretty_uint(curve_count).c_str(),
        curve_count > 1 ? "s" : "",
        pretty_time(stopwatch.get_seconds()).c_str());

    return object;
}

auto_release_ptr<CurveObject> CurveObjectReader::load_mitsuba_curve_file(
    const SearchPaths&      search_paths,
    const char*             name,
    const ParamArray&       params)
{
    // todo: fix for big endian CPUs.

    auto_release_ptr<CurveObject> object(CurveObjectFactory().create(name, params));

    const string filepath = to_string(search_paths.qualify(params.get("filepath")));

    FILE* file = fopen(filepath.c_str(), "rb");
    if (file == nullptr)
    {
        RENDERER_LOG_ERROR("failed to open curve file %s.", filepath.c_str());
        return object;
    }

    char signature[12];

    if (fread(signature, 1, 11, file) != 11)
       RENDERER_LOG_ERROR("failed to load curve file %s: unknown signature.", filepath.c_str());

    signature[11] = '\0';

    if (strcmp(signature, "BINARY_HAIR") != 0)
    {
        fclose(file);
        RENDERER_LOG_ERROR("failed to load curve file %s: unknown signature.", filepath.c_str());
        return object;
    }

    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    uint32 vertex_count;
    if (fread(&vertex_count, sizeof(vertex_count), 1, file) != 1)
    {
        fclose(file);
        RENDERER_LOG_ERROR("failed to load curve file %s: i/o error.", filepath.c_str());
        return object;
    }

    const size_t degree = params.get_optional<size_t>("degree", 3);
    if (degree != 1 && degree != 3)
    {
        RENDERER_LOG_ERROR("curves degree must be 1 or 3 but was " FMT_SIZE_T ".", degree);
        return object;
    }

    const float radius = params.get_optional<float>("radius", 0.01f);
    if (radius <= 0.0f)
    {
        RENDERER_LOG_ERROR("curves radius must be greater than zero.");
        return object;
    }

    vector<GVector3> vertices, new_vertices;

    for (uint32 vertex_index = 0; vertex_index < vertex_count; )
    {
        float x;
        if (fread(&x, sizeof(x), 1, file) != 1)
        {
            fclose(file);
            RENDERER_LOG_ERROR("failed to load curve file %s: i/o error.", filepath.c_str());
            return object;
        }

        if (FP<float>::is_inf(x))
        {
            // End of the current hair strand, beginning of a new one.

            switch (degree)
            {
              case 1:
                if (vertices.size() >= 2)
                {
                    for (size_t i = 0, e = vertices.size() - 1; i < e; ++i)
                    {
                        const Curve1Type curve(&vertices[i], radius, GScalar(1.0f), GColor3(0.2, 0.0, 0.7));
                        object->push_curve1(curve);
                    }
                }
                break;

              case 3:
                if (vertices.size() >= 4)
                {
                    assert(new_vertices.empty());

                    for (size_t i = 0; i < vertices.size(); ++i)
                    {
                        new_vertices.push_back(vertices[i]);

                        if (i > 0 && i % 2 == 0 && i + 1 < vertices.size())
                        {
                            // Add a midpoint.
                            new_vertices.push_back(0.5f * (vertices[i] + vertices[i + 1]));
                        }
                    }

                    for (size_t i = 0, e = new_vertices.size(); i + 3 < e; i += 3)
                    {
                        const Curve3Type curve(&new_vertices[i], radius, GScalar(1.0f), GColor3(0.2, 0.0, 0.7));
                        object->push_curve3(curve);
                    }
                }
                break;

              assert_otherwise;
            }

            clear_keep_memory(vertices);
            clear_keep_memory(new_vertices);

            continue;
        }

        Vector2f yz;
        if (fread(&yz, sizeof(yz), 1, file) != 1)
        {
            fclose(file);
            RENDERER_LOG_ERROR("failed to load curve file %s: i/o error.", filepath.c_str());
            return object;
        }

        vertices.emplace_back(x, yz[0], yz[1]);
        ++vertex_index;
    }

    fclose(file);

    stopwatch.measure();

    const size_t curve_count = object->get_curve1_count() + object->get_curve3_count();

    RENDERER_LOG_INFO(
        "loaded curve file %s (%s curve%s) in %s.",
        filepath.c_str(),
        pretty_uint(curve_count).c_str(),
        curve_count > 1 ? "s" : "",
        pretty_time(stopwatch.get_seconds()).c_str());

    return object;
}
*/
}   // namespace renderer
