
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

// Project headers.
#include "commandlinehandler.h"

// appleseed.shared headers.
#include "application/application.h"
#include "application/superlogger.h"

// appleseed.renderer headers.
#include "renderer/api/object.h"
#include "renderer/api/project.h"
#include "renderer/api/scene.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/math/cdf.h"
#include "foundation/math/qmc.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/filter.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/uid.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstring>
#include <map>
#include <string>
#include <vector>

using namespace appleseed::makefluffy;
using namespace appleseed::shared;
using namespace foundation;
using namespace renderer;
using namespace std;
namespace bf = boost::filesystem;

namespace
{
    //
    // Fluffification parameters.
    //

    struct FluffParams
    {
        RegExFilter m_include_filter;
        RegExFilter m_exclude_filter;

        size_t      m_curve_count;
        GScalar     m_curve_length;
        GScalar     m_root_width;
        GScalar     m_tip_width;
        GScalar     m_length_fuzziness;
        GScalar     m_curliness;
        size_t      m_split_count;

        explicit FluffParams(const CommandLineHandler& cl)
        {
            m_include_filter.set_pattern(cl.m_include.value().c_str());
            m_exclude_filter.set_pattern(cl.m_exclude.value().c_str());

            m_curve_count = cl.m_curves.value();
            m_curve_length = static_cast<GScalar>(cl.m_length.value());
            m_root_width = static_cast<GScalar>(cl.m_root_width.value());
            m_tip_width = static_cast<GScalar>(cl.m_tip_width.value());
            m_length_fuzziness = static_cast<GScalar>(cl.m_length_fuzziness.value());
            m_curliness = static_cast<GScalar>(cl.m_curliness.value());
            m_split_count = cl.m_presplits.value();
        }
    };


    //
    // Fluffification algorithm.
    //

    struct SupportTriangle
    {
        GVector3    m_v0, m_v1, m_v2;
        GVector3    m_normal;
        GScalar     m_area;
    };

    void extract_support_triangles(
        const MeshObject&           object,
        vector<SupportTriangle>&    support_triangles,
        CDF<size_t, GScalar>&       cdf)
    {
        const size_t triangle_count = object.get_triangle_count();
        for (size_t triangle_index = 0; triangle_index < triangle_count; ++triangle_index)
        {
            // Fetch the triangle.
            const Triangle& triangle = object.get_triangle(triangle_index);

            // Retrieve object instance space vertices of the triangle.
            const GVector3& v0 = object.get_vertex(triangle.m_v0);
            const GVector3& v1 = object.get_vertex(triangle.m_v1);
            const GVector3& v2 = object.get_vertex(triangle.m_v2);

            // Compute the geometric normal to the triangle and the area of the triangle.
            GVector3 normal = compute_triangle_normal(v0, v1, v2);
            const GScalar normal_norm = norm(normal);
            if (normal_norm == GScalar(0.0))
                continue;
            const GScalar rcp_normal_norm = GScalar(1.0) / normal_norm;
            const GScalar area = GScalar(0.5) * normal_norm;
            normal *= rcp_normal_norm;
            assert(is_normalized(normal));

            // Create and store the support triangle.
            SupportTriangle support_triangle;
            support_triangle.m_v0 = v0;
            support_triangle.m_v1 = v1;
            support_triangle.m_v2 = v2;
            support_triangle.m_normal = normal;
            support_triangle.m_area = area;
            support_triangles.push_back(support_triangle);

            // Insert the support triangle into the CDF.
            cdf.insert(support_triangles.size() - 1, area);
        }

        assert(cdf.valid());
        cdf.prepare();
    }

    void split_and_store(
        CurveObject&                object,
        const Curve3Type&           curve,
        const size_t                split_count)
    {
        if (split_count > 0)
        {
            Curve3Type child1, child2;
            curve.split(child1, child2);
            split_and_store(object, child1, split_count - 1);
            split_and_store(object, child2, split_count - 1);
        }
        else object.push_curve3(curve);
    }

    auto_release_ptr<CurveObject> create_curve_object(
        const Assembly&             assembly,
        const MeshObject&           support_object,
        const FluffParams&          params)
    {
        const size_t ControlPointCount = 4;

        vector<SupportTriangle> support_triangles;
        CDF<size_t, GScalar> cdf;
        extract_support_triangles(support_object, support_triangles, cdf);

        const string curve_object_name = string(support_object.get_name()) + "_curves";
        auto_release_ptr<CurveObject> curve_object =
            CurveObjectFactory::create(
                curve_object_name.c_str(),
                ParamArray());

        curve_object->reserve_curves3(params.m_curve_count);

        GVector3 points[ControlPointCount];
        GScalar widths[ControlPointCount];

        MersenneTwister rng;

        for (size_t i = 0; i < params.m_curve_count; ++i)
        {
            static const size_t Bases[] = { 2, 3 };
            const GVector3 s(hammersley_sequence<double, 3>(Bases, params.m_curve_count, i));

            const size_t triangle_index = cdf.sample(s[0]).first;
            const SupportTriangle& st = support_triangles[triangle_index];
            const GVector3 bary = sample_triangle_uniform(GVector2(s[1], s[2]));

            points[0] = st.m_v0 * bary[0] + st.m_v1 * bary[1] + st.m_v2 * bary[2];
            widths[0] = params.m_root_width;

            GScalar f, length;
            do
            {
                f = rand1(rng, -params.m_length_fuzziness, +params.m_length_fuzziness);
                length = max(params.m_curve_length * (GScalar(1.0) + f), GScalar(0.0));
            } while (length <= 0.0);

            for (size_t p = 1; p < ControlPointCount; ++p)
            {
                const GScalar r = static_cast<GScalar>(p) / (ControlPointCount - 1);
                const GVector3 f = params.m_curliness * sample_sphere_uniform(rand_vector2<GVector2>(rng));
                points[p] = points[0] + length * (r * st.m_normal + f);
                widths[p] = lerp(params.m_root_width, params.m_tip_width, r);
            }

            const Curve3Type curve(&points[0], &widths[0]);
            split_and_store(curve_object.ref(), curve, params.m_split_count);
        }

        return curve_object;
    }

    void make_fluffy(const Assembly& assembly, const FluffParams& params)
    {
        typedef vector<const ObjectInstance*> ObjectInstanceVector;
        typedef map<const MeshObject*, ObjectInstanceVector> ObjectToInstanceMap;

        // Establish an object -> object instance mapping.
        ObjectToInstanceMap objects_to_instances;
        for (const_each<ObjectInstanceContainer> i = assembly.object_instances(); i; ++i)
        {
            const ObjectInstance& object_instance = *i;

            // Skip excluded or non-included object instances.
            if (!params.m_include_filter.accepts(object_instance.get_name()) ||
                params.m_exclude_filter.accepts(object_instance.get_name()))
                continue;

            // Find the object referenced by this instance.
            const Object* object = object_instance.find_object();
            if (object == 0)
                continue;

            // Skip non-mesh objects.
            if (strcmp(object->get_model(), MeshObjectFactory::get_model()) != 0)
                continue;

            // Insert the (object, instance) pair into the mapping.
            objects_to_instances[
                static_cast<const MeshObject*>(object)].push_back(&object_instance);
        }

        // Loop over the collected objects.
        for (const_each<ObjectToInstanceMap> i = objects_to_instances; i; ++i)
        {
            const MeshObject& support_object = *i->first;
            const ObjectInstanceVector& support_object_instances = i->second;

            // Create a curve object.
            auto_release_ptr<CurveObject> curve_object =
                create_curve_object(assembly, support_object, params);

            // Instantiate the curve object into the assembly.
            for (const_each<ObjectInstanceVector> j = support_object_instances; j; ++j)
            {
                const ObjectInstance& support_instance = **j;
                const string curve_object_instance_name = string(curve_object->get_name()) + "_inst";
                assembly.object_instances().insert(
                    ObjectInstanceFactory::create(
                        curve_object_instance_name.c_str(),
                        support_instance.get_parameters(),
                        curve_object->get_name(),
                        support_instance.get_transform(),
                        support_instance.get_front_material_mappings(),
                        support_instance.get_back_material_mappings()));
            }

            // Insert the curve object into the assembly.
            assembly.objects().insert(auto_release_ptr<Object>(curve_object));
        }
    }

    void make_fluffy(Project& project, const FluffParams& params)
    {
        assert(project.get_scene());

        const Scene& scene = *project.get_scene();

        for (const_each<AssemblyContainer> i = scene.assemblies(); i; ++i)
            make_fluffy(*i, params);
    }
}


//
// Entry point of makefluffy.
//

int main(int argc, const char* argv[])
{
    // Initialize the logger that will be used throughout the program.
    SuperLogger logger;

    // Make sure appleseed is correctly installed.
    Application::check_installation(logger);

    // Parse the command line.
    CommandLineHandler cl;
    cl.parse(argc, argv, logger);

    // Load an apply settings from the settings file.
    Dictionary settings;
    Application::load_settings("appleseed.tools.xml", settings, logger);
    logger.configure_from_settings(settings);

    // Apply command line arguments.
    cl.apply(logger);

    // Configure the renderer's global logger.
    // Must be done after settings have been loaded and the command line
    // has been parsed, because these two operations may replace the log
    // target of the global logger.
    global_logger().initialize_from(logger);

    // Retrieve the command line arguments.
    const string& input_filepath = cl.m_filenames.values()[0];
    const string& output_filepath = cl.m_filenames.values()[1];
    const FluffParams params(cl);

    // Construct the schema file path.
    const bf::path schema_filepath =
          bf::path(Application::get_root_path())
        / "schemas"
        / "project.xsd";

    // Read the input project from disk.
    ProjectFileReader reader;
    auto_release_ptr<Project> project(
        reader.read(
            input_filepath.c_str(),
            schema_filepath.string().c_str()));

    // Bail out if the project couldn't be loaded.
    if (project.get() == 0)
        return 1;

    // Fluffify the project.
    make_fluffy(project.ref(), params);

    // Write the project back to disk.
    const bool success =
        ProjectFileWriter::write(
            project.ref(),
            output_filepath.c_str());

    return success ? 0 : 1;
}
