
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "projectfileupdater.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/edf/diffuseedf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/environmentedf/constantenvironmentedf.h"
#include "renderer/modeling/environmentedf/constanthemisphereenvironmentedf.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentedf/gradientenvironmentedf.h"
#include "renderer/modeling/environmentedf/latlongmapenvironmentedf.h"
#include "renderer/modeling/environmentedf/mirrorballmapenvironmentedf.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/colorsource.h"
#include "renderer/modeling/light/directionallight.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/light/pointlight.h"
#include "renderer/modeling/light/spotlight.h"
#include "renderer/modeling/light/sunlight.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/project/configuration.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/project/regexrenderlayerrule.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/root.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstring>
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Base updater class.
    //

    class Updater
      : public NonCopyable
    {
      public:
        Updater(Project& project, const size_t from_revision)
          : m_project(project)
          , m_from_revision(from_revision)
          , m_to_revision(from_revision + 1)
        {
            assert(m_project.get_format_revision() == m_from_revision);

            RENDERER_LOG_INFO(
                "migrating project format from revision " FMT_SIZE_T " to revision " FMT_SIZE_T "...",
                m_from_revision,
                m_to_revision);
        }

        virtual ~Updater()
        {
            m_project.set_format_revision(m_to_revision);
        }

        virtual void update() = 0;

      protected:
        Project&                m_project;
        const size_t            m_from_revision;
        const size_t            m_to_revision;

        // Copy a key from one dictionary to another.
        static void copy_if_exist(
            Dictionary&         dest,
            const Dictionary&   src,
            const char*         key)
        {
            if (src.strings().exist(key))
                dest.strings().insert(key, src.strings().get(key));
        }

        // Copy a key from one dictionary to another, and rename the key.
        static void copy_if_exist(
            Dictionary&         dest,
            const char*         dest_key,
            const Dictionary&   src,
            const char*         src_key)
        {
            if (src.strings().exist(src_key))
                dest.strings().insert(dest_key, src.strings().get(src_key));
        }

        // Move a key from one dictionary to another at a given path.
        static void move_if_exist(
            ParamArray&         dest,
            const char*         dest_path,
            Dictionary&         src,
            const char*         src_key)
        {
            if (src.strings().exist(src_key))
            {
                dest.insert_path(dest_path, src.strings().get(src_key));
                src.strings().remove(src_key);
            }
        }

        // Move a key to a new path in the same parameter array.
        static void move_if_exist(
            ParamArray&         params,
            const char*         dest_path,
            const char*         src_key)
        {
            move_if_exist(params, dest_path, params, src_key);
        }

        // Helper function, same functionality as above.
        static void move_if_exist(
            Entity&             entity,
            const char*         dest_path,
            const char*         src_key)
        {
            move_if_exist(entity.get_parameters(), dest_path, src_key);
        }
    };


    //
    // Update from revision 0 to revision 1.
    //

    class UpdateFromRevision_0
      : public Updater
    {
      public:
        explicit UpdateFromRevision_0(Project& project)
          : Updater(project, 0)
        {
        }

        virtual void update() APPLESEED_OVERRIDE
        {
            // Nothing to do.
        }
    };


    //
    // Update from revision 1 to revision 2.
    //

    class UpdateFromRevision_1
      : public Updater
    {
      public:
        explicit UpdateFromRevision_1(Project& project)
          : Updater(project, 1)
        {
        }

        virtual void update() APPLESEED_OVERRIDE
        {
            // Nothing to do.
        }
    };


    //
    // Update from revision 2 to revision 3.
    //

    class UpdateFromRevision_2
      : public Updater
    {
      public:
        explicit UpdateFromRevision_2(Project& project)
          : Updater(project, 2)
        {
        }

        virtual void update() APPLESEED_OVERRIDE
        {
            introduce_pixel_renderers();
            move_filter_parameters_from_configurations_to_frame();
        }

      private:
        static double max_variation_to_quality(const double variation)
        {
            const double q = -log(variation, 10.0);
            const int n = static_cast<int>(q * 10.0);
            return static_cast<double>(n) / 10.0;
        }

        void introduce_pixel_renderers()
        {
            for (each<ConfigurationContainer> i = m_project.configurations(); i; ++i)
            {
                Dictionary& root = i->get_parameters();

                if (!root.dictionaries().exist("generic_tile_renderer"))
                    continue;

                Dictionary& gtr = root.dictionary("generic_tile_renderer");

                copy_if_exist(root, "pixel_renderer", gtr, "sampler");

                {
                    Dictionary upr;
                    copy_if_exist(upr, "samples", gtr, "max_samples");
                    root.insert("uniform_pixel_renderer", upr);
                }

                {
                    Dictionary apr;
                    copy_if_exist(apr, gtr, "min_samples");
                    copy_if_exist(apr, gtr, "max_samples");
                    if (gtr.strings().exist("max_variation"))
                        apr.insert("quality", max_variation_to_quality(gtr.get<double>("max_variation")));
                    copy_if_exist(apr, "enable_diagnostics", gtr, "enable_adaptive_sampler_diagnostics");
                    root.insert("adaptive_pixel_renderer", apr);
                }

                gtr.strings().remove("sampler");
                gtr.strings().remove("min_samples");
                gtr.strings().remove("max_samples");
                gtr.strings().remove("max_contrast");
                gtr.strings().remove("max_variation");
                gtr.strings().remove("enable_adaptive_sampler_diagnostics");
            }
        }

        void move_filter_parameters_from_configurations_to_frame()
        {
            Frame* frame = m_project.get_frame();

            for (each<ConfigurationContainer> i = m_project.configurations(); i; ++i)
            {
                Dictionary& root = i->get_parameters();

                if (!root.dictionaries().exist("generic_tile_renderer"))
                    continue;

                Dictionary& gtr = root.dictionary("generic_tile_renderer");

                if (frame && strcmp(i->get_name(), "final") == 0)
                {
                    copy_if_exist(frame->get_parameters(), gtr, "filter");
                    copy_if_exist(frame->get_parameters(), gtr, "filter_size");
                }

                gtr.strings().remove("filter");
                gtr.strings().remove("filter_size");
            }
        }
    };


    //
    // Update from revision 3 to revision 4.
    //

    class UpdateFromRevision_3
      : public Updater
    {
      public:
        explicit UpdateFromRevision_3(Project& project)
          : Updater(project, 3)
        {
        }

        virtual void update() APPLESEED_OVERRIDE
        {
            const Scene* scene = m_project.get_scene();

            if (scene)
            {
                for (each<EnvironmentEDFContainer> i = scene->environment_edfs(); i; ++i)
                    rename_exitance_inputs(*i);

                rename_exitance_inputs(scene->assemblies());
            }
        }

      private:
        void rename_exitance_inputs(AssemblyContainer& assemblies)
        {
            for (each<AssemblyContainer> i = assemblies; i; ++i)
            {
                rename_exitance_inputs(*i);
                rename_exitance_inputs(i->assemblies());
            }
        }

        void rename_exitance_inputs(Assembly& assembly)
        {
            for (each<EDFContainer> i = assembly.edfs(); i; ++i)
                rename_exitance_inputs(*i);

            for (each<LightContainer> i = assembly.lights(); i; ++i)
                rename_exitance_inputs(*i);
        }

        void rename_exitance_inputs(EDF& edf)
        {
            if (strcmp(edf.get_model(), DiffuseEDFFactory().get_model()) == 0)
            {
                move_if_exist(edf, "radiance", "exitance");
                move_if_exist(edf, "radiance_multiplier", "exitance_multiplier");
            }
        }

        void rename_exitance_inputs(Light& light)
        {
            if (strcmp(light.get_model(), DirectionalLightFactory().get_model()) == 0 ||
                strcmp(light.get_model(), PointLightFactory().get_model()) == 0 ||
                strcmp(light.get_model(), SpotLightFactory().get_model()) == 0)
            {
                move_if_exist(light, "radiance", "exitance");
                move_if_exist(light, "radiance_multiplier", "exitance_multiplier");
            }
            else if (strcmp(light.get_model(), SunLightFactory().get_model()) == 0)
            {
                move_if_exist(light, "radiance_multiplier", "exitance_multiplier");
            }
        }

        void rename_exitance_inputs(EnvironmentEDF& edf)
        {
            if (strcmp(edf.get_model(), ConstantEnvironmentEDFFactory().get_model()) == 0)
            {
                move_if_exist(edf, "radiance", "exitance");
            }
            else if (strcmp(edf.get_model(), ConstantHemisphereEnvironmentEDFFactory().get_model()) == 0)
            {
                move_if_exist(edf, "upper_hemi_radiance", "upper_hemi_exitance");
                move_if_exist(edf, "lower_hemi_radiance", "lower_hemi_exitance");
            }
            else if (strcmp(edf.get_model(), GradientEnvironmentEDFFactory().get_model()) == 0)
            {
                move_if_exist(edf, "horizon_radiance", "horizon_exitance");
                move_if_exist(edf, "zenith_radiance", "zenith_exitance");
            }
            else if (strcmp(edf.get_model(), LatLongMapEnvironmentEDFFactory().get_model()) == 0 ||
                     strcmp(edf.get_model(), MirrorBallMapEnvironmentEDFFactory().get_model()) == 0)
            {
                move_if_exist(edf, "radiance", "exitance");
                move_if_exist(edf, "radiance_multiplier", "exitance_multiplier");
            }
        }
    };


    //
    // Update from revision 4 to revision 5.
    //

    class UpdateFromRevision_4
      : public Updater
    {
      public:
        explicit UpdateFromRevision_4(Project& project)
          : Updater(project, 4)
        {
        }

        virtual void update() APPLESEED_OVERRIDE
        {
            for (each<ConfigurationContainer> i = m_project.configurations(); i; ++i)
            {
                ParamArray& root = i->get_parameters();
                move_if_exist(root, "texture_store.max_size", "texture_cache_size");
            }
        }
    };


    //
    // Update from revision 5 to revision 6.
    //

    class UpdateFromRevision_5
      : public Updater
    {
      public:
        explicit UpdateFromRevision_5(Project& project)
          : Updater(project, 5)
        {
        }

        virtual void update() APPLESEED_OVERRIDE
        {
            const Scene* scene = m_project.get_scene();

            if (scene)
                update_assemblies(scene->assemblies());
        }

      private:
        class BlinnExponentFunction
        {
          public:
            explicit BlinnExponentFunction(const double e)
              : m_e(e)
            {
            }

            double operator()(const double x) const
            {
                return 100.0 * pow_int(x, 3) + 9900.0 * pow(x, 30.0) - m_e;
            }

          private:
            const double m_e;
        };

        static void update_assemblies(const AssemblyContainer& assemblies)
        {
            for (const_each<AssemblyContainer> i = assemblies; i; ++i)
            {
                update_bsdfs(*i, i->bsdfs());
                update_assemblies(i->assemblies());
            }
        }

        static void update_bsdfs(const Assembly& assembly, BSDFContainer& bsdfs)
        {
            for (each<BSDFContainer> i = bsdfs; i; ++i)
            {
                BSDF& bsdf = *i;

                if (strcmp(bsdf.get_model(), "microfacet_brdf"))
                    continue;

                ParamArray& params = bsdf.get_parameters();

                const string mdf = params.get_optional<string>("mdf", "");
                const string mdf_param = params.get_optional<string>("mdf_parameter", "");

                if (mdf_param.empty())
                    continue;

                double mdf_param_value;
                if (try_parse_scalar(mdf_param, mdf_param_value))
                {
                    double glossiness;
                    if (!mdf_param_to_glossiness(mdf, mdf_param_value, glossiness))
                    {
                        RENDERER_LOG_ERROR(
                            "while updating bsdf \"%s\", failed to convert mdf parameter %f.",
                            bsdf.get_name(),
                            mdf_param_value);
                        continue;
                    }

                    params.insert("glossiness", glossiness);
                }
                else
                {
                    ColorEntity* color = find_color_entity(assembly, mdf_param);

                    if (color)
                    {
                        const ColorSource source(*color);

                        double mdf_param_value;
                        source.evaluate_uniform(mdf_param_value);

                        double glossiness;
                        if (!mdf_param_to_glossiness(mdf, mdf_param_value, glossiness))
                        {
                            RENDERER_LOG_ERROR(
                                "while updating bsdf \"%s\", failed to convert mdf parameter %f in color entity \"%s\".",
                                bsdf.get_name(),
                                mdf_param_value,
                                color->get_name());
                            continue;
                        }

                        ParamArray new_color_params = color->get_parameters();
                        new_color_params.remove_path("multiplier");

                        ColorValueArray new_color_values;
                        new_color_values.push_back(static_cast<float>(glossiness));

                        auto_release_ptr<ColorEntity> new_color_entity(
                            ColorEntityFactory::create(
                                color->get_name(),
                                new_color_params,
                                new_color_values));

                        Assembly* parent_assembly = dynamic_cast<Assembly*>(color->get_parent());

                        if (parent_assembly)
                        {
                            parent_assembly->colors().remove(color);
                            parent_assembly->colors().insert(new_color_entity);
                        }
                        else
                        {
                            Scene* parent_scene = dynamic_cast<Scene*>(color->get_parent());
                            assert(parent_scene);

                            parent_scene->colors().remove(color);
                            parent_scene->colors().insert(new_color_entity);
                        }
                    }

                    move_if_exist(params, "glossiness", "mdf_parameter");
                }
            }
        }

        static bool try_parse_scalar(const string& s, double& value)
        {
            try
            {
                value = from_string<double>(s);
                return true;
            }
            catch (const ExceptionStringConversionError&)
            {
                return false;
            }
        }

        static ColorEntity* find_color_entity(const Assembly& assembly, const string& name)
        {
            for (each<ColorContainer> i = assembly.colors(); i; ++i)
            {
                if (i->get_name() == name)
                    return &*i;
            }

            Assembly* parent_assembly = dynamic_cast<Assembly*>(assembly.get_parent());

            if (parent_assembly)
                return find_color_entity(*parent_assembly, name);

            Scene* parent_scene = dynamic_cast<Scene*>(assembly.get_parent());
            assert(parent_scene);

            return find_color_entity(*parent_scene, name);
        }

        static ColorEntity* find_color_entity(const Scene& scene, const string& name)
        {
            for (each<ColorContainer> i = scene.colors(); i; ++i)
            {
                if (i->get_name() == name)
                    return &*i;
            }

            return 0;
        }

        static bool mdf_param_to_glossiness(const string& mdf, const double mdf_param, double& glossiness)
        {
            if (mdf == "blinn")
            {
                const BlinnExponentFunction f(mdf_param);
                return find_root_bisection(f, 0.0, 1.0, 1.0e-6, 100, glossiness);
            }
            else
            {
                glossiness = saturate(1.0 - mdf_param);
                return true;
            }
        }
    };


    //
    // Update from revision 6 to revision 7.
    //

    class UpdateFromRevision_6
      : public Updater
    {
      public:
        explicit UpdateFromRevision_6(Project& project)
          : Updater(project, 6)
        {
        }

        virtual void update() APPLESEED_OVERRIDE
        {
            const Scene* scene = m_project.get_scene();

            if (scene)
            {
                update_collection(scene->environment_edfs());
                update_collection(scene->environment_shaders());
                update_collection(scene->assemblies());
                update_collection(scene->assembly_instances());
            }
        }

      private:
        template <typename Collection>
        void update_collection(Collection& collection)
        {
            for (each<Collection> i = collection; i; ++i)
                update_entity(*i);
        }

        template <typename Entity>
        void update_entity(Entity& entity)
        {
            StringDictionary& string_params = entity.get_parameters().strings();

            const string render_layer_name =
                string_params.exist("render_layer")
                    ? string_params.get<string>("render_layer")
                    : string();

            if (!render_layer_name.empty())
            {
                const string entity_path = entity.get_path();

                string rule_name = entity_path;
                replace(rule_name.begin(), rule_name.end(), '/', '_');

                m_project.add_render_layer_rule(
                    RegExRenderLayerRuleFactory().create(
                        rule_name.c_str(),
                        ParamArray()
                            .insert("render_layer", render_layer_name)
                            .insert("order", "1")
                            .insert("pattern", "^" + entity_path + "$")));

                string_params.remove("render_layer");
            }
        }

        void update_entity(Assembly& assembly)
        {
            update_collection(assembly.edfs());
            update_collection(assembly.lights());
            update_collection(assembly.materials());
            update_collection(assembly.objects());
            update_collection(assembly.object_instances());
            update_collection(assembly.surface_shaders());
            update_collection(assembly.assemblies());
            update_collection(assembly.assembly_instances());
        }
    };


    //
    // Update from revision 7 to revision 8.
    //

    class UpdateFromRevision_7
      : public Updater
    {
      public:
        explicit UpdateFromRevision_7(Project& project)
          : Updater(project, 7)
        {
        }

        virtual void update() APPLESEED_OVERRIDE
        {
            const Scene* scene = m_project.get_scene();

            if (scene)
                update_collection(scene->assemblies());
        }

      private:
        template <typename Collection>
        void update_collection(Collection& collection)
        {
            for (each<Collection> i = collection; i; ++i)
                update_entity(*i);
        }

        void update_entity(Assembly& assembly)
        {
            update_collection(assembly.object_instances());
            update_collection(assembly.assemblies());
        }

        void update_entity(ObjectInstance& object_instance)
        {
            const Object* object = object_instance.find_object();

            if (object && object->get_material_slot_count() == 1)
            {
                const string slot_name = object->get_material_slot(0);

                rebuild_material_mappings(object_instance.get_front_material_mappings(), slot_name);
                rebuild_material_mappings(object_instance.get_back_material_mappings(), slot_name);
            }
        }

        void rebuild_material_mappings(StringDictionary& mappings, const string& slot_name)
        {
            if (!mappings.empty())
            {
                const string material_name = mappings.begin().value();

                mappings.clear();
                mappings.insert(slot_name, material_name);
            }
        }
    };


    //
    // Update from revision 8 to revision 9.
    //

    class UpdateFromRevision_8
      : public Updater
    {
      public:
        explicit UpdateFromRevision_8(Project& project)
          : Updater(project, 8)
        {
        }

        virtual void update() APPLESEED_OVERRIDE
        {
            const Scene* scene = m_project.get_scene();

            if (scene)
                rename_radiance_inputs(scene->assemblies());
        }

      private:
        void rename_radiance_inputs(AssemblyContainer& assemblies)
        {
            for (each<AssemblyContainer> i = assemblies; i; ++i)
            {
                rename_radiance_inputs(*i);
                rename_radiance_inputs(i->assemblies());
            }
        }

        void rename_radiance_inputs(Assembly& assembly)
        {
            for (each<LightContainer> i = assembly.lights(); i; ++i)
                rename_radiance_inputs(*i);
        }

        void rename_radiance_inputs(Light& light)
        {
            if (strcmp(light.get_model(), DirectionalLightFactory().get_model()) == 0)
            {
                move_if_exist(light, "irradiance", "radiance");
                move_if_exist(light, "irradiance_multiplier", "radiance_multiplier");
            }
            else if (strcmp(light.get_model(), PointLightFactory().get_model()) == 0 ||
                     strcmp(light.get_model(), SpotLightFactory().get_model()) == 0)
            {
                move_if_exist(light, "intensity", "radiance");
                move_if_exist(light, "intensity_multiplier", "radiance_multiplier");
            }
        }
    };
}

bool ProjectFileUpdater::update(Project& project, const size_t to_revision)
{
    bool modified = false;

    size_t format_revision = project.get_format_revision();

#define CASE_UPDATE_FROM_REVISION(from)             \
    case from:                                      \
    {                                               \
        if (format_revision >= to_revision)         \
            break;                                  \
                                                    \
        UpdateFromRevision_##from updater(project); \
        updater.update();                           \
                                                    \
        format_revision = from + 1;                 \
        modified = true;                            \
    }

    switch (format_revision)
    {
      CASE_UPDATE_FROM_REVISION(0);
      CASE_UPDATE_FROM_REVISION(1);
      CASE_UPDATE_FROM_REVISION(2);
      CASE_UPDATE_FROM_REVISION(3);
      CASE_UPDATE_FROM_REVISION(4);
      CASE_UPDATE_FROM_REVISION(5);
      CASE_UPDATE_FROM_REVISION(6);
      CASE_UPDATE_FROM_REVISION(7);
      CASE_UPDATE_FROM_REVISION(8);

      case 9:
        // Project is up-to-date.
        break;

      default:
        RENDERER_LOG_ERROR("unsupported project format revision: " FMT_SIZE_T, format_revision);
        break;
    }

#undef CASE_UPDATE_FROM_REVISION

    return modified;
}

}   // namespace renderer
