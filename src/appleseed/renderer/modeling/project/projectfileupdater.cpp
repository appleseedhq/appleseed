
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

// Interface header.
#include "projectfileupdater.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/disneybrdf.h"
#include "renderer/modeling/bsdf/glassbsdf.h"
#include "renderer/modeling/bsdf/glossybrdf.h"
#include "renderer/modeling/bsdf/metalbrdf.h"
#include "renderer/modeling/bsdf/specularbtdf.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/bssrdf/gaussianbssrdf.h"
#include "renderer/modeling/camera/camera.h"
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
#include "renderer/modeling/material/disneymaterial.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/project/configuration.h"
#include "renderer/modeling/project/eventcounters.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/project/projectformatrevision.h"
#include "renderer/modeling/project/regexrenderlayerrule.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/surfaceshader/physicalsurfaceshader.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/root.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/iterators.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstring>
#include <string>
#include <vector>

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
        static float max_variation_to_quality(const float variation)
        {
            const float q = -log(variation, 10.0f);
            const int n = static_cast<int>(q * 10.0f);
            return static_cast<float>(n) / 10.0f;
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
                        apr.insert("quality", max_variation_to_quality(gtr.get<float>("max_variation")));
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
            if (Scene* scene = m_project.get_scene())
            {
                for (each<EnvironmentEDFContainer> i = scene->environment_edfs(); i; ++i)
                    rename_exitance_inputs(*i);

                rename_exitance_inputs(scene->assemblies());
            }
        }

      private:
        static void rename_exitance_inputs(AssemblyContainer& assemblies)
        {
            for (each<AssemblyContainer> i = assemblies; i; ++i)
            {
                rename_exitance_inputs(*i);
                rename_exitance_inputs(i->assemblies());
            }
        }

        static void rename_exitance_inputs(Assembly& assembly)
        {
            for (each<EDFContainer> i = assembly.edfs(); i; ++i)
                rename_exitance_inputs(*i);

            for (each<LightContainer> i = assembly.lights(); i; ++i)
                rename_exitance_inputs(*i);
        }

        static void rename_exitance_inputs(EDF& edf)
        {
            if (strcmp(edf.get_model(), DiffuseEDFFactory().get_model()) == 0)
            {
                move_if_exist(edf, "radiance", "exitance");
                move_if_exist(edf, "radiance_multiplier", "exitance_multiplier");
            }
        }

        static void rename_exitance_inputs(Light& light)
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

        static void rename_exitance_inputs(EnvironmentEDF& edf)
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
            if (Scene* scene = m_project.get_scene())
                update_assemblies(scene->assemblies());
        }

      private:
        class BlinnExponentFunction
        {
          public:
            explicit BlinnExponentFunction(const float e)
              : m_e(e)
            {
            }

            float operator()(const float x) const
            {
                return 100.0f * pow_int<3>(x) + 9900.0f * pow_int<30>(x) - m_e;
            }

          private:
            const float m_e;
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

                float mdf_param_value;
                if (try_parse_scalar(mdf_param, mdf_param_value))
                {
                    float glossiness;
                    if (!mdf_param_to_glossiness(mdf, mdf_param_value, glossiness))
                    {
                        RENDERER_LOG_ERROR(
                            "while updating bsdf \"%s\", failed to convert mdf parameter %f.",
                            bsdf.get_path().c_str(),
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

                        float mdf_param_value;
                        source.evaluate_uniform(mdf_param_value);

                        float glossiness;
                        if (!mdf_param_to_glossiness(mdf, mdf_param_value, glossiness))
                        {
                            RENDERER_LOG_ERROR(
                                "while updating bsdf \"%s\", failed to convert mdf parameter %f in color entity \"%s\".",
                                bsdf.get_path().c_str(),
                                mdf_param_value,
                                color->get_path().c_str());
                            continue;
                        }

                        ParamArray new_color_params = color->get_parameters();
                        new_color_params.remove_path("multiplier");

                        ColorValueArray new_color_values;
                        new_color_values.push_back(glossiness);

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

        static bool try_parse_scalar(const string& s, float& value)
        {
            try
            {
                value = from_string<float>(s);
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

        static bool mdf_param_to_glossiness(const string& mdf, const float mdf_param, float& glossiness)
        {
            if (mdf == "blinn")
            {
                const BlinnExponentFunction f(mdf_param);
                return find_root_bisection(f, 0.0f, 1.0f, 1.0e-6f, 100, glossiness);
            }
            else
            {
                glossiness = saturate(1.0f - mdf_param);
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
            if (Scene* scene = m_project.get_scene())
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
                const string entity_path(entity.get_path().c_str());

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
            if (Scene* scene = m_project.get_scene())
                update_collection(scene->assemblies());
        }

      private:
        template <typename Collection>
        static void update_collection(Collection& collection)
        {
            for (each<Collection> i = collection; i; ++i)
                update_entity(*i);
        }

        static void update_entity(Assembly& assembly)
        {
            update_collection(assembly.object_instances());
            update_collection(assembly.assemblies());
        }

        static void update_entity(ObjectInstance& object_instance)
        {
            const Object* object = object_instance.find_object();

            if (object && object->get_material_slot_count() == 1)
            {
                const string slot_name = object->get_material_slot(0);

                rebuild_material_mappings(object_instance.get_front_material_mappings(), slot_name);
                rebuild_material_mappings(object_instance.get_back_material_mappings(), slot_name);
            }
        }

        static void rebuild_material_mappings(StringDictionary& mappings, const string& slot_name)
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
            if (Scene* scene = m_project.get_scene())
                rename_radiance_inputs(scene->assemblies());
        }

      private:
        static void rename_radiance_inputs(AssemblyContainer& assemblies)
        {
            for (each<AssemblyContainer> i = assemblies; i; ++i)
            {
                rename_radiance_inputs(*i);
                rename_radiance_inputs(i->assemblies());
            }
        }

        static void rename_radiance_inputs(Assembly& assembly)
        {
            for (each<LightContainer> i = assembly.lights(); i; ++i)
                rename_radiance_inputs(*i);
        }

        static void rename_radiance_inputs(Light& light)
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


    //
    // Update from revision 9 to revision 10.
    //

    class UpdateFromRevision_9
      : public Updater
    {
      public:
        explicit UpdateFromRevision_9(Project& project)
          : Updater(project, 9)
        {
        }

        virtual void update() APPLESEED_OVERRIDE
        {
            if (Scene* scene = m_project.get_scene())
                visit(scene->assemblies());
        }

      private:
        struct MaterialInfo
        {
            bool        m_updated;
            Material*   m_material;
            BSDF*       m_bsdf;
            string      m_bsdf_reflectance;
            string      m_bsdf_reflectance_multiplier;
            string      m_bsdf_transmittance;
            string      m_bsdf_transmittance_multiplier;
            string      m_bsdf_fresnel_multiplier;
            float       m_bsdf_from_ior;
            float       m_bsdf_to_ior;
            string      m_bssrdf;
            string      m_edf;
            string      m_alpha_map;
            string      m_displacement_map;
            string      m_displacement_method;
            string      m_bump_amplitude;
            string      m_normal_map_up;
        };

        static void visit(AssemblyContainer& assemblies)
        {
            for (each<AssemblyContainer> i = assemblies; i; ++i)
                visit(*i);
        }

        static void visit(Assembly& assembly)
        {
            visit(assembly.assemblies());
            update(assembly);
        }

        static void update(Assembly& assembly)
        {
            vector<MaterialInfo> materials;
            collect_refractive_materials(assembly, materials);

            for (each<vector<MaterialInfo> > i = materials; i; ++i)
            {
                if (i->m_updated)
                    continue;

                for (each<vector<MaterialInfo> > j = succ(i); j; ++j)
                {
                    if (j->m_updated)
                        continue;

                    MaterialInfo& mat1 = *i;
                    MaterialInfo& mat2 = *j;

                    if (are_paired(mat1, mat2))
                    {
                        // Extract mat1 from the assembly.
                        auto_release_ptr<Material> mat1_owner = assembly.materials().remove(mat1.m_material);

                        // Update mat1's BSDF.
                        // todo: make sure the BSDF is not used in another material.
                        auto_release_ptr<BSDF> bsdf_owner = assembly.bsdfs().remove(mat1.m_bsdf);
                        update_bsdf(mat1);
                        assembly.bsdfs().insert(bsdf_owner);

                        // Rename mat1.
                        const string old_mat1_name = mat1.m_material->get_name();
                        cleanup_entity_name(*mat1.m_material);
                        update_material_mappings(
                            assembly.object_instances(),
                            old_mat1_name.c_str(),
                            mat1.m_material->get_name());
                        update_material_mappings(
                            assembly.object_instances(),
                            mat2.m_material->get_name(),
                            mat1.m_material->get_name());

                        // Insert mat1 back into the assembly.
                        assembly.materials().insert(mat1_owner);

                        // Remove mat2.
                        assembly.bsdfs().remove(mat2.m_bsdf);
                        assembly.materials().remove(mat2.m_material);

                        mat1.m_updated = true;
                        mat2.m_updated = true;
                        break;
                    }
                }

                if (!i->m_updated)
                {
                    // Update the material's BSDF.
                    // todo: make sure the BSDF is not used in another material.
                    auto_release_ptr<BSDF> bsdf_owner = assembly.bsdfs().remove(i->m_bsdf);
                    update_bsdf(*i);
                    assembly.bsdfs().insert(bsdf_owner);

                    i->m_updated = true;
                }
            }
        }

        static void collect_refractive_materials(Assembly& assembly, vector<MaterialInfo>& materials)
        {
            for (each<MaterialContainer> i = assembly.materials(); i; ++i)
            {
                Material& material = *i;

                if (strcmp(material.get_model(), "generic_material"))
                    continue;

                const ParamArray& material_params = material.get_parameters();

                if (!material_params.strings().exist("bsdf"))
                    continue;

                const string bsdf_name = material_params.get<string>("bsdf");
                BSDF* bsdf = assembly.bsdfs().get_by_name(bsdf_name.c_str());

                if (bsdf == 0)
                    continue;

                if (strcmp(bsdf->get_model(), "specular_btdf"))
                    continue;

                const ParamArray& bsdf_params = bsdf->get_parameters();

                if (!bsdf_params.strings().exist("from_ior") ||
                    !bsdf_params.strings().exist("to_ior") ||
                    !bsdf_params.strings().exist("reflectance") ||
                    !bsdf_params.strings().exist("transmittance"))
                    continue;

                // At this point we have found a material that needs to be updated.

                MaterialInfo info;
                info.m_updated = false;
                info.m_material = &material;
                info.m_bsdf = bsdf;
                info.m_bsdf_reflectance = bsdf_params.get<string>("reflectance");
                info.m_bsdf_reflectance_multiplier = bsdf_params.get_optional<string>("reflectance_multiplier", "1.0");
                info.m_bsdf_transmittance = bsdf_params.get<string>("transmittance");
                info.m_bsdf_transmittance_multiplier = bsdf_params.get_optional<string>("transmittance_multiplier", "1.0");
                info.m_bsdf_fresnel_multiplier = bsdf_params.get_optional<string>("fresnel_multiplier", "1.0");
                info.m_bsdf_from_ior = bsdf_params.get<float>("from_ior");
                info.m_bsdf_to_ior = bsdf_params.get<float>("to_ior");
                info.m_bssrdf = material_params.get_optional<string>("bssrdf", "");
                info.m_edf = material_params.get_optional<string>("edf", "");
                info.m_alpha_map = material_params.get_optional<string>("alpha_map", "");
                info.m_displacement_map = material_params.get_optional<string>("displacement_map", "");
                info.m_displacement_method = material_params.get_optional<string>("displacement_method", "");
                info.m_bump_amplitude = material_params.get_optional<string>("bump_amplitude", "");
                info.m_normal_map_up = material_params.get_optional<string>("normal_map_up", "");

                materials.push_back(info);
            }
        }

        static bool are_paired(const MaterialInfo& lhs, const MaterialInfo& rhs)
        {
            assert(!lhs.m_updated && !rhs.m_updated);

            return
                lhs.m_bsdf_reflectance == rhs.m_bsdf_reflectance &&
                lhs.m_bsdf_reflectance_multiplier == rhs.m_bsdf_reflectance_multiplier &&
                lhs.m_bsdf_transmittance == rhs.m_bsdf_transmittance &&
                lhs.m_bsdf_transmittance_multiplier == rhs.m_bsdf_transmittance_multiplier &&
                lhs.m_bsdf_fresnel_multiplier == rhs.m_bsdf_fresnel_multiplier &&
                feq(lhs.m_bsdf_from_ior, rhs.m_bsdf_to_ior) &&
                feq(lhs.m_bsdf_to_ior, rhs.m_bsdf_from_ior) &&
                lhs.m_bssrdf == rhs.m_bssrdf &&
                lhs.m_edf == rhs.m_edf &&
                lhs.m_alpha_map == rhs.m_alpha_map &&
                lhs.m_displacement_map == rhs.m_displacement_map &&
                lhs.m_displacement_method == rhs.m_displacement_method &&
                lhs.m_bump_amplitude == rhs.m_bump_amplitude &&
                lhs.m_normal_map_up == rhs.m_normal_map_up;
        }

        static void cleanup_entity_name(Entity& entity)
        {
            string name = entity.get_name();
            name = replace(name, "_front_", "");
            name = replace(name, "_front", "");
            name = replace(name, "front_", "");
            name = replace(name, "_back_", "");
            name = replace(name, "_back", "");
            name = replace(name, "back_", "");
            entity.set_name(name.c_str());
        }

        static void update_material_mappings(
            ObjectInstanceContainer&    object_instances,
            const char*                 old_material_name,
            const char*                 new_material_name)
        {
            for (each<ObjectInstanceContainer> i = object_instances; i; ++i)
            {
                update_material_mappings(i->get_front_material_mappings(), old_material_name, new_material_name);
                update_material_mappings(i->get_back_material_mappings(), old_material_name, new_material_name);
            }
        }

        static void update_material_mappings(
            StringDictionary&           mappings,
            const char*                 old_material_name,
            const char*                 new_material_name)
        {
            for (const_each<StringDictionary> i = mappings; i; ++i)
            {
                if (strcmp(i->value(), old_material_name) == 0)
                    mappings.set(i->key(), new_material_name);
            }
        }

        static void update_bsdf(const MaterialInfo& info)
        {
            ParamArray& bsdf_params = info.m_bsdf->get_parameters();

            bsdf_params.strings().remove("from_ior");
            bsdf_params.strings().remove("to_ior");

            const float ior =
                feq(info.m_bsdf_from_ior, 1.0f) ? info.m_bsdf_to_ior : info.m_bsdf_from_ior;
            bsdf_params.strings().insert("ior", ior);

            cleanup_entity_name(*info.m_bsdf);
            info.m_material->get_parameters().insert("bsdf", info.m_bsdf->get_name());
        }
    };

    //
    // Update from revision 10 to revision 11.
    //

    class UpdateFromRevision_10
      : public Updater
    {
      public:
        explicit UpdateFromRevision_10(Project& project)
          : Updater(project, 10)
        {
        }

        virtual void update() APPLESEED_OVERRIDE
        {
            if (Scene* scene = m_project.get_scene())
                update_bssrdf_ior_inputs(scene->assemblies());
        }

      private:
        static void update_bssrdf_ior_inputs(AssemblyContainer& assemblies)
        {
            for (each<AssemblyContainer> i = assemblies; i; ++i)
            {
                update_bssrdf_ior_inputs(*i);
                update_bssrdf_ior_inputs(i->assemblies());
            }
        }

        static void update_bssrdf_ior_inputs(Assembly& assembly)
        {
            for (each<BSSRDFContainer> i = assembly.bssrdfs(); i; ++i)
                update_bssrdf_ior_inputs(*i);
        }

        static void update_bssrdf_ior_inputs(BSSRDF& bssrdf)
        {
            move_if_exist(bssrdf, "ior", "inside_ior");
            bssrdf.get_parameters().remove_path("outside_ior");
        }
    };

    //
    // Update from revision 11 to revision 12.
    //

    class UpdateFromRevision_11
      : public Updater
    {
      public:
        explicit UpdateFromRevision_11(Project& project)
          : Updater(project, 11)
        {
        }

        virtual void update() APPLESEED_OVERRIDE
        {
            if (Scene* scene = m_project.get_scene())
                update_bssrdf_mfp_inputs(scene->assemblies());
        }

      private:
        static void update_bssrdf_mfp_inputs(AssemblyContainer& assemblies)
        {
            for (each<AssemblyContainer> i = assemblies; i; ++i)
            {
                update_bssrdf_mfp_inputs(*i);
                update_bssrdf_mfp_inputs(i->assemblies());
            }
        }

        static void update_bssrdf_mfp_inputs(Assembly& assembly)
        {
            for (each<BSSRDFContainer> i = assembly.bssrdfs(); i; ++i)
                update_bssrdf_mfp_inputs(*i);
        }

        static void update_bssrdf_mfp_inputs(BSSRDF& bssrdf)
        {
            move_if_exist(bssrdf, "mfp", "dmfp");
            move_if_exist(bssrdf, "mfp_multiplier", "dmfp_multiplier");
        }
    };

    //
    // Update from revision 12 to revision 13.
    //

    class UpdateFromRevision_12
      : public Updater
    {
      public:
        explicit UpdateFromRevision_12(Project& project)
          : Updater(project, 12)
        {
        }

        virtual void update() APPLESEED_OVERRIDE
        {
            Frame* frame = m_project.get_frame();
            const Scene* scene = m_project.get_scene();

            if (frame == 0 || scene == 0 || scene->cameras().empty())
                return;

            ParamArray& frame_params = frame->get_parameters();

            if (!frame_params.strings().exist("camera"))
            {
                // The frame does not reference any camera: use the first camera.
                frame_params.insert(
                    "camera",
                    scene->cameras().get_by_index(0)->get_name());
            }
            else
            {
                const char* camera_name = frame_params.strings().get("camera");
                if (scene->cameras().get_by_name(camera_name) == 0)
                {
                    // The frame references a non-existing camera: use the first camera.
                    frame_params.insert(
                        "camera",
                        scene->cameras().get_by_index(0)->get_name());
                }
            }
        }
    };

    //
    // Update from revision 13 to revision 14.
    //

    class UpdateFromRevision_13
      : public Updater
    {
      public:
        explicit UpdateFromRevision_13(Project& project)
          : Updater(project, 13)
        {
        }

        virtual void update() APPLESEED_OVERRIDE
        {
            if (Scene* scene = m_project.get_scene())
            {
                update_bsdfs_inputs(scene->assemblies());
                update_gaussian_bssrdfs(scene->assemblies());
            }
        }

      private:
        static void update_bsdfs_inputs(AssemblyContainer& assemblies)
        {
            for (each<AssemblyContainer> i = assemblies; i; ++i)
            {
                update_bsdfs_inputs(*i);
                update_bsdfs_inputs(i->assemblies());
            }
        }

        static void update_bsdfs_inputs(Assembly& assembly)
        {
            for (each<BSDFContainer> i = assembly.bsdfs(); i; ++i)
                update_bsdf_inputs(*i);
        }

        static void update_bsdf_inputs(BSDF& bsdf)
        {
            if (strcmp(bsdf.get_model(), GlassBSDFFactory().get_model()) == 0)
            {
                bsdf.get_parameters().insert("volume_parameterization", "transmittance");
                move_if_exist(bsdf, "anisotropy", "anisotropic");
            }
            else if (strcmp(bsdf.get_model(), GlossyBRDFFactory().get_model()) == 0 ||
                     strcmp(bsdf.get_model(), MetalBRDFFactory().get_model()) == 0)
            {
                move_if_exist(bsdf, "anisotropy", "anisotropic");
            }
            else if (strcmp(bsdf.get_model(), SpecularBTDFFactory().get_model()) == 0)
            {
                move_if_exist(bsdf, "volume_density", "density");
                move_if_exist(bsdf, "volume_scale", "scale");
            }
        }

        static void update_gaussian_bssrdfs(AssemblyContainer& assemblies)
        {
            for (each<AssemblyContainer> i = assemblies; i; ++i)
            {
                update_gaussian_bssrdfs(*i);
                update_gaussian_bssrdfs(i->assemblies());
            }
        }

        static void update_gaussian_bssrdfs(Assembly& assembly)
        {
            for (each<BSSRDFContainer> i = assembly.bssrdfs(); i; ++i)
            {
                if (strcmp(i->get_model(), GaussianBSSRDFFactory().get_model()) == 0)
                    update_gaussian_bssrdf(*i);
            }
        }

        static void update_gaussian_bssrdf(BSSRDF& bssrdf)
        {
            ParamArray& params = bssrdf.get_parameters();

            try
            {
                const float v = params.get<float>("v");
                const float mfp = sqrt(v * 16.0f) / 7.0f;
                params.insert("mfp", mfp);
                params.remove_path("v");
            }
            catch (const Exception&)
            {
                RENDERER_LOG_ERROR(
                    "while updating gaussianbssrdf \"%s\", failed to convert v parameter.",
                    bssrdf.get_name());
            }
        }
    };

    //
    // Update from revision 14 to revision 15.
    //

    class UpdateFromRevision_14
      : public Updater
    {
      public:
        explicit UpdateFromRevision_14(Project& project)
          : Updater(project, 14)
        {
        }

        virtual void update() APPLESEED_OVERRIDE
        {
            if (Scene* scene = m_project.get_scene())
                update_entities(scene->assemblies());
        }

      private:
        static void update_entities(AssemblyContainer& assemblies)
        {
            for (each<AssemblyContainer> i = assemblies; i; ++i)
            {
                update_entities(*i);
                update_entities(i->assemblies());
            }
        }

        static void update_entities(Assembly& assembly)
        {
            for (each<BSDFContainer> i = assembly.bsdfs(); i; ++i)
                update_bsdf_inputs(*i);

            for (each<MaterialContainer> i = assembly.materials(); i; ++i)
                update_material_inputs(*i);
        }

        static void update_bsdf_inputs(BSDF& bsdf)
        {
            if (strcmp(bsdf.get_model(), DisneyBRDFFactory().get_model()) == 0)
            {
                ParamArray& params = bsdf.get_parameters();
                if (!params.strings().exist("specular"))
                    params.insert("specular", 0.5f);
                if (!params.strings().exist("roughness"))
                    params.insert("roughness", 0.5f);
                if (!params.strings().exist("sheen_tint"))
                    params.insert("sheen_tint", 0.5f);
            }
        }

        static void update_material_inputs(Material& material)
        {
            if (strcmp(material.get_model(), DisneyMaterialFactory().get_model()) == 0)
            {
                ParamArray& params = material.get_parameters();
                for (each<DictionaryDictionary> i = params.dictionaries(); i; ++i)
                {
                    Dictionary& layer_params = i->value();
                    if (!layer_params.strings().exist("roughness"))
                        layer_params.insert("roughness", 0.5f);
                }
            }
        }
    };

    //
    // Update from revision 15 to revision 16.
    //

    class UpdateFromRevision_15
      : public Updater
    {
      public:
        explicit UpdateFromRevision_15(Project& project)
          : Updater(project, 15)
        {
        }

        virtual void update() APPLESEED_OVERRIDE
        {
            if (Scene* scene = m_project.get_scene())
                update_physical_surface_shader_inputs(scene->assemblies());
        }

      private:
        static void update_physical_surface_shader_inputs(AssemblyContainer& assemblies)
        {
            for (each<AssemblyContainer> i = assemblies; i; ++i)
            {
                update_physical_surface_shader_inputs(*i);
                update_physical_surface_shader_inputs(i->assemblies());
            }
        }

        static void update_physical_surface_shader_inputs(Assembly& assembly)
        {
            for (each<SurfaceShaderContainer> i = assembly.surface_shaders(); i; ++i)
                update_physical_surface_shader_inputs(*i);
        }

        static void update_physical_surface_shader_inputs(SurfaceShader& surface_shader)
        {
            if (strcmp(surface_shader.get_model(), PhysicalSurfaceShaderFactory().get_model()) == 0)
            {
                move_if_exist(surface_shader, "lighting_samples", "front_lighting_samples");

                ParamArray& params = surface_shader.get_parameters();
                params.strings().remove("translucency");
                params.strings().remove("back_lighting_samples");
                params.strings().remove("aerial_persp_sky_color");
                params.strings().remove("aerial_persp_mode");
                params.strings().remove("aerial_persp_distance");
                params.strings().remove("aerial_persp_intensity");
            }
        }
    };
}

bool ProjectFileUpdater::update(
    Project&        project,
    const size_t    to_revision)
{
    EventCounters event_counters;
    update(project, event_counters, to_revision);
    return event_counters.get_error_count() == 0;
}

void ProjectFileUpdater::update(
    Project&        project,
    EventCounters&  event_counters,
    const size_t    to_revision)
{
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
      CASE_UPDATE_FROM_REVISION(9);
      CASE_UPDATE_FROM_REVISION(10);
      CASE_UPDATE_FROM_REVISION(11);
      CASE_UPDATE_FROM_REVISION(12);
      CASE_UPDATE_FROM_REVISION(13);
      CASE_UPDATE_FROM_REVISION(14);
      CASE_UPDATE_FROM_REVISION(15);

      case ProjectFormatRevision:
        // Project is up-to-date.
        break;

      default:
        if (format_revision > ProjectFormatRevision)
        {
            RENDERER_LOG_ERROR(
                "cannot update project in format revision " FMT_SIZE_T ", latest supported revision is " FMT_SIZE_T ".",
                format_revision,
                ProjectFormatRevision);
            event_counters.signal_error();
        }
        else
        {
            RENDERER_LOG_ERROR(
                "cannot update project format from revision " FMT_SIZE_T " to revision " FMT_SIZE_T ", one or more update steps are missing.",
                format_revision,
                ProjectFormatRevision);
            event_counters.signal_error();
        }
        break;
    }

#undef CASE_UPDATE_FROM_REVISION
}

}   // namespace renderer
