
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Esteban Tovagliari, The appleseedhq Organization
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
#include "appleseedprojectfilewriter.h"

// appleseed.renderer headers.
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/display/display.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/curveobject.h"
#include "renderer/modeling/object/curveobjectwriter.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/meshobjectwriter.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/postprocessingstage/postprocessingstage.h"
#include "renderer/modeling/project/assethandler.h"
#include "renderer/modeling/project/configuration.h"
#include "renderer/modeling/project/configurationcontainer.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/project/projectfilewriter.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/proceduralassembly.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/shadergroup/shader.h"
#include "renderer/modeling/shadergroup/shaderconnection.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/modeling/shadergroup/shaderparam.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/modeling/volume/volume.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/array/applyvisitor.h"
#include "foundation/array/array.h"
#include "foundation/array/arrayref.h"
#include "foundation/array/keyframedarray.h"
#include "foundation/containers/dictionary.h"
#include "foundation/core/appleseed.h"
#include "foundation/math/transform.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/string/string.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/indenter.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/z85.h"

// Boost headers.
#include "boost/filesystem.hpp"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <exception>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <type_traits>
#include <vector>

using namespace boost;
using namespace foundation;
namespace bf = boost::filesystem;

namespace renderer
{

//
// AppleseedProjectFileWriter class implementation.
//

namespace
{
    // Floating-point formatting settings.
    const char* MatrixFormat     = "%.17f";
    const char* FloatFormat      = "%.17f";
    const char* ColorValueFormat = "%.9f";

    template <typename Vec>
    void write_vector(
        FILE*                   file,
        const Indenter&         indenter,
        const Vec&              v,
        const size_t            size,
        const size_t            columns,
        const char*             fmt)
    {
        assert(columns > 0);

        for (size_t i = 0; i < size; ++i)
        {
            const size_t col = i % columns;

            if (col == 0)
                fputs(indenter.c_str(), file);
            else fputc(' ', file);

            fprintf(file, fmt, v[i]);

            if (col == columns - 1 || i == size - 1)
                fputc('\n', file);
        }
    }

    class ScopedBlock
    {
      public:
        ScopedBlock(FILE* file, Indenter& indenter)
          :  m_file(file)
          ,  m_indenter(indenter)
        {
            fprintf(m_file, "%s{\n", m_indenter.c_str());
            ++m_indenter;
        }

        ~ScopedBlock()
        {
            --m_indenter;
            fprintf(m_file, "%s}\n", m_indenter.c_str());
        }

      private:
        FILE*       m_file;
        Indenter&   m_indenter;
    };

    class WriteArrayVisitor
    {
      public:
        template< bool B, class T = void >
        using EnableIf = typename std::enable_if<B,T>::type;
    };

    class WriteAsciiArrayVisitor : public WriteArrayVisitor
    {
      public:
        WriteAsciiArrayVisitor(FILE* file, const Indenter& indenter)
          :  m_file(file)
          ,  m_indenter(indenter)
        {
        }

        template <typename T, EnableIf<std::is_integral<T>::value, int> = 0>
        void operator()(const ArrayView<T>& view)
        {
            write_integer_array(
                reinterpret_cast<const T*>(view.begin()),
                view.size());
        }

        void operator()(const ArrayView<CompressedUnitVector>& view)
        {
            write_integer_array(
                reinterpret_cast<const CompressedUnitVector::ValueType*>(view.begin()),
                view.size() * 2);
        }

        void operator()(const ArrayView<float>& view)
        {
            write_float_array(
                reinterpret_cast<const float*>(view.begin()),
                view.size());
        }

        template <size_t N>
        void operator()(const ArrayView<Vector<float, N>>& view)
        {
            write_float_array(
                reinterpret_cast<const float*>(view.begin()),
                view.size() * view.item_size() / sizeof(float));
        }

        void operator()(const ArrayView<Color3f>& view)
        {
            write_float_array(
                reinterpret_cast<const float*>(view.begin()),
                view.size() * view.item_size() / sizeof(float));
        }

      private:
        FILE*           m_file;
        const Indenter& m_indenter;

        void write_float_array(const float* p, const size_t size)
        {
            write_vector(m_file, m_indenter, p, size, 8, FloatFormat);
        }

        template <typename T>
        void write_integer_array(const T* p, const size_t size)
        {
            write_vector(m_file, m_indenter, p, size, 8, "%d");
        }
    };

    class WriteZ85EncodedArrayVisitor : public WriteArrayVisitor
    {
      public:
        WriteZ85EncodedArrayVisitor(FILE* file, const Indenter& indenter)
          :  m_file(file)
          ,  m_indenter(indenter)
        {
        }

        template <typename T>
        void operator()(const ArrayView<T>& view)
        {
            write_z85_encoded_array(
                reinterpret_cast<const unsigned char*>(view.begin()),
                view.size() * view.item_size());
        }

      private:
        FILE*           m_file;
        const Indenter& m_indenter;

        void write_z85_encoded_array(
            const unsigned char*    ptr,
            const std::size_t       size_in_bytes)
        {
            assert(size_in_bytes % 4 == 0);
            std::vector<char> buffer(z85_encoded_size(size_in_bytes) + 1);
            z85_encode(ptr, size_in_bytes, buffer.data());
            buffer[buffer.size() - 1] = 0;
            fprintf(m_file, "%s%s\n", m_indenter.c_str(), buffer.data());
        }
    };

    class Writer
    {
      public:
        // Constructor.
        Writer(
            const char*         filepath,
            FILE*               file,
            const int           options)
          : m_project_new_root_dir(filesystem::path(filepath).parent_path())
          , m_file(file)
          , m_options(options)
          , m_indenter(4)
        {
        }

        void write_project(const Project& project)
        {
            fprintf(m_file, "project %s\n", project.get_name());
            const auto block = begin_block();

            fprintf(
                m_file,
                "%sformat_revision " FMT_SIZE_T "\n\n",
                m_indenter.c_str(),
                project.get_format_revision());

            write_search_paths(project);

            if (project.get_display())
                write(*project.get_display());

            if (project.get_scene())
                write_scene(*project.get_scene());

            if (project.get_frame())
                write_frame(*project.get_frame());

            write_configurations(project);
        }

      private:
        const filesystem::path  m_project_new_root_dir;
        FILE*                   m_file;
        const int               m_options;
        Indenter                m_indenter;

        // Return a lexicographically-sorted vector of references to entities.
        template <typename Collection>
        std::vector<std::reference_wrapper<const typename Collection::value_type>> sorted(const Collection& collection)
        {
            return sorted_impl<const typename Collection::value_type>(collection.begin(), collection.end());
        }

        template <typename Collection>
        std::vector<std::reference_wrapper<typename Collection::value_type>> sorted(Collection& collection)
        {
            return sorted_impl<typename Collection::value_type>(collection.begin(), collection.end());
        }

        template <typename ValueType, typename Iterator>
        std::vector<std::reference_wrapper<ValueType>> sorted_impl(Iterator input_begin, Iterator input_end)
        {
            std::vector<std::reference_wrapper<ValueType>> result;
            result.reserve(std::distance(input_begin, input_end));

            for (Iterator it = input_begin; it != input_end; ++it)
                result.emplace_back(*it);

            sort(result.begin(), result.end(), [](const Entity& lhs, const Entity& rhs)
            {
                return strcmp(lhs.get_name(), rhs.get_name()) < 0;
            });

            return result;
        }

        ScopedBlock begin_block()
        {
            return ScopedBlock(m_file, m_indenter);
        }

        void do_write_array(const Array& array)
        {
            if (m_options & ProjectFileWriter::AsciiArrays)
                apply_visitor(array, WriteAsciiArrayVisitor(m_file, m_indenter));
            else
                apply_visitor(array, WriteZ85EncodedArrayVisitor(m_file, m_indenter));
        }

        void write_array(const char* name, const Array& array)
        {
            const char* encoding =
                m_options & ProjectFileWriter::AsciiArrays
                ? "ascii"
                : "z85";

            fprintf(
                m_file,
                "%sarray %s %s 1 " FMT_SIZE_T " %s\n",
                m_indenter.c_str(),
                name,
                array_type_to_string(array.type()),
                array.size(),
                encoding);
            const auto block = begin_block();
            do_write_array(array);
        }

        void write_keyframed_array(const char* name, const KeyFramedArray& array)
        {
            const char* encoding =
                m_options & ProjectFileWriter::AsciiArrays
                ? "ascii"
                : "z85";

            fprintf(
                m_file,
                "%sarray %s %s " FMT_SIZE_T " " FMT_SIZE_T " %s\n",
                m_indenter.c_str(),
                name,
                array_type_to_string(array.type()),
                array.get_key_count(),
                array.get_key(0).size(),
                encoding);
            const auto block = begin_block();

            for (size_t i = 0, e = array.get_key_count(); i < e; ++i)
                do_write_array(array.get_key(i));
        }

        void write_dictionary(const Dictionary& dictionary)
        {
            for (const auto& item : dictionary.strings())
            {
                fprintf(
                    m_file,
                    "%s%s %s\n",
                    m_indenter.c_str(),
                    item.key(),
                    item.value());
            }

            for (const auto& item : dictionary.dictionaries())
            {
                fprintf(m_file, "%s%s\n", m_indenter.c_str(), item.key());
                const auto block = begin_block();
                write_dictionary(item.value());
            }
        }

        void write_params(const Dictionary& params)
        {
            if (!params.empty())
            {
                fprintf(m_file, "%sparams\n", m_indenter.c_str());
                const auto block = begin_block();

                for (const auto& item : params.strings())
                {
                    fprintf(
                        m_file,
                        "%s%s %s\n",
                        m_indenter.c_str(),
                        item.key(),
                        item.value());
                }

                for (const auto& item : params.dictionaries())
                {
                    fprintf(m_file, "%s%s\n", m_indenter.c_str(), item.key());
                    const auto block = begin_block();
                    write_dictionary(item.value());
                }
            }
        }

        void write_matrix(const Matrix4d& matrix, const float* time = nullptr)
        {
            if (time)
                fprintf(m_file, "%smatrix %f\n", m_indenter.c_str(), *time);
            else
                fprintf(m_file, "%smatrix\n", m_indenter.c_str());

            const auto block = begin_block();
            write_vector(
                m_file,
                m_indenter,
                matrix,
                16,
                4,
                MatrixFormat);
        }

        template <typename T>
        void write_transform(const Transform<T>& transform)
        {
            if (transform.get_local_to_parent() == Matrix<T, 4, 4>::identity())
                return;

            fprintf(m_file, "%stransform\n", m_indenter.c_str());
            const auto block = begin_block();
            write_matrix(transform.get_local_to_parent(), nullptr);
        }

        void write_transform_sequence(const TransformSequence& transform_sequence)
        {
            if (transform_sequence.empty())
                return;

            if (transform_sequence.size() == 1)
            {
                float time;
                Transformd transform;
                transform_sequence.get_transform(0, time, transform);

                if (transform.get_local_to_parent() == Matrix4d::identity())
                    return;
            }

            fprintf(m_file, "%stransform_sequence\n", m_indenter.c_str());
            const auto block = begin_block();

            for (size_t i = 0, e = transform_sequence.size(); i < e; ++i)
            {
                float time;
                Transformd transform;
                transform_sequence.get_transform(i, time, transform);
                write_matrix(transform.get_local_to_parent(), &time);
            }
        }

        void write_value_array(const char* element_name, const ColorValueArray& values)
        {
            fprintf(m_file, "%s%s\n", m_indenter.c_str(), element_name);
            const auto block = begin_block();
            write_vector(
                m_file,
                m_indenter,
                values,
                values.size(),
                8,
                ColorValueFormat);
        }

        template <typename Entity>
        void write_entity(const char* element_name, const Entity& entity)
        {
            fprintf(
                m_file,
                "%s%s %s %s\n",
                m_indenter.c_str(),
                element_name,
                entity.get_model(),
                entity.get_name());

            const auto block = begin_block();
            write_params(entity.get_parameters());
        }

        template <typename Collection>
        void write_collection(const Collection& collection)
        {
            if (!collection.empty())
            {
                for (const auto& entity : sorted(collection))
                    write(entity);
            }
        }

        void write(const AOV& aov)
        {
            fprintf(m_file, "%saov %s\n", m_indenter.c_str(), aov.get_name());
            const auto block = begin_block();
            write_params(aov.get_parameters());
        }

        void write(const Assembly& assembly)
        {
            fprintf(
                m_file,
                "%sassembly %s %s\n",
                m_indenter.c_str(),
                assembly.get_model(),
                assembly.get_name());

            const auto block = begin_block();
            write_params(assembly.get_parameters());

            // Don't write the content of the assembly if it was
            // generated procedurally.
            if (dynamic_cast<const ProceduralAssembly*>(&assembly) == nullptr)
            {
                write_collection(assembly.colors());
                write_collection(assembly.textures());
                write_collection(assembly.texture_instances());
                write_collection(assembly.bsdfs());
                write_collection(assembly.bssrdfs());
                write_collection(assembly.edfs());
                write_collection(assembly.shader_groups());
                write_collection(assembly.surface_shaders());
                write_collection(assembly.materials());
                write_collection(assembly.lights());
                write_collection(assembly.objects());
                write_collection(assembly.object_instances());
                write_collection(assembly.volumes());
                write_collection(assembly.assemblies());
                write_collection(assembly.assembly_instances());
            }
        }

        void write(const AssemblyInstance& assembly_instance)
        {
            fprintf(
                m_file,
                "%sassembly_instance %s\n",
                m_indenter.c_str(),
                assembly_instance.get_name());

            const auto block = begin_block();
            fprintf(
                m_file,
                "%sassembly %s\n",
                m_indenter.c_str(),
                assembly_instance.get_assembly_name());

            write_params(assembly_instance.get_parameters());
            write_transform_sequence(assembly_instance.transform_sequence());
        }

        void write_assign_material(
            const std::string&               slot,
            const std::string&               side,
            const std::string&               name)
        {
            fprintf(
                m_file,
                "%sassign_material %s %s %s\n",
                m_indenter.c_str(),
                slot.c_str(),
                side.c_str(),
                name.c_str());
        }

        void write_assign_materials(
            const ObjectInstance::Side  side,
            const StringDictionary&     material_mappings)
        {
            const std::string side_string = side == ObjectInstance::FrontSide ? "front" : "back";

            for (const_each<StringDictionary> i = material_mappings; i; ++i)
                write_assign_material(i->key(), side_string, i->value<std::string>());
        }

        void write(const BSDF& bsdf)
        {
            write_entity("bsdf", bsdf);
        }

        void write(const BSSRDF& bssrdf)
        {
            write_entity("bssrdf", bssrdf);
        }

        void write(const Camera& camera)
        {
            fprintf(
                m_file,
                "%scamera %s %s\n",
                m_indenter.c_str(),
                camera.get_model(),
                camera.get_name());

            const auto block = begin_block();
            write_params(camera.get_parameters());

            write_transform_sequence(camera.transform_sequence());
        }

        void write(const ColorEntity& color_entity)
        {
            fprintf(
                m_file,
                "%scolor %s\n",
                m_indenter.c_str(),
                color_entity.get_name());

            const auto block = begin_block();
            write_params(color_entity.get_parameters());
            write_value_array("values", color_entity.get_values());
            write_value_array("alpha", color_entity.get_alpha());
        }

        void write_configuration(const Configuration& configuration)
        {
            fprintf(
                m_file,
                "%sconfiguration %s\n",
                m_indenter.c_str(),
                configuration.get_name());

            const auto block = begin_block();

            if (configuration.get_base())
            {
                fprintf(
                    m_file,
                    "%sbase %s\n",
                    m_indenter.c_str(),
                    configuration.get_base()->get_name());
            }

            write_params(configuration.get_parameters());
        }

        void write_configurations(const Project& project)
        {
            fprintf(m_file, "%sconfigurations\n", m_indenter.c_str());
            const auto block = begin_block();

            // Write configurations.
            for (const Configuration& configuration : sorted(project.configurations()))
            {
                if (!BaseConfigurationFactory::is_base_configuration(configuration.get_name()))
                    write_configuration(configuration);
            }
        }

        void write(const Display& display)
        {
            fprintf(m_file, "%sdisplay %s\n", m_indenter.c_str(), display.get_name());
            const auto block = begin_block();
            write_params(display.get_parameters());
        }

        void write(const EDF& edf)
        {
            write_entity("edf", edf);
        }

        void write(const Environment& environment)
        {
            write_entity("environment", environment);
        }

        void write(const EnvironmentEDF& env_edf)
        {
            fprintf(
                m_file,
                "%senvironment_edf %s %s\n",
                m_indenter.c_str(),
                env_edf.get_model(),
                env_edf.get_name());

            const auto block = begin_block();
            write_params(env_edf.get_parameters());
            write_transform_sequence(env_edf.transform_sequence());
        }

        void write(const EnvironmentShader& env_shader)
        {
            write_entity("environment_shader", env_shader);
        }

        void write_frame(const Frame& frame)
        {
            fprintf(m_file, "%sframe %s\n", m_indenter.c_str(), frame.get_name());
            const auto block = begin_block();
            write_params(frame.get_parameters());
            write_collection(frame.aovs());
            write_collection(frame.post_processing_stages());
        }

        void write(const Light& light)
        {
            fprintf(
                m_file,
                "%slight %s %s\n",
                m_indenter.c_str(),
                light.get_model(),
                light.get_name());

            const auto block = begin_block();
            write_params(light.get_parameters());
            write_transform(light.get_transform());
        }

        void write(const Material& material)
        {
            write_entity("material", material);
        }

        void write(const Volume& material)
        {
            write_entity("volume", material);
        }

        void write_mesh_geometry(const MeshObject& object)
        {
            const auto num_keys = object.get_motion_segment_count() + 1;

            // Write points.
            {
                KeyFramedArray array(
                    ArrayType::Vector3fType,
                    object.get_vertex_count(),
                    num_keys);

                ArrayRef<Vector3f> points(array.get_key(0));
                for (size_t i = 0, ie = object.get_vertex_count(); i < ie; ++i)
                    points[i] = object.get_vertex(i);

                for (size_t k = 1; k < num_keys; ++k)
                {
                    ArrayRef<Vector3f> points(array.get_key(k));
                    for (size_t i = 0, ie = object.get_vertex_count(); i < ie; ++i)
                        points[i] = object.get_vertex_pose(i, k - 1);
                }

                write_keyframed_array("P", array);
            }

            // Write UVs
            if (object.get_tex_coords_count() != 0)
            {
                Array array(ArrayType::Vector2fType, object.get_tex_coords_count());
                ArrayRef<Vector2f> uvs(array);

                for (size_t i = 0, e = object.get_tex_coords_count(); i < e; ++i)
                    uvs[i] = object.get_tex_coords(i);

                write_array("uv", array);
            }

            // Write normals.
            if (object.get_vertex_normal_count() != 0)
            {
                KeyFramedArray array(
                    ArrayType::Vector3fType,
                    object.get_vertex_normal_count(),
                    num_keys);

                ArrayRef<Vector3f> N(array.get_key(0));
                for (size_t i = 0, ie = object.get_vertex_normal_count(); i < ie; ++i)
                    N[i] = object.get_vertex_normal(i);

                for (size_t k = 1; k < num_keys; ++k)
                {
                    ArrayRef<Vector3f> N(array.get_key(k));
                    for (size_t i = 0, ie = object.get_vertex_normal_count(); i < ie; ++i)
                        N[i] = object.get_vertex_normal_pose(i, k - 1);
                }

                write_keyframed_array("N", array);
            }

            // Write tangents.
            if (object.get_vertex_tangent_count() != 0)
            {
                KeyFramedArray array(
                    ArrayType::Vector3fType,
                    object.get_vertex_tangent_count(),
                    num_keys);

                ArrayRef<Vector3f> T(array.get_key(0));
                for (size_t i = 0, ie = object.get_vertex_tangent_count(); i < ie; ++i)
                    T[i] = object.get_vertex_tangent(i);

                for (size_t k = 1; k < num_keys; ++k)
                {
                    ArrayRef<Vector3f> N(array.get_key(k));
                    for (size_t i = 0, ie = object.get_vertex_tangent_count(); i < ie; ++i)
                        T[i] = object.get_vertex_tangent_pose(i, k - 1);
                }

                write_keyframed_array("T", array);
            }

            // Write indices.
            const size_t num_triangles = object.get_triangle_count();

            // Vertices per face.
            {
                Array array(ArrayType::UInt32Type);
                ArrayRef<std::uint32_t> nverts(array);
                nverts.fill(num_triangles, 3);
                write_array("nverts", array);
            }

            // Vertex indices.
            {
                Array array(ArrayType::UInt32Type, num_triangles * 3);
                ArrayRef<std::uint32_t> vindx(array);

                for (size_t i = 0; i < num_triangles; ++i)
                {
                    const auto& triangle = object.get_triangle(i);
                    vindx[3 * i + 0] = triangle.m_v0;
                    vindx[3 * i + 1] = triangle.m_v1;
                    vindx[3 * i + 2] = triangle.m_v2;
                }

                write_array("vindx", array);
            }

            // UV indices.
            if (object.get_tex_coords_count() != 0)
            {
                Array array(ArrayType::UInt32Type, num_triangles * 3);
                ArrayRef<std::uint32_t> uvindx(array);

                for (size_t i = 0; i < num_triangles; ++i)
                {
                    const auto& triangle = object.get_triangle(i);
                    uvindx[3 * i + 0] = triangle.m_a0;
                    uvindx[3 * i + 1] = triangle.m_a1;
                    uvindx[3 * i + 2] = triangle.m_a2;
                }

                write_array("uvindx", array);
            }

            // Normal indices.
            if (object.get_vertex_normal_count() != 0)
            {
                Array array(ArrayType::UInt32Type, num_triangles * 3);
                ArrayRef<std::uint32_t> nindx(array);

                for (size_t i = 0; i < num_triangles; ++i)
                {
                    const auto& triangle = object.get_triangle(i);
                    nindx[3 * i + 0] = triangle.m_n0;
                    nindx[3 * i + 1] = triangle.m_n1;
                    nindx[3 * i + 2] = triangle.m_n2;
                }

                write_array("nindx", array);
            }

            // Tangent indices.
            if (object.get_vertex_tangent_count() != 0)
            {
                Array array(ArrayType::UInt32Type, num_triangles * 3);
                ArrayRef<std::uint32_t> tindx(array);

                for (size_t i = 0; i < num_triangles; ++i)
                {
                    const auto& triangle = object.get_triangle(i);
                    tindx[3 * i + 0] = triangle.m_v0;
                    tindx[3 * i + 1] = triangle.m_v1;
                    tindx[3 * i + 2] = triangle.m_v2;
                }

                write_array("tindx", array);
            }

            // Write material indices.
            if (object.get_material_slot_count() > 1)
            {
                Array array(ArrayType::UInt32Type, num_triangles);
                ArrayRef<std::uint32_t> mindx(array);

                for (size_t i = 0; i < num_triangles; ++i)
                    mindx[i] = object.get_triangle(i).m_pa;

                write_array("mindx", array);
            }
        }

        void write_curve_geometry(const CurveObject& /*object*/)
        {
            // TODO: implement me...
        }

        void write(const Object& object)
        {
            fprintf(
                m_file,
                "%sobject %s %s\n",
                m_indenter.c_str(),
                object.get_model(),
                object.get_name());

            const auto block = begin_block();
            write_params(object.get_parameters());

            if (strcmp(object.get_model(), MeshObjectFactory().get_model()) == 0)
                write_mesh_geometry(static_cast<const MeshObject&>(object));
            else if (strcmp(object.get_model(), CurveObjectFactory().get_model()) == 0)
                write_curve_geometry(static_cast<const CurveObject&>(object));

            // Write material slots.
            for (size_t i = 0, e = object.get_material_slot_count(); i < e; ++i)
            {
                fprintf(
                    m_file,
                    "%smaterial_slot %s\n",
                    m_indenter.c_str(),
                    object.get_material_slot(i));
            }
        }

        void write(const ObjectInstance& object_instance)
        {
            fprintf(
                m_file,
                "%sobject_instance %s\n",
                m_indenter.c_str(),
                object_instance.get_name());

            const auto block = begin_block();
            fprintf(
                m_file,
                "%sobject %s\n",
                m_indenter.c_str(),
                object_instance.get_object_name());

            write_params(object_instance.get_parameters());
            write_transform(object_instance.get_transform());

            write_assign_materials(ObjectInstance::FrontSide, object_instance.get_front_material_mappings());
            write_assign_materials(ObjectInstance::BackSide, object_instance.get_back_material_mappings());
        }

        void write(const PostProcessingStage& stage)
        {
            write_entity("post_processing_stage", stage);
        }

        void write_scene(const Scene& scene)
        {
            fprintf(m_file, "%sscene\n", m_indenter.c_str());
            const auto block = begin_block();

            write_params(scene.get_parameters());

            write_collection(scene.cameras());
            write_collection(scene.colors());
            write_collection(scene.textures());
            write_collection(scene.texture_instances());
            write_collection(scene.environment_edfs());
            write_collection(scene.environment_shaders());

            if (scene.get_environment())
                write(*scene.get_environment());

            write_collection(scene.shader_groups());
            write_collection(scene.assemblies());
            write_collection(scene.assembly_instances());
        }

        void write_search_paths(const Project& project)
        {
            const SearchPaths& search_paths = project.search_paths();

            if (search_paths.get_explicit_path_count() > 0)
            {
                fprintf(m_file, "%ssearch_paths\n", m_indenter.c_str());
                const auto block = begin_block();

                for (size_t i = 0; i < search_paths.get_explicit_path_count(); ++i)
                {
                    fprintf(
                        m_file,
                        "%s%s\n",
                        m_indenter.c_str(),
                        search_paths.get_explicit_path(i));
                }
            }
        }

        void write(const ShaderParam& param)
        {
            fprintf(
                m_file,
                "%s%s %s\n",
                m_indenter.c_str(),
                param.get_name(),
                param.get_value_as_string().c_str());
        }

        void write_osl_code(const char* /*code*/)
        {
            fprintf(m_file, "%scode\n", m_indenter.c_str());
            const auto block = begin_block();
            // TODO: implement me...
        }

        void write(const Shader& shader)
        {
            fprintf(
                m_file,
                "%sshader %s %s %s\n",
                m_indenter.c_str(),
                shader.get_type(),
                shader.get_name(),
                shader.get_layer());

            const auto block = begin_block();
            write_collection(shader.shader_params());

            if (shader.get_source_code())
                write_osl_code(shader.get_source_code());
        }

        void write(const ShaderConnection& connection)
        {
            fprintf(
                m_file,
                "%s%s %s -> %s %s\n",
                m_indenter.c_str(),
                connection.get_src_layer(),
                connection.get_src_param(),
                connection.get_dst_layer(),
                connection.get_dst_param());
        }

        void write(const ShaderGroup& shader_group)
        {
            fprintf(
                m_file,
                "%sshader_group %s\n",
                m_indenter.c_str(),
                shader_group.get_name());

            const auto block = begin_block();

            // Write shaders in the original order.
            for (const Shader& shader : shader_group.shaders())
                write(shader);

            // Write shader connections in the original order.
            if (!shader_group.shader_connections().empty())
            {
                fprintf(
                    m_file,
                    "%sconnections\n",
                    m_indenter.c_str());

                const auto block = begin_block();

                for (const ShaderConnection& shader_connection : shader_group.shader_connections())
                    write(shader_connection);
            }
        }

        void write(const SurfaceShader& surface_shader)
        {
            write_entity("surface_shader", surface_shader);
        }

        void write(const Texture& texture)
        {
            write_entity("texture", texture);
        }

        void write(const TextureInstance& texture_instance)
        {
            fprintf(
                m_file,
                "%stexture_instance %s\n",
                m_indenter.c_str(),
                texture_instance.get_name());

            const auto block = begin_block();
            write_params(texture_instance.get_parameters());
            write_transform(texture_instance.get_transform());
        }
    };
}

bool AppleseedProjectFileWriter::write_project_file(
    Project&        project,
    const char*     filepath,
    const int       options)
{
    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    RENDERER_LOG_INFO("writing project file %s...", filepath);

    // Open the file for writing.
    FILE* file = fopen(filepath, "wt");
    if (file == nullptr)
    {
        RENDERER_LOG_ERROR("failed to write project file %s: i/o error.", filepath);
        return false;
    }

    // Write the file header.
    fprintf(file, "# File generated by %s\n", Appleseed::get_synthetic_version_string());
    fprintf(file, "\n");

    // Write the project.
    Writer writer(filepath, file, options);
    writer.write_project(project);

    // Close the file.
    fclose(file);

    stopwatch.measure();

    RENDERER_LOG_INFO(
        "wrote project file %s in %s.",
        filepath,
        pretty_time(stopwatch.get_seconds()).c_str());

    return true;
}
}   // namespace renderer
