
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.python headers.
#include "bindentitycontainers.h"
#include "dict2dict.h"
#include "metadata.h"
#include "unalignedtransform.h"

// appleseed.renderer headers.
#include "renderer/api/scene.h"
#include "renderer/api/texture.h"

// appleseed.foundation headers.
#include "foundation/platform/python.h"
#include "foundation/utility/searchpaths.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    void string_list_to_search_paths(
        const bpy::list&            search_paths,
        SearchPaths&                paths)
    {
        for (bpy::ssize_t i = 0, e = bpy::len(search_paths); i < e; ++i)
        {
            bpy::extract<const char*> extractor(search_paths[i]);
            if (extractor.check())
                paths.push_back_explicit_path(extractor());
            else
            {
                PyErr_SetString(PyExc_TypeError, "Incompatible type. Only strings accepted.");
                bpy::throw_error_already_set();
            }
        }
    }

    auto_release_ptr<Texture> create_texture(
        const std::string&         model,
        const std::string&         name,
        const bpy::dict&           params,
        const bpy::list&           search_paths)
    {
        TextureFactoryRegistrar factories;
        const ITextureFactory* factory = factories.lookup(model.c_str());

        if (factory)
        {
            SearchPaths paths;
            string_list_to_search_paths(search_paths, paths);

            return factory->create(name.c_str(), bpy_dict_to_param_array(params), paths);
        }
        else
        {
            PyErr_SetString(PyExc_RuntimeError, "Texture model not found");
            bpy::throw_error_already_set();
        }

        return auto_release_ptr<Texture>();
    }

    auto_release_ptr<Texture> factory_create_texture(
        const ITextureFactory*      factory,
        const char*                 name,
        const bpy::dict&            params,
        const bpy::list&            search_paths)
    {
        SearchPaths paths;
        string_list_to_search_paths(search_paths, paths);

        return factory->create(name, bpy_dict_to_param_array(params), paths);
    }

    auto_release_ptr<TextureInstance> create_texture_instance(
        const std::string&              name,
        const bpy::dict&                params,
        const std::string&              texture_name,
        const UnalignedTransformf&      transform)
    {
        return
            TextureInstanceFactory::create(
                name.c_str(),
                bpy_dict_to_param_array(params),
                texture_name.c_str(),
                transform.as_foundation_transform());
    }

    UnalignedTransformf texture_inst_get_transform(const TextureInstance* tx)
    {
        return UnalignedTransformf(tx->get_transform());
    }

    std::string texture_inst_get_texture_name(const TextureInstance* tx)
    {
        return tx->get_texture_name();
    }
}

void bind_texture()
{
    bpy::enum_<TextureAddressingMode>("TextureAddressingMode")
        .value("Clamp", TextureAddressingClamp)
        .value("Wrap", TextureAddressingWrap);

    bpy::enum_<TextureFilteringMode>("TextureFilteringMode")
        .value("Nearest", TextureFilteringNearest)
        .value("Bilinear", TextureFilteringBilinear)
        .value("Bicubic", TextureFilteringBicubic)
        .value("Feline", TextureFilteringFeline)
        .value("EWA", TextureFilteringEWA);

    bpy::enum_<TextureAlphaMode>("TextureAlphaMode")
        .value("AlphaChannel", TextureAlphaModeAlphaChannel)
        .value("Luminance", TextureAlphaModeLuminance)
        .value("Detect", TextureAlphaModeDetect);

    bpy::class_<Texture, auto_release_ptr<Texture>, bpy::bases<Entity>, boost::noncopyable>("Texture", bpy::no_init)
        .def("get_model_metadata", &detail::get_entity_model_metadata<TextureFactoryRegistrar>).staticmethod("get_model_metadata")
        .def("get_input_metadata", &detail::get_entity_input_metadata<TextureFactoryRegistrar>).staticmethod("get_input_metadata")
        .def("__init__", bpy::make_constructor(create_texture))
        .def("get_model", &Texture::get_model)
        .def("get_color_space", &Texture::get_color_space);

    bind_typed_entity_vector<Texture>("TextureContainer");

    bpy::class_<TextureInstance, auto_release_ptr<TextureInstance>, bpy::bases<Entity>, boost::noncopyable>("TextureInstance", bpy::no_init)
        .def("__init__", bpy::make_constructor(create_texture_instance))
        .def("get_addressing_mode", &TextureInstance::get_addressing_mode)
        .def("get_alpha_mode", &TextureInstance::get_alpha_mode)
        .def("get_filtering_mode", &TextureInstance::get_filtering_mode)
        .def("get_transform", &texture_inst_get_transform)
        .def("get_texture_name", &texture_inst_get_texture_name)
        .def("find_texture", &TextureInstance::find_texture, bpy::return_value_policy<bpy::reference_existing_object>());

    bind_typed_entity_vector<TextureInstance>("TextureInstanceContainer");

    bpy::class_<ITextureFactory, boost::noncopyable>("ITextureFactory", bpy::no_init)
        .def("create", &factory_create_texture);

    bpy::class_<TextureFactoryRegistrar, boost::noncopyable>("TextureFactoryRegistrar", bpy::no_init)
        .def("lookup", &TextureFactoryRegistrar::lookup, bpy::return_value_policy<bpy::reference_existing_object>());
}
