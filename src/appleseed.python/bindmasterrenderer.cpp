
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
#include "dict2dict.h"
#include "gillocks.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"
#include "renderer/api/rendering.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/python.h"
#include "foundation/utility/searchpaths.h"

// Standard headers.
#include <memory>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    // A class that wraps MasterRenderer and keeps a Python
    // reference to the project object to prevent it being
    // destroyed by Python before the MasterRenderer is destroyed.
    class MasterRendererWrapper
      : public NonCopyable
    {
      public:
        bpy::object                     m_project_object;       // unreferenced but necessary
        std::unique_ptr<MasterRenderer> m_renderer;

        MasterRendererWrapper(
            bpy::object                 project_object,
            const ParamArray&           params,
            const bpy::list&            resource_search_paths,
            ITileCallbackFactory*       tile_callback_factory = nullptr)
          : m_project_object(project_object)
        {
            Project* project = bpy::extract<Project*>(project_object);
            m_renderer.reset(
                new MasterRenderer(
                    *project,
                    params,
                    init_search_paths(resource_search_paths),
                    tile_callback_factory));
        }

        MasterRendererWrapper(
            bpy::object                 project_object,
            const ParamArray&           params,
            const bpy::list&            resource_search_paths,
            ITileCallback*              tile_callback)
          : m_project_object(project_object)
        {
            Project* project = bpy::extract<Project*>(project_object);
            m_renderer.reset(
                new MasterRenderer(
                    *project,
                    params,
                    init_search_paths(resource_search_paths),
                    tile_callback));
        }

      private:
        static SearchPaths init_search_paths(const bpy::list& paths)
        {
            SearchPaths search_paths;
            for (bpy::ssize_t i = 0, e = bpy::len(paths); i < e; ++i)
            {
                const bpy::extract<const char*> extractor(paths[i]);
                if (extractor.check())
                    search_paths.push_back_explicit_path(extractor());
                else
                {
                    PyErr_SetString(PyExc_TypeError, "Incompatible type. Only strings accepted.");
                    bpy::throw_error_already_set();
                }
            }

            return search_paths;
        }
    };

    std::shared_ptr<MasterRendererWrapper> create_master_renderer(
        bpy::object             project,
        const bpy::dict&        params,
        const bpy::list&        resource_search_paths)
    {
        return
            std::make_shared<MasterRendererWrapper>(
                *project,
                bpy_dict_to_param_array(params),
                resource_search_paths);
    }

    std::shared_ptr<MasterRendererWrapper> create_master_renderer_with_tile_callback(
        bpy::object             project,
        const bpy::dict&        params,
        const bpy::list&        resource_search_paths,
        ITileCallback*          tile_callback)
    {
        return
            std::make_shared<MasterRendererWrapper>(
                *project,
                bpy_dict_to_param_array(params),
                resource_search_paths,
                tile_callback);
    }

    bpy::dict master_renderer_get_parameters(const MasterRendererWrapper* m)
    {
        return param_array_to_bpy_dict(m->m_renderer->get_parameters());
    }

    void master_renderer_set_parameters(
        MasterRendererWrapper*  m,
        const bpy::dict&        params)
    {
        m->m_renderer->get_parameters() = bpy_dict_to_param_array(params);
    }

    bool master_renderer_render(MasterRendererWrapper* m, IRendererController& renderer_controller)
    {
        // Unlock Python's global interpreter lock (GIL) while we do lengthy C++ computations.
        // The GIL is locked again when unlock goes out of scope.
        ScopedGILUnlock unlock;

        const auto result = m->m_renderer->render(renderer_controller);

        return result.m_status == MasterRenderer::RenderingResult::Succeeded;
    }
}

void bind_master_renderer()
{
    bpy::class_<MasterRendererWrapper, std::shared_ptr<MasterRendererWrapper>, boost::noncopyable>("MasterRenderer", bpy::no_init)
        .def("__init__", bpy::make_constructor(create_master_renderer))
        .def("__init__", bpy::make_constructor(create_master_renderer_with_tile_callback))
        .def("get_parameters", master_renderer_get_parameters)
        .def("set_parameters", master_renderer_set_parameters)
        .def("render", master_renderer_render);
}
