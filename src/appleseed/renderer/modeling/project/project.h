
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

#ifndef APPLESEED_RENDERER_MODELING_PROJECT_PROJECT_H
#define APPLESEED_RENDERER_MODELING_PROJECT_PROJECT_H

// appleseed.renderer headers.
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/project/configurationcontainer.h"
#include "renderer/modeling/project/renderlayerrule.h"
#include "renderer/modeling/project/renderlayerrulecontainer.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>
#include <string>

// Forward declarations.
namespace foundation    { class SearchPaths; }
namespace renderer      { class Camera; }
namespace renderer      { class Display; }
namespace renderer      { class Frame; }
namespace renderer      { class Scene; }
namespace renderer      { class TraceContext; }

namespace renderer
{

//
// A rendering project.
//

class APPLESEED_DLLSYMBOL Project
  : public Entity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Delete this instance.
    virtual void release() APPLESEED_OVERRIDE;

    // Set/get the format revision of the project.
    // The default value is renderer::ProjectFormatRevision (see projectformatrevision.h).
    void set_format_revision(const size_t format_revision);
    size_t get_format_revision() const;

    // Set/get the project path.
    bool has_path() const;
    void set_path(const char* path);
    const char* get_path() const;

    // Access the search paths.
    foundation::SearchPaths& search_paths() const;

    // Return the search paths as a string, in reverse order (for OIIO/OSL).
    std::string make_search_path_string() const;

    // Set the scene, replacing the existing scene.
    void set_scene(foundation::auto_release_ptr<Scene> scene);

    // Access the scene.
    // Return 0 if the project does not contain a scene.
    Scene* get_scene() const;

    // Set the frame, replacing the existing frame.
    void set_frame(foundation::auto_release_ptr<Frame> frame);

    // Access the frame.
    // Return 0 if the project does not contain a frame.
    Frame* get_frame() const;

    // Set the display, replacing the existing display.
    void set_display(foundation::auto_release_ptr<Display> display);

    // Access the display.
    // Return 0 if the project does not contain a display.
    Display* get_display() const;

    // Access the camera specified in the frame as active.
    // Return 0 if the scene does not contain cameras or if
    // no cameras are specified in the frame.
    Camera* get_uncached_active_camera() const;

    // Add a render layer rule.
    void add_render_layer_rule(foundation::auto_release_ptr<RenderLayerRule> rule);

    // Access the render layer rules.
    RenderLayerRuleContainer& render_layer_rules() const;

    // Access the configurations.
    ConfigurationContainer& configurations() const;

    // Add the default configurations to the project.
    void add_default_configurations();

    // Return true if the trace context has already been built.
    bool has_trace_context() const;

    // Get the trace context.
    const TraceContext& get_trace_context() const;

    // Synchronize the trace context with the scene.
    void update_trace_context();

  private:
    friend class ProjectFactory;

    struct Impl;
    Impl* impl;

    // Constructor.
    explicit Project(const char* name);

    // Destructor.
    ~Project();

    void add_base_configurations();
    void add_default_configuration(const char* name, const char* base_name);
};


//
// Project factory.
//

class APPLESEED_DLLSYMBOL ProjectFactory
{
  public:
    // Create a new project.
    static foundation::auto_release_ptr<Project> create(const char* name);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_PROJECT_PROJECT_H
