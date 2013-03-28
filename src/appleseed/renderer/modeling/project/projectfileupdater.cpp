
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/project/configuration.h"
#include "renderer/modeling/project/project.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cassert>
#include <cstddef>
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

        static void copy_if_exist(
            Dictionary&         dest,
            const Dictionary&   src,
            const char*         key)
        {
            if (src.strings().exist(key))
                dest.strings().insert(key, src.strings().get(key));
        }

        static void copy_if_exist(
            Dictionary&         dest,
            const char*         dest_key,
            const Dictionary&   src,
            const char*         src_key)
        {
            if (src.strings().exist(src_key))
                dest.strings().insert(dest_key, src.strings().get(src_key));
        }

        static void move_if_exist(
            Dictionary&         dest,
            Dictionary&         src,
            const char*         key)
        {
            if (src.strings().exist(key))
            {
                dest.strings().insert(key, src.strings().get(key));
                src.strings().remove(key);
            }
        }

        static void move_if_exist(
            Dictionary&         dest,
            const char*         dest_key,
            Dictionary&         src,
            const char*         src_key)
        {
            if (src.strings().exist(src_key))
            {
                dest.strings().insert(dest_key, src.strings().get(src_key));
                src.strings().remove(src_key);
            }
        }
    };


    //
    // Update from revision 2 to revision 3.
    //

    class Updater_2_to_3
      : public Updater
    {
      public:
        explicit Updater_2_to_3(Project& project)
          : Updater(project, 2)
        {
        }

        virtual void update() OVERRIDE
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
}

void ProjectFileUpdater::update(Project& project)
{
    const size_t format_revision = project.get_format_revision();

    switch (format_revision)
    {
      case 2: { Updater_2_to_3 updater(project); updater.update(); }

      case 3:
        // Project is up-to-date.
        break;

      default:
        RENDERER_LOG_ERROR("unsupported project format revision: " FMT_SIZE_T, format_revision);
        break;
    }
}

}   // namespace renderer
