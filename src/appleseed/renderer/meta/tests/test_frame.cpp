
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/aov/aovcontainer.h"
#include "renderer/modeling/aov/diffuseaov.h"
#include "renderer/modeling/aov/glossyaov.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/thread.h"
#include "foundation/utility/test.h"

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

namespace bf = boost::filesystem;
using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Modeling_Frame_Frame)
{
    struct Fixture
    {
        const bf::path              m_output_directory;
        auto_release_ptr<Frame>     m_frame;

        Fixture()
          : m_output_directory(bf::absolute("unit tests/outputs/test_frame/"))
        {
            remove_all(m_output_directory);

            // On Windows, the create_directory() call below will fail with an Access Denied error
            // if a File Explorer window was opened in the output directory that we just deleted.
            // A small pause solves the problem. The namespace qualifier is required on Linux.
            foundation::sleep(50);

            create_directory(m_output_directory);

            AOVContainer aovs;

            aovs.insert(
                DirectDiffuseAOVFactory().create(
                    ParamArray()
                        .insert("output_filename", (m_output_directory / "default-direct-diffuse.exr").string())));

            aovs.insert(
                IndirectDiffuseAOVFactory().create(
                    ParamArray()
                        .insert("output_filename", (m_output_directory / "default-indirect-diffuse.png").string())));

            aovs.insert(
                DirectGlossyAOVFactory().create(
                    ParamArray()
                        .insert("output_filename", (m_output_directory / "default-direct-glossy").string())));

            aovs.insert(
                IndirectGlossyAOVFactory().create(
                    ParamArray()
                        .insert("output_filename", (m_output_directory / "default-indirect-glossy.").string())));

            m_frame =
                FrameFactory::create(
                    "beauty",
                    ParamArray()
                        .insert("resolution", "64 64")
                        .insert("output_filename", (m_output_directory / "default-main.png").string()),
                    aovs,
                    AOVContainer());

            m_frame->clear_main_and_aov_images();
        }
    };

    TEST_CASE_F(WriteMainAndAOVImages, Fixture)
    {
        m_frame->write_main_and_aov_images();

        EXPECT_TRUE(bf::exists(m_output_directory / "default-main.png"));

        EXPECT_TRUE(bf::exists(m_output_directory / "default-direct-diffuse.exr"));
        EXPECT_TRUE(bf::exists(m_output_directory / "default-indirect-diffuse.exr"));       // note: png -> exr
        EXPECT_TRUE(bf::exists(m_output_directory / "default-direct-glossy.exr"));          // note: exr extension added
        EXPECT_TRUE(bf::exists(m_output_directory / "default-indirect-glossy.exr"));        // note: exr extension added
    }

    TEST_CASE_F(WriteMainImage_FilenameHasEXRExtension_WritesEXRFile, Fixture)
    {
        m_frame->write_main_image((m_output_directory / "override.exr").string().c_str());

        EXPECT_TRUE(bf::exists(m_output_directory / "override.exr"));                       // note: file name overridden
    }

    TEST_CASE_F(WriteMainImage_FilenameHasPNGExtension_WritesPNGFile, Fixture)
    {
        m_frame->write_main_image((m_output_directory / "override.png").string().c_str());

        EXPECT_TRUE(bf::exists(m_output_directory / "override.png"));                       // note: file name overridden
    }

    TEST_CASE_F(WriteMainImage_FilenameHasNoExtension_WritesEXRFile, Fixture)
    {
        m_frame->write_main_image((m_output_directory / "override").string().c_str());

        EXPECT_TRUE(bf::exists(m_output_directory / "override.exr"));                       // note: file name overridden and exr extension added
    }

    TEST_CASE_F(WriteMainImage_FilenameEndsWithDot_WritesEXRFile, Fixture)
    {
        m_frame->write_main_image((m_output_directory / "override.").string().c_str());

        EXPECT_TRUE(bf::exists(m_output_directory / "override.exr"));                       // note: file name overridden and exr extension added
    }

    TEST_CASE_F(WriteAOVImages_FilenameHasEXRExtension_WritesEXRFiles, Fixture)
    {
        m_frame->write_aov_images((m_output_directory / "override.exr").string().c_str());

        EXPECT_TRUE(bf::exists(m_output_directory / "override.direct_diffuse.exr"));        // note: file name overridden
        EXPECT_TRUE(bf::exists(m_output_directory / "override.indirect_diffuse.exr"));      // note: file name overridden and png -> exr
        EXPECT_TRUE(bf::exists(m_output_directory / "override.direct_glossy.exr"));         // note: file name overridden and png -> exr
        EXPECT_TRUE(bf::exists(m_output_directory / "override.indirect_glossy.exr"));       // note: file name overridden and png -> exr
    }

    TEST_CASE_F(WriteAOVImages_FilenameHasPNGExtension_WritesEXRFiles, Fixture)
    {
        m_frame->write_aov_images((m_output_directory / "override.png").string().c_str());

        EXPECT_TRUE(bf::exists(m_output_directory / "override.direct_diffuse.exr"));        // note: file name overridden and png -> exr
        EXPECT_TRUE(bf::exists(m_output_directory / "override.indirect_diffuse.exr"));      // note: file name overridden and png -> exr
        EXPECT_TRUE(bf::exists(m_output_directory / "override.direct_glossy.exr"));         // note: file name overridden and png -> exr
        EXPECT_TRUE(bf::exists(m_output_directory / "override.indirect_glossy.exr"));       // note: file name overridden and png -> exr
    }

    TEST_CASE_F(WriteAOVImages_FilenameHasNoExtension_WritesEXRFiles, Fixture)
    {
        m_frame->write_aov_images((m_output_directory / "override").string().c_str());

        EXPECT_TRUE(bf::exists(m_output_directory / "override.direct_diffuse.exr"));        // note: file name overridden and exr extension added
        EXPECT_TRUE(bf::exists(m_output_directory / "override.indirect_diffuse.exr"));      // note: file name overridden and exr extension added
        EXPECT_TRUE(bf::exists(m_output_directory / "override.direct_glossy.exr"));         // note: file name overridden and exr extension added
        EXPECT_TRUE(bf::exists(m_output_directory / "override.indirect_glossy.exr"));       // note: file name overridden and exr extension added
    }

    TEST_CASE_F(WriteAOVImages_FilenameEndsWithDot_WritesEXRFiles, Fixture)
    {
        m_frame->write_aov_images((m_output_directory / "override.").string().c_str());

        EXPECT_TRUE(bf::exists(m_output_directory / "override.direct_diffuse.exr"));        // note: file name overridden and exr extension added
        EXPECT_TRUE(bf::exists(m_output_directory / "override.indirect_diffuse.exr"));      // note: file name overridden and exr extension added
        EXPECT_TRUE(bf::exists(m_output_directory / "override.direct_glossy.exr"));         // note: file name overridden and exr extension added
        EXPECT_TRUE(bf::exists(m_output_directory / "override.indirect_glossy.exr"));       // note: file name overridden and exr extension added
    }
}
