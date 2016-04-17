
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/project/projectfilereader.h"
#include "renderer/modeling/project/projectfilewriter.h"

// appleseed.foundation headers.
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/test.h"
#include "foundation/utility/testutils.h"

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Modeling_Project_ProjectFileReader)
{
    TEST_CASE(ParsingOfConfigurationBlocks)
    {
        ProjectFileReader reader;
        auto_release_ptr<Project> project =
            reader.read(
                "unit tests/inputs/test_projectfilereader_configurationblocks.appleseed",
                "../../../schemas/project.xsd");    // path relative to input file

        ASSERT_NEQ(0, project.get());

        const bool success =
            ProjectFileWriter::write(
                project.ref(),
                "unit tests/outputs/test_projectfilereader_configurationblocks.appleseed",
                ProjectFileWriter::OmitHeaderComment);

        ASSERT_TRUE(success);

        const bool identical =
            compare_text_files(
                "unit tests/inputs/test_projectfilereader_configurationblocks.appleseed",
                "unit tests/outputs/test_projectfilereader_configurationblocks.appleseed");

        EXPECT_TRUE(identical);
    }
}
