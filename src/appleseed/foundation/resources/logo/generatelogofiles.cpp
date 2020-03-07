
#if 0

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

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/string/string.h"
#include "foundation/utility/test.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cstddef>
#include <cstdio>
#include <memory>
#include <string>

namespace bf = boost::filesystem;
using namespace foundation;

TEST_SUITE(Foundation_Resources_Logos)
{
    void generate_logo_file(
        const bf::path&     output_folder,
        const bf::path&     input_filepath)
    {
        const bf::path input_filename = input_filepath.filename();
        const bf::path header_filename = bf::path(input_filename).replace_extension(".h");
        const bf::path output_filename = bf::path(input_filename).replace_extension(".cpp");
        const bf::path output_filepath = output_folder / output_filename;
        const string variable_name = replace(bf::path(input_filename).replace_extension("").string(), "-", "_");

        FILE* file = fopen(output_filepath.string().c_str(), "wt");

        fprintf(file, "\n");
        fprintf(file, "//\n");
        fprintf(file, "// This source file is part of appleseed.\n");
        fprintf(file, "// Visit https://appleseedhq.net/ for additional information and resources.\n");
        fprintf(file, "//\n");
        fprintf(file, "// This software is released under the MIT license.\n");
        fprintf(file, "//\n");
        fprintf(file, "// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization\n");
        fprintf(file, "//\n");
        fprintf(file, "// Permission is hereby granted, free of charge, to any person obtaining a copy\n");
        fprintf(file, "// of this software and associated documentation files (the \"Software\"), to deal\n");
        fprintf(file, "// in the Software without restriction, including without limitation the rights\n");
        fprintf(file, "// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell\n");
        fprintf(file, "// copies of the Software, and to permit persons to whom the Software is\n");
        fprintf(file, "// furnished to do so, subject to the following conditions:\n");
        fprintf(file, "//\n");
        fprintf(file, "// The above copyright notice and this permission notice shall be included in\n");
        fprintf(file, "// all copies or substantial portions of the Software.\n");
        fprintf(file, "//\n");
        fprintf(file, "// THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR\n");
        fprintf(file, "// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,\n");
        fprintf(file, "// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE\n");
        fprintf(file, "// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER\n");
        fprintf(file, "// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,\n");
        fprintf(file, "// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN\n");
        fprintf(file, "// THE SOFTWARE.\n");
        fprintf(file, "//\n");
        fprintf(file, "\n");
        fprintf(file, "// Interface header.\n");
        fprintf(file, "#include \"%s\"\n", header_filename.string().c_str());
        fprintf(file, "\n");
        fprintf(file, "namespace foundation\n");
        fprintf(file, "{\n");
        fprintf(file, "\n");
        fprintf(file, "const float %s[] =\n", variable_name.c_str());
        fprintf(file, "{\n");

        GenericImageFileReader reader;
        unique_ptr<Image> image(reader.read(input_filepath.string().c_str()));

        const CanvasProperties& props = image->properties();

        size_t written_pixels = 0;

        for (size_t y = 0; y < props.m_canvas_height; ++y)
        {
            for (size_t x = 0; x < props.m_canvas_width; ++x)
            {
                if (written_pixels > 0)
                {
                    if (written_pixels % 4 == 0)
                    {
                        fprintf(file, ",\n");
                        fprintf(file, "    ");
                    }
                    else
                    {
                        fprintf(file, ", ");
                    }
                }
                else
                {
                    fprintf(file, "    ");
                }

                Color4f color;
                image->get_pixel(x, y, color);

                color.rgb() = srgb_to_linear_rgb(color.rgb());

                fprintf(file, "%.9ff, %.9ff, %.9ff, %.9ff", color[0], color[1], color[2], color[3]);
                ++written_pixels;
            }
        }

        fprintf(file, "\n");
        fprintf(file, "};\n");
        fprintf(file, "\n");
        fprintf(file, "}   // namespace foundation\n");

        fclose(file);
    }

    TEST_CASE(GenerateLogoFiles)
    {
        // Unit tests are executed from sandbox/tests/.
        const bf::path input_folder = "../../resources/logo/";
        const bf::path output_folder = "../../src/appleseed/foundation/resources/logo/";

        generate_logo_file(output_folder, input_folder / "appleseed-seeds-16.png");
        generate_logo_file(output_folder, input_folder / "appleseed-seeds-32.png");
        generate_logo_file(output_folder, input_folder / "appleseed-seeds-64.png");
    }
}

#endif
