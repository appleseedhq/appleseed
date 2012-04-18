
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

#ifndef APPLESEED_FOUNDATION_MESH_GENERICMESHFILEREADER_H
#define APPLESEED_FOUNDATION_MESH_GENERICMESHFILEREADER_H

// appleseed.foundation headers.
#include "foundation/mesh/imeshfilereader.h"
#include "foundation/mesh/objmeshfilereader.h"

// Standard headers.
#include <string>

// Forward declarations.
namespace foundation    { class IMeshBuilder; }

namespace foundation
{

//
// Read a mesh file using the right reader based on the extension of the mesh file name.
//

class GenericMeshFileReader
  : public IMeshFileReader
{
  public:
    // Constructor.
    explicit GenericMeshFileReader(const std::string& filename);

    // Get/set options for the Wavefront OBJ mesh file reader.
    int get_obj_options() const;
    void set_obj_options(const int obj_options);

    // Read a mesh.
    virtual void read(IMeshBuilder& builder);

  private:
    const std::string   m_filename;
    int                 m_obj_options;
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MESH_GENERICMESHFILEREADER_H
