
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYEDITORFORMFACTORYBASE_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYEDITORFORMFACTORYBASE_H

// appleseed.studio headers.
#include "mainwindow/project/entityeditor.h"

// Standard headers.
#include <string>

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }

namespace appleseed {
namespace studio {

class EntityEditorFormFactoryBase
  : public EntityEditor::IFormFactory
{
  public:
    static const std::string NameParameter;
    static const std::string ModelParameter;

  protected:
    typedef EntityEditor::InputMetadataCollection InputMetadataCollection;

    explicit EntityEditorFormFactoryBase(const std::string& entity_name);

    void add_name_input_metadata(
        const foundation::Dictionary&       input_values,
        InputMetadataCollection&            metadata) const;

    void add_input_metadata(
        const foundation::DictionaryArray&  input_metadata,
        const foundation::Dictionary&       input_values,
        InputMetadataCollection&            metadata) const;

  private:
    const std::string   m_entity_name;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYEDITORFORMFACTORYBASE_H
