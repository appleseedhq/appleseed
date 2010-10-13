
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "assemblycollectionitem.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblyitem.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cassert>

using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

AssemblyCollectionItem::AssemblyCollectionItem(
    ProjectBuilder&     project_builder,
    AssemblyContainer&  assemblies)
  : CollectionItemBase("Assemblies")
  , m_project_builder(project_builder)
{
    for (each<AssemblyContainer> i = assemblies; i; ++i)
        add_item(*i);
}

void AssemblyCollectionItem::add_item(Assembly& assembly)
{
    AssemblyItem* item = new AssemblyItem(m_project_builder, assembly);
    m_assembly_items[assembly.get_uid()] = item;
    addChild(item);
}

AssemblyItem& AssemblyCollectionItem::get_item(const Assembly& assembly) const
{
    const AssemblyItemMap::const_iterator i = m_assembly_items.find(assembly.get_uid());
    assert(i != m_assembly_items.end());
    return *(i->second);
}

}   // namespace studio
}   // namespace appleseed
