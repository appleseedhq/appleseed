
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "texturecollectionitem.h"

// appleseed.studio headers.
#include "mainwindow/project/basegroupitem.h"
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/itemregistry.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/textureitem.h"
#include "utility/interop.h"
#include "utility/miscellaneous.h"
#include "utility/settingskeys.h"

// appleseed.renderer headers.
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QDir>
#include <QMenu>
#include <QString>
#include <QStringList>

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <string>

using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

namespace
{
    const UniqueID g_class_uid = new_guid();
}

TextureCollectionItem::TextureCollectionItem(
    EntityEditorContext&    editor_context,
    TextureContainer&       textures,
    BaseGroup&              parent,
    BaseGroupItem*          parent_item)
  : CollectionItemBase<Texture>(editor_context, g_class_uid, "Textures")
  , m_parent(parent)
  , m_parent_item(parent_item)
{
    add_items(textures);
}

QMenu* TextureCollectionItem::get_single_item_context_menu() const
{
    QMenu* menu = CollectionItemBase<Texture>::get_single_item_context_menu();

    menu->addSeparator();
    menu->addAction("Import Textures...", this, SLOT(slot_import_textures()));

    return menu;
}

namespace
{
    auto_release_ptr<Texture> create_texture(const string& path)
    {
        const string texture_name =
            filesystem::path(path).replace_extension().filename().string();

        ParamArray texture_params;
        texture_params.insert("filename", path);
        texture_params.insert("color_space", "srgb");

        return
            DiskTexture2dFactory().create(
                texture_name.c_str(),
                texture_params,
                SearchPaths());
    }

    auto_release_ptr<TextureInstance> create_texture_instance(const string& texture_name)
    {
        const string texture_instance_name = texture_name + "_inst";

        return
            TextureInstanceFactory::create(
                texture_instance_name.c_str(),
                ParamArray(),
                texture_name.c_str(),
                Transformd::identity());
    }
}

void TextureCollectionItem::slot_import_textures()
{
    const QStringList filepaths =
        get_open_filenames(
            treeWidget(),
            "Import Textures...",
            g_bitmap_files_filter,
            m_editor_context.m_settings,
            SETTINGS_FILE_DIALOG_PROJECTS);

    if (filepaths.empty())
        return;

    const filesystem::path path(
        QDir::toNativeSeparators(filepaths.first()).toStdString());

    // todo: schedule creation of texture and texture instances when rendering.
    for (int i = 0; i < filepaths.size(); ++i)
    {
        const string filepath = QDir::toNativeSeparators(filepaths[i]).toStdString();

        auto_release_ptr<Texture> texture = create_texture(filepath);
        auto_release_ptr<TextureInstance> texture_instance = create_texture_instance(texture->get_name());

        m_parent_item->add_item(texture.get());
        m_parent_item->add_item(texture_instance.get());

        m_parent.textures().insert(texture);
        m_parent.texture_instances().insert(texture_instance);

    }

    m_editor_context.m_project_builder.notify_project_modification();
}

ItemBase* TextureCollectionItem::create_item(Texture* texture)
{
    assert(texture);

    ItemBase* item =
        new TextureItem(
            m_editor_context,
            texture,
            m_parent,
            this,
            m_parent_item);

    m_editor_context.m_item_registry.insert(*texture, item);

    return item;
}

}   // namespace studio
}   // namespace appleseed
