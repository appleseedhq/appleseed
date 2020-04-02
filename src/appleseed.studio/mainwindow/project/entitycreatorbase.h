
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

#pragma once

// appleseed.studio headers.
#include "mainwindow/project/exceptioninvalidentityname.h"

// appleseed.renderer headers.
#include "renderer/api/scene.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"

// Qt headers.
#include <QString>

namespace appleseed {
namespace studio {

class EntityCreatorBase
{
  protected:
    template <typename T>
    void catch_entity_creation_errors(
        void (T::*                      method)(const foundation::Dictionary&),
        const foundation::Dictionary&   values,
        const QString&                  entity_name);

  private:
    static void display_entity_creation_error(
        const QString&                  entity_name,
        const QString&                  message);
};


//
// EntityCreatorBase class implementation.
//

template <typename T>
void EntityCreatorBase::catch_entity_creation_errors(
    void (T::*                          method)(const foundation::Dictionary&),
    const foundation::Dictionary&       values,
    const QString&                      entity_name)
{
    try
    {
        (static_cast<T*>(this)->*method)(values);
    }
    catch (const ExceptionInvalidEntityName&)
    {
        display_entity_creation_error(
            entity_name,
            "A valid name is required.");
    }
    catch (const foundation::ExceptionDictionaryKeyNotFound& e)
    {
        display_entity_creation_error(
            entity_name,
            QString("Required parameter \"%1\" missing.").arg(e.string()));
    }
    catch (const renderer::ExceptionUnknownEntity& e)
    {
        display_entity_creation_error(
            entity_name,
            QString("Unknown entity \"%1\".").arg(e.string()));
    }
}

}   // namespace studio
}   // namespace appleseed
