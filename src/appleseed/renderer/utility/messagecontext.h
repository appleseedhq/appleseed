
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

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <string>

// Forward declarations.
namespace renderer  { class Entity; }

namespace renderer
{

//
// Base class for message contexts.
//

class APPLESEED_DLLSYMBOL MessageContext
{
  public:
    MessageContext();
    explicit MessageContext(const char* message);
    explicit MessageContext(const std::string& message);

    ~MessageContext();

    bool empty() const;
    const char* get() const;

  protected:
    void set_message(const char* message);
    void set_message(const std::string& message);

  private:
    struct Impl;
    Impl* impl;
};


//
// Message context when constructing entities.
//

class APPLESEED_DLLSYMBOL EntityDefMessageContext
  : public MessageContext
{
  public:
    EntityDefMessageContext(
        const char*     entity_type,
        const Entity*   entity);
};


//
// Message context when calling `on_render_begin()` on entities.
//

class APPLESEED_DLLSYMBOL OnRenderBeginMessageContext
  : public MessageContext
{
  public:
    OnRenderBeginMessageContext(
        const char*     entity_type,
        const Entity*   entity);
};


//
// Message context when calling `on_frame_begin()` on entities.
//

class APPLESEED_DLLSYMBOL OnFrameBeginMessageContext
  : public MessageContext
{
  public:
    OnFrameBeginMessageContext(
        const char*     entity_type,
        const Entity*   entity);
};


//
// MessageContext class implementation.
//

inline MessageContext::MessageContext(const std::string& message)
  : impl(nullptr)
{
    set_message(message);
}

inline void MessageContext::set_message(const std::string& message)
{
    set_message(message.c_str());
}

}   // namespace renderer
