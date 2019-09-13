
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

// Interface header.
#include "messagelist.h"

// appleseed.foundation headers.
#include "foundation/platform/snprintf.h"
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cstdarg>
#include <cstddef>
#include <string>
#include <vector>

namespace foundation
{

//
// MessageList class implementation.
//

struct MessageList::Impl
{
    struct Message
    {
        LogMessage::Category    m_category;
        std::string             m_text;
    };

    typedef std::vector<Message> Messages;

    Messages    m_messages;
};

MessageList::MessageList()
  : impl(new Impl())
{
}

MessageList::~MessageList()
{
    delete impl;
}

void MessageList::add(
    const LogMessage::Category          category,
    APPLESEED_PRINTF_FMT const char*    format, ...)
{
    // Size in bytes of the temporary message buffer.
    static const size_t BufferSize = 4096;

    // Print the formatted message into the temporary buffer.
    va_list argptr;
    va_start(argptr, format);
    char buffer[BufferSize];
    portable_vsnprintf(buffer, BufferSize, format, argptr);

    // Create and append a message.
    Impl::Message msg;
    msg.m_category = category;
    msg.m_text = buffer;
    impl->m_messages.push_back(msg);
}

void MessageList::print(Logger& logger) const
{
    for (const_each<Impl::Messages> i = impl->m_messages; i; ++i)
        LOG(logger, i->m_category, "%s", i->m_text.c_str());
}

}   // namespace foundation
