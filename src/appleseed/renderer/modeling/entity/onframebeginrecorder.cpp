
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2017 Francois Beaune, The appleseedhq Organization
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
#include "onframebeginrecorder.h"

// appleseed.renderer headers.
#include "renderer/modeling/entity/entity.h"

// Standard headers.
#include <cassert>
#include <stack>

using namespace std;

namespace renderer
{

struct OnFrameBeginRecorder::Impl
{
    struct Record
    {
        Entity*             m_entity;
        const BaseGroup*    m_parent;
    };

    stack<Record> m_records;
};

OnFrameBeginRecorder::OnFrameBeginRecorder()
  : impl(new Impl())
{
}

OnFrameBeginRecorder::~OnFrameBeginRecorder()
{
    assert(impl->m_records.empty());
    delete impl;
}

void OnFrameBeginRecorder::record(Entity* entity, const BaseGroup* parent)
{
    Impl::Record record;
    record.m_entity = entity;
    record.m_parent = parent;
    impl->m_records.push(record);
}

void OnFrameBeginRecorder::on_frame_end(const Project& project)
{
    while (!impl->m_records.empty())
    {
        const Impl::Record& record = impl->m_records.top();
        record.m_entity->on_frame_end(project, record.m_parent);
        impl->m_records.pop();
    }
}

}   // namespace renderer
