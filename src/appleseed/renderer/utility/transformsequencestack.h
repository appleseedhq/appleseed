
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"

// Standard headers.
#include <cassert>
#include <stack>
#include <utility>

namespace renderer
{

//
// TransformSequenceStack.
//

class TransformSequenceStack
{
  public:
    TransformSequenceStack();

    void push(const TransformSequence& seq);

    void push(const foundation::Transformd& m);

    TransformSequence pop();

    void clear();

    std::size_t size() const;

    const TransformSequence& top() const;

  private:
    std::stack<TransformSequence> m_stack;
};


//
// TransformSequence class implementation.
//

inline TransformSequenceStack::TransformSequenceStack()
{
    clear();
}

inline void TransformSequenceStack::push(const TransformSequence& seq)
{
    m_stack.push(seq * m_stack.top());
}

inline void TransformSequenceStack::push(const foundation::Transformd& m)
{
    TransformSequence seq;
    seq.set_transform(0.0f, m);
    push(seq);
}

inline TransformSequence TransformSequenceStack::pop()
{
    assert(!m_stack.empty());

    TransformSequence seq(std::move(m_stack.top()));
    m_stack.pop();
    return seq;
}

inline void TransformSequenceStack::clear()
{
    while (!m_stack.empty())
        pop();

    m_stack.push(TransformSequence());
}

inline std::size_t TransformSequenceStack::size() const
{
    return m_stack.size();
}

inline const TransformSequence& TransformSequenceStack::top() const
{
    assert(!m_stack.empty());

    return m_stack.top();
}

}       // namespace renderer
