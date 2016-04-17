
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Esteban Tovagliari, The appleseedhq Organization
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
#include "gillocks.h"

ScopedGILLock::ScopedGILLock()
  : m_threads_initialized(PyEval_ThreadsInitialized() ? true : false)
{
    if (m_threads_initialized)
        m_state = PyGILState_Ensure();
}

ScopedGILLock::~ScopedGILLock()
{
    if (m_threads_initialized)
        PyGILState_Release(m_state);
}

ScopedGILUnlock::ScopedGILUnlock()
  : m_threads_initialized(PyEval_ThreadsInitialized() ? true : false)
{
    if (m_threads_initialized)
        m_state = PyEval_SaveThread();
}

ScopedGILUnlock::~ScopedGILUnlock()
{
    if (m_threads_initialized)
        PyEval_RestoreThread(m_state);
}
