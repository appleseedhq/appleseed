
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

// Interface header.
#include "module.h"

// appleseed.foundation headers.
#include "foundation/cuda/exception.h"

// Standard headers.
#include <algorithm>
#include <cassert>

namespace foundation
{

CUDAModule::CUDAModule()
  : m_module(nullptr)
{
}

CUDAModule::CUDAModule(const char* filename)
{
    check_cuda_result(cuModuleLoad(&m_module, filename));
}

CUDAModule::CUDAModule(CUDAModule&& rhs)
  : m_module(rhs.m_module)
{
    rhs.m_module = nullptr;
}

CUDAModule::~CUDAModule()
{
    if (m_module)
        cuModuleUnload(m_module);
}

CUDAModule& CUDAModule::operator=(CUDAModule&& rhs)
{
    CUDAModule tmp(std::move(rhs));
    std::swap(m_module, tmp.m_module);
    return *this;
}

CUfunction CUDAModule::get_function(const char* name)
{
    assert(m_module);

    CUfunction f;
    check_cuda_result(cuModuleGetFunction(&f, m_module, name));
    return f;
}

}       // namespace foundation
