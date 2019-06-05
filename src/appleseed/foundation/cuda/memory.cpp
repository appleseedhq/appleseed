
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
#include "foundation/cuda/memory.h"

namespace foundation
{

GlobalDeviceMemoryAllocator::pointer_type GlobalDeviceMemoryAllocator::alloc(
    const size_t        size,
    const unsigned int  flags)
{
    pointer_type p;
    check_cuda_result(cuMemAlloc(&p, size));
    return p;
}

CUresult GlobalDeviceMemoryAllocator::free(pointer_type p)
{
    assert(p);
    return cuMemFree(p);
}


ZeroCopyDeviceMemoryAllocator::pointer_type ZeroCopyDeviceMemoryAllocator::alloc(
    const size_t        size,
    const unsigned int  flags)
{
    pointer_type p;
    check_cuda_result(cuMemHostAlloc(&p, size, flags));
    return p;
}

CUresult ZeroCopyDeviceMemoryAllocator::free(pointer_type p)
{
    return cuMemFreeHost(p);
}


UnifiedDeviceMemoryAllocator::pointer_type UnifiedDeviceMemoryAllocator::alloc(
    const size_t        size,
    const unsigned int  flags)
{
    pointer_type p;
    check_cuda_result(cuMemAllocManaged(&p, size, flags));
    return p;
}

CUresult UnifiedDeviceMemoryAllocator::free(pointer_type p)
{
    return cuMemFree(p);
}

}       // namespace foundation
