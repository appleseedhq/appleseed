
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

#ifndef APPLESEED_FOUNDATION_UTILITY_CACHEMANAGER_H
#define APPLESEED_FOUNDATION_UTILITY_CACHEMANAGER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Standard headers.
#include <cstddef>

//
// On Windows, define FOUNDATIONDLL to __declspec(dllexport) when building the DLL
// and to __declspec(dllimport) when building an application using the DLL.
// Other platforms don't use this export mechanism and the symbol FOUNDATIONDLL is
// defined to evaluate to nothing.
//

#ifndef FOUNDATIONDLL
#ifdef _WIN32
#ifdef APPLESEED_FOUNDATION_EXPORTS
#define FOUNDATIONDLL __declspec(dllexport)
#else
#define FOUNDATIONDLL __declspec(dllimport)
#endif
#else
#define FOUNDATIONDLL
#endif
#endif

namespace foundation
{

// todo: the CacheManager class must provide a stream when asking an object
// to flush its data, so that the object can decide to serialize them to
// the stream, instead of simply throwing them away.


//
// IFlushable interface.
//

class FOUNDATIONDLL IFlushable
  : public NonCopyable
{
  public:
    // Destructor.
    virtual ~IFlushable() {}

    // Return the number of bytes that can be flushed.
    virtual size_t get_flushable_size() const = 0;

    // Request immediate flushing.
    virtual void flush() = 0;
};


//
// A generic cache manager, designed to be used by multiple threads
// concurrently.
//
// All methods are thread-safe.
//

class FOUNDATIONDLL CacheManager
{
  public:
    // Constructor.
    explicit CacheManager(const size_t memory_limit);

    // Set/get the memory limit.
    void set_memory_limit(const size_t memory_limit);
    size_t get_memory_limit() const;

    // Insert an item implementing the IFlushable interface.
    // This item immediately becomes candidate for flushing.
    void insert(IFlushable* item);

    // Remove an item from the cache manager.
    void remove(IFlushable* item);

    // Touch an item already present in the cache manager.
    // Touching items informs the cache manager about which
    // items are most frequently accessed, which items were
    // recently accessed, etc. allowing the cache manager
    // to take reasonable flushing decisions.
    void touch(IFlushable* item);

    // Request immediate flushing to a given watermark.
    // Returns the number of bytes that were flushed.
    size_t flush(const size_t watermark);
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_CACHEMANAGER_H
