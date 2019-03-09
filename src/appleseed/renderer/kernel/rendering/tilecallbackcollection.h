
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Achal Pandey, The appleseedhq Organization
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
#include "itilecallback.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

namespace renderer
{

//
// A collection of tile callback factories.
//

class APPLESEED_DLLSYMBOL TileCallbackCollectionFactory
  : public ITileCallbackFactory
{
  public:
    // Constructor.
    TileCallbackCollectionFactory();

    // Destructor.
    ~TileCallbackCollectionFactory();

    // Delete this instance.
    void release() override;

    // Return a new instance.
    ITileCallback* create() override;

    // Insert a factory into the collection.
    void insert(ITileCallbackFactory* factory);

  private:
    struct Impl;
    Impl* impl;
};

}   // namespace renderer
