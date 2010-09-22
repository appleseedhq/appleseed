
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

//
// Global header file, included by every other header file of appleseed.renderer.
//

#ifndef APPLESEED_RENDERER_GLOBAL_GLOBAL_H
#define APPLESEED_RENDERER_GLOBAL_GLOBAL_H

// Globally included appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/global/globaluidsource.h"
#include "renderer/utility/paramarray.h"

// Globally included appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/core/concepts/iunknown.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/log.h"
#include "foundation/utility/otherwise.h"

// Globally included standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <limits>
#include <memory>
#include <string>

//
// On Windows, define RENDERERDLL to __declspec(dllexport) when building the DLL
// and to __declspec(dllimport) when building an application using the DLL.
// Other platforms don't use this export mechanism and the symbol RENDERERDLL is
// defined to evaluate to nothing.
//

#ifndef RENDERERDLL
#ifdef _WIN32
#ifdef APPLESEED_RENDERER_EXPORTS
#define RENDERERDLL __declspec(dllexport)
#else
#define RENDERERDLL __declspec(dllimport)
#endif
#else
#define RENDERERDLL
#endif
#endif

#endif  // !APPLESEED_RENDERER_GLOBAL_GLOBAL_H
