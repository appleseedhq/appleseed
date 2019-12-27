
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Francois Beaune, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/math/fp.h"
#include "foundation/platform/arch.h"

// Standard headers.
#include <cstring>

namespace foundation
{

//
// Poisoning is the act of setting a variable or an object to a remarkable value
// that will help detect when it is used without being initialized.
//
// To poison a variable or object `x` in any build mode:
//
//     foundation::always_poison(x);
//
// To poison a variable or object `x` in APPLESEED_DEBUG mode only:
//
//    foundation::debug_poison(x);
//
// To implement poisoning of custom objects of type `T`, specialize `foundation::PoisonImpl<T>::do_poison()`. 
// Within `the do_poison()` method, call `always_poison()` exclusively since `debug_poison()` is only enabled
// in debug mode:
//
//     namespace foundation
//     {
//         template <>
//         class PoisonImpl<ns::MyObject>
//         {
//           public:
//             static void do_poison(ns::MyObject& object)
//             {
//                 always_poison(object.x);
//                 always_poison(object.y);
//                 always_poison(object.z);
//             }
//         };
//     }
//

template <typename T>
void debug_poison(T& x);

template <typename T>
void always_poison(T& x);


//
// Implementation.
//

template <typename T>
class PoisonImpl
{
  public:
    static void do_poison(T& x)
    {
        std::memset(&x, 0xADU, sizeof(x));
    }
};

template <typename T>
inline void always_poison(T& x)
{
    PoisonImpl<T>::do_poison(x);
}

template <typename T>
inline void debug_poison(T& x)
{
#ifdef APPLESEED_DEBUG
    PoisonImpl<T>::do_poison(x);
#endif
}

template <typename T>
class PoisonImpl<T*>
{
  public:
    static void do_poison(T*& p)
    {
#ifdef APPLESEED_ARCH32
        p = reinterpret_cast<T*>(0xDEADBEEFu);
#else
        p = reinterpret_cast<T*>(0xDEADBEEFDEADBEEFull);
#endif
    }
};

template <>
class PoisonImpl<float>
{
  public:
    static void do_poison(float& x)
    {
        x = FP<float>::snan();
    }
};

template <>
class PoisonImpl<double>
{
  public:
    static void do_poison(double& x)
    {
        x = FP<double>::snan();
    }
};

}   // namespace foundation
