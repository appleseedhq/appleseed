
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Esteban Tovagliari, Jupiter Jazz Limited
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

// Original code from: https://gist.github.com/1528856
// This is a rewrite and analysis of the technique in this article:
// http://bloglitb.blogspot.com/2010/07/access-to-private-members-thats-easy.html

#ifndef APPLESEED_FOUNDATION_UTILITY_PRIVATE_ACCESS_H
#define APPLESEED_FOUNDATION_UTILITY_PRIVATE_ACCESS_H

namespace foundation
{

// ------- Framework -------
// The little library required to work this magic

// Generate a static data member of type Tag::type in which to store
// the address of a private member.  It is crucial that Tag does not
// depend on the /value/ of the the stored address in any way so that
// we can access it from ordinary code without directly touching
// private data.
template <typename Tag>
struct Stowed
{
    static typename Tag::type value;
};

template <typename Tag>
typename Tag::type Stowed<Tag>::value;

// Generate a static data member whose constructor initializes
// Stowed<Tag>::value.  This type will only be named in an explicit
// instantiation, where it is legal to pass the address of a private
// member.
template <typename Tag, typename Tag::type x>
struct StowPrivate
{
     StowPrivate() { Stowed<Tag>::value = x;}
     static StowPrivate instance;
};

template <typename Tag, typename Tag::type x>
StowPrivate<Tag,x> StowPrivate<Tag,x>::instance;

// ------- Usage -------
// A demonstration of how to use the library, with explanation
#if 0
    struct A
    {
         A() : x("proof!") {}

    private:

        int fun()
        {
            return 42;
        }

        char const* x;
    };

    // A tag type for A::x.  Each distinct private member you need to
    // access should have its own tag.  Each tag should contain a
    // nested ::type that is the corresponding pointer-to-member type.
    struct A_x
    {
        typedef char const*(A::*type);
    };

    // Explicit instantiation; the only place where it is legal to pass
    // the address of a private member.  Generates the static ::instance
    // that in turn initializes Stowed<Tag>::value.
    template class StowPrivate<A_x,&A::x>;

    // Repeat for member function pointer

    struct A_fun
    {
        typedef int(A::*type)();
    };

    template class StowPrivate<A_fun,&A::fun>;

    int access_private()
    {
        A a;

        // Use the Stowed private members pointer
        std::cout << a.*Stowed<A_x>::value << std::endl;
        std::cout << (a.*Stowed<A_fun>::value)() << std::endl;
    }
#endif

}       // namespace foundation

#endif // !APPLESEED_FOUNDATION_UTILITY_PRIVATE_ACCESS_H
