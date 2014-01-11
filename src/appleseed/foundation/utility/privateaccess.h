
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_UTILITY_PRIVATEACCESS_H
#define APPLESEED_FOUNDATION_UTILITY_PRIVATEACCESS_H

//
// Original code from: https://gist.github.com/1528856
// This is a rewrite and analysis of the technique in this article:
// http://bloglitb.blogspot.com/2010/07/access-to-private-members-thats-easy.html
//

namespace foundation
{

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
    StowPrivate() { Stowed<Tag>::value = x; }
    static StowPrivate instance;
};

template <typename Tag, typename Tag::type x>
StowPrivate<Tag, x> StowPrivate<Tag, x>::instance;

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_PRIVATEACCESS_H
