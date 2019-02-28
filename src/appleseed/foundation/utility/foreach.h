
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

namespace foundation
{

//
// Helper classes to iterate over the elements of a collection.
//
// Example:
//
//   vector<int> v;
//   ...
//   for (const_each<vector<int>> i = v; i; ++i)
//       cerr << *i << endl;
//

template <typename C>
class const_each
{
  public:
    // Types.
    typedef typename C::const_iterator const_iterator;
    typedef typename const_iterator::value_type value_type;

    // Constructor.
    const_each(const C& c);

    // Conversion to bool, return true if the iterator
    // hasn't yet reached the end of the collection.
    operator bool() const;

    // Preincrement operator.
    const_each& operator++();

    // Iterator comparison.
    bool operator==(const const_iterator& rhs) const;
    bool operator!=(const const_iterator& rhs) const;
    bool operator<(const const_iterator& rhs) const;
    bool operator>(const const_iterator& rhs) const;

    // Iterator dereference.
    const value_type& operator*() const;
    const value_type* operator->() const;

    // Return the underlying iterator.
    const_iterator it() const;

  private:
    const_iterator i, e;
};

template <typename C>
class each
{
  public:
    // Types.
    typedef typename C::iterator iterator;
    typedef typename iterator::value_type value_type;

    // Constructor.
    each(C& c);

    // Conversion to bool, return true if the iterator
    // hasn't yet reached the end of the collection.
    operator bool() const;

    // Preincrement operator.
    each& operator++();

    // Iterator comparison.
    bool operator==(const iterator& rhs) const;
    bool operator!=(const iterator& rhs) const;
    bool operator<(const iterator& rhs) const;
    bool operator>(const iterator& rhs) const;

    // Iterator dereference.
    value_type& operator*();
    value_type* operator->();

    // Return the underlying iterator.
    iterator it() const;

  private:
    iterator i, e;
};


//
// const_each class implementation.
//

template <typename C>
inline const_each<C>::const_each(const C& c)
  : i(c.begin())
  , e(c.end())
{
}

template <typename C>
inline const_each<C>::operator bool() const
{
    return i != e;
}

template <typename C>
inline const_each<C>& const_each<C>::operator++()
{
    ++i;
    return *this;
}

template <typename C>
inline bool const_each<C>::operator==(const const_iterator& rhs) const
{
    return i == rhs;
}

template <typename C>
inline bool const_each<C>::operator!=(const const_iterator& rhs) const
{
    return i != rhs;
}

template <typename C>
inline bool const_each<C>::operator<(const const_iterator& rhs) const
{
    return i < rhs;
}

template <typename C>
inline bool const_each<C>::operator>(const const_iterator& rhs) const
{
    return i > rhs;
}

template <typename C>
inline const typename const_each<C>::value_type& const_each<C>::operator*() const
{
    return *i;
}

template <typename C>
inline const typename const_each<C>::value_type* const_each<C>::operator->() const
{
    return &(*i);
}

template <typename C>
inline typename const_each<C>::const_iterator const_each<C>::it() const
{
    return i;
}


//
// each class implementation.
//

template <typename C>
inline each<C>::each(C& c)
  : i(c.begin())
  , e(c.end())
{
}

template <typename C>
inline each<C>::operator bool() const
{
    return i != e;
}

template <typename C>
inline each<C>& each<C>::operator++()
{
    ++i;
    return *this;
}

template <typename C>
inline bool each<C>::operator==(const iterator& rhs) const
{
    return i == rhs;
}

template <typename C>
inline bool each<C>::operator!=(const iterator& rhs) const
{
    return i != rhs;
}

template <typename C>
inline bool each<C>::operator<(const iterator& rhs) const
{
    return i < rhs;
}

template <typename C>
inline bool each<C>::operator>(const iterator& rhs) const
{
    return i > rhs;
}

template <typename C>
inline typename each<C>::value_type& each<C>::operator*()
{
    return *i;
}

template <typename C>
inline typename each<C>::value_type* each<C>::operator->()
{
    return &(*i);
}

template <typename C>
inline typename each<C>::iterator each<C>::it() const
{
    return i;
}

}   // namespace foundation
