// Copyright David Abrahams 2004. Distributed under the Boost
// Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// This is really an incomplete test; should be fleshed out.

#include <boost/iterator/iterator_facade.hpp>
#include <boost/iterator/new_iterator_tests.hpp>

// This is a really, really limited test so far.  All we're doing
// right now is checking that the postfix++ proxy for single-pass
// iterators works properly.
template <class Ref>
class counter_iterator
  : public boost::iterator_facade<
        counter_iterator<Ref>
      , int const
      , boost::single_pass_traversal_tag
      , Ref
    >
{
 public:
    counter_iterator() {}
    counter_iterator(int* state) : state(state) {}

    void increment()
    {
        ++*state;
    }

    Ref
    dereference() const
    {
        return *state;
    }

    bool equal(counter_iterator const& y) const
    {
        return *this->state == *y.state;
    }

    int* state;
};

struct proxy
{
    proxy(int& x) : state(x) {}
        
    operator int const&() const
    {
        return state;
    }

    int& operator=(int x) { state = x; return state; }
        
    int& state;
};

struct value
{
    void mutator() {} // non-const member function
};

struct input_iter
  : boost::iterator_facade<
        input_iter
      , value
      , boost::single_pass_traversal_tag
      , value
    >
{
 public:
    input_iter() {}

    void increment()
    {
    }
    value
    dereference() const
    {
        return value();
    }

    bool equal(input_iter const& y) const
    {
        return false;
    }
};

int main()
{
    int state = 0;
    boost::readable_iterator_test(counter_iterator<int const&>(&state), 0);
    state = 3;
    boost::readable_iterator_test(counter_iterator<proxy>(&state), 3);
    boost::writable_iterator_test(counter_iterator<proxy>(&state), 9, 7);
    BOOST_TEST(state == 8);

    // test for a fix to http://tinyurl.com/zuohe
    // These two lines should be equivalent (and both compile)
    input_iter p;
    (*p).mutator();
    p->mutator();
    
    return boost::report_errors();
}
