// Copyright Vladimir Prus 2002-2004.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt
// or copy at http://www.boost.org/LICENSE_1_0.txt)


#include <boost/program_options/options_description.hpp>
using namespace boost::program_options;

#include <boost/function.hpp>
using namespace boost;

#include <utility>
#include <string>
#include <sstream>
using namespace std;

#include "minitest.hpp"

void test_type()
{
    options_description desc;
    desc.add_options()
        ("foo", value<int>(), "")
        ("bar", value<std::string>(), "")
        ;
    
    const typed_value_base* b = dynamic_cast<const typed_value_base*>
        (desc.find("foo", false).semantic().get());
    BOOST_CHECK(b);
    BOOST_CHECK(b->value_type() == typeid(int));

    const typed_value_base* b2 = dynamic_cast<const typed_value_base*>
        (desc.find("bar", false).semantic().get());
    BOOST_CHECK(b2);
    BOOST_CHECK(b2->value_type() == typeid(std::string));
}

void test_approximation()
{
    options_description desc;
    desc.add_options()
        ("foo", new untyped_value())
        ("fee", new untyped_value())
        ("baz", new untyped_value())
        ("all-chroots", new untyped_value())
        ("all-sessions", new untyped_value())
        ("all", new untyped_value())
        ;

    BOOST_CHECK_EQUAL(desc.find("fo", true).long_name(), "foo");

    BOOST_CHECK_EQUAL(desc.find("all", true).long_name(), "all");
    BOOST_CHECK_EQUAL(desc.find("all-ch", true).long_name(), "all-chroots");


//    BOOST_CHECK(desc.count_approx("foo") == 1);
//    set<string> a = desc.approximations("f");
//    BOOST_CHECK(a.size() == 2);
//    BOOST_CHECK(*a.begin() == "fee");
//    BOOST_CHECK(*(++a.begin()) == "foo");
}

void test_formatting()
{
    // Long option descriptions used to crash on MSVC-8.0.
    options_description desc;
    desc.add_options()(
        "test", new untyped_value(),
        "foo foo foo foo foo foo foo foo foo foo foo foo foo foo"
        "foo foo foo foo foo foo foo foo foo foo foo foo foo foo"
        "foo foo foo foo foo foo foo foo foo foo foo foo foo foo"
        "foo foo foo foo foo foo foo foo foo foo foo foo foo foo");

    stringstream ss;
    ss << desc;
}

int main(int, char* [])
{
    test_type();
    test_approximation();
    test_formatting();
    return 0;
}
