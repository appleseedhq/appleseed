// Copyright David Abrahams 2002.
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
#include <boost/python/list.hpp>
#include <boost/python/ssize_t.hpp>

namespace boost { namespace python { namespace detail {


detail::new_non_null_reference list_base::call(object const& arg_)
{
    return (detail::new_non_null_reference)
        (expect_non_null)(
            PyObject_CallFunction(
                (PyObject*)&PyList_Type, "(O)", 
                arg_.ptr()));
}

list_base::list_base()
    : object(detail::new_reference(PyList_New(0)))
{}

list_base::list_base(object_cref sequence)
    : object(list_base::call(sequence))
{}

void list_base::append(object_cref x)
{
    if (PyList_CheckExact(this->ptr()))
    {
        if (PyList_Append(this->ptr(), x.ptr()) == -1)
            throw_error_already_set();
    }
    else
    {
        this->attr("append")(x);
    }
}

//long list_base::count(object_cref value) const;

void list_base::extend(object_cref sequence)
{
    this->attr("extend")(sequence);
}

long list_base::index(object_cref value) const
{
    object result_obj(this->attr("index")(value));
    long result = PyInt_AsLong(result_obj.ptr());
    if (result == -1)
        throw_error_already_set();
    return result;
}

void list_base::insert(ssize_t index, object_cref item)
{
    if (PyList_CheckExact(this->ptr()))
    {
        if (PyList_Insert(this->ptr(), index, item.ptr()) == -1)
            throw_error_already_set();
    }
    else
    {
        this->attr("insert")(index, item);
    }
}

void list_base::insert(object const& index, object_cref x)
{
    long index_ = PyInt_AsLong(index.ptr());
    if (index_ == -1 && PyErr_Occurred())
        throw_error_already_set();
    this->insert(index_, x);
}

object list_base::pop()
{
    return this->attr("pop")();
}

object list_base::pop(ssize_t index)
{
    return this->pop(object(index));
}

object list_base::pop(object const& index)
{
    return this->attr("pop")(index);
}

void list_base::remove(object_cref value)
{
    this->attr("remove")(value);
}
    
void list_base::reverse()
{
    if (PyList_CheckExact(this->ptr()))
    {
        if (PyList_Reverse(this->ptr()) == -1)
            throw_error_already_set();
    }
    else
    {
        this->attr("reverse")();
    }
}

void list_base::sort()
{
    if (PyList_CheckExact(this->ptr()))
    {
        if (PyList_Sort(this->ptr()) == -1)
            throw_error_already_set();
    }
    else
    {
        this->attr("sort")();
    }
}

void list_base::sort(object_cref cmpfunc)
{
    this->attr("sort")(cmpfunc);
}

// For some reason, moving this to the end of the TU suppresses an ICE
// with vc6.
long list_base::count(object_cref value) const
{
    object result_obj(this->attr("count")(value));
    long result = PyInt_AsLong(result_obj.ptr());
    if (result == -1)
        throw_error_already_set();
    return result;
}

static struct register_list_pytype_ptr
{
    register_list_pytype_ptr()
    {
        const_cast<converter::registration &>(
            converter::registry::lookup(boost::python::type_id<boost::python::list>())
            ).m_class_object = &PyList_Type;
    }
}register_list_pytype_ptr_;

}}} // namespace boost::python
