//  (C) Copyright David Abrahams 2000.
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//
//  The author gratefully acknowleges the support of Dragon Systems, Inc., in
//  producing this work.

#include <boost/python/scope.hpp>
#include <boost/python/object/add_to_namespace.hpp>

namespace boost { namespace python { namespace detail {

BOOST_PYTHON_DECL void scope_setattr_doc(char const* name, object const& x, char const* doc)
{
    // Use function::add_to_namespace to achieve overloading if
    // appropriate.
    scope current;
    objects::add_to_namespace(current, name, x, doc);
}

namespace
{
  PyMethodDef initial_methods[] = { { 0, 0, 0, 0 } };
}

BOOST_PYTHON_DECL void init_module(char const* name, void(*init_function)())
{
    
    PyObject* m
        = Py_InitModule(const_cast<char*>(name), initial_methods);

    if (m != 0)
    {
        // Create the current module scope
        object m_obj(((borrowed_reference_t*)m));
        scope current_module(m_obj);
        
        handle_exception(init_function);
    }
}

}}} // namespace boost::python::detail

namespace boost { namespace python {

namespace detail
{
  BOOST_PYTHON_DECL PyObject* current_scope = 0;
}

}}
