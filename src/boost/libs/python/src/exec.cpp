// Copyright Stefan Seefeld 2005.
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <boost/python/exec.hpp>
#include <boost/python/borrowed.hpp>
#include <boost/python/extract.hpp>
#include <boost/python/handle.hpp>

namespace boost 
{ 
namespace python 
{

object BOOST_PYTHON_DECL eval(str string, object global, object local)
{
  // should be 'char const *' but older python versions don't use 'const' yet.
  char *s = python::extract<char *>(string);
  PyObject* result = PyRun_String(s, Py_eval_input, global.ptr(), local.ptr());
  if (!result) throw_error_already_set();
  return object(detail::new_reference(result));
}

object BOOST_PYTHON_DECL exec(str string, object global, object local)
{
  // should be 'char const *' but older python versions don't use 'const' yet.
  char *s = python::extract<char *>(string);
  PyObject* result = PyRun_String(s, Py_file_input, global.ptr(), local.ptr());
  if (!result) throw_error_already_set();
  return object(detail::new_reference(result));
}

// Execute python source code from file filename.
// global and local are the global and local scopes respectively,
// used during execution.
object BOOST_PYTHON_DECL exec_file(str filename, object global, object local)
{
  // should be 'char const *' but older python versions don't use 'const' yet.
  char *f = python::extract<char *>(filename);
  // Let python open the file to avoid potential binary incompatibilities.
  PyObject *pyfile = PyFile_FromString(f, "r");
  if (!pyfile) throw std::invalid_argument(std::string(f) + " : no such file");
  python::handle<> file(pyfile);
  PyObject* result = PyRun_File(PyFile_AsFile(file.get()),
                f,
                Py_file_input,
                global.ptr(), local.ptr());
  if (!result) throw_error_already_set();
  return object(detail::new_reference(result));
}

}  // namespace boost::python
}  // namespace boost
