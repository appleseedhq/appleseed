
#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
# Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#

# The appleseed.python module built into appleseed.studio
# is called _appleseedpythonbuiltin. Try to load it first.
# If that fails it means that we are not in appleseed.studio;
# in that case just load the normal appleseed.python module.
try:
    from _appleseedpythonbuiltin import *
except ImportError:
    from sys import hexversion as appleseed_python_hexversion

    if appleseed_python_hexversion < 0x030000F0:
        # Python 2.X
        from _appleseedpython import *
    else:
        # Python 3.X
        from ._appleseedpython3 import *

class ConsoleLogTarget(ILogTarget):
    def __init__(self, stream):
        super().__init__()
        self.__stream = stream

    def write(self, category, file, line, header, message):
        lines = message.split('\n')
        for line in lines:
            self.__stream.write(header + line + '\n')


class FileLogTarget(ILogTarget):
    def __init__(self):
        super().__init__()
        self.__file = None

    def open(self, filename):
        if self.is_open():
            self.close()

        self.__file = open(filename, "w")

    def close(self):
        if self.is_open():
            self.__file.close()
            self.__file = None

    def is_open(self):
        return self.__file is not None

    def write(self, category, file, line, header, message):
        if self.is_open():
            lines = message.split('\n')
            for line in lines:
                self.__file.write(header + line + '\n')