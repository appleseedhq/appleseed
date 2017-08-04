
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2017 Gleb Mishchenko, The appleseedhq Organization
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

import Qt

def wrapinstance(addr, type):
    # Function won't change and will throw Exception for PySide2 and PyQt5.
    raise Exception("No wrapinstance function defined for " + Qt.__binding__)

def resolve_wrapper():
    global wrapinstance

    if Qt.__binding__ == 'PyQt4':
        # Make an import to global namespace
        global sip
        import sip

        def _wrapinstance(addr, type):
            return sip.wrapinstance(addr, type)

        wrapinstance = _wrapinstance

    elif Qt.__binding__ == 'PySide':
        # Make an import to global namespace
        global shiboken
        import shiboken

        def _wrapinstance(addr, type):
            return shiboken.wrapInstance(addr, type)

        wrapinstance = _wrapinstance

resolve_wrapper()
