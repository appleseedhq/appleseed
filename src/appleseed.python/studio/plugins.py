
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2017 Esteban Tovagliari, The appleseedhq Organization
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

import os
import imp

def load_plugins(bundled_plugins_path):
    for plugin in os.listdir(bundled_plugins_path):
        plugin_path = os.path.join(bundled_plugins_path, plugin)

        if not os.path.isdir(plugin_path):
            continue

        path, name = os.path.split(plugin_path)
        name, ext = os.path.splitext(name)

        try:
            file, filename, data = imp.find_module(name, [path])
            plugin_module = imp.load_module(name, file, filename, data)
        except ImportError as e:
            print "*** plugin %s could not be imported, exc=%s" % (plugin, e)
            continue

        if not hasattr(plugin_module, 'register'):
            print "*** plugin %s has no register function" % plugin
            continue

        try:
            plugin_module.register()
        except Exception as e:
            print "*** Could not init plugin %s, exc=%s", (plugin_module, e)
            continue