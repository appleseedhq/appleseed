
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
import traceback


def load_plugins(bundled_plugins_path):
    if os.path.isdir(bundled_plugins_path):
        print("Loading Python plugins from {0}...".format(bundled_plugins_path))
        load_plugins_from_dir(bundled_plugins_path)

    user_plugins_path = os.environ.get('APPLESEED_STUDIO_PLUGIN_PATH')
    if user_plugins_path is not None:
        print("Loading Python plugins from {0}...".format(user_plugins_path))
        load_plugins_from_dir(user_plugins_path)


def load_plugins_from_dir(bundled_plugins_path):
    for plugin in os.listdir(bundled_plugins_path):
        plugin_path = os.path.join(bundled_plugins_path, plugin)

        if os.path.isdir(plugin_path):
            load_plugin(plugin_path)


def load_plugin(plugin_path):
    path, name = os.path.split(plugin_path)
    name, ext = os.path.splitext(name)

    try:
        file, filename, data = imp.find_module(name, [path])
        plugin_module = imp.load_module(name, file, filename, data)
    except ImportError as e:
        print("Plugin '{0}' could not be imported: {1}".format(name, e))
        return

    if not hasattr(plugin_module, 'register'):
        print("Plugin '{0}' has no register function.".format(name))
        return

    try:
        plugin_module.register()
    except Exception as e:
        print("Could not initialize plugin '{0}': {1}".format(name, e))
        traceback.print_exc()
        return

    print("Plugin '{0}' successfully imported.".format(name))
