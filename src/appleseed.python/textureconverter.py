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


import os
import sys
import logging
logging.basicConfig(level=logging.INFO, stream=sys.stdout)

class TextureConverter(object):
    def __init__(self, maketx_path):
        self.converted = {}
        self.maketx_path = maketx_path

    def convert(self, path):
        if path in self.converted:
            return self.converted[path]
        else:
            path_converted = self._convert_with_maketx(path)
            self.converted[path] = path_converted
            return path_converted

    def _convert_with_maketx(self, path):
        base_path, _ = os.path.splitext(path)
        tx_path = base_path + ".tx"

        if os.path.exists(tx_path):
            logging.warning('{} already exists.'.format(tx_path))
            return None

        status = os.system('{} -o "{}" "{}"'.format(self.maketx_path, tx_path, path))

        if status != 0:
            logging.error('maketx failed with error code {}.'.format(status))
            return None

        return tx_path
