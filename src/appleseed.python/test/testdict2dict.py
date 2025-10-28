
#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2015-2018 Esteban Tovagliari, The appleseedhq Organization
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

import unittest
import appleseed as asr


class TestDict2Dict(unittest.TestCase):
    """
    Test conversion between Python dictionaries and appleseed's ParamArray.
    """

    def setUp(self):
        pass

    def test_roundtrip(self):
        src_params = {
            'int': 1,
            'positive_long': int(8 * 1024 * 1024 * 1024),
            'negative_long': int(-8 * 1024 * 1024 * 1024),
            'float': 2.0,
            'string': 'string',
            'bool': False,
            'vector2d' : asr.Vector2d(3.0, 4.0),
            'vector3d' : asr.Vector3d(3.0, 4.0, 5.0)
        }

        light = asr.Light('point_light', 'light', src_params)
        dst_params = light.get_parameters()

        self.assertEqual(src_params, dst_params)

    def tearDown(self):
        pass

if __name__ == "__main__":
    unittest.main()
