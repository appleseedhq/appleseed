
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2016-2017 Francois Beaune, The appleseedhq Organization
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


class TestBasis(unittest.TestCase):

    def setUp(self):
        pass

    def test_transform_to_local(self):
        basis = asr.Basis3f(asr.Vector3f(1.0, 0.0, 0.0),    # normal
                            asr.Vector3f(0.0, 0.0, 1.0),    # u
                            asr.Vector3f(0.0, 1.0, 0.0))    # v

        v = basis.transform_to_local(asr.Vector3f(1.0, 2.0, 3.0))

        self.assertEqual(asr.Vector3f(3.0, 1.0, 2.0), v)

    def test_transform_to_parent(self):
        basis = asr.Basis3f(asr.Vector3f(1.0, 0.0, 0.0),    # normal
                            asr.Vector3f(0.0, 0.0, 1.0),    # u
                            asr.Vector3f(0.0, 1.0, 0.0))    # v

        v = basis.transform_to_parent(asr.Vector3f(3.0, 1.0, 2.0))

        self.assertEqual(asr.Vector3f(1.0, 2.0, 3.0), v)

    def tearDown(self):
        pass

if __name__ == "__main__":
    unittest.main()
