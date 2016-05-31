
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2016 Esteban Tovagliari, The appleseedhq Organization
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

class TestEntityVector(unittest.TestCase):
    """
        Basic entity vector tests.
    """

    def setUp(self):
        self.ass = asr.Assembly("test_assembly")
        self.color_vec = self.ass.colors()

    def test_get_by_name(self):
        col = asr.ColorEntity("color", {})
        self.color_vec.insert(col)

        col = asr.ColorEntity("another_color", {})
        self.color_vec.insert(col)

        c = self.color_vec.get_by_name("color")
        self.assertEqual(c.get_name(), "color")

        c = self.color_vec.get_by_name("another_color")
        self.assertEqual(c.get_name(), "another_color")

        c = self.color_vec.get_by_name("no_such_color")
        self.assertEqual(c, None)

    def test_get_by_uuid(self):
        col = asr.ColorEntity("color", {})
        uid1 = col.get_uid()
        self.color_vec.insert(col)

        col = asr.ColorEntity("another_color", {})
        uid2 = col.get_uid()
        self.color_vec.insert(col)

        c = self.color_vec.get_by_uid(uid1)
        self.assertEqual(c.get_name(), "color")

        c = self.color_vec.get_by_uid(uid2)
        self.assertEqual(c.get_name(), "another_color")

        c = self.color_vec.get_by_uid(77567)
        self.assertEqual(c, None)

    def test_insert_remove(self):
        col = asr.ColorEntity("color", {})
        self.color_vec.insert(col)
        self.assertEqual(len(self.color_vec), 1)

        c= self.color_vec.get_by_name("color")
        col = self.color_vec.remove(c)
        self.assertEqual(len(self.color_vec), 0)

        self.color_vec.insert(col)
        self.assertEqual(len(self.color_vec), 1)

    def test_get_item(self):
        col = asr.ColorEntity("color", {})
        self.color_vec.insert(col)

        col = asr.ColorEntity("another_color", {})
        self.color_vec.insert(col)

        self.assertEqual(self.color_vec[0].get_name(), "color")
        self.assertEqual(self.color_vec[1].get_name(), "another_color")
        self.assertEqual(self.color_vec[-1].get_name(), "another_color")

    def test_iters(self):
        names = ['color', 'color2', 'color3']
        uids = []

        for name in names:
            col = asr.ColorEntity(name, {})
            uids.append(col.get_uid())
            self.color_vec.insert(col)

        result_names = []
        result_uids = []
        for col in self.color_vec:
            result_names.append(col.get_name())
            result_uids.append(col.get_uid())

        self.assertEqual(names, result_names)
        self.assertEqual(uids, result_uids)

if __name__ == "__main__":
    unittest.main()
