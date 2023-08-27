#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2016-2018 Esteban Tovagliari, The appleseedhq Organization
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


class TestEntityMap(unittest.TestCase):
    """
    Basic entity map tests.
    """

    def setUp(self):
        self.scn = asr.Scene()
        self.assembly_map = self.scn.assemblies()

    def test_get_by_name(self):
        ass = asr.Assembly("assembly", {})
        self.assembly_map.insert(ass)

        ass = asr.Assembly("another_assembly", {})
        self.assembly_map.insert(ass)

        a = self.assembly_map.get_by_name("assembly")
        self.assertEqual(a.get_name(), "assembly")

        a = self.assembly_map.get_by_name("another_assembly")
        self.assertEqual(a.get_name(), "another_assembly")

        a = self.assembly_map.get_by_name("no_such_assembly")
        self.assertEqual(a, None)

    def test_get_by_uuid(self):
        ass = asr.Assembly("assembly")
        uid1 = ass.get_uid()
        self.assembly_map.insert(ass)

        ass = asr.Assembly("another_assembly")
        uid2 = ass.get_uid()
        self.assembly_map.insert(ass)

        a = self.assembly_map.get_by_uid(uid1)
        self.assertEqual(a.get_name(), "assembly")

        a = self.assembly_map.get_by_uid(uid2)
        self.assertEqual(a.get_name(), "another_assembly")

        a = self.assembly_map.get_by_uid(77567)
        self.assertEqual(a, None)

    def test_get_item(self):
        ass = asr.Assembly("assembly")
        uid1 = ass.get_uid()
        self.assembly_map.insert(ass)

        ass = asr.Assembly("another_assembly")
        uid2 = ass.get_uid()
        self.assembly_map.insert(ass)

        self.assertEqual(self.assembly_map["assembly"].get_uid(), uid1)
        self.assertEqual(self.assembly_map["another_assembly"].get_uid(), uid2)

    def test_insert_remove_by_uid(self):
        ass = asr.Assembly("assembly")
        self.assembly_map.insert(ass)
        self.assertEqual(len(self.assembly_map), 1)

        a = self.assembly_map.get_by_name("assembly")
        ass = self.assembly_map.remove_by_uid(a.get_uid())
        self.assertEqual(len(self.assembly_map), 0)

        self.assembly_map.insert(ass)
        self.assertEqual(len(self.assembly_map), 1)

    def test_keys(self):
        self.assertEqual(list(self.assembly_map.keys()), [])

        ass = asr.Assembly("assembly")
        self.assembly_map.insert(ass)

        ass = asr.Assembly("another_assembly")
        self.assembly_map.insert(ass)

        self.assertEqual(list(self.assembly_map.keys()), ["assembly", "another_assembly"])

    def test_values(self):
        ass = asr.Assembly("assembly")
        uid1 = ass.get_uid()
        self.assembly_map.insert(ass)

        ass = asr.Assembly("another_assembly")
        uid2 = ass.get_uid()
        self.assembly_map.insert(ass)

        values = list(self.assembly_map.values())
        self.assertEqual(len(values), 2)
        self.assertEqual(values[0].get_uid(), uid1)
        self.assertEqual(values[1].get_uid(), uid2)

    def test_iters(self):
        names = ['assembly', 'assembly2', 'assembly3']
        uids = []

        for name in names:
            ass = asr.Assembly(name)
            uids.append(ass.get_uid())
            self.assembly_map.insert(ass)

        result_names = []
        result_uids = []
        for ass in self.assembly_map:
            result_names.append(ass)
            result_uids.append(self.assembly_map[ass].get_uid())

        self.assertEqual(sorted(names), sorted(result_names))
        self.assertEqual(sorted(uids), sorted(result_uids))

if __name__ == "__main__":
    unittest.main()

