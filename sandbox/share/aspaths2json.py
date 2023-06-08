#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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

import argparse
import struct
import sys


# -------------------------------------------------------------------------------------------------
# Utility functions.
# -------------------------------------------------------------------------------------------------

def fatal(message):
    print((f"Fatal: {message}. Aborting."))
    sys.exit(1)


# -------------------------------------------------------------------------------------------------
# A basic JSON writer that does not require keeping the whole JSON object in memory, and with just
# enough features for our needs. Misses methods like begin_object_prop(), end_object_prop(), etc.
# -------------------------------------------------------------------------------------------------

class JsonWriter:

    def __init__(self, f):
        self.file = f
        self.indent = 0
        self.need_comma = False

    def begin_file(self):
        self.begin_object()

    def end_file(self):
        self.end_object()
        self.file.write("\n")

    def write_prop(self, name, value):
        if self.need_comma:
            self.file.write(",\n")
        self.__write_indented('"{0}": {1}'.format(name, value))
        self.need_comma = True

    def begin_array_prop(self, name):
        if self.need_comma:
            self.file.write(",\n")
        self.__write_indented('"{0}": [\n'.format(name))
        self.indent += 1
        self.need_comma = False

    def end_array_prop(self):
        self.file.write("\n")
        self.indent -= 1
        self.__write_indented("]")
        self.need_comma = True

    def begin_object(self):
        if self.need_comma:
            self.file.write(",\n")
        self.__write_indented("{\n")
        self.indent += 1
        self.need_comma = False

    def end_object(self):
        self.file.write("\n")
        self.indent -= 1
        self.__write_indented("}")
        self.need_comma = True

    def write_string(self, s):
        if self.need_comma:
            self.file.write(",\n")
        self.__write_indented('"{0}"'.format(s))
        self.need_comma = True

    def __write_indented(self, line):
        self.file.write(" " * 4 * self.indent)
        self.file.write(line)


# -------------------------------------------------------------------------------------------------
# Conversion code.
# The Light Paths File format is described in the following page on appleseed's wiki:
#   https://github.com/appleseedhq/appleseed/wiki/Light-Paths-File-Format
# -------------------------------------------------------------------------------------------------

def convert(input_file, output_file):
    json = JsonWriter(output_file)
    json.begin_file()

    # Parse and check signature.
    sig = input_file.read(7)
    if sig != "ASPATHS":
        fatal("Invalid Light Paths file")

    # Parse and check file format version.
    version, = struct.unpack('<H', input_file.read(2))
    if version != 1:
        fatal("Unsupported Light Paths file format version: {0}".format(version))
    json.write_prop("version", version)

    # Parse number of light paths.
    total_path_count, = struct.unpack('<L', input_file.read(4))
    json.write_prop("totalPathCount", total_path_count)

    # Parse width and height of index.
    index_width, = struct.unpack('<H', input_file.read(2))
    index_height, = struct.unpack('<H', input_file.read(2))
    json.write_prop("indexWidth", index_width)
    json.write_prop("indexHeight", index_height)

    # Parse index.
    json.begin_array_prop("index")
    for i in range(0, index_width * index_height):
        first_path_offset, = struct.unpack('<Q', input_file.read(8))
        path_count, = struct.unpack('<H', input_file.read(2))
        json.begin_object()
        json.write_prop("firstPathOffset", first_path_offset)
        json.write_prop("pathCount", path_count)
        json.end_object()
    json.end_array_prop()

    # Parse name table.
    json.begin_array_prop("names")
    name_count, = struct.unpack('<H', input_file.read(2))
    for i in range(0, name_count):
        name_length, = struct.unpack('<H', input_file.read(2))
        name = input_file.read(name_length)
        json.write_string(name)
    json.end_array_prop()

    # Parse light paths.
    json.begin_array_prop("paths")
    for i in range(0, total_path_count):
        json.begin_object()

        # Parse NDC image sample coordinates.
        ndc_x, = struct.unpack('<f', input_file.read(4))
        ndc_y, = struct.unpack('<f', input_file.read(4))
        json.write_prop("ndcX", ndc_x)
        json.write_prop("ndcY", ndc_y)

        # Parse light path vertices.
        json.begin_array_prop("vertices")
        vertex_count, = struct.unpack('<H', input_file.read(2))
        for j in range(0, vertex_count):
            json.begin_object()

            # Parse index in name table of the name of the object this vertex lies on.
            object_name_index, = struct.unpack('<H', input_file.read(2))
            json.write_prop("objectNameIndex", object_name_index)

            # Parse world space position of this vertex.
            vertex_x, = struct.unpack('<f', input_file.read(4))
            vertex_y, = struct.unpack('<f', input_file.read(4))
            vertex_z, = struct.unpack('<f', input_file.read(4))
            json.write_prop("position", [vertex_x, vertex_y, vertex_z])

            # Parse radiance carried by this path up to this vertex.
            radiance_r, = struct.unpack('<f', input_file.read(4))
            radiance_g, = struct.unpack('<f', input_file.read(4))
            radiance_b, = struct.unpack('<f', input_file.read(4))
            json.write_prop("radiance", [radiance_r, radiance_g, radiance_b])

            json.end_object()   # vertex

        json.end_array_prop()   # vertices
        json.end_object()       # path

    json.end_array_prop()       # paths

    json.end_file()


# -------------------------------------------------------------------------------------------------
# Entry point.
# -------------------------------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="convert Light Paths files to JSON files.")
    parser.add_argument("input_file", metavar="input-file", help="Light Paths file (*.aspaths)")
    parser.add_argument("output_file", metavar="output-file", help="JSON file (*.json)")
    args = parser.parse_args()

    with open(args.input_file, "rb") as input_file:
        with open(args.output_file, "w") as output_file:
            convert(input_file, output_file)

if __name__ == '__main__':
    main()

