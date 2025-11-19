#!/usr/bin/python

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

import argparse
import os
import sys


# -------------------------------------------------------------------------------------------------
# Constants.
# -------------------------------------------------------------------------------------------------

DISNEY_BRDF_KEYS = set(["anisotropic", "baseColorB", "baseColorG", "baseColorR", "clearcoat", "clearcoatGloss", "metallic", "roughness",
                        "sheen", "sheenTint", "specular", "specularTint", "subsurface", "subsurfaceColorB", "subsurfaceColorG", "subsurfaceColorR"])


# -------------------------------------------------------------------------------------------------
# Entry point.
# -------------------------------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Converts RFM Disney BRDF presets to appleseed Disney Materials.")
    parser.add_argument("-s", "--stop-error", dest="stop_error", action="store_true", help="stop on errors")
    parser.add_argument("-o", "--output", metavar="output-directory", dest="output_directory",
                        required=True, help="directory where appleseed materials will be stored.")
    parser.add_argument("directory", help="directory to scan for RFM Disney BRDF preset files.")
    args = parser.parse_args()

    for root, dirname, files in os.walk(args.directory):
        for filename in files:
            if filename.endswith(".mel"):
                print("Processing file: " + os.path.join(root, filename))

                material_name = filename.replace(".mel", "")

                in_file = open(os.path.join(root, filename), "r")
                lines = in_file.readlines()

                params = {}

                for line in lines:
                    l = line.strip()

                    if l.startswith("blendAttr"):
                        tokens = l.split()
                        param_name = tokens[1].replace('"', "")
                        value = tokens[2].replace(";", "")
                        params[param_name] = value

                # Check if the material is a Disney BRDF
                if not DISNEY_BRDF_KEYS.issubset(set(params.keys())):
                    print(f"Error: material preset {material_name} is not a Disney BRDF.")

                    if args.stop_error:
                        sys.exit(1)
                    else:
                        continue

                # Check the material for possible incompatibilities
                if params["emitColorR"] != "0.0" or params["emitColorG"] != "0.0" or params["emitColorB"] != "0.0":
                    print(f"Error: material {material_name} preset uses emision.")

                    if args.stop_error:
                        sys.exit(1)

                # Subsurface color is missing in appleseed's Disney BRDF implementation.
                if params["subsurface"] != "0.0":
                    if params["subsurfaceColorR"] != params["baseColorR"] or params["subsurfaceColorG"] != params["baseColorG"] or params["subsurfaceColorB"] != params["baseColorB"]:
                        print(f"Error: material preset {material_name} uses subsurface color.")

                        if args.stop_error:
                            sys.exit(-1)

                out_filename = os.path.join(args.output_directory, material_name + ".dmt")
                print("Generating file: " + out_filename)

                out_file = open(out_filename, "w")

                out_file.write('<?xml version="1.0" encoding="UTF-8"?>\n')
                out_file.write('<settings>\n')
                out_file.write('    <parameter name="__model" value="disney_material" />\n')
                out_file.write('    <parameter name="__name" value="%s" />\n' % material_name)
                out_file.write('    <parameter name="bump_amplitude" value="1.0" />\n')
                out_file.write('    <parameter name="displacement_method" value="bump" />\n')
                out_file.write('    <parameter name="normal_map_up" value="z" />\n')
                out_file.write('    <parameter name="shade_alpha_cutouts" value="false" />\n')
                out_file.write('    <parameters name="layer1">\n')
                out_file.write('        <parameter name="layer_folded" value="false" />\n')
                out_file.write('        <parameter name="layer_name" value="layer1" />\n')
                out_file.write('        <parameter name="layer_number" value="0" />\n')
                out_file.write('        <parameter name="mask" value="1.0" />\n')

                base_color = (params['baseColorR'], params['baseColorG'], params['baseColorB'])
                out_file.write('        <parameter name="base_color" value="[%s, %s, %s]" />\n' % base_color)
                out_file.write('        <parameter name="anisotropic" value="%s" />\n' % params['anisotropic'])
                out_file.write('        <parameter name="clearcoat" value="%s" />\n' % params['clearcoat'])
                out_file.write('        <parameter name="clearcoat_gloss" value="%s" />\n' % params['clearcoatGloss'])
                out_file.write('        <parameter name="metallic" value="%s" />\n' % params['metallic'])
                out_file.write('        <parameter name="roughness" value="%s" />\n' % params['roughness'])
                out_file.write('        <parameter name="sheen" value="%s" />\n' % params['sheen'])
                out_file.write('        <parameter name="sheen_tint" value="%s" />\n' % params['sheenTint'])
                out_file.write('        <parameter name="specular" value="%s" />\n' % params['specular'])
                out_file.write('        <parameter name="specular_tint" value="%s" />\n' % params['specularTint'])
                out_file.write('        <parameter name="subsurface" value="%s" />\n' % params['subsurface'])
                out_file.write('    </parameters>\n')
                out_file.write('</settings>\n')

if __name__ == "__main__":
    main()

