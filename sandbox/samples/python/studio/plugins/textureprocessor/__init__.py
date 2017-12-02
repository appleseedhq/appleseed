
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

import appleseed as asr
import appleseed.studio as studio
from appleseed.studio import ui
from appleseed.studio.Qt import QtWidgets
from appleseed.textureconverter import *

import os
import sys
import logging

logging.basicConfig(level=logging.INFO, stream=sys.stdout)


def register():
    menu = ui.find_or_create_menu("Plugins")

    act = QtWidgets.QAction("Convert Textures", menu)
    act.triggered.connect(convert_all_textures_to_tx)

    menu.addAction(act)


def convert_all_textures_to_tx():
    def _find_maketx():
        root_path = studio.get_root_path()
        maketx_path = os.path.join(root_path, 'bin', 'maketx')

        if os.path.exists(maketx_path):
            return maketx_path
        else:
            raise Exception('maketx binary not found')

    project = studio.current_project()

    if project is None:
        return

    scene = project.get_scene()
    textures = get_textures(scene)

    tx_converter = TextureConverter(_find_maketx())

    for texture in textures:
        if texture.get_model() != 'disk_texture_2d':
            continue

        texture_parameters = texture.get_parameters()

        texture_path = texture_parameters['filename']
        texture_full_path = get_full_path(texture_path, project)

        if texture_full_path.endswith('.tx'):
            logging.debug('Skipped conversion of {}'.format(texture_full_path))
            continue

        new_texture_full_path = tx_converter.convert(texture_full_path)

        if new_texture_full_path is None:
            logging.info('Skipped conversion of {}'.format(texture_full_path))
        else:
            new_texture_path = os.path.join(os.path.dirname(texture_path),
                                            os.path.basename(new_texture_full_path))

            texture_parameters['filename'] = new_texture_path
            texture.set_parameters(texture_parameters)
            studio.set_project_dirty()
            logging.info('{} converted to {}'.format(texture_path, new_texture_path))


def get_textures(container):
    assert isinstance(container, asr.BaseGroup)

    textures = list(container.textures())

    assemblies = container.assemblies()
    for key in assemblies:
        textures += get_textures(assemblies[key])

    return textures


def get_full_path(texture_path, project):
    if os.path.isabs(texture_path):
        return texture_path
    else:
        return project.qualify_path(texture_path)
