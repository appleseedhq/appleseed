import appleseed as asr
import appleseed.studio as studio
from appleseed.textureconverter import *

import os
import sys
import logging
logging.basicConfig(level=logging.INFO, stream=sys.stdout)

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


def convert_all_textures_to_tx():
    def _find_maketx():
        root_path = studio.get_root_path()
        maketx_path = os.path.join(root_path, 'bin', 'maketx')

        if os.path.exists(maketx_path):
            return maketx_path
        else:
            raise Exception('maketx binary is not found')

    project = studio.current_project()
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
            logging.info('{} converted to {}'.format(texture_path, new_texture_path))


def find_or_create_menu(menu_name):
    from appleseed.studio.Qt import QtCore, QtGui, QtWidgets
    from appleseed.studio.ui import wrapinstance

    ptr = studio.main_window()
    main_window = wrapinstance(long(ptr), QtWidgets.QMainWindow)
    menu_bar = main_window.menuBar()

    for menu in menu_bar.actions():
        cur_menu_name = menu.text().replace('&', '').lower()

        if cur_menu_name == menu_name.lower():
            return menu.menu()
    else:
        new_menu = QtWidgets.QAction(menu_name, main_window)
        new_menu.setMenu(QtWidgets.QMenu(main_window))

        menu_bar.addAction(new_menu)
        return new_menu.menu()