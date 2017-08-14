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


def convert_all_textures_to_tx(maketx_path):
    project = studio.current_project()
    scene = project.get_scene()
    textures = get_textures(scene)

    tx_converter = TextureConverter(maketx_path)

    for texture in textures:
        texture_parameters = texture.get_parameters()
        texture_path = texture_parameters['filename']

        logging.debug("Processing {}".format(texture_path))
        print texture_path

        texture_full_path = get_full_path(texture_path, project)

        if texture.get_model() != 'disk_texture_2d' or texture_full_path.endswith('.tx'):
            logging.debug('Skipped converting {}'.format(texture_full_path))
            continue

        new_texture_full_path = tx_converter.convert(texture_full_path)

        if new_texture_full_path is None:
            logging.info('Skipped converting of {}'.format(texture_full_path))
        else:
            new_texture_path = os.path.join(os.path.dirname(texture_path),
                                            os.path.basename(new_texture_full_path))

            texture_parameters['filename'] = new_texture_path
            texture.set_parameters(texture_parameters)
            logging.info('{} converted to {}'.format(texture_path, new_texture_path))
