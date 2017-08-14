import appleseed as asr
import appleseed.studio as studio
from appleseed.textureconverter import *

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


def convert_all_textures_to_tx(maketx_path):
    scene = studio.current_project().get_scene()
    textures = get_textures(scene)

    tx_converter = TextureConverter(maketx_path)

    for texture in textures:
        texture_parameters = texture.get_parameters()
        texture_path = texture_parameters['filename']

        if texture.get_model() != 'disk_texture_2d' or texture_path.endswith('.tx'):
            logging.debug('Skipped converting {}'.format(texture_path))
            continue

        new_texture_path = tx_converter.convert(texture_path)
        if new_texture_path is None:
            logging.info('Skipped converting of {}'.format(texture_path))
        else:
            texture_parameters['filename'] = new_texture_path
            texture.set_parameters(texture_parameters)
            logging.info('{} converted to {}'.format(texture_path, new_texture_path))
