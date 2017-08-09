import appleseed as asr
import appleseed.studio as studio

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

    tx_converter = asr.TXConverter(maketx_path)

    for texture in textures:
        texture_parameters = texture.get_parameters()
        texture_path = texture_parameters['filename']

        if texture_path.endswith('.tx'):
            continue

        new_texture_path = tx_converter.convert(texture_path)
        if new_texture_path is None:
            print('Skipped converting of {texture}'.format(texture=texture_path))
        else:
            texture_parameters['filename'] = new_texture_path
            texture.set_parameters(texture_parameters)
            print('{} converted to {}'.format(texture_path, new_texture_path))