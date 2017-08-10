import os

class TextureConverter:
    def __init__(self, maketx_path):
        self.converted = {}
        self.maketx_path = maketx_path

    def convert(self, path):
        if path in self.converted:
            return self.converted[path]
        else:
            path_converted = self._convert(path)
            self.converted[path] = path_converted
            return path_converted

    def _convert(self, path):
        tx_path = self._change_ext_to_tx(path)
        if os.path.exists(tx_path):
            print('Warning: {tx_texture} exists.'.format(tx_texture=tx_path))
            return None

        status = os.system('{maketx} -o "{tx_texture}" "{texture}"'.format(
            maketx=self.maketx_path, tx_texture=tx_path, texture=path))

        if status != 0:
            print('maketx failed with error code {}'.format(status))
            return None

        return tx_path

    def _change_ext_to_tx(self, path):
        extension = '.tx'
        if path.rfind(os.sep) < path.rfind('.'):
            path_to_tx = path[:path.rfind('.')] + extension
        else:
            path_to_tx += extension

        return path_to_tx