import os
import sys
import logging
logging.basicConfig(level=logging.INFO, stream=sys.stdout)

class TextureConverter(object):
    def __init__(self, maketx_path):
        self.converted = {}
        self.maketx_path = maketx_path

    def convert(self, path):
        if path in self.converted:
            return self.converted[path]
        else:
            path_converted = self._convert_with_maketx(path)
            self.converted[path] = path_converted
            return path_converted

    def _convert_with_maketx(self, path):
        base_path, _ = os.path.splitext(path)
        tx_path = base_path + ".tx"

        if os.path.exists(tx_path):
            logging.warning('{} already exists.'.format(tx_path))
            return None

        status = os.system('{} -o "{}" "{}"'.format(self.maketx_path, tx_path, path))

        if status != 0:
            logging.error('maketx failed with error code {}.'.format(status))
            return None

        return tx_path
