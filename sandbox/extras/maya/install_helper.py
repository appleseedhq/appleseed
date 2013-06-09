
#
# Copyright (c) 2012-2013 Jonathan Topf
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

import sys
import os
import maya.cmds as cmds

def append_env(variable, path):
    separator = ';' if sys.platform == 'win32' or sys.platform == 'win64' else ':'
    return 'putenv "{0}" (`getenv "{0}"` + \"{1}{2}\");'.format(variable, separator, path.replace('\\', '/'))

def install(userSetup_file, install_dir):
    installation_name = 'mayaseed'

    print 'adding paths for {0} to {1}'.format(installation_name, install_dir)

    file = open(userSetup_file, 'r')
    file_contents = file.read()
    file.close()

    file = open(userSetup_file, 'w')

    inside_block = False
    for line in file_contents.split('\n'):
      is_block_delimiter = line[:len(installation_name) + 3] == '// ' + installation_name
      if is_block_delimiter:
        inside_block = not inside_block
      if not inside_block and not is_block_delimiter:
        file.write(line + '\n')

    file.write('// ' + installation_name + '  -------------------------------------------------------------------------------\n')
    file.write('\n')
    file.write(append_env("MAYA_SCRIPT_PATH", os.path.join(install_dir, 'scripts')) + '\n')
    file.write(append_env("MAYA_SCRIPT_PATH", os.path.join(install_dir, 'graphics')) + '\n')
    file.write(append_env("MAYA_PLUG_IN_PATH", os.path.join(install_dir, 'plugins')) + '\n')
    file.write('\n')
    file.write('// ' + installation_name + '  -------------------------------------------------------------------------------\n')

    file.close()

    cmds.confirmDialog(title=installation_name + ' installation', message='All done! Just restart Maya and enable any plugins not already enabled in the plugin manager.', button='OK')
