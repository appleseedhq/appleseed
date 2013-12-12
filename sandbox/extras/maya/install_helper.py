
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

import maya.cmds as cmds
import maya.mel as mel
import os
import sys

def install():

    cmds.confirmDialog(m='After clicking \'OK\' a file dialogue will open. Please navigate to and select the mayaseed directory and click \'select\'.', button=['ok'])

    maya_app_dir = mel.eval('getenv MAYA_APP_DIR')
    mayaseed_dir = cmds.fileDialog2(fm=2, okc='Select', cap='Select mayaseed directory')
    
    if mayaseed_dir is not None:
        
        mayaseed_dir = mayaseed_dir[0]
        
        try:
            sys.path.append(os.path.join(mayaseed_dir, 'scripts'))
            import ms_commands
        except:
            print 'No valid mayaseed directory found.'
            return False
        
        
        modules_path = os.path.join(maya_app_dir, 'modules')
        
        if not os.path.exists(modules_path):
            print '{0} does not exist, creating...'.format(modules_path)
            os.makedirs(modules_path)
            
            if not os.path.exists(modules_path):
                print 'Failed to create module directory.'
                return False
                    
        module_file_path = os.path.join(modules_path, 'mayaseed.module')
        
        if os.path.exists(module_file_path):
            continue_return_value = cmds.confirmDialog(m='A mayaseed module file already exists from a previous installation, would you like to overwrite it?', button=['yes','no'])
            if continue_return_value == 'no':
                return False
        
        try:
            module_file_contents = "+ mayaseed {0} {1} \n\nicons: graphics \nscripts: scripts".format(ms_commands.MAYASEED_VERSION, mayaseed_dir)
            file = open(module_file_path, 'w')
            file.write(module_file_contents)
            file.close()
            
        except:
            print 'Error creating the mayaseed module file'
            return False
                
    return True

def install_mayaseed(): 

    if not install():
        cmds.confirmDialog(m='mayaseed was not installed. See the script editor for details.', button=['ok'])
    else:
        cmds.confirmDialog(m='mayaseed has been successfully installed. Please restart maya and enable mayaseed.py from the plugin manager.', button=['ok'])
