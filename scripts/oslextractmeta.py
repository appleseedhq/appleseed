#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2015-2018 Hans Hoogenboom, The appleseedhq Organization
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

import json
import glob
import os.path
import sys
import argparse
try:
    from ConfigParser import ConfigParser
except:
    from configparser import ConfigParser
import datetime
import getpass
import subprocess

from utils import print_runtime_details  # local module


# -------------------------------------------------------------------------------------------------
# Constants.
# -------------------------------------------------------------------------------------------------

VERSION = "1.0"

FileTypes = {'.oso': "openshadinglanguage"}

# metadata according to the OSL specification
_shaderTypes = ["surface", "displacement", "light", "volume", "shader"]
_dataTypes = ["int", "float", "point", "vector", "normal", "color", "matrix", "string", "void"]
_shaderKeys = ["name", "label", "type", "help", "url", "value", "page", "widget", "units"]
# These osl parameters are not part of the official shadinglanguage but more guidelines as how to
# make up the interface of the shader inside a 3rd party program. Not yet decided what to do with it...
#_parmWidgets = ["number", "string", "boolean", "checkBox", "popup", "mapper", "filename", "null"]
#_parmInteger = ["min", "max", "sensitivity", "slider"]
#_parmFloat   = _parmInteger + ["digits"]
#_parmSlider  = ["slidermin", "slidermax", "slidercenter", "sliderexponent"]
#_parmKeyword = ["output"]


# -------------------------------------------------------------------------------------------------
# Functions to sanitize olsinfo output
# -------------------------------------------------------------------------------------------------

def _error(msg, crash=False):
    sys.stderr.write(msg)
    sys.stderr.write('\n')
    if crash:
        sys.exit(1)
    return False


def _fatalError(msg):
    _error(msg, True)


def _formatVal(st):
    value = st.replace('"', '', 2)
    value = value.strip()
    return value


def _getKeyValue(st):
    signPos = st.index('=')
    value = st[signPos + 1:]
    key = st[:signPos - 1]
    key = key.split()
    key = key[-1].strip()
    return (key, value)


# -------------------------------------------------------------------------------------------------
# File handling
# -------------------------------------------------------------------------------------------------

def isValidFile(filename, filetypes):
    (head, tail) = os.path.splitext(filename)
    return (os.path.isfile(filename) and tail in filetypes)


def isValidExtension(fp, filetypes):
    return (os.path.splitext(fp)[1] in filetypes)


def createFileList(filetypes, osl_cfg, recursive=False, args=None, pathfile=None):
    filelist = list()
    # files/dirs from external file
    if pathfile:
        for fp in pathfile:
            try:
                fp = open(pathfile)
                for line in fp:
                    filelist.append(line)
                fp.close()
            except:
                _error("Could not read from file %s" % pathfile)
    # files/dirs from command line arguments
    if args:
        for arg in args:
            filelist.append(arg)
    # files/dirs from config file
    osl_dir = osl_cfg.get('settings', 'osldir')
    if len(osl_dir) > 0:
        osldir_list = osl_dir.split(',')
        for arg in osldir_list:
            filelist.append(arg)

    # expand vars
    args_expanded = list()
    for arg in filelist:
        args_expanded.append(os.path.expandvars(arg))
    # clear filelist and glob
    filelist = list()
    for arg in args_expanded:
        filelist.extend([x for x in glob.iglob(arg)])

    # split files from directories
    dirlist = list()
    dirlist = [x for x in filelist if os.path.isdir(x)]
    filelist[:] = [x for x in filelist if isValidFile(x, filetypes)]

    # travel directories and add shader files to filelist
    for directory in dirlist:
        if recursive:
            for dirpath, dirnames, filenames in os.walk(directory):
                for filename in filenames:
                    (head, tail) = os.path.splitext(filename)
                    if tail in filetypes:
                        filelist.append(os.path.join(dirpath, filename))
        else:
            dirpath, dirnames, filenames = next(os.walk(directory))
            for filename in filenames:
                (head, tail) = os.path.splitext(filename)
                if tail in filetypes:
                    filelist.append(os.path.join(dirpath, filename))

    # clear duplicate entries, do not care for order
    filelist = list(set(filelist))

    # if there are no files/paths quit
    if len(filelist) < 1:
        _fatalError("No files or directories found, exiting.")

    return filelist


# -------------------------------------------------------------------------------------------------
# Functions for parsing *.oso files
# -------------------------------------------------------------------------------------------------

def parseOslInfo(compiledShader, osl_cfg):

    oslpath = osl_cfg.get('settings', 'oslpath')
    if os.path.isfile(oslpath):
        cmd = str(oslpath) + ' -v %s' % compiledShader
    else:
        cmd = 'oslinfo -v %s' % compiledShader
    cmd = cmd.split()
    try:
        fp = subprocess.check_output(cmd)
    except subprocess.CalledProcessError as fp_ret:
        _fatalError("Could not run oslinfo, exiting.\nReturncode: %s" % fp_ret.returncode)

    # check if output of oslinfo is correct
    # if false skip shader and write error message to console
    lines = fp.splitlines()
    if not lines:
        _error('Missing shader definition for %s' % compiledShader)
        return False
    count = 0
    shaderDef = lines[count]
    args = shaderDef.split()

    # tempShader stores all the data
    tempShader = dict()
    # store the order in which oslinfo outputs its data
    # and separate the parameters from general shader data
    parmlist = list()
    if args[0] not in _shaderTypes:
        _error("Not a valid shader type: %s" % args[0])
        return False
    else:
        tempShader['type'] = _formatVal(args[0])
        tempShader['name'] = _formatVal(args[1])
        tempShader['hasMetaData'] = False
        tempShader['hasParmHelp'] = False

    # parse the rest of the file to get parameters
    # number of entries in lines
    length = len(lines) - 1
    # lines iterator
    count = 1
    while True:
        line = lines[count]
        if not line:
            _error("No more lines to read, invalid shader %s?" % compiledShader)
            break

        args = line.split()
        # find parameter name
        if args[0] not in ["Default", "metadata:"]:  # or args[0] == "export":
            tempparm = dict()
            if len(args) < 3:
                tempparm['name'] = _formatVal(args[0])
                tempparm['type'] = _formatVal(args[1])
            else:
                tempparm['output'] = True
                tempparm['name'] = _formatVal(args[0])
                tempparm['type'] = _formatVal(args[2])
            condition = True
            widget = str()
            while condition:
                # read next line
                count += 1
                if count > length:
                    break
                line = lines[count]
                parmargs = line.split()
                if parmargs[0] == "Default":
                    tempparm['value'] = _formatVal(' '.join(parmargs[2:]))
                elif parmargs[0] == "metadata:":
                    (key, value) = _getKeyValue(line)
                    value = _formatVal(value)
                    if key != 'widget':
                        tempparm[key] = value
                    else:
                        widget = value
                else:
                    condition = False
                    # move one line back
                    count -= 1
            if len(widget) > 0 and 'widget' not in tempparm:
                tempparm['widget'] = widget
            tempShader[tempparm['name']] = tempparm
            parmlist.append(tempparm['name'])
            if 'help' in tempparm:
                tempShader['hasParmHelp'] = True
        # we didn't find a parameter yet, so there must be some general stuff
        else:
            if args[0] == "metadata:":
                (key, value) = _getKeyValue(line)
                value = _formatVal(value)
                tempparm[key] = value
                tempShader['hasMetaData'] = True

        if count > length:
            break
        else:
            count += 1
            # parsed all lines
    tempShader['parmlist'] = parmlist
    return tempShader


def parseShaderInfo(compiledShader, FileTypes, osl_cfg):
    (name, extension) = os.path.splitext(compiledShader)
    shaderUI = None
    if extension == '.oso':
        shaderUI = parseOslInfo(compiledShader, osl_cfg)

    if not shaderUI:
        _error("Could not process %s" % compiledShader)
        return None
    else:
        compShader = dict()
        compShader['name'] = shaderUI['name']
        compShader['path'] = compiledShader
        compShader['mtime'] = str(os.path.getmtime(compiledShader))
        compShader['ctime'] = str(datetime.datetime.now())
        compShader['language'] = FileTypes[extension]
        # holds the output of parseOslInfo (the actual shader metadata/ui)
        compShader['ui'] = shaderUI
        return compShader


# -------------------------------------------------------------------------------------------------
# Functions for handling the shader dictionary
# -------------------------------------------------------------------------------------------------

def getNumberOfShaders(jsonFile):
    return len(jsonFile['shaders'])


def cleanJsonShaders(jsonDict):
    num_del = 0
    for shaderpath in jsonDict.keys():
        if not os.path.isfile(shaderpath):
            del jsonDict[shaderpath]
            num_del += 1
    return (num_del, jsonDict)


def existsJsonShader(jsonFile, shaderName):
    for shader in jsonFile['shaders']:
        if shader['name'] == shaderName:
            return True
        else:
            return False


def writeJsonHeader(filename, numElements):
    headerDict = dict()
    headerDict['creator'] = getpass.getuser()
    headerDict['creation date'] = str(datetime.datetime.now())
    headerDict['name'] = os.path.basename(filename)
    headerDict['elements'] = numElements
    headerDict['last update'] = str(datetime.datetime.now())
    return headerDict


def updateJsonHeader(jsonFile, numElements):
    headerDict = jsonFile
    headerDict['last update'] = str(datetime.datetime.now())
    headerDict['elements'] = numElements
    return headerDict


def cli():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description='''
oslextractmetadata stores the user interface and metadata of a
compiled OSL (openshadinglanguage) shader(s) into a JSON file.
The JSON dictionary consists of a 'header' and a 'shader' part.
jsondict['shader'] will return a dictionary with all shaders. The
user interface of the shader is stored as a sub-dictionary, the
metadata can be retrieved using the 'ui' key on the elements, e.g.:

    for x in jsondict['shaders'].values():
        print x['ui']
''')
    parser.add_argument('-i', '--input', nargs='+', action='store', dest='files', metavar='compiled shaders', help='List of file(s) to parse.')
    parser.add_argument('-v', '--verbose', action='store_true', dest='verbosity', help='Increase output verbosity.')
    parser.add_argument('-o', '--output', nargs=1, action='store', dest='output',
                        required=True, metavar='output file', help="Store shader UI in file.")
    parser.add_argument('-f', '--file', nargs='+', action='store', dest='read_file', metavar='file',  help="Read file paths from file(s).")
    parser.add_argument('-U', '--update', action='store_true', dest='update', help="Update existing shader file.")
    parser.add_argument('-O', '--overwrite', action='store_true', dest='overwrite', help="Overwrite existing files.")
    parser.add_argument('-c', '--clean', action='store_true', dest='clean', help="Clean file, remove non existant shaders.")
    parser.add_argument('-r', '--recursive', action='store_true', dest='recursive', help="Add directories recursively.")

    args = parser.parse_args()

    # user input checks
    output = args.output[0]
    existingFile = os.path.exists(output)
    if not existingFile:
        args.overwrite = False
        args.update = False
        args.clean = False
    if args.overwrite:
        args.update = False
        args.clean = False

    return (args, output, existingFile)


# -------------------------------------------------------------------------------------------------
# Entry point.
# -------------------------------------------------------------------------------------------------

def main():
    (args, output, existingFile) = cli()

    print_runtime_details("oslextractmetadata", VERSION, os.path.realpath(__file__))

    # read configuration file
    cfg_defaults = {'oslpath': '/usr/bin/oslinfo'}
    osl_cfg = ConfigParser(cfg_defaults)
    osl_cfg.read('oslextractmeta.conf')

    # create list of files specified on cli or read from file
    files = createFileList(FileTypes, osl_cfg, args.recursive, args.files, args.read_file)

    # parse files for shader metadata
    shaders = dict()
    for shaderfile in files:
        if args.verbosity:
            print("Processing file %s" % shaderfile)
        shaderUI = parseShaderInfo(shaderfile, FileTypes, osl_cfg)
        if shaderUI:
            shaders[shaderUI['path']] = shaderUI

    jsonDict = dict()
    # retrieve existing values in case of updating or cleaning
    if existingFile and not args.overwrite:
        with open(output, 'r') as fp:
            try:
                jsonDict = json.load(fp)
            except:
                _fatalError("JSON object could not be decoded.")

    # create/update/clean json shader and header dictionaries
    changes = 0
    if args.clean:
        (changes, jsonDict['shaders']) = cleanJsonShaders(jsonDict['shaders'])
        if args.verbosity:
            print("Removed %s shaders." % changes)
    if args.update:
        changes = len(shaders)
        jsonDict['shaders'].update(shaders)
        if args.verbosity:
            print("%s shaders updated." % changes)
    if args.overwrite:
        changes = len(shaders)
        jsonDict['header'] = writeJsonHeader(output, changes)
        jsonDict['shaders'] = shaders
        if args.verbosity:
            print("%s shaders added to %s" % (changes, output))
    # only adding new shaders
    else:
        temp_changes = changes
        if jsonDict.has_key('shaders'):
            existing_keys = jsonDict['shaders'].keys()
            for key in shaders:
                if key not in existing_keys:
                    jsonDict['shaders'][key] = shaders[key]
                    changes += 1
        else:
            jsonDict['shaders'] = shaders
            changes = len(shaders)
        if args.verbosity:
            added_shaders = changes - temp_changes
            print("Added %s shaders." % added_shaders)

    # write to file shaders to file if changed
    if existingFile and changes:
        with open(output, 'w') as fp:
            fp.seek(0)
            fp.truncate()
            jsonDict['header'] = updateJsonHeader(jsonDict['header'], len(jsonDict['shaders']))
            json.dump(jsonDict, fp)
    elif not existingFile and changes:
        with open(output, 'w') as fp:
            jsonDict['header'] = writeJsonHeader(output, len(shaders))
            json.dump(jsonDict, fp)
    elif args.verbosity:
        print("No shaders found for adding to %s, exiting." % output)

    return 0


if __name__ == "__main__":
    main()
