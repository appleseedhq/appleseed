#! /usr/bin/env python

"""
The MIT License (MIT)

Copyright (c) 2015, Hans Hoogenboom, the appleseedhq Organization

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
"""

# add option to specify path to oslinfo -> use ini file
# add option to add or overwrite
# add option to check if shader already exists


import json
import glob
import os.path
import sys
import optparse
from ConfigParser import ConfigParser
import hashlib
import datetime
import getpass


DEBUG = False

FileTypes = {'.oso' : "openshadinglanguage" }

# metadata according to the OSL specification
_shaderTypes = ["surface", "displacement", "light", "volume", "shader"]
_dataTypes   = ["int", "float", "point", "vector", "normal", "color", "matrix", "string", "void"]
_shaderKeys  = ["name", "label", "type", "help", "url", "value", "page", "widget", "units"]
# These osl parameters are not part of the official shadinglanguage but more guidelines as how to
# make up the interface of the shader inside a 3rd party program. Not yet decided what to do with it...
#_parmWidgets = ["number", "string", "boolean", "checkBox", "popup", "mapper", "filename", "null"]
#_parmInteger = ["min", "max", "sensitivity", "slider"]
#_parmFloat   = _parmInteger + ["digits"]
#_parmSlider  = ["slidermin", "slidermax", "slidercenter", "sliderexponent"]
#_parmKeyword = ["output"]


#----------------------------------------------------------
# Functions to sanitize olsinfo output
#----------------------------------------------------------

def _error( msg, crash = False ):
    sys.stderr.write( msg )
    sys.stderr.write( '\n' )
    if crash:
        sys.exit(1)
    return False


def _formatVal( st ):
    value = st.replace('"','',2)
    value = value.strip()
    return value


def _getKeyValue( st ):
    signPos = st.index('=')
    value   = st[signPos+1:]
    key     = st[:signPos-1]
    key     = key.split()
    key     = key[-1].strip()
    return (key, value)


#----------------------------------------------------------
# File handling
#----------------------------------------------------------

def isValidFile( fp ):
    return ( os.path.isdir( fp ) or os.path.isfile( fp ) )


def isValidExtension( fp, filetypes ):
    return  ( os.path.splitext( fp )[1] in filetypes )


#TODO: better/more efficient traveling of directories
# and parsing of files/globs/env-vars
def createFileList( filetypes, args = None, pathfile = None ):
    global DEBUG
    fileList = list()

    # expand command line arguments
    if args:
        for arg in args:
            args_expanded = glob.glob( os.path.expandvars( arg ) )
            for x in args_expanded:
                if ( isValidFile( x ) and isValidExtension( x, filetypes ) ):
                    fileList.append( os.path.abspath( x ) )

    # process paths stored in external file
    if pathfile:
        try:
            fp = open( pathfile )
            while True:
                line = fp.readline()
                if not line:
                    break;
                args_expanded = os.path.expandvars( line )
                for i in args_expanded:
                    fileList.append( i )
            fp.close()
        except:
            _error( "Could not read file." )

    if DEBUG:
        print( "Files: " )
        for i in fileList:
            print( i )

    # if there are no files/paths quit
    if len( fileList ) < 1:
        _error( "No files or directories found, exiting.", True )
    
    return fileList


#----------------------------------------------------------
# Functions for parsing *.oso files
#----------------------------------------------------------

def parseOslInfo( compiledShader ):
    global DEBUG

    try:
        cmd = 'oslinfo -v %s' % compiledShader
        fp = os.popen(cmd, 'r')
    except:
        _error( "Could not run oslinfo, exiting." )
        return False
    
    # check if output of oslinfo is correct
    # if false skip shader and write error message to console
    lines = fp.readlines()
    if not lines:
        _error('Missing shader definition for %s' % compiledShader)
        return False
    count = 0
    shaderDef = lines[ count ]    
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
        tempShader['type'] = _formatVal( args[0] )
        tempShader['name'] = _formatVal( args[1] ) 
        tempShader['hasMetaData'] = False
        tempShader['hasParmHelp'] = False

    # parse the rest of the file to get parameters
    # number of entries in lines
    length = len( lines ) - 1
    # lines iterator
    count = 1
    while True:
        line = lines[ count ]
        if not line:
            _error( "No more lines to read, invalid shader %s?" % compiledShader )
            break

        args = line.split()
        # find parameter name
        if args[0] not in ["Default", "metadata:"]: # or args[0] == "export":
            tempparm = dict()
            if len( args ) < 3:
                tempparm['name'] = _formatVal( args[0] )
                tempparm['type'] = _formatVal( args[1] )
            else:
                tempparm['output'] = True
                tempparm['name']   = _formatVal( args[0] )
                tempparm['type']   = _formatVal( args[2] )
            condition = True
            widget = str()
            while condition:
                # read next line
                count += 1
                if count > length:
                    break
                line = lines[ count ]
                parmargs = line.split()
                if parmargs[0] == "Default":
                    tempparm['value'] = _formatVal( ' '.join(parmargs[2:]) )
                elif parmargs[0] == "metadata:":
                    (key, value) = _getKeyValue( line )
                    value = _formatVal( value )
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
                (key, value) = _getKeyValue( line )
                value = _formatVal( value )
                tempparm[key] = value
                tempShader['hasMetaData'] = True
  
        if count > length:
           break
        else:
            count += 1
        # parsed all lines
    tempShader['parmlist'] = parmlist
    
    if DEBUG:
        for key in tempShader:
            print( "%s: %s" % ( key, tempShader[key] ) )

    return tempShader


def parseShaderInfo( compiledShader, FileTypes ):
    (name, extension) = os.path.splitext( compiledShader )
    shaderUI = None
    if extension == '.oso':
        shaderUI = parseOslInfo( compiledShader )

    if not shaderUI:
        _error( "Could not process %s" % compiledShader )
        return None
    else:
        compShader = dict()
        #initialize id here, set one level up
        compShader[ 'element' ] = 0
        compShader[ 'name' ]    = shaderUI[ 'name' ]
        compShader[ 'path' ]    = compiledShader
        compShader[ 'mtime' ]   = str( os.path.getmtime( compiledShader ) )
        compShader[ 'ctime' ]   = str( datetime.datetime.now() )
        compShader[ 'language' ]= FileTypes[ extension ]
        # holds the output of parseOslInfo (the actual shader metadata/ui )
        compShader[ 'ui'   ]    = shaderUI

        return compShader

#----------------------------------------------------------
# Functions for handling the shader dictionary
#----------------------------------------------------------

def getNumberOfShaders( jsonFile ):
    return len( jsonFile['shaders'] )


def cleanJsonShaders( jsonDict ):
    for shaderpath in jsonDict.keys():
        if not os.path.isfile( shaderpath ):
            del jsonDict[ shaderpath ]
    return jsonDict


def existsJsonShader( jsonFile, shaderName ):
    for shader in jsonFile['shaders']:
        if data[ 'name' ] == shaderName:
            return True
        else:
            return False


def writeJsonHeader( filename, numElements ):
    headerDict = dict()
    headerDict[ 'creator' ]       = getpass.getuser()
    headerDict[ 'creation date' ] = str( datetime.datetime.now() )
    headerDict[ 'name' ]          = os.path.basename( filename )
    headerDict[ 'elements' ]      = numElements
    headerDict[ 'last update' ]   = str( datetime.datetime.now() )
    
    return headerDict


def updateJsonHeader( jsonFile, numElements ):
    headerDict = jsonFile
    headerDict[ 'last update' ] = str( datetime.datetime.now() )
    headerDict[ 'elements' ] = numElements

    return headerDict


#----------------------------------------------------------
# Main body
#----------------------------------------------------------

def main():
    usage = """%prog [options] [shaderfiles]
    oslextractmetadata stores the user interface and metadata of a 
    compiled OSL (openshadinglanguage) shaders into a JSON file. The
    user interface of the shader is stored as a sub-dictionary in 
    the file and can be retrieved using the 'ui' key on the elements.
    """

    parser = optparse.OptionParser( usage )

    parser.add_option( "-v", "--verbose", action="store_true", dest="verbose", help="Output verbosity." )
    parser.add_option( "-o", "--output", action="store", type="string", dest="output_file", help="Store shader UI in file." )
    parser.add_option( "-f", "--file", action="store", type="string", dest="read_file", help="Read paths from file." )
    parser.add_option( "-U", "--update", action="store_true", dest="update", help="Update existing shader file." )
    parser.add_option( "-O", "--overwrite", action="store_true", dest="overwrite", help="Overwrite existing files." )
    parser.add_option( "-c", "--clean", action="store_true", dest="clean", help="Clean file, remove non existant shaders." )

    parser.set_defaults(
        output_file = "oslui.json",
        read_file = None )
    
    (options, args) = parser.parse_args()
    verbose = options.verbose
    out_file = options.output_file
    inp_file = options.read_file
    update = options.update
    overwrite = options.overwrite
    clean = options.clean

    # user input checks
    if len( sys.argv[1:] ) == 0:
        parser.print_help()
        _error( "", True )
    if len( args ) == 0 and inp_file == None:
        _error( "No shader files specified. Exiting.", True )

    existingFile = os.path.exists( out_file )
    if existingFile and not ( update or overwrite):
        _error( "File already exists. Exiting.", True )
    if not existingFile:
        overwrite = False
        update = False
        clean = False

    # create list of files specified on cli or read from file
    files = createFileList( FileTypes, args, inp_file )

    # parse files for shaders
    shaders = dict()
    for shaderfile in files:
        if verbose:
            print( "Processing file %s" % shaderfile )
        shaderUI = parseShaderInfo( shaderfile, FileTypes )
        if shaderUI:
            shaders[ shaderUI['path'] ] = shaderUI

    # retrieve existing values in case of updating or cleaning
    if existingFile and (update or clean):
        with open( out_file, 'r' ) as fp:
            json_temp = json.load( fp )
            json_temp_header  = json_temp[ 'header' ]
            json_temp_shaders = json_temp[ 'shaders' ]

    # write shaders to file
    with open( out_file, 'w' ) as fp:
        if existingFile and (overwrite or update):
            fp.seek( 0 )
            fp.truncate()
        jsonDict = dict()
        if update or clean:
            if clean:
                json_temp_shaders = cleanJsonShaders( json_temp_shaders )
            if update:
                json_temp_shaders.update( shaders )
            jsonDict['header']  = updateJsonHeader( json_temp_header, len( json_temp_shaders ) )
            jsonDict['shaders'] = json_temp_shaders          
        else:
            jsonDict['header']  = writeJsonHeader( out_file, len( shaders ) )
            jsonDict['shaders'] = shaders
        json.dump( jsonDict, fp )

    return 0


if __name__ == "__main__":
    main()
