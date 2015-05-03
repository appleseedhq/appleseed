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

# TODO: check the amount of times open is used on a file, 
#       i bet this could be reduced quite a bit.
# add option to specify path to oslinfo -> use ini file
# DONE: add option for rsl shaders and vex shaders, detect file extension
# add option to query json file? YES, maybe other tool
# add option to add or overwrite
# add option to check if shader already exists
# DONE: add option to parse the/multiple directory/ies (or is default behaviour, check if it is a path or if we have files
# DONE: add option to read paths and files from a file


import json
import glob
import os.path
import sys
import optparse
import hashlib
import datetime
import getpass

DEBUG = True


class Shader():
    def __init__( self ):
        self.name = "name"
        self.path = "path"
        self.hash = "hash"
        self.num  = "num"
    

FileTypes = {'.oso' : "openshadinglanguage",
             '.sdl' : "3delight rsl",
             '.slo' : "RSL",
             '.otl' : "vex"
            }

# metadata according to the OSL specification
_shaderTypes = ["surface", "displacement", "light", "volume", "shader"]
_dataTypes   = ["int", "float", "point", "vector", "normal", "color", "matrix", "string", "void"]
_shaderKeys  = ["name", "label", "type", "help", "url", "value", "page", "widget", "units"]
#_parmWidgets = ["number", "string", "boolean", "checkBox", "popup", "mapper", "filename", "null"]
#_parmInteger = ["min", "max", "sensitivity", "slider"]
#_parmFloat   = _parmInteger + ["digits"]
#_parmSlider  = ["slidermin", "slidermax", "slidercenter", "sliderexponent"]
#_parmKeyword = ["output"]


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


def checkInfoExecutables():
    pass


def isValidFile( fp ):
    return ( os.path.isdir( fp ) or os.path.isfile( fp ) )


def isValidExtension( fp, filetypes ):
    return  ( os.path.splitext( fp )[1] in filetypes )


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


def parseOslInfo( compiledShader ):
    global DEBUG

    try:
        cmd = 'oslinfo -v %s' % compiledShader
        fp = os.popen(cmd, 'r')
    except:
        # this should give an error oslinfo not found
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
            #widget = list()
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
                        #widget.append( value )
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


def calcCheckSum( compiledShader, blocksize=65536 ):
    try:
        fp = open( compiledShader, 'rb' )
    except:
        return None

    hashMethod = hashlib.sha256()
    buf = fp.read( blocksize )
    while len( buf ) > 0:
        hashMethod.update( buf )
        buf = fp.read( blocksize )
    fp.close()

    return hashMethod.digest()[:16]


def parseShaderInfo( compiledShader, FileTypes ):
    (name, extension) = os.path.splitext( compiledShader )
    shaderUI = None
    if extension == '.oso':
        shaderUI = parseOslInfo( compiledShader )
    #if extension == '.sdl':
    #    shaderUI = parseRSL3Delight( compiledShader )
    #if extension == '.slo':
    #    shaderUI = parseRSLPixar( compiledShader )

    if not shaderUI:
        _error( "Could not process %s" % compiledShader )
        return None
    else:
        compShader = dict()
        #initialize id here, set one level up
        compShader[ 'element' ]    = 0
        compShader[ 'name' ]  = shaderUI[ 'name' ]
        compShader[ 'path' ] = compiledShader
        compShader[ 'hash' ]  = 1
        #str( calcCheckSum( compiledShader ) )
        compShader[ 'mtime' ] = str( os.path.getmtime( compiledShader ) )
        compShader[ 'ctime' ] = str( datetime.datetime.now() )
        compShader[ 'language' ] = FileTypes[ extension ]
        compShader[ 'ui'   ]  = shaderUI

        return compShader


def getNumberOfElements( jsonFile ):
    for i in range ( len( jsonFile ) ):
        if 'path' not in jsonFile[i]:
            return jsonFile[i][ 'elements' ]
        else:
            return None


def getShaderElement( jsonFile, shaderName, key ):
    for i in range( len( jsonFile ) ):
        if jsonFile[i][ 'name' ] == shaderName:
            return jsonFile[i][ key ]
            break
        else:
            return None


def writeJsonHeader( filename, numElements ):
    headerDict = dict()
    headerDict[ 'creator' ]       = getpass.getuser()
    headerDict[ 'creation date' ] = str( datetime.datetime.now() )
    headerDict[ 'name' ]          = os.path.basename( filename )
    headerDict[ 'elements' ]      = numElements
    headerDict[ 'last update' ]   = str( datetime.datetime.now() )
    return headerDict


def updateJsonHeader( jsonFile, numElements ):
    for i in range( len( jsonFile) ):
        if 'creator' in jsonFile[i]:
            existingHeader = jsonFile[i]
            existingHeader[ 'last update' ] = str( datetime.datetime.now() )
            existingHeader[ 'elements' ] = numElements
            return  existingHeader


def deleteHeader( jsonFile ):
    deleted = False
    for i in range( len( jsonFile) ):
        if 'creator' in jsonFile[i]:
            jsonFile.pop( i )
            deleted = True
            break
    return deleted


def deleteJsonShaderObject( jsonFile, shaderName ):
    deleted = False
    for i in range( len( jsonFile ) ):
        if jsonFile[i][ 'name' ] == shaderName and jsonFile[i]['hash'] != None:
            jsonFile.pop( i )
            deleted = True
            break
    return deleted


def existsJsonShader( jsonFile, shaderName ):
    for i in range( len( jsonFile ) ):
        if jsonFile[i][ 'name' ] == shaderName:
            return True
        else:
            return False


#----------------------------------------------------------
# Main body
#----------------------------------------------------------

usage = """%prog [options] [shaderfiles]
oslui stores the user interface of compiled OSL (openshadinglanguage)
shaders into a single json file and/or into multiple xml files.
"""

parser = optparse.OptionParser( usage )

parser.add_option( "-v", "--verbose", action="store_true", dest="verbose", help="Output verbosity." )
parser.add_option( "-j", "--json", action="store", type="string", dest="create_json", help="Store shader UI in json file." )
parser.add_option( "-f", "--file", action="store", type="string", dest="read_file", help="Read paths from file." )
parser.add_option( "-u", "--update", action="store_true", dest="update", help="Update existing shader file." )
parser.add_option( "-o", "--overwrite", action="store_true", dest="overwrite", help="Overwrite existing files." )

parser.set_defaults(
    create_json = "oslui_json",
    read_file = None )
    
(options, args) = parser.parse_args()
verbose = options.verbose
sui_json = options.create_json
inp_file = options.read_file
update = options.update
overwrite = options.overwrite

if len( sys.argv[1:] ) == 0:
    parser.print_help()
    _error( "", True )
if len( args ) == 0 and inp_file == None:
    _error( "No shader files specified. Exiting.", True )

files = createFileList( FileTypes, args, inp_file )

# copy, write, save method?
if json:
    numElements = 0

    existingFile = os.path.exists( sui_json )
    if existingFile and not overwrite:
        with open( sui_json, 'r' ) as fp_json:
            #try:
            json_temp = json.load( fp_json )
            #print( json_temp )
            #numElements = getNumberOfElements( json_temp )
            #except ValueError:
            #    print( "Could not read value from file." )
            #    json_temp = None

    with open( sui_json, 'w' ) as fp_json:
        fp_json.seek( 0 )
        fp_json.truncate()

        for shader in files:
            if verbose:
                print( "Processing: %s" % shader )

            shaderUI = parseShaderInfo( shader, FileTypes )
            if shaderUI:
                if update and existingFile:
                    element = getShaderElement( json_temp, shaderUI[ 'name' ], 'element' )
                    deleted = deleteJsonShaderObject( json_temp, shaderUI[ 'name' ] )
                    #add new shader
                    shaderUI[ 'element' ] = element
                    json.dump( shaderUI, fp_json )
                else:
                    if existingFile:
                        if not existsJsonShader( json_temp, shaderUI['name'] ):
                            numElements = numElements + 1
                            shaderUI[ 'element' ] = numElements
                            json.dump( shaderUI, fp_json )
                        else:
                            pass
                    else:
                        numElements = numElements + 1
                        shaderUI[ 'element' ] = numElements
                        json.dump( shaderUI, fp_json )
      
                        

        # dump the remaining part of the file
        if update and existingFile:
            newHeader = updateJsonHeader( json_temp, numElements )
            deleteHeader( json_temp )
            json.dump( json_temp, sui_json )
            # and update the header
            json.dump( newHeader, fp_json )
        else:
            json.dump( writeJsonHeader( sui_json, numElements ), fp_json )

#if DEBUG:
#    print( json.dump( shaderUI, indent=4 ) )

#   for shader in files
#       if json:
#           createJson()
#       if xml:
#           createXML()
