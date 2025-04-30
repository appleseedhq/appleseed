#!/bin/bash

set -e


# ================================================================
# README
# ================================================================
_README=\
$'# ================================================================
# This script can be used to install Appleseed and all of its de-
# pendencies.
# Use -h or --help to get a list of all possible arguments and
# options.
# 
# Note: This script has only been tested on Ubuntu 22.04!
# ================================================================'

# This text is also displayed if the script is called with no arguments.


# ================================================================
# Constants
# ================================================================

# Script Directory -- path to the location of the script.
# (If `fixOSL1957.cpp` is not found in ROOT, it will also be searched for in the script's directory.)
_SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# "Main Name" of the "master script".
_NAME="SETUP"
# If _DEBUG is set to `echo` most commands (e.g. rm, wget, or running scripts) will only be printed and not executed.
# Note: The former "debug mode" was renamed to "preview mode".
_DEBUG=
#_DEBUG=echo

# DEPENDENCIES_DIR relative to APPLESEED_ROOT
_DEFAULT_DEPENDENCIES_DIR_NAME="dependencies"
_CXX_STD=17

# Appleseed (Name)
_APPLESEED=Appleseed

# Dependencies (Names)
_BOOST=Boost
_EMBREE=Embree
_IMATH=Imath
_OCIO=OpenColorIO
_OIIO=OpenImageIO
_OPENEXR=OpenEXR
_OSL=OSL
_PARTIO=PartIO
_XERCES=Xerces

# Dependencies Releases Links
_BOOST_DL="https://github.com/boostorg/boost/releases/download/boost-1.88.0/boost-1.88.0-b2-nodocs.tar.gz"
_EMBREE_DL="https://github.com/RenderKit/embree/releases/download/v4.4.0/embree-4.4.0.x86_64.linux.tar.gz"
_IMATH_DL="https://github.com/AcademySoftwareFoundation/Imath/releases/download/v3.1.12/Imath-3.1.12.tar.gz"
_OCIO_DL="https://github.com/AcademySoftwareFoundation/OpenColorIO/archive/refs/tags/v2.4.2.tar.gz"
_OEXR_DL="https://github.com/AcademySoftwareFoundation/openexr/releases/download/v3.3.3/openexr-3.3.3.tar.gz"
_OIIO_DL="https://github.com/AcademySoftwareFoundation/OpenImageIO/releases/download/v2.5.18.0/OpenImageIO-2.5.18.0.tar.gz"
_OSL_DL="https://github.com/AcademySoftwareFoundation/OpenShadingLanguage/archive/refs/tags/v1.13.12.0.tar.gz" # TODO: update to 1.14
_PARTIO_DL="https://github.com/wdas/partio/archive/refs/tags/v1.19.0.tar.gz"
_XERCES_DL="https://github.com/apache/xerces-c/archive/refs/tags/v3.3.0.tar.gz"

# Colors
_COLOR_CLEAR='\033[0m'

_COLOR_CYAN='\033[0;36m'
_COLOR_GRAY='\033[0;37m'
_COLOR_GREEN='\033[0;32m'
_COLOR_ORANGE='\033[0;33m'
_COLOR_PINK='\033[1;35m'
_COLOR_PURPLE='\033[0;35m'
_COLOR_RED='\033[0;31m'

_COLOR_DEFAULT=$_COLOR_PURPLE
_COLOR_INSTALL_DIR=$_COLOR_CYAN

# ================================================================
# Variables and Default Values
# ================================================================

_sCCompiler=""
_sCXXCompiler=""

_sBuildType="Ship"

_sRoot=$(pwd)
_sDependenciesDir=""
_bCustomDependenciesDir=false

_sAppleseedSource=""
_sAppleseedSourceGitBranch=""
_sAppleseedRoot=""
_bAppleseedRootPrepared=false

# options
_bAsk=true
_bClang=false
_bInstallDependencies=true
_bNewBuild=false
_bNuke=false
_sVerbose=""

#utilities
_bCollect=false
_sCollectType="copy" # options: "copy" | "link"

# appleseed optional components
_sWithClient=OFF
_sWithStudio=OFF
_sWithBench=OFF
_sWithTools=OFF
_sWithPython2Bindings=OFF
_sWithEmbree=OFF

# dependency install directories
_sBoostInstallDir=""
_sEmbreeInstallDir=""
_sImathInstallDir=""
_sOCIOInstallDir=""
_sOIIOInstallDir=""
_sOpenEXRInstallDir=""
_sOSLInstallDir=""
_sPartIOInstallDir=""
_sXercesInstallDir=""

_sBoostConfigDir="" # is version dependent
_sEmbreeConfigDir="" # is version dependent

_sOSLPatchFilePath=""

# temporary variables used during installations
_sTarFile=""
_sSourceDir=""
_sBuildDir=""
_sInstallDir=""

# temporary variables for collect utility
_sCollectBinPath=""
_sCollectIncPath=""
_sCollectLibPath=""

# ================================================================
# Functions
# ================================================================

# ----------------------------------------------------------------
# Helper Functions
# ----------------------------------------------------------------

cleanInstallStep() {
  $_DEBUG cd $_sDependenciesDir

  rm -f  $_sTarFile
  rm -fr $_sSourceDir
  rm -fr $_sBuildDir

  _sTarFile=""
  _sSourceDir=""
  _sBuildDir=""
  _sInstallDir=""

  $_DEBUG cd $_sRoot
}

collect() {
  if [[ $1 = "copy" ]]; then
    op="cp -r"
  elif [[ $1 = "link" ]]; then
    op="ln -s"
  fi
  installDir=$2

  # bins
  if [ -d "$installDir/bin" ]; then
    $_DEBUG cd $installDir/bin
    for bin in $(ls -p | grep -v /); do
      $_DEBUG $op $(pwd)/$bin $_sCollectBinPath/$bin
    done
    $_DEBUG cd ..
  fi

  # include
  $_DEBUG cd $installDir/include
  for header in $(ls); do
    $_DEBUG $op $(pwd)/$header $_sCollectIncPath/$header
  done
  $_DEBUG cd ..

  # libs
  $_DEBUG cd $installDir/lib
  for lib in $(ls -p | grep -v /); do
    $_DEBUG $op $(pwd)/$lib $_sCollectLibPath/$lib
  done
  $_DEBUG cd ..
}

nuke() {
  $_DEBUG cd $_sRoot
  $_DEBUG rm -fr $_sDependenciesDir
  $_DEBUG rm -fr $_sRoot/appleseed
  echo "Removed \"$_sRoot/appleseed\", \"$_sDependenciesDir\". Exiting."
  exit 0
}

# ----------------------------------------------------------------
# Formatting Functions
# ----------------------------------------------------------------

# Prints a "step info" of the form '~~> [Name] Step Message'.
stepInfo() {
  if [[ $3 = "" ]]; then
    printf "${_COLOR_DEFAULT}~~> [$1] $2${_COLOR_CLEAR}\n"
  else
    color=$3
    printf "${color}~~> [$1] $2${_COLOR_CLEAR}\n"
  fi
}

# Makes a fully capitalized "root variable name" of the form 'NAME_ROOT'.
rootVarName() {
  echo $1"_ROOT" | tr a-z A-Z
}

dependencyInstallInfo() {
  # $1 _NAME, $2 _sNameRoot
  if [[ $2 = "" ]]; then
    printf "${_COLOR_GRAY}  $1 - None${_COLOR_CLEAR}\n"
  else
    printf "${_COLOR_INSTALL_DIR}  $1 - \"$2\"${_COLOR_CLEAR}\n"
  fi
}

# ----------------------------------------------------------------
# Handle Arguments and Options Function
# ----------------------------------------------------------------

# Function to display script usage
usage() {
  echo "Usage: $0"
  echo "Arguments:"
  echo "  -a, --appleseed-src APPLESEED SOURCE Specify a source for appleseed. (Can be a directory, a git HTTPS or SSH link for cloning, or a ZIP or TAR file.) (Defaults to \"<ROOT>/appleseed\".)"
  echo "                        Can be used to specify a directory other then \"<ROOT>/appleseed\" for the Appleseed root directory."
  echo "                        By specifying a link or path to a ZIP or TAR file, the script will clone/unpack appleseed into \"<ROOT>/appleseed\" automatically and set the root directory accordingly."
  echo "  --branch            Specify a branch for APPLESEED SOURCE if it is a git link. (Defaults to specifying no branch.)"
  echo "  --cc                Specify a c compiler (path to). (Will use the gcc compiler by default.)"
  echo "  --cxx               Specify a c++ compiler (path to). (Will use the g++ compiler by default.)"
  echo "  -b, --build         Specify a build type. (Defaults to \"Ship\".)"
  echo "  -d, --deps          DEPENDENCIES DIRECTORY Specify the directory containing the install directories of dependencies (who's install directory are not explicitly given with one of the arguments below). (Defaults to \"<ROOT>/dependencies\".)"
  echo "  -r, --root          ROOT Specify the root directory from which this script is supposed to be run. (Defaults to the current working directory.)"
  echo ""
  echo "  Install directories of dependencies. If non is given for a specific dependency, it will be downloaded and installed in \"<ROOT>/$_DEFAULT_DEPENDENCIES_DIR_NAME\" (or \"<DEPENDENCIES DIRECTORY>\", if it is given)."
  echo "  --boost-install"
  echo "  --embree-install"
  echo "  --imath-install"
  echo "  --ocio-install"
  echo "  --oiio-install"
  echo "  --openexr-install"
  echo "  --osl-install"
  echo "  --partio-install"
  echo "  --xerces-install"
  echo ""
  echo "Options:"
  echo "  -h, --help          Display this help message."
  echo "  -n, --new           Delete appleseed build directory to start new."
  echo "  --no-install        Do not install any missing dependencies, exit instead."
  echo "  --nuke              Remove all directories (and their contents) created by this script."
  echo "  --preview           Run in *preview mode*, where most commands (e.g. rm, wget, or running scripts) are only printed and not executed."
  echo "  --use-clang         Compile with Clang instead of gcc. (Will search for compiler in \"usr/bin/\".) (Note: This is necessary even if --cc and --cxx is given, when building boost, as boost builds with different \"tool sets\" [here: gcc or clang].)"
  echo "  -v, --verbose       Verbose mode (only affects Appleseed's build)."
  echo "  -y, --yes           Assume \"yes\" for all queries."
  echo ""
  echo "Optional Appleseed Components:"
  echo "  --all               Built all optional components."
  echo "  --client            Built the client (WITH_CLIENT=ON)."
  echo "  --studio            Built the studio (WITH_STUDIO=ON)."
  echo "  --bench             Built the bench (WITH_BENCH=ON)."
  echo "  --tools             Built the tools (WITH_TOOLS=ON)."
  echo "  --python2-bindings  Built the Python 2.7 bindings (WITH_PYTHON2_BINDINGS=ON)."
  echo "  --embree            Built with Embree (WITH_EMBREE=ON)."
  echo ""
  echo "Utilities:" # TODO
  echo "  --collect           (See --collect-link.)"
  echo "  --collect-copy      Copy all binaries, headers and libraries into a single directory (like the pre-built dependencies) \"<Root>/collected-deps\". (Will install any missing dependencies and/or Appleseed before doing so.)"
  echo "  --collect-link      Like --collect-copy but only sim linking."
}

# Extract options and arguments
has_argument() {
  [[ ("$1" == *=* && -n ${1#*=}) || ( ! -z "$2" && "$2" != -*)  ]];
}

extract_argument() {
  echo "${2:-${1#*=}}"
}

# Handle options and arguments
handle_options() {
  while [ $# -gt 0 ]; do
    case $1 in
      # Arguments
      -a | --apple-root)
        if ! has_argument $@; then
            echo "APPLESEED SOURCE was given no value." >&2
            usage
            exit 1
        fi
        _sAppleseedSource=$(extract_argument $@)
        shift
        ;;
      --branch)
        if ! has_argument $@; then
            echo "BRANCH was given no value." >&2
            usage
            exit 1
        fi
        _sAppleseedSourceGitBranch=$(extract_argument $@)
        shift
        ;;
      --cc)
        if ! has_argument $@; then
            echo "C COMPILER was given no value." >&2
            usage
            exit 1
        fi
        _sCCompiler=$(extract_argument $@)
        shift
        ;;
      --cxx)
        if ! has_argument $@; then
            echo "C++ COMPILER was given no value." >&2
            usage
            exit 1
        fi
        _sCXXCompiler=$(extract_argument $@)
        shift
        ;;
      -b | --build)
        if ! has_argument $@; then
            echo "BUILD TYPE was given no value." >&2
            usage
            exit 1
        fi
        _sBuildType=$(extract_argument $@)
        shift
        ;;
      -d | --deps)
        if ! has_argument $@; then
            echo "DEPENDENCIES DIRECTORY was given no value." >&2
            usage
            exit 1
        fi
        _sDependenciesDir=$(extract_argument $@)
        _bCustomDependenciesDir=true
        shift
        ;;
      -r | --root)
        if ! has_argument $@; then
            echo "ROOT" >&2
            usage
            exit 1
        fi
        _sRoot=$(extract_argument $@)
        shift
        ;;
      # Dependency Installation Arguments
      --boost-install)
        if ! has_argument $@; then
            echo "$(rootVarName $_BOOST)" >&2
            usage
            exit 1
        fi
        _sBoostInstallDir=$(extract_argument $@)
        shift
        ;;
      --embree-install)
        if ! has_argument $@; then
            echo "$(rootVarName $_EMBREE)" >&2
            usage
            exit 1
        fi
        _sEmbreeInstallDir=$(extract_argument $@)
        shift
        ;;
      --imath-install)
        if ! has_argument $@; then
            echo "$(rootVarName $_IMATH)" >&2
            usage
            exit 1
        fi
        _sImathInstallDir=$(extract_argument $@)
        shift
        ;;
      --ocio-install)
        if ! has_argument $@; then
            echo "$(rootVarName $_OCIO)" >&2
            usage
            exit 1
        fi
        _sOCIOInstallDir=$(extract_argument $@)
        shift
        ;;
      --oiio-install)
        if ! has_argument $@; then
            echo "$(rootVarName $_OIIO)" >&2
            usage
            exit 1
        fi
        _sOIIOInstallDir=$(extract_argument $@)
        shift
        ;;
      --openexr-install)
        if ! has_argument $@; then
            echo "$(rootVarName $_OPENEXR)" >&2
            usage
            exit 1
        fi
        _sOpenEXRInstallDir=$(extract_argument $@)
        shift
        ;;
      --osl-install)
        if ! has_argument $@; then
            echo "$(rootVarName $_OSL)" >&2
            usage
            exit 1
        fi
        _sOSLInstallDir=$(extract_argument $@)
        shift
        ;;
      --partio-install)
        if ! has_argument $@; then
            echo "$(rootVarName $_PARTIO)" >&2
            usage
            exit 1
        fi
        _sPartIOInstallDir=$(extract_argument $@)
        shift
        ;;
      --xerces-install)
        if ! has_argument $@; then
            echo "$(rootVarName $_XERCES)" >&2
            usage
            exit 1
        fi
        _sXercesInstallDir=$(extract_argument $@)
        shift
        ;;
      # Options
      -h | --help)
        usage
        exit 0
        ;;
      -n | --new)
        _bNewBuild=true
        ;;
      --no-install)
        _bInstallDependencies=false
        ;;
      --nuke)
        _bNuke=true
        ;;
      --preview)
        _DEBUG=echo
        ;;
      -s | --source)
        _bSource=true
        ;;
      --use-clang)
        _bClang=true
        ;;
      -v | --verbose)
        _sVerbose="VERBOSE=1"
        ;;
      -y | --yes)
        _bAsk=false
        ;;
      # Utilties
      --collect | --collect-copy)
        _bCollect=true
        ;;
      --collect-link)
        _bCollect=true
        _sCollectType="link"
        ;;
      # Optional Components
      --all)
        _sWithClient=ON
        _sWithStudio=ON
        _sWithBench=ON
        _sWithTools=ON
        _sWithPython2Bindings=ON
        _sWithEmbree=ON
        ;;
      --client)
        _sWithClient=ON
        ;;
      --studio)
        _sWithStudio=ON
        _sWithPython2Bindings=ON # required for studio
        ;;
      --bench)
        _sWithBench=ON
        ;;
      --tools)
        _sWithTools=ON
        ;;
      --python2-bindings)
        _sWithTools=ON
        ;;
      --embree)
        _sWithEmbree=ON
        ;;
      *)
        echo "Invalid option: $1" >&2
        usage
        exit 1
        ;;
    esac
    shift
  done
}


# ================================================================
# MAIN
# ================================================================

# Handle options
handle_options "$@"

# ----------------------------------------------------------------
# Nuke
# ----------------------------------------------------------------

# nuke appleseed build

if [ $_bNuke = true ]; then
  if [ $_bAsk = true ]; then
    # Checkpoint
    printf "${_COLOR_RED}Do you really wish to remove all directories (and their contents) created by this script?${_COLOR_CLEAR}\n"
    select strictreply in "Yes" "No"; do
      relaxedreply=${strictreply:-$REPLY}
      case $relaxedreply in
        Yes | YES | yes | y ) nuke;;
        No  | NO  | no  | n ) echo "Aborting nuke. Exiting."; exit 0;;
      esac
    done
  else
    # Nuke (No Checkpoint)
    nuke
  fi
fi


# ----------------------------------------------------------------
# Check Important Arguments
# ----------------------------------------------------------------

# Debug Mode

if [[ $_DEBUG != "" ]]; then
  printf "${_COLOR_PINK}*Script running in PREVIEW MODE.*${_COLOR_CLEAR}\n"
fi

# Root Directory

case $_sRoot in
  *appleseed/scripts/Ubuntu_20.04_setup/|*appleseed/scripts/Ubuntu_20.04_setup|*appleseed/scripts/|*appleseed/scripts|*appleseed/|*appleseed)
    echo "It looks like the path in ROOT may be a child directory of the Appleseed root directory. Are you sure you want to continue with \"$_sRoot\" as the root directory?"
    select strictreply in "Yes" "No"; do
      relaxedreply=${strictreply:-$REPLY}
      case $relaxedreply in
        Yes | YES | yes | y ) break;;
        No  | NO  | no  | n )
          echo "Tip: Try running this script from the parent directory of the Appleseed root directory or setting ROOT directly using -r or --root. (Use -h or --help to see usage help.) Exiting.";
          exit 0
          ;;
      esac
    done
    ;;
  *);;
esac

# dependencies directory
_sDependenciesDir=$_sRoot/$_DEFAULT_DEPENDENCIES_DIR_NAME

# Appleseed Source

if [[ $_sAppleseedSource == "" ]]; then
  stepInfo $_NAME "APPLEESEED SOURCE was not specified, assuming \"<ROOT>/appleseed\"."
  _sAppleseedRoot=$_sRoot/appleseed

elif [[ $_sAppleseedSource =~ ^git@.* ]] || [[ $_sAppleseedSource =~ ^https.* ]]; then
  stepInfo $_NAME "APPLEESEED SOURCE is a git link."
  _sAppleseedRoot=$_sRoot/appleseed
  if [ -d "$_sAppleseedRoot/src" ]; then
    stepInfo $_NAME "APPLEESEED ROOT \"$_sAppleseedRoot/src\" exists. (Appleseed was already cloned.)"
    _bAppleseedRootPrepared=true
  else
    stepInfo $_NAME "Appleseed will be cloned later."
  fi

elif [[ $_sAppleseedSource =~ ^.*\.zip ]]; then
  stepInfo $_NAME "APPLEESEED SOURCE is a ZIP file."
  _sAppleseedRoot=$_sRoot/appleseed
  if [ -d "$_sAppleseedRoot/src" ]; then
    stepInfo $_NAME "APPLEESEED ROOT \"$_sAppleseedRoot/src\" exists. (ZIP file was already unzipped.)"
    _bAppleseedRootPrepared=true
  else
    stepInfo $_NAME "Appleseed will be unzipped later."
  fi

elif [[ $_sAppleseedSource =~ ^.*\.tar ]] || [[ $_sAppleseedSource =~ ^.*\.tar\.gz ]]; then
  stepInfo $_NAME "APPLEESEED SOURCE is a TAR file."
  _sAppleseedRoot=$_sRoot/appleseed
  if [ -d "$_sAppleseedRoot/src" ]; then
    stepInfo $_NAME "APPLEESEED ROOT \"$_sAppleseedRoot/src\" exists. (TAR file was already unpacked.)"
    _bAppleseedRootPrepared=true
  else
    stepInfo $_NAME "Appleseed will be unpacked later."
  fi

elif [ -d $_sAppleseedSource ]; then
  stepInfo $_NAME "APPLEESEED SOURCE is a directory - setting APPLESEED ROOT accordingly."
  _sAppleseedRoot=$_sAppleseedSource
  _bAppleseedRootPrepared=true

else
  echo "Error: Could not identify nature of APPLESEED SOURCE: \"$_sAppleseedSource\". Exiting."
  exit 1

fi


# ----------------------------------------------------------------
# Echo Settings
# ----------------------------------------------------------------

stepInfo $_NAME "Confirm settings ..."

if [ $_bClang = true ]; then
  echo "Using Clang."
fi

echo "  Root directory: \"$_sRoot\""

if [ -n $_sBuildType ]; then
  echo "  Build type: $_sBuildType"
fi

if [[ $_bVerbose = true ]]; then
  echo "  Verbose mode enabled."
fi

# Paths:
echo "  Appleseed Root Directory: \"$_sAppleseedRoot\""

echo "  Install Directories:"
printf "${_COLOR_GRAY}  Note: If no install directory is given, it is searched in \"$_sDependenciesDir\" for an installation."
if [ $_bInstallDependencies = true ]; then
  printf " If non is found there, one will be installed there.${_COLOR_CLEAR}\n"
else
  printf "${_COLOR_CLEAR}\n"
fi

dependencyInstallInfo $_BOOST   $_sBoostInstallDir
dependencyInstallInfo $_EMBREE  $_sEmbreeInstallDir
dependencyInstallInfo $_OCIO    $_sOCIOInstallDir
dependencyInstallInfo $_OIIO    $_sOIIOInstallDir
dependencyInstallInfo $_OPENEXR $_sOpenEXRInstallDir
dependencyInstallInfo $_OSL     $_sOSLInstallDir
dependencyInstallInfo $_PARTIO  $_sPartIOInstallDir
dependencyInstallInfo $_XERCES  $_sXercesInstallDir

if [ $_bAsk = true ]; then
  # Checkpoint
  echo "Do you wish to proceed with these settings?"
  select strictreply in "Yes" "No"; do
    relaxedreply=${strictreply:-$REPLY}
    case $relaxedreply in
      Yes | YES | yes | y ) break;;
      No  | NO  | no  | n )
        echo "Tip: Use the -h or --help options to get usage help. Exiting.";
        exit 1;;
    esac
  done
fi

# ----------------------------------------------------------------
# Setup
# ----------------------------------------------------------------

stepInfo $_NAME "Setting Up ..."

# Create `dependencies` directory, if it does not exits.
$_DEBUG cd $_sRoot
$_DEBUG mkdir -p $_sDependenciesDir

# build essentials
$_DEBUG sudo apt install -y build-essential cmake

if [[ $_sCCompiler == "" ]]; then
  if [ $_bClang = true ]; then
    _sCCompiler=/usr/bin/clang
    _sCXXCompiler=/usr/bin/clang++
  else
    _sCCompiler=/usr/bin/gcc
    _sCXXCompiler=/usr/bin/g++
  fi
fi
# check compiler
if [ ! -f $_sCCompiler ]; then
  echo "Error: Could not find C compiler \"$_sCCompiler\". Exiting."
  exit 1
fi
if [ ! -f $_sCXXCompiler ]; then
  echo "Error: Could not find C++ compiler \"$_sCXXCompiler\". Exiting."
  exit 1
fi
# set compiler vars
export CC=$_sCCompiler
export CXX=$_sCXXCompiler

stepInfo $_NAME "Setup complete."

# ================================================================
# Installing Dependencies
# ================================================================

stepInfo $_NAME "Installing Dependencies ..."

# ----------------------------------------------------------------
# Boost
# ----------------------------------------------------------------

if [[ $_sBoostInstallDir = "" ]]; then

  # setup
  depName=$_BOOST
  _sTarFile=${_BOOST_DL##*/}
  sourceFile=${_sTarFile//"-b2-nodocs.tar.gz"/} # boost specific
  sourceVersion=${sourceFile//"boost-"/}
  _sSourceDir="$_sDependenciesDir/$sourceFile"
  _sInstallDir="$_sDependenciesDir/$depName-install"

  toolset=gcc
  if [ $_bClang = true ]; then
    toolset=clang
  fi

  # check for boost installation in _sInstallDir (via existence of lib file)
  if [ ! -f $_sInstallDir/lib/libboost_filesystem.so ]; then
    if [ $_bInstallDependencies = false ]; then
      echo "Error: Could not find a $depName installation in \"$_sDependenciesDir\". Exiting."
      exit 1
    fi

    # apt install dependencies (needed for python bindings with appleseed)
    $_DEBUG sudo apt install -y \
      python2.7-dev \
      pybind11-dev

    # Remove any files left from a previous failed install.
    $_DEBUG rm -fr $_sSourceDir
    $_DEBUG rm -fr $_sInstallDir
    
    # Install Boost
    stepInfo $_NAME "Installing $depName ..."

    $_DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $_DEBUG wget -c $_BOOST_DL
    fi

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $_DEBUG tar -zxf $_sTarFile

    $_DEBUG cd $_sSourceDir

    stepInfo $depName "Installing from \"$_sSourceDir\" ..."
    $_DEBUG ./bootstrap.sh --prefix=$_sInstallDir --with-toolset=$toolset --with-python-version=2.7

    # Configure/check configuration of project for Python 2.7
    if [[ $_DEBUG = "" ]]; then
      if grep -Fxq "# Python 2.7 Config" "project-config.jam"
      then
        echo "Already configured for Python 2.7 in \`project-config.jam\`."
      else
        echo "# Python 2.7 Config"                          >> project-config.jam
        echo "import toolset : using ;"                     >> project-config.jam
        echo "using python : 2.7 : /usr/bin/python2.7 ;"    >> project-config.jam
        echo ""                                             >> project-config.jam

        echo "Added configuration for Python 2.7 to project-config.jam"
      fi
    fi

    $_DEBUG ./b2 toolset=$toolset cxxflags="-std=c++$_CXX_STD" install
    # Installed Boost
  fi
  
  _sBoostInstallDir=$_sInstallDir
  _sBoostConfigDir="$_sInstallDir/lib/cmake/Boost-$sourceVersion"
  stepInfo $_NAME "$depName installed in \"$_sInstallDir\" with config file in \"$_sBoostConfigDir\"." $_COLOR_INSTALL_DIR

  # clean step
  $_DEBUG cleanInstallStep
fi

# ----------------------------------------------------------------
# Embree
# ----------------------------------------------------------------

if [[ $_sEmbreeInstallDir = "" && $_sWithEmbree = ON ]]; then

  # setup
  depName=$_EMBREE
  _sTarFile=${_EMBREE_DL##*/}
  tarFileDir="$_sDependenciesDir/$_sTarFile"
  sourceFile=${_sTarFile//".x86_64.linux.tar.gz"/}
  sourceVersion=${sourceFile//"embree-"/}
  _sSourceDir="$_sDependenciesDir/$sourceFile"
  _sInstallDir=$_sSourceDir # header only library

  # check for Embree installation in _sInstallDir (via existence of lib file)
  if [ ! -f $_sInstallDir/lib/libembree4.so ]; then
    if [ $_bInstallDependencies = false ]; then
      echo "Error: Could not find a $depName installation in \"$_sDependenciesDir\". Exiting."
      exit 1
    fi

    # Remove any files left from a previous failed install.
    $_DEBUG rm -fr $_sSourceDir
    $_DEBUG rm -fr $_sInstallDir
    
    # Install Embree
    stepInfo $_NAME "Installing $depName ..."

    $_DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $_DEBUG wget -c $_EMBREE_DL
    fi

    $_DEBUG mkdir $sourceFile
    $_DEBUG cd $sourceFile

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $_DEBUG tar -zxf $tarFileDir

    $_DEBUG cd ..

    # install
    stepInfo $depName "Installing from \"$sourceFile\" ..."
    $_DEBUG source "$sourceFile/embree-vars.sh"
  fi

  _sEmbreeInstallDir=$_sInstallDir
  _sEmbreeConfigDir="$_sInstallDir/lib/cmake/embree-$sourceVersion"
  stepInfo $_NAME "$depName installed in \"$_sInstallDir\" with config file in \"$_sEmbreeConfigDir\"." $_COLOR_INSTALL_DIR 

  # clean step
  _sSourceDir="" # else the install directory is removed
  $_DEBUG cleanInstallStep
fi


# ----------------------------------------------------------------
# Imath
# ----------------------------------------------------------------

if [[ $_sImathInstallDir = "" ]]; then

  # setup
  depName=$_IMATH
  _sTarFile=${_IMATH_DL##*/}
  sourceFile=${_sTarFile//".tar.gz"/}
  _sSourceDir="$_sDependenciesDir/$sourceFile"
  _sInstallDir="$_sDependenciesDir/$depName-install"

  # check for Imath installation in _sInstallDir (via existence of lib file)
  if [ ! -f $_sInstallDir/lib/libImath.so ]; then
    if [ $_bInstallDependencies = false ]; then
      echo "Error: Could not find a $depName installation in \"$_sDependenciesDir\". Exiting."
      exit 1
    fi

    # Remove any files left from a previous failed install.
    $_DEBUG rm -fr $_sSourceDir
    $_DEBUG rm -fr $_sInstallDir

    # Build Directory
    _sBuildDir="$_sDependenciesDir/$_IMATH-build"
    if [ -d $_sBuildDir ]; then
      $_DEBUG rm -fr $_sBuildDir
    fi
    $_DEBUG mkdir -p $_sBuildDir
    
    # Install Imath
    stepInfo $_NAME "Installing $depName ..."

    $_DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $_DEBUG wget -c $_IMATH_DL
    fi

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $_DEBUG tar -zxf $_sTarFile

    stepInfo $depName "Installing from \"$_sSourceDir\" ..."

    $_DEBUG cd $_sBuildDir

    $_DEBUG cmake $_sSourceDir --install-prefix $_sInstallDir \
      -DCMAKE_CXX_STANDARD=$_CXX_STD \
      -DIMATH_CXX_STANDARD=$_CXX_STD
    $_DEBUG cmake --build $_sBuildDir --target install --config Release -j$(nproc)
  fi

  stepInfo $_NAME "$depName installed in \"$_sInstallDir\"." $_COLOR_INSTALL_DIR
  _sImathInstallDir=$_sInstallDir

  # clean step
  $_DEBUG cleanInstallStep
fi


# ----------------------------------------------------------------
# OpenEXR
# ----------------------------------------------------------------

if [[ $_sOpenEXRInstallDir = "" ]]; then

  # setup
  depName=$_OPENEXR
  _sTarFile=${_OEXR_DL##*/}
  sourceFile=${_sTarFile//".tar.gz"/}
  _sSourceDir="$_sDependenciesDir/$sourceFile"
  _sInstallDir="$_sDependenciesDir/$depName-install"

  # check for OpenEXR installation in _sInstallDir (via existence of lib file)
  if [ ! -f $_sInstallDir/lib/libOpenEXRCore.so ]; then
    if [ $_bInstallDependencies = false ]; then
      echo "Error: Could not find a $depName installation in \"$_sDependenciesDir\". Exiting."
      exit 1
    fi

    # Remove any files left from a previous failed install.
    $_DEBUG rm -fr $_sSourceDir
    $_DEBUG rm -fr $_sInstallDir

    # Build Directory
    _sBuildDir="$_sDependenciesDir/$_OPENEXR-build"
    if [ -d $_sBuildDir ]; then
      $_DEBUG rm -fr $_sBuildDir
    fi
    $_DEBUG mkdir -p $_sBuildDir
    
    # Install OpenEXR
    stepInfo $_NAME "Installing $depName ..."

    $_DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $_DEBUG wget -c $_OEXR_DL
    fi

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $_DEBUG tar -zxf $_sTarFile

    stepInfo $depName "Installing from \"$_sSourceDir\" ..."

    $_DEBUG cd $_sBuildDir

    $_DEBUG cmake $_sSourceDir --install-prefix $_sInstallDir \
      -DCMAKE_CXX_STANDARD=$_CXX_STD \
      -DOPENEXR_CXX_STANDARD=$_CXX_STD \
      -DImath_ROOT=$_sImathInstallDir
    $_DEBUG cmake --build $_sBuildDir --target install --config Release -j$(nproc)
  fi

  stepInfo $_NAME "$depName installed in \"$_sInstallDir\"." $_COLOR_INSTALL_DIR
  _sOpenEXRInstallDir=$_sInstallDir

  # clean step
  $_DEBUG cleanInstallStep
fi


# ----------------------------------------------------------------
# OpenColorIO
# ----------------------------------------------------------------

if [[ $_sOCIOInstallDir = "" ]]; then

  # setup
  depName=$_OCIO
  _sTarFile=${_OCIO_DL##*/}
  sourceVersion=${_sTarFile//".tar.gz"/}
  sourceVersion=${sourceVersion//"v"/}
  sourceFile="OpenColorIO-$sourceVersion"
  _sSourceDir="$_sDependenciesDir/$sourceFile"
  _sInstallDir="$_sDependenciesDir/$depName-install"

  # check for OpenColorIO installation in _sInstallDir (via existence of lib file)
  if [ ! -f $_sInstallDir/lib/libOpenColorIO.so ]; then
    if [ $_bInstallDependencies = false ]; then
      echo "Error: Could not find a $depName installation in \"$_sDependenciesDir\". Exiting."
      exit 1
    fi

    # Remove any files left from a previous failed install.
    $_DEBUG rm -fr $_sSourceDir
    $_DEBUG rm -fr $_sInstallDir

    # Build Directory
    _sBuildDir="$_sDependenciesDir/$_OCIO-build"
    if [ -d $_sBuildDir ]; then
      $_DEBUG rm -fr $_sBuildDir
    fi
    $_DEBUG mkdir -p $_sBuildDir
    
    # Install OpenColorIO
    stepInfo $_NAME "Installing $depName ..."

    $_DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $_DEBUG wget -c $_OCIO_DL
    fi

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $_DEBUG tar -zxf $_sTarFile

    $_DEBUG cd $_sSourceDir

    stepInfo $depName "Installing from \"$_sSourceDir\" ..."

    $_DEBUG cd $_sBuildDir

    $_DEBUG cmake $_sSourceDir \
      -DCMAKE_CXX_STANDARD=$_CXX_STD \
      -DCMAKE_INSTALL_PREFIX=$_sInstallDir \
      -DImath_ROOT=$_sImathInstallDir \
      -DOCIO_BUILD_PYTHON=OFF
    $_DEBUG make install -j$(nproc)
  fi

  stepInfo $_NAME "$depName installed in \"$_sInstallDir\"." $_COLOR_INSTALL_DIR
  _sOCIOInstallDir=$_sInstallDir

  # clean step
  $_DEBUG cleanInstallStep
fi


# ----------------------------------------------------------------
# OpenImageIO
# ----------------------------------------------------------------

if [[ $_sOIIOInstallDir = "" ]]; then

  # setup
  depName=$_OIIO
  _sTarFile=${_OIIO_DL##*/}
  sourceFile=${_sTarFile//".tar.gz"/}
  _sSourceDir="$_sDependenciesDir/$sourceFile"
  _sInstallDir="$_sDependenciesDir/$depName-install"

  # check for OpenImageIO installation in _sInstallDir (via existence of lib file)
  if [ ! -f $_sInstallDir/lib/libOpenImageIO.so ]; then
    if [ $_bInstallDependencies = false ]; then
      echo "Error: Could not find a $depName installation in \"$_sDependenciesDir\". Exiting."
      exit 1
    fi

    # apt install dependencies
    $_DEBUG sudo apt install -y \
      python2.7-dev \
      pybind11-dev \
      zlib1g \
      zlib1g-dev \
      libtiff5-dev

    # Remove any files left from a previous failed install.
    $_DEBUG rm -fr $_sSourceDir
    $_DEBUG rm -fr $_sInstallDir
    
    # Install OpenImageIO
    stepInfo $_NAME "Installing $depName ..."

    $_DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $_DEBUG wget -c $_OIIO_DL
    fi

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $_DEBUG tar -zxf $_sTarFile

    $_DEBUG cd $_sSourceDir

    stepInfo $depName "Installing from \"$_sSourceDir\" ..."

    $_DEBUG cd $_sSourceDir
    
    $_DEBUG cmake -B build -S $_sSourceDir \
        -DCMAKE_CXX_STANDARD=$_CXX_STD \
        -DCMAKE_CXX_FLAGS=-D_GLIBCXX_USE_CXX11_ABI=1 \
        -DCMAKE_INSTALL_PREFIX=$_sInstallDir \
        -DUSE_PYTHON=1 \
        -DBoost_ROOT=$_sBoostInstallDir \
        -DImath_ROOT=$_sImathInstallDir \
        -DOpenEXR_ROOT=$_sOpenEXRInstallDir \
        -DOpenColorIO_ROOT=$_sOCIOInstallDir
    $_DEBUG cmake --build build --target install -j$(nproc)
  fi

  stepInfo $_NAME "$depName installed in \"$_sInstallDir\"." $_COLOR_INSTALL_DIR
  _sOIIOInstallDir=$_sInstallDir

  # clean step
  $_DEBUG cleanInstallStep
fi


# ----------------------------------------------------------------
# PartIO
# ----------------------------------------------------------------

if [[ $_sPartIOInstallDir = "" ]]; then

  # setup
  depname=$_PARTIO
  _sTarFile=${_PARTIO_DL##*/}
  sourceVersion=${_sTarFile//".tar.gz"/}
  sourceVersion=${sourceVersion//"v"/}
  sourceFile="partio-$sourceVersion"
  _sSourceDir="$_sDependenciesDir/$sourceFile"
  _sInstallDir="$_sDependenciesDir/$depname-install"

  # check for PartIO installation in _sInstallDir (via existence of lib file)
  if [ ! -f $_sInstallDir/lib/libpartio.so ]; then
    if [ $_bInstallDependencies = false ]; then
      echo "Error: Could not find a $depName installation in \"$_sDependenciesDir\". Exiting."
      exit 1
    fi

    # apt install dependencies
    $_DEBUG sudo apt install -y freeglut3-dev

    # Remove any files left from a previous failed install.
    $_DEBUG rm -fr $_sSourceDir
    $_DEBUG rm -fr $_sInstallDir
    
    # Install PartIO
    stepInfo $_NAME "Installing $depname ..."

    $_DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depname "Downloading $_sTarFile ..."
      $_DEBUG wget -c $_PARTIO_DL
    fi

    # unpack
    stepInfo $depname "Unpacking $_sTarFile ..."
    $_DEBUG tar -zxf $_sTarFile

    $_DEBUG cd $_sSourceDir

    stepInfo $depname "Installing from \"$_sSourceDir\" ..."
    
    $_DEBUG cd $_sSourceDir

    $_DEBUG make CXXFLAGS_STD=c++$_CXX_STD prefix=$_sInstallDir install -j$(nproc)
  fi

  stepInfo $_NAME "$depname installed in \"$_sInstallDir\"." $_COLOR_INSTALL_DIR
  _sPartIOInstallDir=$_sInstallDir

  # clean step
  $_DEBUG cleanInstallStep
fi


# ----------------------------------------------------------------
# OpenShadingLangauge
# ----------------------------------------------------------------

if [[ $_sOSLInstallDir = "" ]]; then

  # setup
  depName=$_OSL
  _sTarFile=${_OSL_DL##*/}
  sourceVersion=${_sTarFile//".tar.gz"/}
  sourceVersion=${sourceVersion//"v"/}
  sourceFile="OpenShadingLanguage-$sourceVersion"
  _sSourceDir="$_sDependenciesDir/$sourceFile"
  _sInstallDir="$_sDependenciesDir/$depName-install"

  # check for OpenShadingLangauge installation in _sInstallDir (via existence of lib file)
  if [ ! -f $_sInstallDir/lib/liboslcomp.so ]; then
    if [ $_bInstallDependencies = false ]; then
      echo "Error: Could not find a $depName installation in \"$_sDependenciesDir\". Exiting."
      exit 1
    fi

    # apt install dependencies
    $_DEBUG sudo apt install -y \
      flex \
      libbison-dev \
      libpugixml-dev

    # Remove any files left from a previous failed install.
    $_DEBUG rm -fr $_sSourceDir
    $_DEBUG rm -fr $_sInstallDir
    
    # Install OpenShadingLangauge
    stepInfo $_NAME "Installing $depName ..."

    $_DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $_DEBUG wget -c $_OSL_DL
    fi

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $_DEBUG tar -zxf $_sTarFile

    # LLVM Check
    if [[ -d $_sDependenciesDir/OSL-LLVM-intall || -d $_sSourceDir/src/build-scripts/llvm-install/ ]]; then
        stepInfo $depName "LLVM (via OSL's build script) already built." $_COLOR_GRAY
    else
        # apt install dependencies
        $_DEBUG sudo apt install -y curl

        stepInfo $depName "Building LLVM via OSL's build script..."
        $_DEBUG cd $_sSourceDir/src/build-scripts/
        $_DEBUG bash build_llvm.bash

        $_DEBUG cp -r $_sSourceDir/src/build-scripts/llvm-install/  $_sDependenciesDir/OSL-LLVM-intall
        stepInfo $depName "Built LLVM via OSL's build script."
    fi

    $_DEBUG cd $_sSourceDir

    # Fix OSL issue #1957
    stepInfo $depName "Fixing OSL issue #1957 ..."
    # check for patch file
    if [ -f $_sRoot/fixOSL1957.cpp ]; then
      _sOSLPatchFilePath="$_sRoot/fixOSL1957.cpp"
    elif [ -f $_SCRIPT_DIR/fixOSL1957.cpp ]; then
      _sOSLPatchFilePath="$_SCRIPT_DIR/fixOSL1957.cpp"
    else
      echo "Error: Patch file \`fixOSL1957.cpp\` needs to be in \"<ROOT>\" or in the same directory as this setup script. Exiting."
      exit 1
    fi
    stepInfo $depName "Found patch file \`fixOSL1957.cpp\` in \"$_sOSLPatchFilePath\"."
    # `llvm_util.cpp` with "fixed" version
    $_DEBUG cp -f $_sOSLPatchFilePath "$_sSourceDir/src/liboslexec/llvm_util.cpp"
    stepInfo $depName "Fixed OSL issue #1957."
    # done

    stepInfo $depName "Installing from \"$_sSourceDir\" ..."
    
    $_DEBUG cd $_sSourceDir

    $_DEBUG cmake -B build -S . \
      -DCMAKE_CXX_STANDARD=$_CXX_STD \
      -DCMAKE_INSTALL_PREFIX=$_sInstallDir \
      -DBoost_ROOT=$_sBoostInstallDir \
      -DImath_DIR=$_sImathInstallDir/lib/cmake/Imath \
      -DOpenEXR_DIR=$_sOpenEXRInstallDir/lib/cmake/OpenEXR \
      -DOpenImageIO_ROOT=$_sOIIOInstallDir/lib/cmake/OpenImageIO \
      -DLLVM_ROOT=$_sDependenciesDir/OSL-LLVM-intall \
      -Dpartio_ROOT=$_sPartIOInstallDir
    $_DEBUG cmake --build build --target install -j$(nproc)
  fi

  stepInfo $_NAME "$depName installed in \"$_sInstallDir\"." $_COLOR_INSTALL_DIR
  _sOSLInstallDir=$_sInstallDir

  # clean step
  $_DEBUG cleanInstallStep
fi


# ----------------------------------------------------------------
# Xerces
# ----------------------------------------------------------------

if [[ $_sXercesInstallDir = "" ]]; then

  # setup
  depName=$_XERCES
  _sTarFile=${_XERCES_DL##*/}
  sourceVersion=${_sTarFile//".tar.gz"/}
  sourceVersion=${sourceVersion//"v"/}
  sourceFile="xerces-c-$sourceVersion"
  _sSourceDir="$_sDependenciesDir/$sourceFile"
  _sInstallDir="$_sDependenciesDir/$depName-install"

  # check for Xerces installation in _sInstallDir (via existence of lib file)
  if [ ! -f $_sInstallDir/lib/libxerces-c.so ]; then
    if [ $_bInstallDependencies = false ]; then
      echo "Error: Could not find a $depName installation in \"$_sDependenciesDir\". Exiting."
      exit 1
    fi

    # Remove any files left from a previous failed install.
    $_DEBUG rm -fr $_sSourceDir
    $_DEBUG rm -fr $_sInstallDir
    
    # Install Xerces
    stepInfo $_NAME "Installing $depName ..."

    $_DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $_DEBUG wget -c $_XERCES_DL
    fi

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $_DEBUG tar -zxf $_sTarFile

    $_DEBUG cd $_sSourceDir

    stepInfo $depName "Installing from \"$_sSourceDir\" ..."
    
    $_DEBUG cd $_sSourceDir

    $_DEBUG mkdir build

    $_DEBUG cmake -G "Unix Makefiles" -S . -B build \
      -DCMAKE_BUILD_TYPE=Ship \
      -DCMAKE_CXX_STANDARD=$_CXX_STD \
      -DCMAKE_INSTALL_PREFIX=$_sInstallDir
    $_DEBUG cmake --build build --target install -j$(nproc)
  fi

  stepInfo $_NAME "$depName installed in \"$_sInstallDir\"." $_COLOR_INSTALL_DIR
  _sXercesInstallDir=$_sInstallDir

  # clean step
  $_DEBUG cleanInstallStep
fi


# The following is a template for installing dependencies. (It is not run [see `true = false`].)
# ----------------------------------------------------------------
# *Template*
# ----------------------------------------------------------------

if [[ true = false && $_sTODOInstallDir = "" ]]; then

  # setup
  depName=_TODO
  _sTarFile=${_TODO_DL##*/}
  _sSourceDir=TODO
  _sInstallDir="$_sDependenciesDir/$depName-install"

  # check for TODO installation in _sInstallDir (via existence of lib file)
  if [ ! -f $_sInstallDir/lib/TODO.so ]; then
    if [ $_bInstallDependencies = false ]; then
      echo "Error: Could not find a $depName installation in \"$_sDependenciesDir\". Exiting."
      exit 1
    fi

    # Remove any files left from a previous failed install.
    $_DEBUG rm -fr $_sSourceDir
    $_DEBUG rm -fr $_sInstallDir
    
    # Install TODO
    stepInfo $_NAME "Installing $depName ..."

    $_DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $_DEBUG wget -c $_TODO_DL
    fi

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $_DEBUG tar -zxf $_sTarFile

    $_DEBUG cd $_sSourceDir

    stepInfo $depName "Installing from \"$_sSourceDir\" ..."
    # TODO: install instructions
  fi

  stepInfo $_NAME "$depName installed in \"$_sInstallDir\"." $_COLOR_INSTALL_DIR
  _sTODOInstallDir=$_sInstallDir

  # clean step
  $_DEBUG cleanInstallStep
fi


# ----------------------------------------------------------------
# Done
# ----------------------------------------------------------------

stepInfo $_NAME "Dependencies installed."


# ================================================================
# Build Appleseed
# ================================================================

stepInfo $_NAME "Building $_APPLESEED ..."

# setup

if [ $_bAppleseedRootPrepared = false ]; then

  if [[ $_sAppleseedSource == "" ]]; then
    if [ ! -d "$_sAppleseedRoot/src" ]; then
      echo "Error: \"$_sAppleseedRoot\" does not contain a valid source for Appleseed. Exiting."
      exit 1
    fi

  elif [[ $_sAppleseedSource =~ ^git@.* ]] || [[ $_sAppleseedSource =~ ^https.* ]]; then
    stepInfo $_NAME "Cloning Appleseed into \"$_sRoot\" ..."
    $_DEBUG cd $_sRoot
    branch=""
    if [[ $_sAppleseedSourceGitBranch != "" ]]; then
      branch="--branch $_sAppleseedSourceGitBranch"
    fi
    $_DEBUG git clone $_sAppleseedSource $branch

  elif [[ $_sAppleseedSource =~ ^.*\.zip ]]; then
    stepInfo $_NAME "Unzipping into \"$_sRoot\" ..."
    bn=$(basename $_sAppleseedSource)
    bn=${bn//".zip"/}
    $_DEBUG unzip -q -d $_sRoot $_sAppleseedSource
    $_DEBUG mv ./$bn ./appleseed # rename

  elif [[ $_sAppleseedSource =~ ^.*\.tar\.gz ]]; then
    stepInfo $_NAME "Unpacking into \"$_sRoot\" ..."
    bn=$(basename $_sAppleseedSource)
    echo $bn
    bn=${bn//".tar.gz"/}
    $_DEBUG tar -zxf $_sAppleseedSource -C $_sRoot
    $_DEBUG mv ./$bn ./appleseed # rename

  elif [[ $_sAppleseedSource =~ ^.*\.tar ]]; then
    stepInfo $_NAME "Unpacking into \"$_sRoot\" ..."
    bn=$(basename $_sAppleseedSource)
    bn=${bn//".tar"/}
    $_DEBUG tar -xf $_sAppleseedSource -C $_sRoot
    $_DEBUG mv ./$bn ./appleseed # rename

  elif [ -d $_sAppleseedSource ]; then
    true # pass

  else
    echo "Error: Could not identify nature of APPLESEED SOURCE: \"$_sAppleseedSource\". Exiting."
    exit 1

  fi

  _bAppleseedRootPrepared=true

fi

# apt install dependencies
$_DEBUG sudo apt install -y \
  freeglut3-dev \
  libpng-dev \
  libpython2.7-dev \
  liblz4-dev \
  qtbase5-dev \
  zlib1g \
  zlib1g-dev
   

$_DEBUG cd $_sAppleseedRoot

if [ ! -d ./build ]; then
  _bNewBuild=true # build new if no build directory exists yet
fi

if [ $_bNewBuild = true ]; then
  $_DEBUG rm -fr build;
fi

$_DEBUG mkdir -p build
$_DEBUG cd build

# cmake
if [ $_bNewBuild = true ]; then
  stepInfo $_APPLESEED "Configuring CMake ..."
  $_DEBUG cmake \
    -Wno-dev \
    -DWARNINGS_AS_ERRORS=OFF \
    -DCMAKE_BUILD_TYPE=$_sBuildType \
    -DCMAKE_CXX_FLAGS=-D_GLIBCXX_USE_CXX11_ABI=1 \
    -DCMAKE_PREFIX_PATH=/usr/include/x86_64-linux-gnu/qt5 \
    -DUSE_SSE42=ON \
    -DUSE_STATIC_BOOST=OFF \
    -DBoost_DIR=$_sBoostConfigDir \
    -DBoost_NO_SYSTEM_PATHS=ON \
    -DEmbree_DIR=$_sEmbreeConfigDir \
    -DImath_ROOT=$_sImathInstallDir/lib/cmake/Imath \
    -DOpenColorIO_DIR=$_sOCIOInstallDir/lib/cmake/OpenColorIO \
    -DOpenEXR_ROOT=$_sOpenEXRInstallDir/lib/cmake/OpenEXR \
    -DOpenImageIO_DIR=$_sOIIOInstallDir/lib/cmake/OpenImageIO \
    -DOPENIMAGEIO_IDIFF=$_sOIIOInstallDir/bin/idiff \
    -DOPENIMAGEIO_OIIOTOOL=$_sOIIOInstallDir/bin/oiiotool \
    -DOSL_DIR=$_sOSLInstallDir/lib/cmake/OSL \
    -DXerces_DIR=$_sXercesInstallDir/lib/cmake/XercesC \
    -DWITH_CLI=$_sWithClient \
    -DWITH_STUDIO=$_sWithStudio \
    -DWITH_BENCH=$_sWithBench \
    -DWITH_TOOLS=$_sWithTools \
    -DWITH_PYTHON2_BINDINGS=$_sWithPython2Bindings \
    -DWITH_PYTHON3_BINDINGS=OFF \
    -DWITH_EMBREE=$_sWithEmbree \
    -DWITH_GPU=OFF \
    -DWITH_SPECTRAL_SUPPORT=OFF \
    ..
  stepInfo $_APPLESEED "Configured CMake."
fi

if [[ -f $_sAppleseedRoot/sandbox/lib/$_sBuildType/libappleseed.so && $_bNewBuild = false ]]; then
  stepInfo $_NAME "Appleseed is already built. (Add -n or --new to re-build.)"
else
  stepInfo $_APPLESEED "Building ..."
  $_DEBUG make $_sVerbose -j$(nproc)
  stepInfo $_APPLESEED "Built $_APPLESEED."
fi

# ----------------------------------------------------------------
# Done
# ----------------------------------------------------------------

stepInfo $_NAME "Built $_APPLESEED."

# ================================================================
# Utilities
# ================================================================

# ----------------------------------------------------------------
# Collect
# ----------------------------------------------------------------

if [[ $_bCollect = true ]]; then

  stepInfo "Collect Utility" "Collecting $_APPLESEED dependencies in \"$_sRoot/collected-deps\"."

  _sCollectBinPath=$_sRoot/collected-deps/bin
  _sCollectIncPath=$_sRoot/collected-deps/include
  _sCollectLibPath=$_sRoot/collected-deps/lib

  rm -fr $_sCollectBinPath $_sCollectIncPath $_sCollectLibPath
  mkdir -p $_sCollectBinPath $_sCollectIncPath $_sCollectLibPath

  cd $_sDependenciesDir

  collect $_sCollectType $_sBoostInstallDir
  if [[ $_sWithEmbree = ON ]]; then collect $_sCollectType $_sEmbreeInstallDir; fi
  collect $_sCollectType $_sOCIOInstallDir
  collect $_sCollectType $_sOIIOInstallDir
  collect $_sCollectType $_sOpenEXRInstallDir
  collect $_sCollectType $_sOSLInstallDir
  collect $_sCollectType $_sPartIOInstallDir
  collect $_sCollectType $_sXercesInstallDir

  stepInfo "Collect Utility" "Done."
fi


# ================================================================
# End
# ================================================================

stepInfo $_NAME "Script finished successfully."
