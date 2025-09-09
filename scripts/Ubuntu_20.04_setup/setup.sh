#!/bin/bash

set -e


# ================================================================
# README
# ================================================================
# This script can be used to install Appleseed and all of its dependencies.
#
# Easiest is to run it from Appleseed's root directory -- e.g. `./scripts/Ubuntu_20.04_setup/setup.sh`.
#
# Use -h or --help to get a list of all possible arguments and options.
#
#
# Below are constants which may be set by the user (though everything can also be set -- and is preferred to be set -- via flags).
#
# All variables beginning with `_` are internal variables and should NOT manually be changed, unless you know what you are doing!
# 
# Note: This script has only been tested on Ubuntu 22.04!

# ================================================================
# Set Constants
# ================================================================

# README: YOU CAN CHANGE THESE VARIABLES

# Note: Flags will overwrite the values here.

# Preview Mode
# Can be set using the `--preview` flag.
# If DEBUG is set to `echo` most commands (e.g. rm, wget, or running scripts) will only be printed and not executed.
DEBUG=
#DEBUG=echo

# Build Type
# Can be set using the `-b` or `--build` flag.
BUILD_TYPE="Ship"

# C and C++ Compiler
# Can be set using the `--cc` and `--cxx` flags respectively.
# Note: If not set here, or via flags, will search for it in `/usr/bin`.
C_COMPILER=
CXX_COMPILER=

# Optional Components
# Can be set using their flags (e.g. `--bench`, `--client`, etc.).
# Note: If at least one such flag is set, it will ONLY build those mentioned (via flag) optional components.
WITH_BENCH=ON
WITH_CLIENT=ON
WITH_STUDIO=ON
WITH_TOOLS=ON
WITH_PYTHON2_BINDINGS=ON
WITH_EMBREE=ON

# Download Links
BOOST_DL="https://github.com/boostorg/boost/releases/download/boost-1.88.0/boost-1.88.0-b2-nodocs.tar.gz"
EMBREE_DL="https://github.com/RenderKit/embree/releases/download/v4.4.0/embree-4.4.0.x86_64.linux.tar.gz"
IMATH_DL="https://github.com/AcademySoftwareFoundation/Imath/releases/download/v3.1.12/Imath-3.1.12.tar.gz"
OCIO_DL="https://github.com/AcademySoftwareFoundation/OpenColorIO/archive/refs/tags/v2.4.2.tar.gz"
OEXR_DL="https://github.com/AcademySoftwareFoundation/openexr/releases/download/v3.3.3/openexr-3.3.3.tar.gz"
OIIO_DL="https://github.com/AcademySoftwareFoundation/OpenImageIO/releases/download/v2.5.18.0/OpenImageIO-2.5.18.0.tar.gz"
OSL_DL="https://github.com/AcademySoftwareFoundation/OpenShadingLanguage/archive/refs/tags/v1.13.12.0.tar.gz" # TODO: update to 1.14
PARTIO_DL="https://github.com/wdas/partio/archive/refs/tags/v1.19.0.tar.gz"
XERCES_DL="https://github.com/apache/xerces-c/archive/refs/tags/v3.3.0.tar.gz"
HAPPLY_RP="https://github.com/MarcusTU/happly"

# README: YOU SHOULD NOT CHANGE ANY OF THE VARIABLES BELOW, UNLESS YOU KNOW WHAT YOU ARE DOING.

# ================================================================
# Constants
# ================================================================

# Script Directory -- path to the location of the script.
# (If `fixOSL1957.cpp` is not found in ROOT, it will also be searched for in the script's directory.)
_SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# DEPENDENCIES_DIR relative to ROOT
_DEFAULT_DEPENDENCIES_DIR_NAME="dependencies"

# C++ Standard
# Note: Building has only been tested for C++ standard 17.
#       C++ standard 17 is the minimum requirement for many of the dependencies build here and thus is our minimum requirement.
_CXX_STD=17

# ----------------------------------------------------------------
# Cosmetic Constants
# ----------------------------------------------------------------
# These constants are only for cosmetics (formatting, how things are named or colored, etc.).

# "Main Name" of the "master script".
_NAME="SETUP"

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
_HAPPLY=Happly

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
# Internal Variables
# ================================================================

_sRoot=$(pwd)
_sDependenciesDir=""
_bCustomDependenciesDir=false

_sAppleseedSource=""
_sAppleseedSourceGitBranch=""

# options
_bAsk=true
_bClang=false
_bInstallDependencies=true
_bNewBuild=false
_bNuke=false
_sVerbose=""

# utilities
_bCollect=false
_sCollectType="copy" # options: "copy" | "link"

# "no optional components flags seen" variable
_bNoOptCompFlags=true

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
_sHapplyInstallDir=""

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

optCompFlagCheck() {
  # If this is called (meaning a "optional component flag" is being handled by `handle_options`) then chick if `_bNoOptCompFlags` is true.
  # If it is, that means this is the first "optional component flag" seen. In this case all "WITH_X" variables are set to OFF and `_bNoOptCompFlags` is set to false. The appropriate "WITH_X" flag should then set to ON by `handle_options`.
  # Else nothing happens. (All "WITH_X" variables were already set to OFF once and should now be set to ON by `handle_options` when an appropriate "optional component flag" is seen.)
  if [ $_bNoOptCompFlags == true ]; then
    WITH_BENCH=OFF
    WITH_CLIENT=OFF
    WITH_STUDIO=OFF
    WITH_TOOLS=OFF
    WITH_PYTHON2_BINDINGS=OFF
    WITH_EMBREE=OFF

    _bNoOptCompFlags=false
  fi
}

cleanInstallStep() {
  if [[ $DEBUG != "" ]]; then echo "cleanInstallStep"; fi
  $DEBUG cd $_sDependenciesDir

  $DEBUG rm -f  $_sTarFile
  if [[ $_sSourceDir != $_sDependenciesDir ]]; then
    $DEBUG rm -fr $_sSourceDir
  fi
  if [[ $_sBuildDir != $_sDependenciesDir ]]; then
    $DEBUG rm -fr $_sBuildDir
  fi
  # Here we check if the source/build dir is != the deps dir.
  # -> Source/build dir is == the deps dir can happen if a "X_DL" variable is set wrongly.
  # With that we prevent the dependencies dir from being completely deleted by accident.

  _sTarFile=""
  _sSourceDir=""
  _sBuildDir=""
  _sInstallDir=""

  $DEBUG cd $_sRoot
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
    $DEBUG cd $installDir/bin
    for bin in $(ls -p | grep -v /); do
      $DEBUG $op $(pwd)/$bin $_sCollectBinPath/$bin
    done
    $DEBUG cd ..
  fi

  # include
  $DEBUG cd $installDir/include
  for header in $(ls); do
    $DEBUG $op $(pwd)/$header $_sCollectIncPath/$header
  done
  $DEBUG cd ..

  # libs
  $DEBUG cd $installDir/lib
  for lib in $(ls -p | grep -v /); do
    $DEBUG $op $(pwd)/$lib $_sCollectLibPath/$lib
  done
  $DEBUG cd ..
}

nuke() {
  $DEBUG rm -fr $_sRoot/build
  $DEBUG rm -fr $_sDependenciesDir
  echo "Removed \"$_sRoot/build\", \"$_sDependenciesDir\". Exiting."
  exit 0
}

# ----------------------------------------------------------------
# Formatting Functions
# ----------------------------------------------------------------

# Prints a "step info" of the form '~~> [Name] Step Message'.
stepInfo() {
  # $1 name $2 message (optional) $3 color 
  if [[ $3 = "" ]]; then
    printf "${_COLOR_DEFAULT}~~> [$1] $2${_COLOR_CLEAR}\n"
  else
    color=$3
    printf "${color}~~> [$1] $2${_COLOR_CLEAR}\n"
  fi
}

# Makes a fully capitalized "root variable name" of the form 'NAME_ROOT'.
rootVarName() {
  # $1 name
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

optionalComponentBuildInfo() {
  # $1 _NAME, $2 WITH_X
  if [ $2 = ON ]; then echo "    $1"; fi
}

# ----------------------------------------------------------------
# Handle Arguments and Options Function
# ----------------------------------------------------------------

# Function to display script usage
usage() {
  echo "Usage: $0"
  echo "Arguments:"
  echo "  -b, --build         Specify a build type. (Defaults to \"Ship\".)"
  echo "  --cc                Specify a c compiler (path to). (Will use the gcc compiler by default.)"
  echo "  --cxx               Specify a c++ compiler (path to). (Will use the g++ compiler by default.)"
  echo "  -d, --deps   DEPS   Specify the directory containing the install directories of dependencies (who's install directory are not explicitly given with one of the arguments below). (Defaults to \"ROOT/$_DEFAULT_DEPENDENCIES_DIR_NAME\".)"
  echo "  -r, --root   ROOT   Specify the Appleseed root directory. (Defaults to the current working directory.)"
  echo "  -s, --source SOURCE Specify a source for Appleseed. This may be a Git HTTPS or SSH link, or a TAR or ZIP file."
  echo "                      The script will then clone/unpack the source using ROOT as the root directory."
printf "                      ${_COLOR_ORANGE}Warning: Fetching a new Appleseed repository source fill remove **everything** in ROOT.${_COLOR_CLEAR}\n"
  echo "  --branch            Specify a branch for SOURCE, if it is a Git link. (Defaults to specifying no branch.)"
  echo ""
  echo "  Install directories of dependencies. If non is given for a specific dependency, it will be downloaded and installed in \"ROOT/$_DEFAULT_DEPENDENCIES_DIR_NAME\" (or \"DEPS\", if it is given)."
  echo "  --boost-install"
  echo "  --embree-install"
  echo "  --imath-install"
  echo "  --ocio-install"
  echo "  --oiio-install"
  echo "  --openexr-install"
  echo "  --osl-install"
  echo "  --partio-install"
  echo "  --xerces-install"
  echo "  --happly-install"
  echo ""
  echo "Options:"
  echo "  -h, --help          Display this help message."
  echo "  -n, --new           Delete Appleseed's build directory to start new."
  echo "  --no-install        Do not install any missing dependencies, exit instead."
  echo "  --nuke              Remove all directories (and their contents) created by this script."
  echo "  --preview           Run in *preview mode*, where most commands (e.g. rm, wget, or running scripts) are only printed and not executed."
  echo "  --use-clang         Compile with Clang instead of gcc. (Will search for compiler in \"usr/bin/\".) (Note: This is necessary even if --cc and --cxx is given, when building boost, as boost builds with different \"tool sets\" [here: gcc or clang].)"
  echo "  -v, --verbose       Verbose mode (only affects Appleseed's build)."
  echo "  -y, --yes           Assume \"yes\" for all queries."
  echo ""
  echo "Build only Optional Appleseed Components:"
  echo "  If non are specified, builds all components."
  echo "  --client            Built the client (WITH_CLIENT=ON)."
  echo "  --studio            Built the studio (WITH_STUDIO=ON)."
  echo "  --bench             Built the bench (WITH_BENCH=ON)."
  echo "  --tools             Built the tools (WITH_TOOLS=ON)."
  echo "  --python2-bindings  Built the Python 2.7 bindings (WITH_PYTHON2_BINDINGS=ON)."
  echo "  --embree            Built with Embree (WITH_EMBREE=ON)."
  echo ""
  echo "Utilities:"
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
      -b | --build)
        if ! has_argument $@; then
            echo "BUILD TYPE was given no value." >&2
            usage
            exit 1
        fi
        BUILD_TYPE=$(extract_argument $@)
        shift
        ;;
      --cc)
        if ! has_argument $@; then
            echo "C COMPILER was given no value." >&2
            usage
            exit 1
        fi
        C_COMPILER=$(extract_argument $@)
        shift
        ;;
      --cxx)
        if ! has_argument $@; then
            echo "C++ COMPILER was given no value." >&2
            usage
            exit 1
        fi
        CXX_COMPILER=$(extract_argument $@)
        shift
        ;;
      -d | --deps)
        if ! has_argument $@; then
            echo "DEPS was given no value." >&2
            usage
            exit 1
        fi
        echo "DEPS extract argument called with:" $@
        _sDependenciesDir=$(extract_argument $@)
        echo "Extracted:" $_sDependenciesDir
        _bCustomDependenciesDir=true
        shift
        ;;
      -r | --root)
        if ! has_argument $@; then
            echo "ROOT was given no value." >&2
            usage
            exit 1
        fi
        _sRoot=$(realpath $(extract_argument $@))
        shift
        ;;
      -s | --source)
        if ! has_argument $@; then
            echo "SOURCE was given no value." >&2
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
      --happly-install)
        if ! has_argument $@; then
            echo "$(rootVarName $_HAPPLY)" >&2
            usage
            exit 1
        fi
        _sHapplyInstallDir=$(extract_argument $@)
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
        DEBUG=echo
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
      --bench)
        optCompFlagCheck
        WITH_BENCH=ON
        ;;
      --client)
        optCompFlagCheck
        WITH_CLIENT=ON
        ;;
      --studio)
        optCompFlagCheck
        WITH_STUDIO=ON
        WITH_PYTHON2_BINDINGS=ON
        ;;
      --tools)
        optCompFlagCheck
        WITH_TOOLS=ON
        ;;
      --python2-bindings)
        optCompFlagCheck
        WITH_PYTHON2_BINDINGS=ON
        ;;
      --embree)
        optCompFlagCheck
        WITH_PYTHON2_BINDINGS=ON
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

if [[ $DEBUG != "" ]]; then
  printf "${_COLOR_PINK}*Script running in PREVIEW MODE.*${_COLOR_CLEAR}\n"
fi

# Root Directory

sRemoveSuffix=""
case $_sRoot in
  *scripts/Ubuntu_20.04_setup/) sRemoveSuffix=scripts/Ubuntu_20.04_setup/;;
  *scripts/Ubuntu_20.04_setup)  sRemoveSuffix=scripts/Ubuntu_20.04_setup;;
  *scripts/)                    sRemoveSuffix=scripts/;;
  *scripts)                     sRemoveSuffix=scripts;;
  *);; # pass
esac

if [[ $sRemoveSuffix != "" ]]; then
  withSuffixRemoved=${_sRoot//$sRemoveSuffix/}
  echo "Current ROOT \"$_sRoot\" may be a child directory of Appleseed's root directory. Do you want to set ROOT to \"$withSuffixRemoved\" instead?"
  select strictreply in "Yes" "No"; do
    relaxedreply=${strictreply:-$REPLY}
    case $relaxedreply in
      Yes | YES | yes | y )
        _sRoot=$withSuffixRemoved
        break
        ;;
      No  | NO  | no  | n )
        break
        ;;
    esac
  done
fi

if [[ $_sAppleseedSource == "" ]] && [ ! -f $_sRoot/src/appleseed/main/dllmain.cpp ]; then
  echo "Error: \"$_sRoot\" is not a valid ROOT directory. Exiting."
  exit 1
fi

# Dependencies Directory

if [ $_bCustomDependenciesDir = false ]; then
  _sDependenciesDir=$_sRoot/$_DEFAULT_DEPENDENCIES_DIR_NAME
fi

echo "Dependencies Directory: $_sDependenciesDir"

# ----------------------------------------------------------------
# Echo Settings
# ----------------------------------------------------------------

stepInfo $_NAME "Confirm settings ..."

if [ -n $BUILD_TYPE ]; then
  echo "  Build type: $BUILD_TYPE"
fi

if [ $_bClang = true ]; then
  echo "  Using Clang."
fi

if [[ $_bVerbose = true ]]; then
  echo "  Verbose mode enabled."
fi

# Paths:
echo "  Root Directory: \"$_sRoot\""
if [[ $_sAppleseedSource != "" ]]; then
  echo "  Appleseed Source: \"$_sAppleseedSource\""
fi

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
dependencyInstallInfo $_HAPPLY  $_sHapplyInstallDir

# Optional Dependencies
echo "  Building the following optional dependencies:"
optionalComponentBuildInfo Client             $WITH_CLIENT
optionalComponentBuildInfo Studio             $WITH_STUDIO
optionalComponentBuildInfo Bench              $WITH_BENCH
optionalComponentBuildInfo Tools              $WITH_TOOLS
optionalComponentBuildInfo "Python2 Bindings" $WITH_PYTHON2_BINDINGS
optionalComponentBuildInfo Embree             $WITH_EMBREE

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

if [[ $_sAppleseedSource != "" ]]; then

  stepInfo $_NAME "Sourcing Appleseed ..."

  if [[ $_sAppleseedSource =~ ^git@.* ]] || [[ $_sAppleseedSource =~ ^https.* ]]; then
    stepInfo $_NAME "Cloning Appleseed into \"$_sRoot\" ..."
    $DEBUG cd $_sRoot
    $DEBUG rm -fr *
    $DEBUG cd ..
    branch=""
    if [[ $_sAppleseedSourceGitBranch != "" ]]; then
      branch="--branch $_sAppleseedSourceGitBranch"
    fi
    $DEBUG git clone $_sAppleseedSource $branch

  elif [[ $_sAppleseedSource =~ ^.*\.zip ]]; then
    stepInfo $_NAME "Unzipping into \"$_sRoot\" ..."
    $DEBUG cd $_sRoot
    $DEBUG cd ..
    $DEBUG rm -fr appleseed
    bn=$(basename $_sAppleseedSource)
    bn=${bn//".zip"/}
    $DEBUG unzip -q -d "." $_sAppleseedSource
    $DEBUG mv ./$bn ./appleseed # rename

  elif [[ $_sAppleseedSource =~ ^.*\.tar\.gz ]]; then
    stepInfo $_NAME "Unpacking into \"$_sRoot\" ..."
    $DEBUG cd $_sRoot
    $DEBUG cd ..
    $DEBUG rm -fr appleseed
    bn=$(basename $_sAppleseedSource)
    echo $bn
    bn=${bn//".tar.gz"/}
    $DEBUG tar -zxf $_sAppleseedSource -C "."
    $DEBUG mv ./$bn ./appleseed # rename

  elif [[ $_sAppleseedSource =~ ^.*\.tar ]]; then
    stepInfo $_NAME "Unpacking into \"$_sRoot\" ..."
    $DEBUG cd $_sRoot
    $DEBUG cd ..
    $DEBUG rm -fr appleseed
    bn=$(basename $_sAppleseedSource)
    bn=${bn//".tar"/}
    $DEBUG tar -xf $_sAppleseedSource -C "."
    $DEBUG mv ./$bn ./appleseed # rename

  else
    echo "Error: Could not handle SOURCE: \"$_sAppleseedSource\". Exiting."
    exit 1

  fi

  stepInfo $_NAME "Sourcing Appleseed."
fi

# Create `dependencies` directory, if it does not exits.
$DEBUG cd $_sRoot
echo "Creating dependencies directory \"$_sDependenciesDir\" ..."
$DEBUG mkdir -p $_sDependenciesDir

# build essentials
$DEBUG sudo apt install -y build-essential cmake

if [[ $C_COMPILER == "" ]]; then
  if [ $_bClang = true ]; then
    C_COMPILER=/usr/bin/clang
    CXX_COMPILER=/usr/bin/clang++
  else
    C_COMPILER=/usr/bin/gcc
    CXX_COMPILER=/usr/bin/g++
  fi
fi
# check compiler
if [ ! -f $C_COMPILER ]; then
  echo "Error: Could not find C compiler \"$C_COMPILER\". Exiting."
  exit 1
fi
if [ ! -f $CXX_COMPILER ]; then
  echo "Error: Could not find C++ compiler \"$CXX_COMPILER\". Exiting."
  exit 1
fi
# set compiler vars
export CC=$C_COMPILER
export CXX=$CXX_COMPILER

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
  _sTarFile=${BOOST_DL##*/}
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
    $DEBUG sudo apt install -y \
      python2.7-dev \
      pybind11-dev

    # Remove any files left from a previous failed install.
    $DEBUG rm -fr $_sSourceDir
    $DEBUG rm -fr $_sInstallDir
    
    # Install Boost
    stepInfo $_NAME "Installing $depName ..."

    $DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $DEBUG wget -c $BOOST_DL
    fi

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $DEBUG tar -zxf $_sTarFile

    $DEBUG cd $_sSourceDir

    stepInfo $depName "Installing from \"$_sSourceDir\" ..."
    $DEBUG ./bootstrap.sh --prefix=$_sInstallDir --with-toolset=$toolset --with-python-version=2.7

    # Configure/check configuration of project for Python 2.7
    if [[ $DEBUG = "" ]]; then
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

    $DEBUG ./b2 toolset=$toolset cxxflags="-std=c++$_CXX_STD" install
    # Installed Boost
  fi
  
  _sBoostInstallDir=$_sInstallDir
  _sBoostConfigDir="$_sInstallDir/lib/cmake/Boost-$sourceVersion"
  stepInfo $_NAME "$depName installed in \"$_sInstallDir\" with config file in \"$_sBoostConfigDir\"." $_COLOR_INSTALL_DIR

  # clean step
  cleanInstallStep
fi

# ----------------------------------------------------------------
# Embree
# ----------------------------------------------------------------

if [[ $_sEmbreeInstallDir = "" && $WITH_EMBREE = ON ]]; then

  # setup
  depName=$_EMBREE
  _sTarFile=${EMBREE_DL##*/}
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
    $DEBUG rm -fr $_sSourceDir
    $DEBUG rm -fr $_sInstallDir
    
    # Install Embree
    stepInfo $_NAME "Installing $depName ..."

    $DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $DEBUG wget -c $EMBREE_DL
    fi

    $DEBUG mkdir $sourceFile
    $DEBUG cd $sourceFile

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $DEBUG tar -zxf $tarFileDir

    $DEBUG cd ..

    # install
    stepInfo $depName "Installing from \"$sourceFile\" ..."
    $DEBUG source "$sourceFile/embree-vars.sh"
  fi

  _sEmbreeInstallDir=$_sInstallDir
  _sEmbreeConfigDir="$_sInstallDir/lib/cmake/embree-$sourceVersion"
  stepInfo $_NAME "$depName installed in \"$_sInstallDir\" with config file in \"$_sEmbreeConfigDir\"." $_COLOR_INSTALL_DIR 

  # clean step
  _sSourceDir="" # else the install directory is removed
  cleanInstallStep
fi


# ----------------------------------------------------------------
# Imath
# ----------------------------------------------------------------

if [[ $_sImathInstallDir = "" ]]; then

  # setup
  depName=$_IMATH
  _sTarFile=${IMATH_DL##*/}
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
    $DEBUG rm -fr $_sSourceDir
    $DEBUG rm -fr $_sInstallDir

    # Build Directory
    _sBuildDir="$_sDependenciesDir/$_IMATH-build"
    if [ -d $_sBuildDir ]; then
      $DEBUG rm -fr $_sBuildDir
    fi
    $DEBUG mkdir -p $_sBuildDir
    
    # Install Imath
    stepInfo $_NAME "Installing $depName ..."

    $DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $DEBUG wget -c $IMATH_DL
    fi

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $DEBUG tar -zxf $_sTarFile

    stepInfo $depName "Installing from \"$_sSourceDir\" ..."

    $DEBUG cd $_sBuildDir

    $DEBUG cmake $_sSourceDir --install-prefix $_sInstallDir \
      -DCMAKE_CXX_STANDARD=$_CXX_STD \
      -DIMATH_CXX_STANDARD=$_CXX_STD
    $DEBUG cmake --build $_sBuildDir --target install --config Release -j$(nproc)
  fi

  stepInfo $_NAME "$depName installed in \"$_sInstallDir\"." $_COLOR_INSTALL_DIR
  _sImathInstallDir=$_sInstallDir

  # clean step
  cleanInstallStep
fi


# ----------------------------------------------------------------
# OpenEXR
# ----------------------------------------------------------------

if [[ $_sOpenEXRInstallDir = "" ]]; then

  # setup
  depName=$_OPENEXR
  _sTarFile=${OEXR_DL##*/}
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
    $DEBUG rm -fr $_sSourceDir
    $DEBUG rm -fr $_sInstallDir

    # Build Directory
    _sBuildDir="$_sDependenciesDir/$_OPENEXR-build"
    if [ -d $_sBuildDir ]; then
      $DEBUG rm -fr $_sBuildDir
    fi
    $DEBUG mkdir -p $_sBuildDir
    
    # Install OpenEXR
    stepInfo $_NAME "Installing $depName ..."

    $DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $DEBUG wget -c $OEXR_DL
    fi

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $DEBUG tar -zxf $_sTarFile

    stepInfo $depName "Installing from \"$_sSourceDir\" ..."

    $DEBUG cd $_sBuildDir

    $DEBUG cmake $_sSourceDir --install-prefix $_sInstallDir \
      -DCMAKE_CXX_STANDARD=$_CXX_STD \
      -DOPENEXR_CXX_STANDARD=$_CXX_STD \
      -DImath_ROOT=$_sImathInstallDir
    $DEBUG cmake --build $_sBuildDir --target install --config Release -j$(nproc)
  fi

  stepInfo $_NAME "$depName installed in \"$_sInstallDir\"." $_COLOR_INSTALL_DIR
  _sOpenEXRInstallDir=$_sInstallDir

  # clean step
  cleanInstallStep
fi


# ----------------------------------------------------------------
# OpenColorIO
# ----------------------------------------------------------------

if [[ $_sOCIOInstallDir = "" ]]; then

  # setup
  depName=$_OCIO
  _sTarFile=${OCIO_DL##*/}
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
    $DEBUG rm -fr $_sSourceDir
    $DEBUG rm -fr $_sInstallDir

    # Build Directory
    _sBuildDir="$_sDependenciesDir/$_OCIO-build"
    if [ -d $_sBuildDir ]; then
      $DEBUG rm -fr $_sBuildDir
    fi
    $DEBUG mkdir -p $_sBuildDir
    
    # Install OpenColorIO
    stepInfo $_NAME "Installing $depName ..."

    $DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $DEBUG wget -c $OCIO_DL
    fi

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $DEBUG tar -zxf $_sTarFile

    $DEBUG cd $_sSourceDir

    stepInfo $depName "Installing from \"$_sSourceDir\" ..."

    $DEBUG cd $_sBuildDir

    $DEBUG cmake $_sSourceDir \
      -DCMAKE_CXX_STANDARD=$_CXX_STD \
      -DCMAKE_INSTALL_PREFIX=$_sInstallDir \
      -DImath_ROOT=$_sImathInstallDir \
      -DOCIO_BUILD_PYTHON=OFF
    $DEBUG make install -j$(nproc)
  fi

  stepInfo $_NAME "$depName installed in \"$_sInstallDir\"." $_COLOR_INSTALL_DIR
  _sOCIOInstallDir=$_sInstallDir

  # clean step
  cleanInstallStep
fi


# ----------------------------------------------------------------
# OpenImageIO
# ----------------------------------------------------------------

if [[ $_sOIIOInstallDir = "" ]]; then

  # setup
  depName=$_OIIO
  _sTarFile=${OIIO_DL##*/}
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
    $DEBUG sudo apt install -y \
      python2.7-dev \
      pybind11-dev \
      zlib1g \
      zlib1g-dev \
      libtiff5-dev

    # Remove any files left from a previous failed install.
    $DEBUG rm -fr $_sSourceDir
    $DEBUG rm -fr $_sInstallDir
    
    # Install OpenImageIO
    stepInfo $_NAME "Installing $depName ..."

    $DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $DEBUG wget -c $OIIO_DL
    fi

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $DEBUG tar -zxf $_sTarFile

    $DEBUG cd $_sSourceDir

    stepInfo $depName "Installing from \"$_sSourceDir\" ..."

    $DEBUG cd $_sSourceDir
    
    $DEBUG cmake -B build -S $_sSourceDir \
        -DCMAKE_CXX_STANDARD=$_CXX_STD \
        -DCMAKE_CXX_FLAGS=-D_GLIBCXX_USE_CXX11_ABI=1 \
        -DCMAKE_INSTALL_PREFIX=$_sInstallDir \
        -DUSE_PYTHON=1 \
        -DBoost_ROOT=$_sBoostInstallDir \
        -DBoost_DIR=$_sBoostConfigDir \
        -DImath_ROOT=$_sImathInstallDir \
        -DOpenEXR_ROOT=$_sOpenEXRInstallDir \
        -DOpenColorIO_ROOT=$_sOCIOInstallDir
    $DEBUG cmake --build build --target install -j$(nproc)
  fi

  stepInfo $_NAME "$depName installed in \"$_sInstallDir\"." $_COLOR_INSTALL_DIR
  _sOIIOInstallDir=$_sInstallDir

  # clean step
  cleanInstallStep
fi


# ----------------------------------------------------------------
# PartIO
# ----------------------------------------------------------------

if [[ $_sPartIOInstallDir = "" ]]; then

  # setup
  depname=$_PARTIO
  _sTarFile=${PARTIO_DL##*/}
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
    $DEBUG sudo apt install -y freeglut3-dev

    # Remove any files left from a previous failed install.
    $DEBUG rm -fr $_sSourceDir
    $DEBUG rm -fr $_sInstallDir
    
    # Install PartIO
    stepInfo $_NAME "Installing $depname ..."

    $DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depname "Downloading $_sTarFile ..."
      $DEBUG wget -c $PARTIO_DL
    fi

    # unpack
    stepInfo $depname "Unpacking $_sTarFile ..."
    $DEBUG tar -zxf $_sTarFile

    $DEBUG cd $_sSourceDir

    stepInfo $depname "Installing from \"$_sSourceDir\" ..."
    
    $DEBUG cd $_sSourceDir

    $DEBUG make CXXFLAGS_STD=c++$_CXX_STD prefix=$_sInstallDir install -j$(nproc)
  fi

  stepInfo $_NAME "$depname installed in \"$_sInstallDir\"." $_COLOR_INSTALL_DIR
  _sPartIOInstallDir=$_sInstallDir

  # clean step
  cleanInstallStep
fi


# ----------------------------------------------------------------
# OpenShadingLangauge
# ----------------------------------------------------------------

if [[ $_sOSLInstallDir = "" ]]; then

  # setup
  depName=$_OSL
  _sTarFile=${OSL_DL##*/}
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
    $DEBUG sudo apt install -y \
      flex \
      libbison-dev \
      libpugixml-dev

    # Remove any files left from a previous failed install.
    $DEBUG rm -fr $_sSourceDir
    $DEBUG rm -fr $_sInstallDir
    
    # Install OpenShadingLangauge
    stepInfo $_NAME "Installing $depName ..."

    $DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $DEBUG wget -c $OSL_DL
    fi

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $DEBUG tar -zxf $_sTarFile

    # LLVM Check
    if [[ -d $_sDependenciesDir/OSL-LLVM-intall || -d $_sSourceDir/src/build-scripts/llvm-install/ ]]; then
        stepInfo $depName "LLVM (via OSL's build script) already built." $_COLOR_GRAY
    else
        # apt install dependencies
        $DEBUG sudo apt install -y curl

        stepInfo $depName "Building LLVM via OSL's build script..."
        $DEBUG cd $_sSourceDir/src/build-scripts/
        $DEBUG bash build_llvm.bash

        $DEBUG cp -r $_sSourceDir/src/build-scripts/llvm-install/  $_sDependenciesDir/OSL-LLVM-intall
        stepInfo $depName "Built LLVM via OSL's build script."
    fi

    $DEBUG cd $_sSourceDir

    # Fix OSL issue #1957
    stepInfo $depName "Fixing OSL issue #1957 ..."
    # check for patch file
    if [ -f $_sRoot/fixOSL1957.cpp ]; then
      _sOSLPatchFilePath="$_sRoot/fixOSL1957.cpp"
    elif [ -f $_sRoot/scripts/Ubuntu_20.04_setup/fixOSL1957.cpp ]; then
      _sOSLPatchFilePath="$_sRoot/scripts/Ubuntu_20.04_setup/fixOSL1957.cpp"
    elif [ -f $_SCRIPT_DIR/fixOSL1957.cpp ]; then
      _sOSLPatchFilePath="$_SCRIPT_DIR/fixOSL1957.cpp"
    else
      echo "Error: Patch file \`fixOSL1957.cpp\` needs to be in \"<ROOT>\" or in the same directory as this setup script. Exiting."
      exit 1
    fi
    stepInfo $depName "Found patch file \`fixOSL1957.cpp\` in \"$_sOSLPatchFilePath\"."
    # `llvm_util.cpp` with "fixed" version
    $DEBUG cp -f $_sOSLPatchFilePath "$_sSourceDir/src/liboslexec/llvm_util.cpp"
    stepInfo $depName "Fixed OSL issue #1957."
    # done

    stepInfo $depName "Installing from \"$_sSourceDir\" ..."
    
    $DEBUG cd $_sSourceDir

    $DEBUG cmake -B build -S . \
      -DCMAKE_CXX_STANDARD=$_CXX_STD \
      -DCMAKE_INSTALL_PREFIX=$_sInstallDir \
      -DBoost_ROOT=$_sBoostInstallDir \
      -DBoost_DIR=$_sBoostConfigDir \
      -DImath_DIR=$_sImathInstallDir/lib/cmake/Imath \
      -DOpenEXR_DIR=$_sOpenEXRInstallDir/lib/cmake/OpenEXR \
      -DOpenImageIO_ROOT=$_sOIIOInstallDir/lib/cmake/OpenImageIO \
      -DLLVM_ROOT=$_sDependenciesDir/OSL-LLVM-intall \
      -Dpartio_ROOT=$_sPartIOInstallDir
    $DEBUG cmake --build build --target install -j$(nproc)
  fi

  stepInfo $_NAME "$depName installed in \"$_sInstallDir\"." $_COLOR_INSTALL_DIR
  _sOSLInstallDir=$_sInstallDir

  # clean step
  cleanInstallStep
fi


# ----------------------------------------------------------------
# Xerces
# ----------------------------------------------------------------

if [[ $_sXercesInstallDir = "" ]]; then

  # setup
  depName=$_XERCES
  _sTarFile=${XERCES_DL##*/}
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
    $DEBUG rm -fr $_sSourceDir
    $DEBUG rm -fr $_sInstallDir
    
    # Install Xerces
    stepInfo $_NAME "Installing $depName ..."

    $DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $DEBUG wget -c $XERCES_DL
    fi

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $DEBUG tar -zxf $_sTarFile

    $DEBUG cd $_sSourceDir

    stepInfo $depName "Installing from \"$_sSourceDir\" ..."
    
    $DEBUG cd $_sSourceDir

    $DEBUG mkdir build

    $DEBUG cmake -G "Unix Makefiles" -S . -B build \
      -DCMAKE_BUILD_TYPE=Ship \
      -DCMAKE_CXX_STANDARD=$_CXX_STD \
      -DCMAKE_INSTALL_PREFIX=$_sInstallDir
    $DEBUG cmake --build build --target install -j$(nproc)
  fi

  stepInfo $_NAME "$depName installed in \"$_sInstallDir\"." $_COLOR_INSTALL_DIR
  _sXercesInstallDir=$_sInstallDir

  # clean step
  cleanInstallStep
fi

# ----------------------------------------------------------------
# Happly
# ----------------------------------------------------------------

if [[ $_sHapplyInstallDir = "" ]]; then

  # setup
  depName=$_HAPPLY
  _sSourceDir="$_sDependenciesDir/$depName"
  _sHapplyInstallDir=$_sSourceDir

  # clone repository if not already
    if [ ! -d $_sSourceDir ]; then
        echo "clone $HAPPLY_RP to $_sSourceDir"
        $_DEBUG git clone $HAPPLY_RP $_sSourceDir  
    fi

  stepInfo $_NAME "$depName cloned to \"$_sSourceDir\"."
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
    $DEBUG rm -fr $_sSourceDir
    $DEBUG rm -fr $_sInstallDir
    
    # Install TODO
    stepInfo $_NAME "Installing $depName ..."

    $DEBUG cd $_sDependenciesDir
    
    # download if not already
    if [[ ! -f $_sTarFile ]]; then
      stepInfo $depName "Downloading $_sTarFile ..."
      $DEBUG wget -c $TODO_DL
    fi

    # unpack
    stepInfo $depName "Unpacking $_sTarFile ..."
    $DEBUG tar -zxf $_sTarFile

    $DEBUG cd $_sSourceDir

    stepInfo $depName "Installing from \"$_sSourceDir\" ..."
    # TODO: install instructions
  fi

  stepInfo $_NAME "$depName installed in \"$_sInstallDir\"." $_COLOR_INSTALL_DIR
  _sTODOInstallDir=$_sInstallDir

  # clean step
  cleanInstallStep
fi

# ----------------------------------------------------------------
# Done
# ----------------------------------------------------------------

stepInfo $_NAME "Dependencies installed."


# ================================================================
# Build Appleseed
# ================================================================

stepInfo $_NAME "Building $_APPLESEED ..."

# apt install dependencies
$DEBUG sudo apt install -y \
  freeglut3-dev \
  libpng-dev \
  libpython2.7-dev \
  liblz4-dev \
  qtbase5-dev \
  zlib1g \
  zlib1g-dev
   

$DEBUG cd $_sRoot

if [ ! -d ./build ]; then
  _bNewBuild=true # build new if no build directory exists yet
fi

if [ $_bNewBuild = true ]; then
  $DEBUG rm -fr build;
fi

$DEBUG mkdir -p build
$DEBUG cd build

# cmake
if [ $_bNewBuild = true ]; then
  stepInfo $_APPLESEED "Configuring CMake ..."
  $DEBUG cmake \
    -Wno-dev \
    -DWARNINGS_AS_ERRORS=OFF \
    -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
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
    -DWITH_CLI=$WITH_CLIENT \
    -DWITH_STUDIO=$WITH_STUDIO \
    -DWITH_BENCH=$WITH_BENCH \
    -DWITH_TOOLS=$WITH_TOOLS \
    -DWITH_PYTHON2_BINDINGS=$WITH_PYTHON2_BINDINGS \
    -DWITH_PYTHON3_BINDINGS=OFF \
    -DWITH_EMBREE=$WITH_EMBREE \
    -DWITH_GPU=OFF \
    -DWITH_SPECTRAL_SUPPORT=OFF \
    -Dhapply_ROOT=$_sHapplyInstallDir \
    ..
  stepInfo $_APPLESEED "Configured CMake."
fi

if [[ -f $_sRoot/sandbox/lib/$BUILD_TYPE/libappleseed.so && $_bNewBuild = false ]]; then
  stepInfo $_NAME "Appleseed is already built. (Add -n or --new to re-build.)"
else
  stepInfo $_APPLESEED "Building ..."
  $DEBUG make $_sVerbose -j$(nproc)
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
  if [[ $WITH_EMBREE = ON ]]; then collect $_sCollectType $_sEmbreeInstallDir; fi
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
