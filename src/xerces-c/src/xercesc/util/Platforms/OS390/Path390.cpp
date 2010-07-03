
/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * $Id: Path390.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

//#include "stdafx.h"
#include <fstream.h>
#include <stdio.h>
#include <ctype.h>
#include <typeinfo>
#define _XOPEN_SOURCE_EXTENDED 1
#include <stdlib.h>
#include <string.h>
#include "Path390.hpp"
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/framework/MemoryManager.hpp>


XERCES_CPP_NAMESPACE_BEGIN

//Constructors:
Path390::Path390() {
   _error = 0;
   _absolute = false;
   _dsnabsolute = false;
   _parsestate=PARSE_NONE;
   _orgparms=0;
   _resultpath=0;
   _orgpath=0;
}
Path390::Path390(char * s) {
   _error = 0;
   _absolute = false;
   _dsnabsolute = false;
   _parsestate=PARSE_NONE;
   _orgparms=0;
   _resultpath=0;
   _orgpath=0;
   setPath(s);
}


//Destructor:
Path390::~Path390() {
   if (_orgparms)
      XMLPlatformUtils::fgMemoryManager->deallocate(_orgparms);//free (_orgparms);
   if (_resultpath)
      XMLPlatformUtils::fgMemoryManager->deallocate(_resultpath);//free(_resultpath);
   if (_orgpath)
      XMLPlatformUtils::fgMemoryManager->deallocate(_orgpath);//free(_orgpath);
}

// This path parser is state driven in order to support an incremental parse of the input path.
// This is so that, for example, someone only wants to determine whether the path is absolute or
// relative, then it will only parse sufficient information to determine this. This information
// is saved in the object so that this does not need to be re-parsed if later one wanted to
// retrieve, for example, the parameters.
// The states are:
//    PARSE_NONE - initial state, nothing is parsed.
//    PARSE_ABSOLUTE_URI - Absolute or relative path has been determined.
//    PARSE_PATHTYPE - The type of the path has been determined
//    PARSE_PUNCT - The important delimiters have been located to make later parsing simpler.
//    PARSE_PARMS - The parms have been located and retrieved.
//    PARSE_PARSED - The path has been fully parsed.
//
// Each of the following methods handle the parsing corresponding to each state.
//




// Determine if the path is absolute or relative.
void Path390::_determine_uri_abs() {
   if (_parsestate == PARSE_NONE) {
      if (*_curpos == '/') {
         _uriabsolute=true;
         _curpos++;
      } else
         _uriabsolute=false;
      _parsestate = PARSE_ABSOLUTE_URI;
   }
}


// Determine the path type. This could be:
//    PATH390_HFS - format is hfs:/xxx/xxx...
//    PATH390_DSN1 - format is dsn:/xxx/xxx...
//    PATH390_DSN2 - format is dsn://xxxxx...
//    PATH390_DD - format is dd:xxx...
//    PATH390_OTHER - format is any other paths.
void Path390::_determine_type() {
   char firstfour[5];
   if (_parsestate == PARSE_ABSOLUTE_URI) {
      char * ff=firstfour;
      int ffi=0;
      while ((_curpos[ffi]) && (ffi<4)) {ff[ffi] = toupper(_curpos[ffi]); ffi++;}

      if ( (ffi>=4) && !strncmp(firstfour,"DSN:",4) ) {
         _pathtype = PATH390_DSN1;
         _curpos+=4;
         _absolute = true;
         if ( ((*_curpos) && (*_curpos == '/')) && ((*(_curpos+1)) && (*(_curpos+1) == '/')) ) {
            _pathtype = PATH390_DSN2;
            }
         }
      else if ( (ffi>=4) && !strncmp(firstfour,"HFS:",4) ) {
         _pathtype = PATH390_HFS;
         _curpos+=4;
         if (*_curpos == '/')
            _absolute = true;
         else
            _absolute = false;
         }
      else if ( (ffi>=3) && !strncmp(firstfour,"DD:",3) ) {
         _absolute = true;
         _pathtype = PATH390_DD;
         _curpos+=3;
         }
      else {
         _pathtype = PATH390_OTHER;
         if (_uriabsolute)
            _curpos--;
         }
      _parsestate = PARSE_PATHTYPE;
   }
}

// This takes one pass through the path any determines the location of important delimiters
// including / ; . and (
// It will also detect some error conditions
void Path390::_determine_punct() {

   if (_parsestate == PARSE_PATHTYPE) {
      char * source = _curpos;
      _lastsemi = 0;
      _lastslash = 0;
      _lastparen = 0;
      _parmStart = 0;
      _pathEnd = 0;
      _numperiods = 0;
      _numsemicolons = 0;
      _extStart = 0;
      while (*source) {
         switch (*source) {
            case ';':
               _lastsemi = source;
               _parmStart = source+1;
               _numsemicolons++;
               break;
            case '/':
               _lastslash = source;
               _pathEnd = source;
               break;
            case '.':
               _extStart = source+1;
               _numperiods++;
               break;
            case '(':
               _lastparen = source+1;
               break;
         }
         *source++;
      }
      if ( (_parmStart) && (_parmStart<=_pathEnd) ) {
         _error = ERROR_SEMICOLON_NOT_ALLOWED;
         _lastsemi = 0;
         _parmStart = 0;
      }
      if ( ((_pathtype == PATH390_DD) || (_pathtype == PATH390_DSN1) || (_pathtype == PATH390_DSN1)) &&
           (_extStart <= _pathEnd) ) {
         if (_extStart)
            _error = ERROR_PERIOD_NOT_ALLOWED;
         _extStart = 0;
      }
      if (_extStart < _lastparen)
         _extStart = 0;
      _parsestate = PARSE_PUNCT;
   }
}

// This extracts the parameters from the path if there are any. It also determines if the parameters
// contain type=record
void Path390::_determine_parms() {

   if (_parsestate == PARSE_PUNCT) {
      char * tr = 0;
      if (_parmStart) {
         _orgparmlen = strlen(_parmStart)+1;
         _orgparms = (char*) XMLPlatformUtils::fgMemoryManager->allocate(_orgparmlen * sizeof(char));// (char *) malloc(_orgparmlen);
         char * ts=_parmStart;
         char * td=_orgparms;
         while (*ts)
           *td++ = tolower(*ts++);
         *td = 0;
         *_lastsemi = 0;
         tr = strstr(_orgparms,"type=record");
      }
      if (tr)
        _typerecord = tr - _orgparms;
      else
        _typerecord = -1;

      _parsestate = PARSE_PARMS;
   }
}

// Complete the rest of the parse.
void Path390::_parse_rest() {

   if (_parsestate == PARSE_PARMS) {
      char *source;
      char *dest;
      char * filename_start;
      char * tmpPos;
      int pathlen = strlen(_curpos);
      _resultpath = (char*) XMLPlatformUtils::fgMemoryManager->allocate((pathlen+10) * sizeof(char));//(char *) malloc(pathlen+10);

      source = _curpos;
      dest = _resultpath;

      switch (_pathtype) {
         case PATH390_DSN1:
            // This format needs to be mangled from a hierarchical (hfs style) path to the
            // traditional MVS format. First check for some errors.
            if (_lastparen) {
               _error = ERROR_NO_PAREN_ALLOWED;
               break;
            }

            if ((_uriabsolute) && (!_absolute)) {
               _error = ERROR_ABS_PATH_REQUIRED;
               break;
            }

            if ( ((_extStart) && (_numperiods > 1)) ||
                 ((!_extStart) && (_numperiods)) ) {
               _error = ERROR_NO_EXTRA_PERIODS_ALLOWED;
               break;
            }

            if ( ((_parmStart) && (_numsemicolons > 1)) ||
                 ((!_parmStart) && (_numsemicolons)) ) {
               _error = ERROR_NO_EXTRA_SEMIS_ALLOWED;
               break;
            }

            // start out the result with //
            *dest++ = '/';
            *dest++ = '/';
            // If the input path starts with a / then it is absolute and it must be
            // enclosed in 's
            _dsnabsolute = false;
            if (*source == '/') {
              _dsnabsolute = true;
              source++;
            } else if (_uriabsolute) {
               _error = ERROR_MUST_BE_ABSOLUTE;
               break;
            }

            char * pathstart;
            pathstart = source;

            // Add in the ' if this is an absolute path'
            if (_dsnabsolute) *dest++ = '\'';

            // If there is a / in the path....
            tmpPos = source;
            if (_pathEnd > source) {
               // copy everything up to the last /, replacing / with .
               while( source < _pathEnd ) {
                  switch( *source ) {
                      case '/':
                         *dest = '.';
                         break;
                      default:
                         *dest = *source;
                  }
                  dest++; source++;
               }
               // bump past the last /
               source++;
            }

            // Now we try to locate the extension, and copy that.
            filename_start = 0;
            if ( _extStart != NULL ) {
               tmpPos = _extStart;
               if ( (*tmpPos != '\0') && (*tmpPos != ';') && (source != pathstart) )
                  *dest++='.';
               while ( (*tmpPos != '\0') && (*tmpPos != ';') ) {
                  *dest++ = *tmpPos++;
               }
               // if there is a filename, add a (
               if (source < (_extStart-1)) {
                 filename_start = tmpPos;
                 *dest++ = '(';
                 }
            }
            else if (source != pathstart)
               *dest++ = '.';

            // Now we copy in the filename.
            tmpPos = source;
            while( ((*tmpPos != '\0') && (*tmpPos != ';'))  && ((_extStart == NULL) || (tmpPos < (_extStart-1))) ) {
               *dest++ = *tmpPos++;
            }

            // Finally cap off the filename with optional ")"
            if ( (_extStart != NULL) && (filename_start) ) *dest++ = ')';

            // Add on the ending ' if necessary.
            if (_dsnabsolute) *dest++ = '\'';

            // make it a null terminated string.
            *dest = '\0';
            break;
         case PATH390_HFS:
            // it is in hfs: format. If it is relative, then add on a ./ otherwise
            // just copy the string.
            if (!_absolute) {
               if (_uriabsolute) {
                  _error = ERROR_MUST_BE_ABSOLUTE;
                  break;
               }
               *dest++='.';
               *dest++='/';
            }
            strcpy(dest,source);
            break;
         case PATH390_DD:
            // It's in dd: format. This is similar to the dsn: format, just shorter.
            // Start it out with dd:
            *dest++='D';
            *dest++='D';
            *dest++=':';

            tmpPos = source;
            // if there is a / present in the path...
            if (_pathEnd > source) {
               // copy everything up to the last /, replacing / with .
               while( source < _pathEnd ) {
                  switch( *source ) {
                      case '/':
                         *dest = '.';
                         break;
                      default:
                         *dest = *source;
                  }
                  dest++; source++;
               }
               // bump past the last /
               source++;
            }

            // Now we try to locate the extension, and copy that.
            filename_start = 0;
            if ( _extStart != NULL ) {
               tmpPos = _extStart;
               if ( (*tmpPos != '\0') && (*tmpPos != ';') && (source != _curpos) )
                  *dest++='.';
               while ( (*tmpPos != '\0') && (*tmpPos != ';') ) {
                  *dest++ = *tmpPos++;
               }
               // if there is a filename, add a (
               if (source < (_extStart-1)) {
                 filename_start = tmpPos;
                 *dest++ = '(';
                 }
            }
            else if (source != _curpos)
               *dest++ = '.';

            // Now we copy in the filename.
            tmpPos = source;
            while( ((*tmpPos != '\0') && (*tmpPos != ';'))  && ((_extStart == NULL) || (tmpPos < (_extStart-1))) ) {
               *dest++ = *tmpPos++;
            }

            // Finally cap off the filename with optional ")"
            if ( (_extStart != NULL) && (filename_start) ) *dest++ = ')';

            *dest = '\0';
            break;
         case PATH390_DSN2:
            // This is in dsn: format with the traditional MVS dataset name. Just fall into
            // the default case to copy the path to the destination after making sure that
            // there are no extra slashes.
            {
            int lastslash=5;
            if (_uriabsolute)
              lastslash=6;
            if ( (_lastslash) && ((_lastslash-_orgpath)>lastslash) ) {
               _error = ERROR_BAD_DSN2;
               break;
            }
            }
         default:
            // for all other cases simply copy over the string.
            strcpy(dest,source);
            break;
      }
      _parsestate = PARSE_PARSED;
   }


}
// Public methods start here:

// This sets a new path into the object. Re-initialize everything and do an initial
// parse.
void Path390::setPath(char * s) {
   if (_orgparms)
      XMLPlatformUtils::fgMemoryManager->deallocate(_orgparms);//free (_orgparms);
   if (_resultpath)
      XMLPlatformUtils::fgMemoryManager->deallocate(_resultpath);//free(_resultpath);
   if (_orgpath)
      XMLPlatformUtils::fgMemoryManager->deallocate(_orgpath);//free(_orgpath);
   _error = 0;
   _orgparms = 0;
   _resultpath = 0;
   _absolute = false;
   _dsnabsolute = false;
   _orglen = strlen(s);
   _orgpath = (char*) XMLPlatformUtils::fgMemoryManager->allocate((_orglen+1) * sizeof(char));//(char *) malloc(_orglen+1);
   strcpy(_orgpath,s);
   _curpos = _orgpath;
   _parsestate=PARSE_NONE;

   // Do an initial parse...
   _determine_uri_abs();
   _determine_type();
}

// Do the parse to completion and return any errors found.
int Path390::fullParse() {

   // Do an initial parse...
   _determine_uri_abs();
   _determine_type();
   _determine_punct();
   if (_error) {
//      printf("found error-%d\n",_error);
      return _error;
   }
   _determine_parms();

   _parse_rest();
   return _error;
}

// Get the path in a format which is required by fopen. First make sure that the path is
// completely parsed
char * Path390::getfopenPath() {

   _determine_uri_abs();
   _determine_type();
   _determine_punct();
   if (_error)  {
//      printf("found error-%d\n",_error);
      return 0;
   }
   _determine_parms();
   _parse_rest();
   if (_error)  {
//      printf("found error-%d\n",_error);
      return 0;
   }

   if (_resultpath[0])
      return _resultpath;
   else
      return 0;
}


// Get the parms in a format which is required by fopen. First make sure that the path is
// completely parsed
char * Path390::getfopenParms() {
   _determine_uri_abs();
   _determine_type();
   _determine_punct();
   if (_error)  {
//      printf("found error-%d\n",_error);
      return 0;
   }
   _determine_parms();
   _parse_rest();
   if (_error)  {
//      printf("found error-%d\n",_error);
      return 0;
   }

   if ( (_orgparms) && (_orgparms[0]) )
      return _orgparms;
   else
      return 0;
}

// return whether there is type=record parameter in the parameter list.
bool Path390::isRecordType() {
   _determine_uri_abs();
   _determine_type();
   _determine_punct();
   if (_error)  {
//      printf("found error-%d\n",_error);
      return false;
   }
   _determine_parms();
   _parse_rest();
   if (_error)  {
//      printf("found error-%d\n",_error);
      return false;
   }
   if ( (_orgparms) && (_typerecord>=0) )
      return true;
   else
      return false;
}

// This returns the path type
int Path390::getPathType() {
   _determine_uri_abs();
   _determine_type();
   return _pathtype;
}

// This returns the error code which was found when the path was parsed
int Path390::getError() {
   _determine_uri_abs();
   _determine_type();
   _determine_punct();
   if (_error)  {
 //     return _error;
   }
   _determine_parms();
   _parse_rest();
   if (_error)  {
 //     return _error;
   }
   return _error;
}

// returns whether the path is relative or absolute.
bool Path390::isRelative() {
   _determine_uri_abs();
   _determine_type();
return !(_absolute|_uriabsolute);
}

XERCES_CPP_NAMESPACE_END
