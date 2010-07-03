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
 * $Id: utypes.h 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef UTYPES_H
#define UTYPES_H
#include <wchar.h>
#include <stdlib.h>

/*===========================================================================*/
/* Include platform-dependent definitions                                    */
/* which are contained in the platform-specific file platform.h              */
/*===========================================================================*/

#include "pos400.h"

XERCES_CPP_NAMESPACE_BEGIN

/* XP_CPLUSPLUS is a cross-platform symbol which should be defined when
   using C++.  It should not be defined when compiling under C. */
#ifdef __cplusplus
#   ifndef XP_CPLUSPLUS
#       define XP_CPLUSPLUS
#   endif
#else
#   undef XP_CPLUSPLUS
#endif

/*===========================================================================*/
/* Boolean data type                                                         */
/*===========================================================================*/

#if ! HAVE_BOOL_T
typedef int8_t bool_t;
#endif

#ifndef TRUE
#   define TRUE  1
#endif
#ifndef FALSE
#   define FALSE 0
#endif

/*===========================================================================*/
/* Unicode string offset                                                     */
/*===========================================================================*/
typedef int32_t UTextOffset;

/*===========================================================================*/
/* Unicode character                                                         */
/*===========================================================================*/
typedef uint16_t UChar;


/*===========================================================================*/
/* For C wrappers, we use the symbol U_CAPI.                                 */
/* This works properly if the includer is C or C++.                          */
/* Functions are declared   U_CAPI return-type U_EXPORT2 function-name() ... */
/*===========================================================================*/

#ifdef XP_CPLUSPLUS
#   define U_CFUNC extern "C"
#   define U_CDECL_BEGIN extern "C" {
#   define U_CDECL_END   }
#else
#   define U_CFUNC
#   define U_CDECL_BEGIN
#   define U_CDECL_END
#endif
#define U_CAPI U_CFUNC U_EXPORT


/* Define NULL pointer value  if it isn't already defined */

#ifndef NULL
#ifdef XP_CPLUSPLUS
#define NULL    0
#else
#define NULL    ((void *)0)
#endif
#endif

/* Maximum value of a (void*) - use to indicate the limit of
   an 'infinite' buffer.  */
#define U_MAX_PTR ((void*)-1)



/*===========================================================================*/
/* UErrorCode                                                                */
/*===========================================================================*/

/** Error code to replace exception handling */
#ifdef __OS400__
enum UErrorCode1 {
#else
enum UErrorCode {
#endif
    U_ERROR_INFO_START        = -128,     /* Start of information results (semantically successful) */
    U_USING_FALLBACK_ERROR    = -128,
    U_USING_DEFAULT_ERROR     = -127,
    U_ERROR_INFO_LIMIT,

    U_ZERO_ERROR              =  0,       /* success */

    U_ILLEGAL_ARGUMENT_ERROR  =  1,       /* Start of codes indicating failure */
    U_MISSING_RESOURCE_ERROR  =  2,
    U_INVALID_FORMAT_ERROR    =  3,
    U_FILE_ACCESS_ERROR       =  4,
    U_INTERNAL_PROGRAM_ERROR  =  5,       /* Indicates a bug in the library code */
    U_MESSAGE_PARSE_ERROR     =  6,
    U_MEMORY_ALLOCATION_ERROR =  7,       /* Memory allocation error */
    U_INDEX_OUTOFBOUNDS_ERROR =  8,
    U_PARSE_ERROR             =  9,       /* Equivalent to Java ParseException */
    U_INVALID_CHAR_FOUND      = 10,       /* In the Character conversion routines: Invalid character or sequence was encountered*/
    U_TRUNCATED_CHAR_FOUND    = 11,       /* In the Character conversion routines: More bytes are required to complete the conversion successfully*/
    U_ILLEGAL_CHAR_FOUND      = 12,       /* In codeset conversion: a sequence that does NOT belong in the codepage has been encountered*/
    U_INVALID_TABLE_FORMAT    = 13,       /* Conversion table file found, but corrupted*/
    U_INVALID_TABLE_FILE      = 14,       /* Conversion table file not found*/
    U_BUFFER_OVERFLOW_ERROR   = 15,       /* A result would not fit in the supplied buffer */
    U_UNSUPPORTED_ERROR       = 16,       /* Requested operation not supported in current context */
    U_ERROR_LIMIT
};
#ifdef __OS400__
typedef int UErrorCode;
#define U_SUCCESS(x) ((x)<=U_ZERO_ERROR)
#define U_FAILURE(x) ((x)>U_ZERO_ERROR)
#else
#ifndef XP_CPLUSPLUS
typedef enum UErrorCode UErrorCode;
#endif
#endif


/* Use the following to determine if an UErrorCode represents */
/* operational success or failure. */
#ifndef __OS400__
#ifdef XP_CPLUSPLUS
inline bool_t U_SUCCESS(UErrorCode code) { return (bool_t)(code<=U_ZERO_ERROR); }
inline bool_t U_FAILURE(UErrorCode code) { return (bool_t)(code>U_ZERO_ERROR); }
#else
#define U_SUCCESS(x) ((x)<=U_ZERO_ERROR)
#define U_FAILURE(x) ((x)>U_ZERO_ERROR)
#endif
#endif

/* Casting function for int32_t (backward compatibility version, here until
   T_INT32 is replaced) */
#define T_INT32(i) ((int32_t)i)


/*===========================================================================*/
/* Debugging                                                                 */
/*===========================================================================*/

/* remove this */

/* This function is useful for debugging; it returns the text name */
/* of an UErrorCode result.  This is not the most efficient way of */
/* doing this but it's just for Debug builds anyway. */

/* Do not use these arrays directly: they will move to a .c file! */
static const char *
_uErrorInfoName[U_ERROR_INFO_LIMIT-U_ERROR_INFO_START]={
    "U_USING_FALLBACK_ERROR",
    "U_USING_DEFAULT_ERROR"
};

static const char *
_uErrorName[U_ERROR_LIMIT]={
    "U_ZERO_ERROR",

    "U_ILLEGAL_ARGUMENT_ERROR",
    "U_MISSING_RESOURCE_ERROR",
    "U_INVALID_FORMAT_ERROR",
    "U_FILE_ACCESS_ERROR",
    "U_INTERNAL_PROGRAM_ERROR",
    "U_MESSAGE_PARSE_ERROR",
    "U_MEMORY_ALLOCATION_ERROR",
    "U_INDEX_OUTOFBOUNDS_ERROR",
    "U_PARSE_ERROR",
    "U_INVALID_CHAR_FOUND",
    "U_TRUNCATED_CHAR_FOUND",
    "U_ILLEGAL_CHAR_FOUND",
    "U_INVALID_TABLE_FORMAT",
    "U_INVALID_TABLE_FILE",
    "U_BUFFER_OVERFLOW_ERROR",
    "U_UNSUPPORTED_ERROR"
};

#ifdef XP_CPLUSPLUS
inline const char *
errorName(UErrorCode code)
{
    if(code>=0 && code<U_ERROR_LIMIT) {
        return _uErrorName[code];
    } else if(code>=U_ERROR_INFO_START && code<U_ERROR_INFO_LIMIT) {
        return _uErrorInfoName[code-U_ERROR_INFO_START];
    } else {
        return "[BOGUS UErrorCode]";
    }
}
#else
#   define errorName(code) \
        ((code)>=0 && (code)<U_ERROR_LIMIT) ? \
            _uErrorName[code] : \
            ((code)>=U_ERROR_INFO_START && (code)<U_ERROR_INFO_LIMIT) ? \
                _uErrorInfoName[code-U_ERROR_INFO_START] : \
                "[BOGUS UErrorCode]"
#endif

XERCES_CPP_NAMESPACE_END

#endif /* _UTYPES */
