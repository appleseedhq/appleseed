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
 * $Id: uniconv.h 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef UNICONV_HPP
#define UNICONV_HPP

#include <xercesc/util/XercesDefs.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// This is the type for the returned handles
typedef void * uniconv_t;
#define UNICONV_ERROR (uniconv_t)-1
#define UNICONV_NOHANDLE (uniconv_t)0

// These are the character conversion services APIs. They are modeled
// after the iconv() APIs.
uniconv_t uniconv_open(const char *,const char *);
int uniconv_close(uniconv_t);
int uniconv(uniconv_t cd, char **inbuf,  size_t *inbytesleft,
                          char **outbuf, size_t *outbytesleft);

// These are the case conversion APIs. They use the same handle type as the
// conversion APIs above.
typedef unsigned short unichar_t;
uniconv_t uniconv_toupper_open();
uniconv_t uniconv_tolower_open();
#define uniconv_toupper_close(_a) _uniconv_case_close(_a)
#define uniconv_tolower_close(_a) _uniconv_case_close(_a)
int _uniconv_case_close(uniconv_t handle_area);
unichar_t uniconv_caseit(uniconv_t cd, unichar_t inchar);

XERCES_CPP_NAMESPACE_END

#endif
