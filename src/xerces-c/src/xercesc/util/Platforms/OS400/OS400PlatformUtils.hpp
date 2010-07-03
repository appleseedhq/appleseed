#if (__OS400_TGTVRM__>=510)                               /* @01a */
    #pragma datamodel(P128)                               /* @01a */
#endif                                                    /* @01a */

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
 * $Id: OS400PlatformUtils.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef _OS400PLATFORMUTILS_H
#define _OS400PLATFORMUTILS_H

XERCES_CPP_NAMESPACE_BEGIN

void  send_message (char * text, char * messageid, char type);
void  convert_errno(char *,int);

#define FILE_OPEN_PROBLEMS      "XMLBED3" /* file open failure           */
#define ICONV_CONVERT_PROBLEM	"XMLBED4" /* failed to convert ICONV     */
#define ICONV_CCSID_PROBLEM     "XMLBED5"
#define GENERAL_PANIC_MESSAGE   "XMLBEED" /* iconv initialization problem     */

XERCES_CPP_NAMESPACE_END

#endif /* _OS400PLATFORMUTILS_H */

#if (__OS400_TGTVRM__>=510)                                /* @01a */  
     #pragma datamodel(pop)                                /* @01a */ 
#endif                                                     /* @01a */

