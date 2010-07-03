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
 * $Id: LibWWWNetAccessor.hpp 568078 2007-08-21 11:43:25Z amassari $
 */


#if !defined(LIBWWWNETACCESSOR_HPP)
#define LIBWWWNETACCESSOR_HPP


#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/util/XMLURL.hpp>
#include <xercesc/util/BinInputStream.hpp>
#include <xercesc/util/XMLNetAccessor.hpp>

XERCES_CPP_NAMESPACE_BEGIN

//
// This class is the wrapper for the libWWW library which provides
// support for HTTP and other network protocols, so that URL's using
// these protocols can be used in system id's in the XML decl clauses.
//

class XMLUTIL_EXPORT LibWWWNetAccessor : public XMLNetAccessor
{
public :
    LibWWWNetAccessor();
    ~LibWWWNetAccessor();

    BinInputStream* makeNew(const XMLURL&  urlSource, const XMLNetHTTPInfo* httpInfo=0);
    const XMLCh* getId() const;

private :
    static const XMLCh fgMyName[];

    LibWWWNetAccessor(const LibWWWNetAccessor&);
    LibWWWNetAccessor& operator=(const LibWWWNetAccessor&);

}; // LibWWWNetAccessor

inline const XMLCh* LibWWWNetAccessor::getId() const
{
    return fgMyName;
}

XERCES_CPP_NAMESPACE_END

#endif // LIBWWWNETACCESSOR_HPP
