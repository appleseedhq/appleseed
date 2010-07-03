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

/**
 * $Id: LibWWWNetAccessor.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/XMLUni.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/NetAccessors/libWWW/BinURLInputStream.hpp>
#include <xercesc/util/NetAccessors/libWWW/LibWWWNetAccessor.hpp>

#include <WWWInit.h>

XERCES_CPP_NAMESPACE_BEGIN

const XMLCh LibWWWNetAccessor::fgMyName[] =
{
    chLatin_l, chLatin_i, chLatin_b, chLatin_W, chLatin_W, chLatin_W,
    chLatin_N, chLatin_e, chLatin_t, chLatin_A, chLatin_c, chLatin_c,
    chLatin_e, chLatin_s, chLatin_s, chLatin_o, chLatin_r, chNull
};


LibWWWNetAccessor::LibWWWNetAccessor()
{
    //
    // Initialize the libWWW library here.
    //
    HTProfile_newPreemptiveClient("XercesC", gXercesFullVersionStr);
    HTConversion_add(HTFormat_conversion(), "text/xml",         "*/*", HTThroughLine, 1.0, 0.0, 0.0);
    HTConversion_add(HTFormat_conversion(), "application/xml",  "*/*", HTThroughLine, 1.0, 0.0, 0.0);
#ifdef XML_DEBUG
    HTSetTraceMessageMask("sop");
#endif
    HTAlert_setInteractive(NO);
    HTHost_setEventTimeout(5000);
}


LibWWWNetAccessor::~LibWWWNetAccessor()
{
    // Cleanup the libWWW library here.

    /* Quote from http://www.w3.org/Library/src/HTProfil.html#Client:
     *
     * This call also supersedes the termination function for the
     * Library core, HTLibTerminate() so that you don't have to call
     * that after calling this function.
    */
    HTProfile_delete();
}


BinInputStream* LibWWWNetAccessor::makeNew(const XMLURL&  urlSource, const XMLNetHTTPInfo* httpInfo/*=0*/)
{
    XMLURL::Protocols  protocol = urlSource.getProtocol();
    switch(protocol)
    {
        case XMLURL::HTTP:
        {
            if(httpInfo!=0 && httpInfo->fHTTPMethod!=XMLNetHTTPInfo::GET)
                ThrowXML(NetAccessorException, XMLExcepts::NetAcc_UnsupportedMethod);
            BinURLInputStream* retStrm =
                new (urlSource.getMemoryManager()) BinURLInputStream(urlSource);
            return retStrm;
        }

        //
        // These are the only protocols we support now. So throw and
        // unsupported protocol exception for the others.
        //
        default :
            ThrowXMLwithMemMgr(MalformedURLException, XMLExcepts::URL_UnsupportedProto, urlSource.getMemoryManager());
    }
}

XERCES_CPP_NAMESPACE_END
