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
 * $Id: URLAccessCFBinInputStream.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

#if !defined(URLACCESSCFBININPUTSTREAM_HPP)
#define URLACCESSCFBININPUTSTREAM_HPP


#include <xercesc/util/XMLURL.hpp>
#include <xercesc/util/XMLExceptMsgs.hpp>
#include <xercesc/util/BinInputStream.hpp>

#if defined(__APPLE__)
    //	Framework includes from ProjectBuilder
	#include <CoreServices/CoreServices.h>
#else
    //	Classic includes otherwise
	#include <CFURL.h>
	#include <CFURLAccess.h>
#endif

XERCES_CPP_NAMESPACE_BEGIN

//
// This class implements the BinInputStream interface specified by the XML
// parser.
//

class XMLUTIL_EXPORT URLAccessCFBinInputStream : public BinInputStream
{
public :
    URLAccessCFBinInputStream(const XMLURL&  urlSource);
    ~URLAccessCFBinInputStream();

    unsigned int curPos() const;
    unsigned int readBytes
    (
                XMLByte* const  toFill
        , const unsigned int    maxToRead
    );


private :
    CFDataRef			mDataRef;
    CFIndex				mBytesProcessed;
};


inline unsigned int
URLAccessCFBinInputStream::curPos() const
{
    return mBytesProcessed;
}

XERCES_CPP_NAMESPACE_END

#endif // URLACCESSCFBININPUTSTREAM_HPP
